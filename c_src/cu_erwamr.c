#include <erl_nif.h>
#include <wasm_c_api.h>
#include <wasm_export.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdint.h>
#include "cu_erwamr_imports.h"

typedef struct {
    const char* module_name;
    const char* field_name;
    ERL_NIF_TERM args;
    ERL_NIF_TERM signature;
    char ret_type;
    wasm_val_t result;
    int has_result;
} ImportCallInfo;

typedef struct {
    wasm_instance_t* instance;
    wasm_module_t* module;
    wasm_memory_t* memory;
    wasm_store_t* store;
    ErlNifPid caller_pid;
    int is_running;
} WasmInstanceResource;

typedef struct {
    wasm_module_t* module;
    wasm_store_t* store;
} WasmModuleResource;

typedef struct {
    char* module_name;
    char* field_name;
    char* signature;
    WasmInstanceResource* instance;
} ImportHook;

static ErlNifResourceType* WASM_MODULE_RESOURCE;
static ErlNifResourceType* WASM_INSTANCE_RESOURCE;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;

#define NIF_DEBUG(format, ...) debug_print(__FILE__, __LINE__, format, ##__VA_ARGS__)

void debug_print(const char* file, int line, const char* format, ...) {
    va_list args;
    va_start(args, format);
    printf(".");
    printf("[%s:%d] NIF_DEBUG: ", file, line);
    vprintf(format, args);
    printf("\n");
    va_end(args);
}

int wasm_val_to_erlang(ErlNifEnv* env, wasm_val_t* val, ERL_NIF_TERM* term) {
    switch (val->kind) {
        case WASM_I32: *term = enif_make_int(env, val->of.i32); break;
        case WASM_I64: *term = enif_make_int64(env, val->of.i64); break;
        case WASM_F32: *term = enif_make_double(env, (double)val->of.f32); break;
        case WASM_F64: *term = enif_make_double(env, val->of.f64); break;
        default: return 0;
    }
    return 1;
}

const char* wasm_externtype_kind_to_string(wasm_externkind_t kind) {
    switch (kind) {
        case WASM_EXTERN_FUNC: return "func";
        case WASM_EXTERN_GLOBAL: return "global";
        case WASM_EXTERN_TABLE: return "table";
        case WASM_EXTERN_MEMORY: return "memory";
        default: return "unknown";
    }
}

int erlang_to_wasm_val(ErlNifEnv* env, ERL_NIF_TERM term, wasm_val_t* val, wasm_valkind_t expected_kind) {
    switch (expected_kind) {
        case WASM_I32:
            {
                int temp;
                if (enif_get_int(env, term, &temp)) {
                    val->kind = WASM_I32;
                    val->of.i32 = temp;
                    return 1;
                }
            }
            break;
        case WASM_I64:
            {
                long temp;
                if (enif_get_int64(env, term, &temp)) {
                    val->kind = WASM_I64;
                    val->of.i64 = temp;
                    return 1;
                }
            }
            break;
        case WASM_F32:
            {
                double temp;
                if (enif_get_double(env, term, &temp)) {
                    val->kind = WASM_F32;
                    val->of.f32 = (float)temp;
                    return 1;
                }
            }
            break;
        case WASM_F64:
            if (enif_get_double(env, term, &val->of.f64)) {
                val->kind = WASM_F64;
                return 1;
            }
            break;
        default:
            return 0;
    }
    return 0;
}

wasm_valkind_t erlang_to_wasm_val_char(ErlNifEnv* env, ERL_NIF_TERM term, wasm_val_t* val, char kind) {
    switch (kind) {
        case 'i': return erlang_to_wasm_val(env, term, val, WASM_I32);
        case 'I': return erlang_to_wasm_val(env, term, val, WASM_I64);
        case 'f': return erlang_to_wasm_val(env, term, val, WASM_F32);
        case 'F': return erlang_to_wasm_val(env, term, val, WASM_F64);
        case 'R': return erlang_to_wasm_val(env, term, val, WASM_EXTERNREF);
        case 'V': return erlang_to_wasm_val(env, term, val, WASM_V128);
        case 'c': return erlang_to_wasm_val(env, term, val, WASM_FUNCREF);
        default: return WASM_I32;
    }
}

wasm_trap_t* generic_import_handler(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    ImportHook* import_hook = (ImportHook*)env;
    WasmInstanceResource* instance = import_hook->instance;

    ErlNifEnv* erl_env = enif_alloc_env();

    // Convert args to Erlang terms
    ERL_NIF_TERM arg_list = enif_make_list(erl_env, 0);
    for (size_t i = 0; i < args->size; i++) {
        ERL_NIF_TERM term;
        if (!wasm_val_to_erlang(erl_env, &args->data[i], &term)) {
            enif_free_env(erl_env);
            wasm_message_t message = { 0 };
            wasm_name_new_from_string_nt(&message, "Failed to convert argument to Erlang term");
            return wasm_trap_new(instance->store, &message);
        }
        arg_list = enif_make_list_cell(erl_env, term, arg_list);
    }

    ERL_NIF_TERM reversed_list;
    enif_make_reverse_list(erl_env, arg_list, &reversed_list);

    // Prepare the message to send to the Erlang side
    ERL_NIF_TERM message = enif_make_tuple4(erl_env,
        enif_make_atom(erl_env, "import"),
        enif_make_atom(erl_env, import_hook->module_name),
        enif_make_atom(erl_env, import_hook->field_name),
        enif_make_tuple2(erl_env, reversed_list, enif_make_string(erl_env, import_hook->signature, ERL_NIF_LATIN1))
    );

    // Send the message to the caller process
    enif_send(NULL, &instance->caller_pid, NULL, message);

    // Receive the response
    ERL_NIF_TERM response;
    int received = enif_recv(erl_env, NULL, &response, 5000); // 5 second timeout

    if (!received) {
        enif_free_env(erl_env);
        return wasm_trap_new(instance->store, "Timeout waiting for import function response");
    }

    // Convert the response back to WASM values
    if (enif_is_tuple(erl_env, response)) {
        const ERL_NIF_TERM* tuple;
        int arity;
        if (enif_get_tuple(erl_env, response, &arity, &tuple) && arity == 2 && enif_is_atom(erl_env, tuple[0])) {
            char atom[256];
            if (enif_get_atom(erl_env, tuple[0], atom, sizeof(atom), ERL_NIF_LATIN1) && strcmp(atom, "ok") == 0) {
                if (!erlang_to_wasm_val(erl_env, tuple[1], &results->data[0], results->data[0].kind)) {
                    enif_free_env(erl_env);
                    return wasm_trap_new(instance->store, "Failed to convert result to WASM value");
                }
            } else {
                enif_free_env(erl_env);
                return wasm_trap_new(instance->store, "Import function returned an error");
            }
        }
    }

    enif_free_env(erl_env);
    return NULL;
}

#define CLEANUP_AND_RETURN_ERROR(erl_env, message) do { \
    cleanup_resources(&args, &results, &exports, &export_types); \
    return enif_make_tuple2(erl_env, atom_error, enif_make_string(erl_env, message, ERL_NIF_LATIN1)); \
} while(0)

static void cleanup_resources(wasm_val_vec_t* args, wasm_val_vec_t* results, 
                              wasm_extern_vec_t* exports, wasm_exporttype_vec_t* export_types) {
    if (args) wasm_val_vec_delete(args);
    if (results) wasm_val_vec_delete(results);
    if (exports) wasm_extern_vec_delete(exports);
    if (export_types) wasm_exporttype_vec_delete(export_types);
}

static void cleanup_wasm_instance(ErlNifEnv* env, void* obj) {
    WasmInstanceResource* res = (WasmInstanceResource*)obj;
    if (res->instance) wasm_instance_delete(res->instance);
    if (res->store) wasm_store_delete(res->store);
}

// Helper function to create a binary term without null terminator
ERL_NIF_TERM make_binary_term(ErlNifEnv* env, const char* data, size_t size) {
    // Check if the last character is a null terminator
    if (size > 0 && data[size - 1] == '\0') {
        size--; // Decrease size to exclude null terminator
    }
    return enif_make_binary(env, &(ErlNifBinary){.size = size, .data = (unsigned char*)data});
}

// Helper function to convert wasm_valtype_t to char
char wasm_valtype_kind_to_char(const wasm_valtype_t* valtype) {
    switch (wasm_valtype_kind(valtype)) {
        case WASM_I32: return 'i';
        case WASM_I64: return 'I';
        case WASM_F32: return 'f';
        case WASM_F64: return 'F';
        case WASM_EXTERNREF: return 'R';
        case WASM_V128: return 'V';
        case WASM_FUNCREF: return 'c';
        default: return '?';
    }
}

int get_function_sig(const wasm_externtype_t* type, char* type_str) {
    if (wasm_externtype_kind(type) == WASM_EXTERN_FUNC) {
        const wasm_functype_t* functype = wasm_externtype_as_functype_const(type);
        const wasm_valtype_vec_t* params = wasm_functype_params(functype);
        const wasm_valtype_vec_t* results = wasm_functype_results(functype);

        type_str[0] = '(';
        size_t offset = 1;

        for (size_t i = 0; i < params->size; ++i) {
            type_str[offset++] = wasm_valtype_kind_to_char(params->data[i]);
        }
        type_str[offset++] = ')';

        for (size_t i = 0; i < results->size; ++i) {
            type_str[offset++] = wasm_valtype_kind_to_char(results->data[i]);
        }
        type_str[offset] = '\0';

        return 1;
    }
    return 0;
}

// New helper function to get function type in the "(iIiI)i" format
ERL_NIF_TERM get_function_type_term(ErlNifEnv* env, const wasm_externtype_t* type) {
    char type_str[256];
    if(get_function_sig(type, type_str)) {
        return make_binary_term(env, type_str, strlen(type_str));
    }
    return enif_make_atom(env, "undefined");
}

wasm_functype_t* wasm_functype_new_generic(
    size_t param_count, wasm_valtype_t** params,
    size_t result_count, wasm_valtype_t** results
) {
    wasm_valtype_vec_t param_vec, result_vec;
    wasm_valtype_vec_new(&param_vec, param_count, params);
    wasm_valtype_vec_new(&result_vec, result_count, results);
    return wasm_functype_new(&param_vec, &result_vec);
}

static ERL_NIF_TERM load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary wasm_binary;
    if (argc != 1 || !enif_inspect_binary(env, argv[0], &wasm_binary)) {
        return enif_make_badarg(env);
    }

    wasm_runtime_set_log_level(WASM_LOG_LEVEL_VERBOSE);

    wasm_engine_t* engine = wasm_engine_new();
    wasm_store_t* store = wasm_store_new(engine);

    wasm_byte_vec_t binary;
    wasm_byte_vec_new(&binary, wasm_binary.size, (const wasm_byte_t*)wasm_binary.data);

    wasm_module_t* module = wasm_module_new(store, &binary);
    if (!module) {
        wasm_byte_vec_delete(&binary);
        wasm_store_delete(store);
        wasm_engine_delete(engine);
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Failed to compile module", ERL_NIF_LATIN1));
    }
    wasm_byte_vec_delete(&binary);

    // Get imports
    wasm_importtype_vec_t imports;
    wasm_module_imports(module, &imports);
    // Get exports
    wasm_exporttype_vec_t exports;
    wasm_module_exports(module, &exports);

    // Create Erlang lists for imports and exports
    ERL_NIF_TERM import_list = enif_make_list(env, 0);
    ERL_NIF_TERM export_list = enif_make_list(env, 0);

    // Process imports
    for (int i = 0; i < imports.size; ++i) {
        const wasm_importtype_t* import = imports.data[i];
        const wasm_name_t* module_name = wasm_importtype_module(import);
        const wasm_name_t* name = wasm_importtype_name(import);
        const wasm_externtype_t* type = wasm_importtype_type(import);

        ERL_NIF_TERM module_name_term = make_binary_term(env, module_name->data, module_name->size);
        ERL_NIF_TERM name_term = make_binary_term(env, name->data, name->size);
        ERL_NIF_TERM type_term = enif_make_atom(env, wasm_externtype_kind_to_string(wasm_externtype_kind(type)));
        ERL_NIF_TERM func_type_term = get_function_type_term(env, type);

        ERL_NIF_TERM import_tuple = enif_make_tuple4(env, type_term, module_name_term, name_term, func_type_term);
        import_list = enif_make_list_cell(env, import_tuple, import_list);
        NIF_DEBUG("Adding ImportHook to Erlang: %s.%s", module_name->data, name->data);
    }

    // Process exports
    for (size_t i = 0; i < exports.size; ++i) {
        const wasm_exporttype_t* export = exports.data[i];
        const wasm_name_t* name = wasm_exporttype_name(export);
        const wasm_externtype_t* type = wasm_exporttype_type(export);

        ERL_NIF_TERM name_term = make_binary_term(env, name->data, name->size);
        ERL_NIF_TERM type_term = enif_make_atom(env, wasm_externtype_kind_to_string(wasm_externtype_kind(type)));
        ERL_NIF_TERM func_type_term = get_function_type_term(env, type);

        ERL_NIF_TERM export_tuple = enif_make_tuple3(env, type_term, name_term, func_type_term);
        export_list = enif_make_list_cell(env, export_tuple, export_list);
    }

    // Clean up
    wasm_importtype_vec_delete(&imports);
    wasm_exporttype_vec_delete(&exports);

    WasmModuleResource* module_res = enif_alloc_resource(WASM_MODULE_RESOURCE, sizeof(WasmModuleResource));
    module_res->module = module;
    module_res->store = store;

    ERL_NIF_TERM module_term = enif_make_resource(env, module_res);
    enif_release_resource(module_res);

    // Return the module term along with import and export lists
    return enif_make_tuple4(env, atom_ok, module_term, import_list, export_list);
}

wasm_memory_t* find_memory_export(const wasm_instance_t* instance) {
    wasm_memory_t* memory = NULL;

    // Get the exports from the instance
    wasm_extern_vec_t instance_exports;
    wasm_instance_exports(instance, &instance_exports);

    // Iterate over the exports to find the memory
    for (size_t i = 0; i < instance_exports.size; i++) {
        wasm_extern_t* export = instance_exports.data[i];

        // Check if the export is of memory kind
        if (wasm_extern_kind(export) == WASM_EXTERN_MEMORY) {
            // Cast the export to wasm_memory_t*
            memory = wasm_extern_as_memory(export);
            break; // Stop after finding the first memory
        }
    }

    // Clean up
    wasm_extern_vec_delete(&instance_exports);

    return memory;
}

static ERL_NIF_TERM instantiate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    WasmInstanceResource* instance_res = enif_alloc_resource(WASM_INSTANCE_RESOURCE, sizeof(WasmInstanceResource));
    WasmModuleResource* module_res;
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], WASM_MODULE_RESOURCE, (void**)&module_res)) {
        return enif_make_badarg(env);
    }

    // Get imports
    wasm_importtype_vec_t imports;
    wasm_module_imports(module_res->module, &imports);
    // Create stubs for imports
    wasm_extern_t *stubs[imports.size];
    for (int i = 0; i < imports.size; i++) {
        stubs[i] = NULL;
    }

    for (int i = 0; i < imports.size; ++i) {
        const wasm_importtype_t* import = imports.data[i];
        const wasm_name_t* module_name = wasm_importtype_module(import);
        const wasm_name_t* name = wasm_importtype_name(import);
        const wasm_externtype_t* type = wasm_importtype_type(import);

        printf("Creating callback for %s.%s\n", module_name->data, name->data);
        ImportHook* hook = malloc(sizeof(ImportHook));
        hook->module_name = module_name->data;
        hook->field_name = name->data;
        hook->signature = malloc(256);
        get_function_sig(type, hook->signature);
        hook->instance = instance_res;

        wasm_func_t *stub_func = wasm_func_new_with_env(module_res->store, type, generic_import_handler, hook, NULL);
        stubs[i] = wasm_func_as_extern(stub_func);

        NIF_DEBUG("Added ImportHook: %s.%s", module_name->data, name->data);
    }

    // Create instance!
    wasm_extern_vec_t externs;
    wasm_extern_vec_new(&externs, imports.size, stubs);
    wasm_trap_t* trap = NULL;
    wasm_instance_t* instance = wasm_instance_new(module_res->store, module_res->module, &externs, &trap);
    // wasm_extern_vec_delete(&imports);
    // for (int i = 0; i < imports.size; i++) {
    //     wasm_func_delete(stubs[i]);
    // }

    if (!instance) {
        const char* error_message = "Failed to create WASM instance";
        if (trap) {
            wasm_message_t message;
            wasm_trap_message(trap, &message);
            error_message = (const char*)message.data;
        }
        ERL_NIF_TERM result = enif_make_tuple2(env, atom_error, enif_make_string(env, error_message, ERL_NIF_LATIN1));
        if (trap) {
            wasm_trap_delete(trap);
        }
        return result;
    }

    NIF_DEBUG("Instance created: %p", instance);

    instance_res->instance = instance;
    instance_res->module = module_res->module;
    instance_res->store = module_res->store;
    instance_res->is_running = 0;  // Initialize the flag
    instance_res->memory = find_memory_export(instance);

    ERL_NIF_TERM instance_term = enif_make_resource(env, instance_res);
    enif_release_resource(instance_res);

    return enif_make_tuple2(env, atom_ok, instance_term);
}

static ERL_NIF_TERM call_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 4) return enif_make_badarg(env);

    WasmInstanceResource* instance_res;
    if (!enif_get_resource(env, argv[0], WASM_INSTANCE_RESOURCE, (void**)&instance_res)) {
        return enif_make_badarg(env);
    }

    // Check if the instance is already running
    if (instance_res->is_running) {
        return enif_make_tuple2(env, atom_error, enif_make_atom(env, "instance_already_running"));
    }
    // Set the running flag
    instance_res->is_running = 1;

    ErlNifBinary function_name_binary;
    if (!enif_inspect_binary(env, argv[1], &function_name_binary)) {
        instance_res->is_running = 0;  // Clear the flag if we're returning due to an error
        return enif_make_badarg(env);
    }

    // Ensure the binary is null-terminated for C string operations
    char* function_name = enif_alloc(function_name_binary.size + 1);
    memcpy(function_name, function_name_binary.data, function_name_binary.size);
    function_name[function_name_binary.size] = '\0';

    wasm_extern_vec_t exports;
    wasm_instance_exports(instance_res->instance, &exports);

    wasm_exporttype_vec_t export_types;
    wasm_module_exports(instance_res->module, &export_types);

    wasm_func_t* func = NULL;
    const wasm_functype_t* func_type = NULL;
    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        if (wasm_extern_kind(ext) == WASM_EXTERN_FUNC) {
            const wasm_name_t* name = wasm_exporttype_name(export_types.data[i]);
            if (name && name->size == strlen(function_name) + 1 && 
                strncmp(name->data, function_name, name->size - 1) == 0) {
                func = wasm_extern_as_func(ext);
                func_type = wasm_func_type(func);
                break;
            }
        }
    }

    if (!func) {
        cleanup_resources(NULL, NULL, &exports, &export_types);
        instance_res->is_running = 0;  // Clear the flag if we're returning due to an error
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Function not found", ERL_NIF_LATIN1));
    }

    const wasm_valtype_vec_t* param_types = wasm_functype_params(func_type);
    const wasm_valtype_vec_t* result_types = wasm_functype_results(func_type);

    ERL_NIF_TERM arg_list = argv[2];
    unsigned arg_count;
    if (!enif_get_list_length(env, arg_list, &arg_count) || param_types->size != arg_count) {
        cleanup_resources(NULL, NULL, &exports, &export_types);
        instance_res->is_running = 0;  // Clear the flag if we're returning due to an error
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Invalid argument count", ERL_NIF_LATIN1));
    }

    wasm_val_vec_t args, results;
    wasm_val_vec_new_uninitialized(&args, param_types->size);
    wasm_val_vec_new_uninitialized(&results, result_types->size);

    ERL_NIF_TERM head, tail = arg_list;
    for (size_t i = 0; i < param_types->size; ++i) {
        if (!enif_get_list_cell(env, tail, &head, &tail) ||
            !erlang_to_wasm_val(env, head, &args.data[i], wasm_valtype_kind(param_types->data[i]))) {
            cleanup_resources(&args, &results, &exports, &export_types);
            instance_res->is_running = 0;  // Clear the flag if we're returning due to an error
            return enif_make_tuple2(env, atom_error, enif_make_string(env, "Failed to convert argument", ERL_NIF_LATIN1));
        }
    }

    ErlNifPid* parent_pid = enif_alloc(sizeof(ErlNifPid));
    if (!enif_get_local_pid(env, argv[3], parent_pid)) {
        enif_free(parent_pid);
        return enif_make_badarg(env);
    }
    instance_res->parent_pid = parent_pid;

    wasm_trap_t* trap = wasm_func_call(call_info->func, &call_info->args, &call_info->results);

    ERL_NIF_TERM result_term;
    if (trap) {
        wasm_message_t message;
        wasm_trap_message(trap, &message);
        result_term = enif_make_tuple2(env, atom_error, enif_make_string(env, message.data, ERL_NIF_LATIN1));
        wasm_trap_delete(trap);
        wasm_byte_vec_delete(&message);
    } else if (call_info->results.size == 1 && wasm_val_to_erlang(env, &call_info->results.data[0], &result_term)) {
        result_term = enif_make_tuple2(env, atom_ok, result_term);
    } else {
        result_term = enif_make_tuple2(env, atom_error, enif_make_string(env, "Unexpected result", ERL_NIF_LATIN1));
    }

    enif_send(NULL, &call_info->caller_pid, NULL, result_term);

    // Cleanup
    wasm_val_vec_delete(&call_info->args);
    wasm_val_vec_delete(&call_info->results);
    enif_free(call_info);

    return enif_make_atom(env, "ok");
}

static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");

    WASM_MODULE_RESOURCE = enif_open_resource_type(env, NULL, "wasm_module_resource", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (!WASM_MODULE_RESOURCE) return -1;

    WASM_INSTANCE_RESOURCE = enif_open_resource_type(env, NULL, "wasm_instance_resource", cleanup_wasm_instance, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (!WASM_INSTANCE_RESOURCE) return -1;

    return 0;
}

// NIF function to read WASM memory
static ERL_NIF_TERM read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    WasmInstanceResource* instance_res;
    uint32_t offset;
    uint32_t length;

    if (argc != 3 || !enif_get_resource(env, argv[0], WASM_INSTANCE_RESOURCE, (void**)&instance_res)
        || !enif_get_uint(env, argv[1], &offset) || !enif_get_uint(env, argv[2], &length)) {
        return enif_make_badarg(env);
    }

    byte_t* data = wasm_memory_data(instance_res->memory);
    size_t data_size = wasm_memory_data_size(instance_res->memory);

    if (offset + length > data_size) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "access_out_of_bounds"));
    }

    ERL_NIF_TERM binary_term;
    unsigned char* binary_data = enif_make_new_binary(env, length, &binary_term);
    memcpy(binary_data, data + offset, length);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), binary_term);
}

// NIF function to write WASM memory
static ERL_NIF_TERM write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    WasmInstanceResource* instance_res;
    uint32_t offset;
    ErlNifBinary input_binary;

    if (argc != 3 || !enif_get_resource(env, argv[0], WASM_INSTANCE_RESOURCE, (void**)&instance_res)
        || !enif_get_uint(env, argv[1], &offset) || !enif_inspect_binary(env, argv[2], &input_binary)) {
        return enif_make_badarg(env);
    }

    byte_t* data = wasm_memory_data(instance_res->memory);
    size_t data_size = wasm_memory_data_size(instance_res->memory);

    if (offset + input_binary.size > data_size) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "access_out_of_bounds"));
    }

    memcpy(data + offset, input_binary.data, input_binary.size);

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"load_nif", 1, load_nif},
    {"instantiate_nif", 2, instantiate_nif},
    {"call_nif", 3, call_nif},
    {"execute_wasm_call", 1, execute_wasm_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"read_nif", 3, read_nif},
    {"write_nif", 3, write_nif}
};

ERL_NIF_INIT(cu_erwamr, nif_funcs, nif_load, NULL, NULL, NULL)
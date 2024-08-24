#include <erl_nif.h>
#include <wasm_c_api.h>
#include <string.h>

// Define the resource types
typedef struct {
    wasm_instance_t* instance;
    wasm_module_t* module;  // Added this line
    wasm_store_t* store;
    ErlNifPid caller_pid;
} WasmInstanceResource;

typedef struct {
    wasm_module_t* module;
    wasm_store_t* store;
} WasmModuleResource;

// Declare global variables for resource types
static ErlNifResourceType* WASM_MODULE_RESOURCE;
static ErlNifResourceType* WASM_INSTANCE_RESOURCE;

// Declare global variables for atoms
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;

int wasm_val_to_erlang(ErlNifEnv* env, wasm_val_t* val, ERL_NIF_TERM* term) {
    switch (val->kind) {
        case WASM_I32:
            *term = enif_make_int(env, val->of.i32);
            break;
        case WASM_I64:
            *term = enif_make_int64(env, val->of.i64);
            break;
        case WASM_F32:
            *term = enif_make_double(env, (double)val->of.f32);
            break;
        case WASM_F64:
            *term = enif_make_double(env, val->of.f64);
            break;
        case WASM_EXTERNREF:
        case WASM_FUNCREF:
            *term = enif_make_resource(env, val->of.ref);
            break;
        default:
            return 0;
    }
    return 1;
}

// Function to clean up WASM instance
static void cleanup_wasm_instance(ErlNifEnv* env, void* obj) {
    WasmInstanceResource* res = (WasmInstanceResource*)obj;
    if (res->instance) {
        wasm_instance_delete(res->instance);
    }
    // Note: We don't delete the module here because it's owned by the module resource
    if (res->store) {
        wasm_store_delete(res->store);
    }
}

// Load function
static ERL_NIF_TERM load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary wasm_binary;
    if (argc != 1 || !enif_inspect_binary(env, argv[0], &wasm_binary)) {
        return enif_make_badarg(env);
    }

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

    // Add debug information
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(module, &export_types);
    printf("Module loaded. Number of exports: %zu\n", export_types.size);
    for (size_t i = 0; i < export_types.size; ++i) {
        const wasm_name_t* name = wasm_exporttype_name(export_types.data[i]);
        if (name) {
            printf("Export %zu name: %.*s\n", i, (int)name->size, name->data);
        } else {
            printf("Export %zu name is NULL\n", i);
        }
    }
    wasm_exporttype_vec_delete(&export_types);

    WasmModuleResource* module_res = enif_alloc_resource(WASM_MODULE_RESOURCE, sizeof(WasmModuleResource));
    module_res->module = module;
    module_res->store = store;

    ERL_NIF_TERM module_term = enif_make_resource(env, module_res);
    enif_release_resource(module_res);

    return enif_make_tuple2(env, atom_ok, module_term);
}

// Instantiate function
static ERL_NIF_TERM instantiate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    WasmModuleResource* module_res;
    if (!enif_get_resource(env, argv[0], WASM_MODULE_RESOURCE, (void**)&module_res)) {
        return enif_make_badarg(env);
    }

    // TODO: Handle the import list (argv[1])

    wasm_extern_vec_t imports = WASM_EMPTY_VEC;
    wasm_instance_t* instance = wasm_instance_new(module_res->store, module_res->module, &imports, NULL);

    if (!instance) {
        const char* error_message = "Failed to create WASM instance";
        ERL_NIF_TERM error_term = enif_make_string(env, error_message, ERL_NIF_LATIN1);
        return enif_make_tuple2(env, atom_error, error_term);
    }

    // Debug: Print export information after instantiation
    wasm_extern_vec_t exports;
    wasm_instance_exports(instance, &exports);
    printf("Instance created. Number of exports: %zu\n", exports.size);
    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        wasm_externkind_t kind = wasm_extern_kind(ext);
        printf("Export %zu: Kind: %d", i, kind);
        if (kind == WASM_EXTERN_FUNC) {
            const wasm_exporttype_t* export_type = wasm_extern_type(ext);
            const wasm_name_t* name = wasm_exporttype_name(export_type);
            if (name) {
                printf(", Name: %.*s", (int)name->size, name->data);
            } else {
                printf(", Name: <unnamed>");
            }
        }
        printf("\n");
    }
    wasm_extern_vec_delete(&exports);

    WasmInstanceResource* instance_res = enif_alloc_resource(WASM_INSTANCE_RESOURCE, sizeof(WasmInstanceResource));
    instance_res->instance = instance;
    instance_res->module = module_res->module;  // Store the module
    instance_res->store = module_res->store;

    ERL_NIF_TERM instance_term = enif_make_resource(env, instance_res);
    enif_release_resource(instance_res);

    return enif_make_tuple2(env, atom_ok, instance_term);
}

int erlang_to_wasm_val(ErlNifEnv* env, ERL_NIF_TERM term, wasm_val_t* val, wasm_valkind_t expected_kind) {
    switch (expected_kind) {
        case WASM_I32:
            if (enif_get_int(env, term, &val->of.i32)) {
                val->kind = WASM_I32;
                return 1;
            }
            break;
        case WASM_I64:
            if (enif_get_int64(env, term, &val->of.i64)) {
                val->kind = WASM_I64;
                return 1;
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

// Call function
static ERL_NIF_TERM call_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) {
        return enif_make_badarg(env);
    }

    WasmInstanceResource* instance_res;
    if (!enif_get_resource(env, argv[0], WASM_INSTANCE_RESOURCE, (void**)&instance_res)) {
        return enif_make_badarg(env);
    }

    char function_name[256];
    if (!enif_get_string(env, argv[1], function_name, sizeof(function_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    printf("Looking for function: '%s' (length: %zu)\n", function_name, strlen(function_name));

    wasm_extern_vec_t exports;
    wasm_instance_exports(instance_res->instance, &exports);

    // Get export types from the module
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(instance_res->module, &export_types);

    printf("Number of exports: %zu\n", exports.size);

    wasm_func_t* func = NULL;
    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        wasm_externkind_t kind = wasm_extern_kind(ext);
        
        printf("Export %zu: ", i);
        printf("Kind: %d", kind);

        if (kind == WASM_EXTERN_FUNC) {
            wasm_func_t* f = wasm_extern_as_func(ext);
            printf(", Param arity: %zu, Result arity: %zu", 
                   wasm_func_param_arity(f), wasm_func_result_arity(f));

            const wasm_name_t* name = wasm_exporttype_name(export_types.data[i]);
            printf(", Name pointer: %p", (void*)name);

            if (name) {
                printf(", Name size: %zu", name->size);
                printf(", Name data pointer: %p", (void*)name->data);
                if (name->data) {
                    printf(", Name: '");
                    for (size_t j = 0; j < name->size; ++j) {
                        printf("%c", name->data[j]);
                    }
                    printf("' (hex: ");
                    for (size_t j = 0; j < name->size; ++j) {
                        printf("%02x", (unsigned char)name->data[j]);
                    }
                    printf(")");

                    // Compare lengths, accounting for null terminator
                    if (name->size == strlen(function_name) + 1) {
                        printf(" (Length match, including null terminator)");
                        if (strncmp(name->data, function_name, name->size - 1) == 0) {
                            printf(" (FULL MATCH)");
                            func = f;
                        } else {
                            printf(" (Content mismatch)");
                            for (size_t j = 0; j < name->size - 1; ++j) {
                                if (name->data[j] != function_name[j]) {
                                    printf(" Mismatch at position %zu: '%c' vs '%c'", j, name->data[j], function_name[j]);
                                }
                            }
                        }
                    } else {
                        printf(" (Length mismatch: %zu vs %zu)", name->size, strlen(function_name) + 1);
                    }
                } else {
                    printf(", Name data is NULL");
                }
            } else {
                printf(", Name: <unnamed>");
            }
        }
        printf("\n");
    }

    wasm_exporttype_vec_delete(&export_types);
    
    if (!func) {
        wasm_extern_vec_delete(&exports);
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Function not found", ERL_NIF_LATIN1));
    }

    const wasm_functype_t* func_type = wasm_func_type(func);
    const wasm_valtype_vec_t* param_types = wasm_functype_params(func_type);
    const wasm_valtype_vec_t* result_types = wasm_functype_results(func_type);

    // Get the list of arguments
    ERL_NIF_TERM arg_list = argv[2];
    unsigned arg_count;
    if (!enif_get_list_length(env, arg_list, &arg_count)) {
        wasm_extern_vec_delete(&exports);
        wasm_exporttype_vec_delete(&export_types);
        return enif_make_badarg(env);
    }

    printf("Function '%s' found. Expected parameters: %zu, Provided: %u\n", 
           function_name, param_types->size, arg_count);

    if (param_types->size != arg_count) {
        char error_message[256];
        snprintf(error_message, sizeof(error_message), 
                 "Invalid argument count. Expected %zu, got %u", 
                 param_types->size, arg_count);
        wasm_extern_vec_delete(&exports);
        wasm_exporttype_vec_delete(&export_types);
        return enif_make_tuple2(env, atom_error, enif_make_string(env, error_message, ERL_NIF_LATIN1));
    }

    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, param_types->size);

    ERL_NIF_TERM head, tail;
    tail = arg_list;
    for (size_t i = 0; i < param_types->size; ++i) {
        if (!enif_get_list_cell(env, tail, &head, &tail)) {
            wasm_val_vec_delete(&args);
            wasm_extern_vec_delete(&exports);
            wasm_exporttype_vec_delete(&export_types);
            return enif_make_badarg(env);
        }

        wasm_valkind_t param_kind = wasm_valtype_kind(param_types->data[i]);
        printf("Parameter %zu expected kind: %d\n", i, param_kind);

        if (!erlang_to_wasm_val(env, head, &args.data[i], param_kind)) {
            char error_message[256];
            snprintf(error_message, sizeof(error_message), 
                     "Failed to convert argument %zu to expected type", i);
            wasm_val_vec_delete(&args);
            wasm_extern_vec_delete(&exports);
            wasm_exporttype_vec_delete(&export_types);
            return enif_make_tuple2(env, atom_error, enif_make_string(env, error_message, ERL_NIF_LATIN1));
        }

        printf("Argument %zu converted successfully\n", i);
    }

    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, result_types->size);

    printf("Calling WASM function\n");
    wasm_trap_t* trap = wasm_func_call(func, &args, &results);

    if (trap) {
        wasm_message_t message;
        wasm_trap_message(trap, &message);
        printf("WASM function call resulted in trap: %s\n", message.data);
        ERL_NIF_TERM error_term = enif_make_string(env, message.data, ERL_NIF_LATIN1);
        wasm_trap_delete(trap);
        wasm_byte_vec_delete(&message);
        wasm_val_vec_delete(&args);
        wasm_val_vec_delete(&results);
        wasm_extern_vec_delete(&exports);
        wasm_exporttype_vec_delete(&export_types);
        return enif_make_tuple2(env, atom_error, error_term);
    }

    ERL_NIF_TERM result_term;
    if (results.size == 1) {
        if (!wasm_val_to_erlang(env, &results.data[0], &result_term)) {
            wasm_val_vec_delete(&args);
            wasm_val_vec_delete(&results);
            wasm_extern_vec_delete(&exports);
            wasm_exporttype_vec_delete(&export_types);
            return enif_make_tuple2(env, atom_error, enif_make_string(env, "Failed to convert result to Erlang term", ERL_NIF_LATIN1));
        }
        printf("WASM function call successful. Result kind: %d\n", results.data[0].kind);
    } else {
        printf("Unexpected number of results: %zu\n", results.size);
        wasm_val_vec_delete(&args);
        wasm_val_vec_delete(&results);
        wasm_extern_vec_delete(&exports);
        wasm_exporttype_vec_delete(&export_types);
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Unexpected number of return values", ERL_NIF_LATIN1));
    }

    wasm_val_vec_delete(&args);
    wasm_val_vec_delete(&results);
    wasm_extern_vec_delete(&exports);
    wasm_exporttype_vec_delete(&export_types);
    return enif_make_tuple2(env, atom_ok, result_term);
}

// NIF initialization
static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");

    WASM_MODULE_RESOURCE = enif_open_resource_type(env, NULL, "wasm_module_resource", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (!WASM_MODULE_RESOURCE) {
        return -1;
    }

    WASM_INSTANCE_RESOURCE = enif_open_resource_type(env, NULL, "wasm_instance_resource", cleanup_wasm_instance, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (!WASM_INSTANCE_RESOURCE) {
        return -1;
    }

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"load_nif", 1, load_nif},
    {"instantiate_nif", 2, instantiate_nif},
    {"call_nif", 3, call_nif}
};

ERL_NIF_INIT(cu_erwamr, nif_funcs, nif_load, NULL, NULL, NULL)
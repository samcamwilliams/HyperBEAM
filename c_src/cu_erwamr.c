#include <erl_driver.h>
#include <ei.h>
#include <wasm_c_api.h>
#include <wasm_export.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include "cu_erwamr_imports.h"

typedef struct {
    ErlDrvMutex* providing_response;
    ErlDrvCond* cond;
    int ready;
    char* error_message;
    wasm_val_vec_t result;
} ImportResponse;

typedef struct {
    wasm_engine_t* engine;
    wasm_instance_t* instance;
    wasm_module_t* module;
    wasm_memory_t* memory;
    wasm_store_t* store;
    ErlDrvPort port;
    ErlDrvTermData port_term;
    ErlDrvMutex* is_running;
    char* current_function;
    ei_term* current_args;
    int current_args_length;
    ImportResponse* current_import;
    ErlDrvTermData pid;
    int is_initialized;
    time_t start_time;
} Proc;

typedef struct {
    void* binary;
    long size;
    Proc* proc;
} LoadWasmReq;

typedef struct {
    char* module_name;
    char* field_name;
    char* signature;
    Proc* proc;
} ImportHook;

static ErlDrvTermData atom_ok;
static ErlDrvTermData atom_error;
static ErlDrvTermData atom_import;

#define DRV_DEBUG(format, ...) debug_print(__FILE__, __LINE__, format, ##__VA_ARGS__)

void debug_print(const char* file, int line, const char* format, ...) {
    va_list args;
    va_start(args, format);
    char* thread_name = erl_drv_thread_name(erl_drv_thread_self());
    printf("[DBG_%s @ %s:%d] ", thread_name, file, line);
    vprintf(format, args);
    printf("\n");
    va_end(args);
}

int wasm_val_to_erl_term(ErlDrvTermData* term, wasm_val_t* val, ErlDrvTermData* erl_type) {
    switch (val->kind) {
        case WASM_I32: 
            *erl_type = ERL_DRV_INT;
            *term = (ErlDrvTermData)val->of.i32;
            return 1;
        case WASM_I64: 
            *erl_type = ERL_DRV_INT64;
            *term = (ErlDrvTermData)val->of.i64;
            return 1;
        case WASM_F32: 
            *erl_type = ERL_DRV_FLOAT;
            *term = (ErlDrvTermData)val->of.f32;
            return 1;
        case WASM_F64: 
            *erl_type = ERL_DRV_FLOAT;
            *term = (ErlDrvTermData)val->of.f64;
            return 1;
        default: 
            return 0;
    }
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

int erl_term_to_wasm_val(ErlDrvTermData term, wasm_val_t* val, wasm_valkind_t expected_kind) {
    switch (expected_kind) {
        case WASM_I32:
            if (term == ERL_DRV_INT) {
                val->kind = WASM_I32;
                val->of.i32 = term;
                return 1;
            }
            break;
        case WASM_I64:
            if (term == ERL_DRV_INT64) {
                val->kind = WASM_I64;
                val->of.i64 = term;
                return 1;
            }
            break;
        case WASM_F32:
        case WASM_F64:
            if (term == ERL_DRV_FLOAT) {
                double temp = term;
                if (expected_kind == WASM_F32) {
                    val->kind = WASM_F32;
                    val->of.f32 = (float)temp;
                } else {
                    val->kind = WASM_F64;
                    val->of.f64 = temp;
                }
                return 1;
            }
            break;
        default:
            return 0;
    }
    return 0;
}

wasm_valkind_t erl_term_to_wasm_val_char(ErlDrvTermData term, wasm_val_t* val, char kind) {
    switch (kind) {
        case 'i': return erl_term_to_wasm_val(term, val, WASM_I32);
        case 'I': return erl_term_to_wasm_val(term, val, WASM_I64);
        case 'f': return erl_term_to_wasm_val(term, val, WASM_F32);
        case 'F': return erl_term_to_wasm_val(term, val, WASM_F64);
        // Note: WASM_EXTERNREF, WASM_V128, and WASM_FUNCREF are not directly supported
        // by ErlDrvTermData. You may need to implement custom encoding for these types.
        default: return WASM_I32;
    }
}

// Helper function to convert wasm_valtype_t to char
int wasm_valtype_kind_to_char(const wasm_valtype_t* valtype) {
    switch (wasm_valtype_kind(valtype)) {
        case WASM_I32: return 'i';
        case WASM_I64: return 'I';
        case WASM_F32: return 'f';
        case WASM_F64: return 'F';
        case WASM_EXTERNREF: return 'e';
        case WASM_V128: return 'v';
        case WASM_FUNCREF: return 'f';
        default: return 'u';
    }
}

int get_function_sig(const wasm_externtype_t* type, char* type_str) {
    if (wasm_externtype_kind(type) == WASM_EXTERN_FUNC) {
        DRV_DEBUG("wasm_externtype_kind(type) == WASM_EXTERN_FUNC");
        const wasm_functype_t* functype = wasm_externtype_as_functype_const(type);
        wasm_valtype_vec_t* params = wasm_functype_params(functype);
        wasm_valtype_vec_t* results = wasm_functype_results(functype);

        if(!params || !results) {
            DRV_DEBUG("Export function params/results are NULL");
            return 0;
        }

        type_str[0] = '(';
        size_t offset = 1;

        for (size_t i = 0; i < params->size; i++) {
            type_str[offset++] = wasm_valtype_kind_to_char(params->data[i]);
        }
        type_str[offset++] = ')';

        for (size_t i = 0; i < results->size; i++) {
            type_str[offset++] = wasm_valtype_kind_to_char(results->data[i]);
        }
        type_str[offset] = '\0';

        return 1;
    }
    return 0;
}

void drv_lock(ErlDrvMutex* mutex) {
    DRV_DEBUG("Locking: %p", mutex);
    erl_drv_mutex_lock(mutex);
    DRV_DEBUG("Locked: %p", mutex);
}

void drv_unlock(ErlDrvMutex* mutex) {
    DRV_DEBUG("Unlocking: %p", mutex);
    erl_drv_mutex_unlock(mutex);
    DRV_DEBUG("Unlocked: %p", mutex);
}

void drv_signal(ErlDrvCond* cond, int* ready) {
    DRV_DEBUG("Signaling: %p. Pre-signal ready state: %d", cond, *ready);
    *ready = 1;
    erl_drv_cond_signal(cond);
    DRV_DEBUG("Signaled: %p. Post-signal ready state: %d", cond, *ready);
}

void drv_wait(ErlDrvCond* cond, ErlDrvMutex* mutex, int* ready) {
    DRV_DEBUG("Started to wait: %p. Ready: %d", cond, *ready);
    drv_lock(mutex);
    while (!*ready) {
        DRV_DEBUG("Waiting: %p", cond);
        erl_drv_cond_wait(cond, mutex);
        DRV_DEBUG("Woke up: %p. Ready: %d", cond, *ready);
    }
    drv_unlock(mutex);
    DRV_DEBUG("Finish waiting: %p", cond);
}

wasm_trap_t* generic_import_handler(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    ImportHook* import_hook = (ImportHook*)env;
    Proc* proc = import_hook->proc;

    // Initialize the message object
    ErlDrvTermData msg[(3*2) + ((args->size + 1) * 2) + ((results->size + 1) * 2) + 2];
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_import;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = driver_mk_atom(import_hook->module_name);
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = driver_mk_atom(import_hook->field_name);

    // Encode args
    for (size_t i = 0; i < args->size; i++) {
        ErlDrvTermData term;
        ErlDrvTermData erl_type;
        wasm_val_to_erl_term(&term, &args->data[i], &erl_type);
        msg[msg_index++] = erl_type;
        msg[msg_index++] = term;
    }
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = args->size;

    // Encode function signature
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData) import_hook->signature;
    msg[msg_index++] = strlen(import_hook->signature);

    // Prepare the message to send to the Erlang side
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 5;

    // Send the message to the caller process
    erl_drv_send_term(driver_mk_port(proc->port), proc->pid, msg, sizeof(msg) / sizeof(ErlDrvTermData));

    // Create and initialize a is_running and condition variable for the response
    proc->current_import = driver_alloc(sizeof(ImportResponse));
    proc->current_import->providing_response = erl_drv_mutex_create("response_mutex");
    proc->current_import->cond = erl_drv_cond_create("response_cond");

    // Initialize the result vector and set the required result types 
    wasm_val_vec_new_uninitialized(&proc->current_import->result, results->size);
    for (size_t i = 0; i < results->size; i++) {
        proc->current_import->result.data[i].kind = results->data[i].kind;
    }

    // Wait for the response
    drv_wait(proc->current_import->cond, proc->current_import->providing_response, &proc->current_import->ready);

    // Handle error in the response
    if (proc->current_import->error_message) {
        DRV_DEBUG("Import execution failed. Error message: %s", proc->current_import->error_message);
        wasm_name_t message;
        wasm_name_new_from_string_nt(&message, proc->current_import->error_message);
        wasm_trap_t* trap = wasm_trap_new(proc->store, &message);
        driver_free(proc->current_import);
        proc->current_import = NULL;
        return trap;
    }

    // Convert the response back to WASM values
    for(int i = 0; i < proc->current_import->result.size; i++) {
        results->data[i] = proc->current_import->result.data[i];
        results->data[i].kind = results->data[i].kind;
    }

    // Clean up
    erl_drv_mutex_destroy(proc->current_import->providing_response);
    erl_drv_cond_destroy(proc->current_import->cond);
    driver_free(proc->current_import);
    proc->current_import = NULL;

    return NULL;
}

// Async initialization function
static void async_init(void* raw) {
    DRV_DEBUG("Initializing WASM module");
    LoadWasmReq* mod_bin = (LoadWasmReq*)raw;
    Proc* proc = mod_bin->proc;
    drv_lock(proc->is_running);
    // Initialize WASM engine, store, etc.
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_VERBOSE);
    proc->engine = wasm_engine_new();
    DRV_DEBUG("Created engine");
    proc->store = wasm_store_new(proc->engine);
    DRV_DEBUG("Created store");
    // Load WASM module
    wasm_byte_vec_t binary;
    wasm_byte_vec_new(&binary, mod_bin->size, (const wasm_byte_t*)mod_bin->binary);
    // for (int i = 0; i < binary.size; i++) {
    //     DRV_DEBUG("Byte %d: %d", i, binary.data[i]);
    // }
    proc->module = wasm_module_new(proc->store, &binary);
    DRV_DEBUG("RETURNED FROM MODULE NEW");
    DRV_DEBUG("Module: %p", proc->module);
    if (!proc->module) {
        DRV_DEBUG("Failed to create module");
        wasm_byte_vec_delete(&binary);
        wasm_store_delete(proc->store);
        wasm_engine_delete(proc->engine);
        return;
    }
    //wasm_byte_vec_delete(&binary);
    DRV_DEBUG("Created module");

    // Get imports
    wasm_importtype_vec_t imports;
    wasm_module_imports(proc->module, &imports);
    DRV_DEBUG("Got imports");
    DRV_DEBUG("Imports size: %d", imports.size);
    wasm_extern_t *stubs[imports.size];

    // Get exports
    wasm_exporttype_vec_t exports;
    wasm_module_exports(proc->module, &exports);

    // Create Erlang lists for imports
    ErlDrvTermData* init_msg = driver_alloc(sizeof(ErlDrvTermData) * (2 + (13 * imports.size) + (11 * exports.size)));
    DRV_DEBUG("Allocated init message");
    int msg_i = 0;
    init_msg[msg_i++] = ERL_DRV_ATOM;
    init_msg[msg_i++] = atom_ok;

    // Process imports
    for (int i = 0; i < imports.size; ++i) {
        DRV_DEBUG("Processing import %d", i);
        const wasm_importtype_t* import = imports.data[i];
        const wasm_name_t* module_name = wasm_importtype_module(import);
        const wasm_name_t* name = wasm_importtype_name(import);
        const wasm_externtype_t* type = wasm_importtype_type(import);

        DRV_DEBUG("Import: %s.%s", module_name->data, name->data);

        char type_str[256];
        get_function_sig(type, type_str);

        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom((char*)wasm_externtype_kind_to_string(wasm_externtype_kind(type)));
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)module_name->data;
        init_msg[msg_i++] = module_name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name->data;
        init_msg[msg_i++] = name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)type_str;
        init_msg[msg_i++] = strlen(type_str) - 1;
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 4;

        printf("Creating callback for %s.%s\n", module_name->data, name->data);
        ImportHook* hook = driver_alloc(sizeof(ImportHook));
        hook->module_name = module_name->data;
        hook->field_name = name->data;
        hook->signature = malloc(256);
        get_function_sig(type, hook->signature);
        hook->proc = proc;

        wasm_func_t *stub_func =
            wasm_func_new_with_env(
                proc->store,
                wasm_externtype_as_functype_const(type),
                generic_import_handler,
                hook,
                NULL
            );
        stubs[i] = wasm_func_as_extern(stub_func);

        DRV_DEBUG("Adding ImportHook to Erlang: %s.%s", module_name->data, name->data);
    }

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = imports.size + 1;

    // Create proc!
    wasm_extern_vec_t externs;
    wasm_extern_vec_new(&externs, imports.size, stubs);
    wasm_trap_t* trap = NULL;
    proc->instance = wasm_instance_new(proc->store, proc->module, &externs, &trap);
    if (!proc->instance) {
        DRV_DEBUG("Failed to create WASM proc");
        return;
    }

    for (size_t i = 0; i < exports.size; ++i) {
        DRV_DEBUG("Processing export %d", i);
        const wasm_exporttype_t* export = exports.data[i];
        const wasm_name_t* name = wasm_exporttype_name(export);
        const wasm_externtype_t* type = wasm_exporttype_type(export);
        const char* kind_str = wasm_externtype_kind_to_string(wasm_externtype_kind(type));

        if (strcmp("memory", kind_str) == 0) {
            proc->memory = wasm_extern_as_memory(exports.data[i]);
            DRV_DEBUG("Found memory: %p", proc->memory);
        }
        char* type_str = malloc(256);
        get_function_sig(type, type_str);
        DRV_DEBUG("Export: %s [%s] -> %s", name->data, kind_str, type_str);

        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom(kind_str);
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name->data;
        init_msg[msg_i++] = name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)type_str;
        init_msg[msg_i++] = strlen(type_str);
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 3;
    }

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = (exports.size) + 1;
    init_msg[msg_i++] = ERL_DRV_TUPLE;
    init_msg[msg_i++] = 3;

    DRV_DEBUG("Sending init message to Erlang. Elements: %d", msg_i);

    int send_res = erl_drv_output_term(proc->port_term, init_msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);
    //driver_free(init_msg);

    proc->is_initialized = 1;
    drv_unlock(proc->is_running);
}

wasm_func_t* get_exported_function(Proc* proc, const char* target_name) {
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);
    wasm_func_t* func = NULL;

    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        if (wasm_extern_kind(ext) == WASM_EXTERN_FUNC) {
            const wasm_name_t* exp_name = wasm_exporttype_name(export_types.data[i]);
            if (exp_name && exp_name->size == strlen(target_name) + 1 && 
                strncmp(exp_name->data, target_name, exp_name->size - 1) == 0) {
                func = wasm_extern_as_func(ext);
                break;
            }
        }
    }
    return func;
}

static void async_call(void* raw) {
    Proc* proc = (Proc*)raw;
    drv_lock(proc->is_running);
    char* function_name = proc->current_function;

    // Find the function in the exports
    wasm_func_t* func = get_exported_function(proc, function_name);
    if (!func) {
        DRV_DEBUG("Function not found: %s", function_name);
        return;
    }

    const wasm_functype_t* func_type = wasm_func_type(func);
    const wasm_valtype_vec_t* param_types = wasm_functype_params(func_type);
    const wasm_valtype_vec_t* result_types = wasm_functype_results(func_type);

    wasm_val_vec_t args, results;
    wasm_val_vec_new_uninitialized(&args, param_types->size);
    for (size_t i = 0; i < param_types->size; i++) {
        args.data[i].kind = wasm_valtype_kind(param_types->data[i]);
        switch (args.data[i].kind) {
            case WASM_I32:
                args.data[i].of.i32 = 0;
                break;
            case WASM_I64:
                args.data[i].of.i64 = 0;
                break;
            case WASM_F32:
                args.data[i].of.f32 = 0;
                break;
            case WASM_F64:
                args.data[i].of.f64 = 0;
                break;
            default:
                DRV_DEBUG("Unsupported parameter type: %d", args.data[i].kind);
                return;
        }
    }
    wasm_val_vec_new_uninitialized(&results, result_types->size);
    for (size_t i = 0; i < result_types->size; i++) {
        results.data[i].kind = wasm_valtype_kind(result_types->data[i]);
    }

    // Call the function
    wasm_trap_t* trap = wasm_func_call(func, &args, &results);

    if (trap) {
        DRV_DEBUG("Function call failed");
        return;
    }

    // Send the results back to Erlang
    ErlDrvTermData msg[(results.size * 2) + 1];
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_ok;
    for (size_t i = 0; i < results.size; i++) {
        ErlDrvTermData term;
        ErlDrvTermData erl_type;
        wasm_val_to_erl_term(&term, &results.data[i], &erl_type);
        msg[msg_index++] = erl_type;
        msg[msg_index++] = term;
    }
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = results.size;
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;
    erl_drv_output_term(proc->port_term, msg, msg_index);

    wasm_val_vec_delete(&results);

    drv_unlock(proc->is_running);
}

static ErlDrvData wasm_driver_start(ErlDrvPort port, char *buff) {
    Proc* proc = driver_alloc(sizeof(Proc));
    proc->port = port;
    DRV_DEBUG("Port: %p", proc->port);
    proc->port_term = driver_mk_port(proc->port);
    DRV_DEBUG("Port term: %p", proc->port_term);
    proc->is_running = erl_drv_mutex_create("wasm_instance_mutex");
    proc->is_initialized = 0;
    proc->start_time = time(NULL);
    DRV_DEBUG("Start time: %ld", proc->start_time);
    return (ErlDrvData)proc;
}

static void wasm_driver_stop(ErlDrvData raw) {
    Proc* proc = (Proc*)raw;
    erl_drv_mutex_destroy(proc->is_running);
    // Cleanup WASM resources
    if (proc->is_initialized) {
        wasm_instance_delete(proc->instance);
        wasm_module_delete(proc->module);
        wasm_store_delete(proc->store);
    }
    driver_free(proc);
}

static void wasm_driver_output(ErlDrvData raw, char *buff, ErlDrvSizeT bufflen) {
    DRV_DEBUG("WASM driver output received");
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Port: %p", proc->port);
    DRV_DEBUG("Port term: %p", proc->port_term);

    int index = 0;
    int version;
    int ver_res = ei_decode_version(buff, &index, &version);
    DRV_DEBUG("Received term has version: %d", version);
    DRV_DEBUG("Index: %d. buff_len: %d. buff: %p", index, bufflen, buff);
    int arity;
    ei_decode_tuple_header(buff, &index, &arity);
    DRV_DEBUG("Term arity: %d", arity);

    char command[MAXATOMLEN];
    ei_decode_atom(buff, &index, command);
    DRV_DEBUG("Command: %s", command);
    
    if (strcmp(command, "init") == 0) {
        // Start async initialization
        proc->pid = driver_caller(proc->port);
        DRV_DEBUG("Caller PID: %d", proc->pid);
        int size, type;
        ei_get_type(buff, &index, &type, &size);
        DRV_DEBUG("WASM binary size: %d bytes. Type: %d", size, type);
        void* wasm_binary = driver_alloc(size);
        long size_l = (long)size;
        ei_decode_binary(buff, &index, wasm_binary, &size_l);
        LoadWasmReq* mod_bin = driver_alloc(sizeof(LoadWasmReq));
        mod_bin->proc = proc;
        mod_bin->binary = wasm_binary;
        mod_bin->size = size;
        DRV_DEBUG("Calling for async thread to init");
        driver_async(proc->port, NULL, async_init, mod_bin, NULL);
    } else if (strcmp(command, "call") == 0) {
        if (!proc->is_initialized) {
            DRV_DEBUG("Not initialized");
            return;
        }
        // Extract the function name and the args from the Erlang term and generate the wasm_val_vec_t
        char function_name[MAXATOMLEN];
        ei_decode_string(buff, &index, &function_name);
        DRV_DEBUG("Function name: %s", function_name);
        int arg_length;
        ei_decode_list_header(buff, &index, &arg_length);
        ei_term* args = driver_alloc(sizeof(ei_term) * arg_length);

        for (int i = 0; i < arg_length; i++) {
            ei_decode_ei_term(buff, &index, &args[i]);
            DRV_DEBUG("Decoded arg %d. Type: %d", i, args[i].ei_type);
        }
        proc->current_function = function_name;
        proc->current_args = args;

        driver_async(proc->port, NULL, async_call, proc, NULL);
    } else if (strcmp(command, "import_response") == 0) {
        // Handle import response
        DRV_DEBUG("Import response received. Providing...");
        if (proc->current_import) {
            drv_lock(proc->current_import->providing_response);
            // Decode the result list from the Erlang side into a new wasm_val_vec_t
            int list_length;
            ei_decode_list_header(buff, &index, &list_length);
            wasm_val_vec_new_uninitialized(&proc->current_import->result, list_length);
            for (int i = 0; i < list_length; i++) {
                switch (proc->current_import->result.data[i].kind) {
                    case WASM_I32:
                        ei_decode_long(buff, &index, (long*)&proc->current_import->result.data[i].of.i32);
                        break;
                    case WASM_I64:
                        ei_decode_longlong(buff, &index, &proc->current_import->result.data[i].of.i64);
                        break;
                    case WASM_F32:
                        {
                            double temp;
                            ei_decode_double(buff, &index, &temp);
                            proc->current_import->result.data[i].of.f32 = (float)temp;
                        }
                        break;
                    case WASM_F64:
                        ei_decode_double(buff, &index, &proc->current_import->result.data[i].of.f64);
                        break;
                    default:
                        DRV_DEBUG("Unsupported return type: %d", proc->current_import->result.data[i].kind);
                        proc->current_import->error_message = "Unsupported return type";
                }
            }

            // Signal that the response is ready
            drv_signal(proc->current_import->cond, &proc->current_import->ready);
        } else {
            DRV_DEBUG("No pending import response waiting");
        }

    }
}

static ErlDrvEntry wasm_driver_entry = {
    NULL,
    wasm_driver_start,
    wasm_driver_stop,
    wasm_driver_output,
    NULL,
    NULL,
    "cu_erwamr",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(wasm_driver) {
    atom_ok = driver_mk_atom("ok");
    atom_error = driver_mk_atom("error");
    atom_import = driver_mk_atom("import");
    return &wasm_driver_entry;
}
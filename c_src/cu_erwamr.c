#include <erl_nif.h>
#include <wasm_c_api.h>
#include "wasi_stubs.h"
#include <string.h>

typedef struct {
    wasm_instance_t* instance;
    wasm_module_t* module;
    wasm_store_t* store;
} WasmInstanceResource;

typedef struct {
    wasm_module_t* module;
    wasm_store_t* store;
} WasmModuleResource;

static ErlNifResourceType* WASM_MODULE_RESOURCE;
static ErlNifResourceType* WASM_INSTANCE_RESOURCE;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;

#define CLEANUP_AND_RETURN_ERROR(env, message) do { \
    cleanup_resources(&args, &results, &exports, &export_types); \
    return enif_make_tuple2(env, atom_error, enif_make_string(env, message, ERL_NIF_LATIN1)); \
} while(0)

static void cleanup_resources(wasm_val_vec_t* args, wasm_val_vec_t* results, 
                              wasm_extern_vec_t* exports, wasm_exporttype_vec_t* export_types) {
    if (args) wasm_val_vec_delete(args);
    if (results) wasm_val_vec_delete(results);
    if (exports) wasm_extern_vec_delete(exports);
    if (export_types) wasm_exporttype_vec_delete(export_types);
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
                long long temp;
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

static void cleanup_wasm_instance(ErlNifEnv* env, void* obj) {
    WasmInstanceResource* res = (WasmInstanceResource*)obj;
    if (res->instance) wasm_instance_delete(res->instance);
    if (res->store) wasm_store_delete(res->store);
}

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

    WasmModuleResource* module_res = enif_alloc_resource(WASM_MODULE_RESOURCE, sizeof(WasmModuleResource));
    module_res->module = module;
    module_res->store = store;

    ERL_NIF_TERM module_term = enif_make_resource(env, module_res);
    enif_release_resource(module_res);

    return enif_make_tuple2(env, atom_ok, module_term);
}

static ERL_NIF_TERM instantiate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    WasmModuleResource* module_res;
    if (!enif_get_resource(env, argv[0], WASM_MODULE_RESOURCE, (void**)&module_res)) {
        return enif_make_badarg(env);
    }

    wasm_extern_vec_t imports = WASM_EMPTY_VEC;
    wasm_instance_t* instance = wasm_instance_new(module_res->store, module_res->module, &imports, NULL);

    if (!instance) {
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Failed to create WASM instance", ERL_NIF_LATIN1));
    }

    register_wasi_functions(instance);

    WasmInstanceResource* instance_res = enif_alloc_resource(WASM_INSTANCE_RESOURCE, sizeof(WasmInstanceResource));
    instance_res->instance = instance;
    instance_res->module = module_res->module;
    instance_res->store = module_res->store;

    ERL_NIF_TERM instance_term = enif_make_resource(env, instance_res);
    enif_release_resource(instance_res);

    return enif_make_tuple2(env, atom_ok, instance_term);
}

static ERL_NIF_TERM call_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    WasmInstanceResource* instance_res;
    if (!enif_get_resource(env, argv[0], WASM_INSTANCE_RESOURCE, (void**)&instance_res)) {
        return enif_make_badarg(env);
    }

    char function_name[256];
    if (!enif_get_string(env, argv[1], function_name, sizeof(function_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

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
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Function not found", ERL_NIF_LATIN1));
    }

    const wasm_valtype_vec_t* param_types = wasm_functype_params(func_type);
    const wasm_valtype_vec_t* result_types = wasm_functype_results(func_type);

    ERL_NIF_TERM arg_list = argv[2];
    unsigned arg_count;
    if (!enif_get_list_length(env, arg_list, &arg_count) || param_types->size != arg_count) {
        cleanup_resources(NULL, NULL, &exports, &export_types);
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
            return enif_make_tuple2(env, atom_error, enif_make_string(env, "Failed to convert argument", ERL_NIF_LATIN1));
        }
    }

    wasm_trap_t* trap = wasm_func_call(func, &args, &results);

    if (trap) {
        wasm_message_t message;
        wasm_trap_message(trap, &message);
        ERL_NIF_TERM error_term = enif_make_tuple2(env, atom_error, enif_make_string(env, message.data, ERL_NIF_LATIN1));
        wasm_trap_delete(trap);
        wasm_byte_vec_delete(&message);
        cleanup_resources(&args, &results, &exports, &export_types);
        return error_term;
    }

    ERL_NIF_TERM result_term;
    if (results.size == 1 && wasm_val_to_erlang(env, &results.data[0], &result_term)) {
        cleanup_resources(&args, &results, &exports, &export_types);
        return enif_make_tuple2(env, atom_ok, result_term);
    } else {
        cleanup_resources(&args, &results, &exports, &export_types);
        return enif_make_tuple2(env, atom_error, enif_make_string(env, "Unexpected result", ERL_NIF_LATIN1));
    }
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

static ErlNifFunc nif_funcs[] = {
    {"load_nif", 1, load_nif},
    {"instantiate_nif", 2, instantiate_nif},
    {"call_nif", 3, call_nif}
};

ERL_NIF_INIT(cu_erwamr, nif_funcs, nif_load, NULL, NULL, NULL)
#include <erl_nif.h>
#include <wasm_export.h>
#include <wasm_c_api.h>

static ERL_NIF_TERM hello_world(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello from erWAMR!", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM init_runtime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Initialize WAMR runtime
    RuntimeInitArgs init_args;
    memset(&init_args, 0, sizeof(RuntimeInitArgs));

    init_args.mem_alloc_type = Alloc_With_System_Allocator;
    init_args.max_thread_num = 1;

    if (!wasm_runtime_full_init(&init_args)) {
        return enif_make_tuple2(env, 
            enif_make_atom(env, "error"),
            enif_make_string(env, "Failed to initialize WAMR runtime", ERL_NIF_LATIN1));
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM cleanup_runtime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Cleanup WAMR runtime
    wasm_runtime_destroy();
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"hello_world", 0, hello_world},
    {"init_runtime", 0, init_runtime},
    {"cleanup_runtime", 0, cleanup_runtime}
};

ERL_NIF_INIT(cu_erwamr, nif_funcs, NULL, NULL, NULL, NULL)
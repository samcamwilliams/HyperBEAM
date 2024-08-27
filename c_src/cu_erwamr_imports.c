#include "cu_erwamr_imports.h"

static int32_t wasi_fd_seek(wasm_exec_env_t exec_env, int32_t fd, int64_t offset, int32_t whence, int64_t *newoffset) {
    return (int32_t)generic_import_handler(exec_env, "wasi_snapshot_preview1", "fd_seek", "(iIiI)i", fd, offset, whence, newoffset);
}

static int32_t wasi_fd_read(wasm_exec_env_t exec_env, int32_t fd, int64_t iov_ptr, int32_t iov_cnt, int64_t *nread) {
    return (int32_t)generic_import_handler(exec_env, "wasi_snapshot_preview1", "fd_read", "(iIII)i", fd, iov_ptr, iov_cnt, nread);
}

static int32_t wasi_environ_sizes_get(wasm_exec_env_t exec_env, int32_t *environ_count, int32_t *environ_buf_size) {
    return (int32_t)generic_import_handler(exec_env, "wasi_snapshot_preview1", "environ_sizes_get", "(II)i", environ_count, environ_buf_size);
}

static int32_t wasi_environ_get(wasm_exec_env_t exec_env, int32_t *environ, int32_t environ_buf) {
    return (int32_t)generic_import_handler(exec_env, "wasi_snapshot_preview1", "environ_get", "(II)i", environ, environ_buf);
}

static int32_t wasi_clock_time_get(wasm_exec_env_t exec_env, int32_t clock_id, int64_t precision, int64_t *time) {
    return (int32_t)generic_import_handler(exec_env, "wasi_snapshot_preview1", "clock_time_get", "(iII)i", clock_id, precision, time);
}

static int32_t wasi_fd_write(wasm_exec_env_t exec_env, int32_t fd, int64_t iov_ptr, int32_t iov_cnt, int64_t *nwritten) {
    return (int32_t)generic_import_handler(exec_env, "wasi_snapshot_preview1", "fd_write", "(iIII)i", fd, iov_ptr, iov_cnt, nwritten);
}

static int32_t wasi_fd_close(wasm_exec_env_t exec_env, int32_t fd) {
    return (int32_t)generic_import_handler(exec_env, "wasi_snapshot_preview1", "fd_close", "(i)i", fd);
}

static void wasi_proc_exit(wasm_exec_env_t exec_env, int32_t rval) {
    generic_import_handler(exec_env, "wasi_snapshot_preview1", "proc_exit", "(i)", rval);
}

static int32_t __syscall_unlinkat(wasm_exec_env_t exec_env, int32_t dirfd, int64_t path_ptr, int32_t flags) {
    return (int32_t)generic_import_handler(exec_env, "env", "__syscall_unlinkat", "(iIi)i", dirfd, path_ptr, flags);
}

static int32_t __syscall_readlinkat(wasm_exec_env_t exec_env, int32_t dirfd, int64_t path_ptr, int64_t buf_ptr, int32_t buf_len) {
    return (int32_t)generic_import_handler(exec_env, "env", "__syscall_readlinkat", "(iIII)i", dirfd, path_ptr, buf_ptr, buf_len);
}

static int32_t _emscripten_system(wasm_exec_env_t exec_env, int64_t command_ptr) {
    return (int32_t)generic_import_handler(exec_env, "env", "_emscripten_system", "(I)i", command_ptr);
}

static int32_t __syscall_rmdir(wasm_exec_env_t exec_env, int64_t path_ptr) {
    return (int32_t)generic_import_handler(exec_env, "env", "__syscall_rmdir", "(I)i", path_ptr);
}

static int32_t __syscall_renameat(wasm_exec_env_t exec_env, int32_t old_dirfd, int64_t old_path_ptr, int32_t new_dirfd, int64_t new_path_ptr) {
    return (int32_t)generic_import_handler(exec_env, "env", "__syscall_renameat", "(iIiI)i", old_dirfd, old_path_ptr, new_dirfd, new_path_ptr);
}

static int32_t __syscall_dup3(wasm_exec_env_t exec_env, int32_t oldfd, int32_t newfd, int32_t flags) {
    return (int32_t)generic_import_handler(exec_env, "env", "__syscall_dup3", "(iii)i", oldfd, newfd, flags);
}

// Define the array of native symbols
NativeSymbol wasi_native_symbols[] = {
    {"fd_seek", generic_import_handler, "(iIiI)i", {"wasi_snapshot_preview1", "fd_seek", "(iIiI)i"}},
    {"fd_read", generic_import_handler, "(iIII)i", {"wasi_snapshot_preview1", "fd_read", "(iIII)i"}},
    {"environ_sizes_get", generic_import_handler, "(II)i", {"wasi_snapshot_preview1", "environ_sizes_get", "(II)i"}},
    {"environ_get", generic_import_handler, "(II)i", {"wasi_snapshot_preview1", "environ_get", "(II)i"}},
    {"clock_time_get", generic_import_handler, "(iII)i", {"wasi_snapshot_preview1", "clock_time_get", "(iII)i"}},
    {"fd_write", generic_import_handler, "(iIII)i", {"wasi_snapshot_preview1", "fd_write", "(iIII)i"}},
    {"fd_close", generic_import_handler, "(i)i", {"wasi_snapshot_preview1", "fd_close", "(i)i"}},
    {"proc_exit", generic_import_handler, "(i)", {"wasi_snapshot_preview1", "proc_exit", "(i)"}}
};

NativeSymbol env_native_symbols[] = {
    {"__syscall_unlinkat", generic_import_handler, "(iIi)i", {"env", "__syscall_unlinkat", "(iIi)i"}},
    {"__syscall_readlinkat", generic_import_handler, "(iIII)i", {"env", "__syscall_readlinkat", "(iIII)i"}},
    {"_emscripten_system", generic_import_handler, "(I)i", {"env", "_emscripten_system", "(I)i"}},
    {"__syscall_rmdir", generic_import_handler, "(I)i", {"env", "__syscall_rmdir", "(I)i"}},
    {"__syscall_renameat", generic_import_handler, "(iIiI)i", {"env", "__syscall_renameat", "(iIiI)i"}},
    {"__syscall_dup3", generic_import_handler, "(iii)i", {"env", "__syscall_dup3", "(iii)i"}},
};

void register_native_symbols() {
    wasm_runtime_register_natives("wasi_snapshot_preview1", wasi_native_symbols, sizeof(wasi_native_symbols) / sizeof(wasi_native_symbols[0]));
    wasm_runtime_register_natives("env", env_native_symbols, sizeof(env_native_symbols) / sizeof(env_native_symbols[0]));
}
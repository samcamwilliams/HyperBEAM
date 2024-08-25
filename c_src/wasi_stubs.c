#include <wasm_c_api.h>
#include "wasi_stubs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Implementation of WASI functions
int32_t wasi_fd_seek(wasm_exec_env_t exec_env, int32_t fd, int64_t offset, int32_t whence, int64_t *newoffset) {
    // Implement fd_seek using 64-bit offset
    // This is just a placeholder implementation
    *newoffset = offset;
    return 0;
}

int32_t wasi_fd_read(wasm_exec_env_t exec_env, int32_t fd, int32_t iov_ptr, int32_t iov_cnt, int64_t *nread) {
    // Implement fd_read using 64-bit nread
    // This is just a placeholder implementation
    *nread = 0;
    return 0;
}

int32_t wasi_environ_sizes_get(wasm_exec_env_t exec_env, int32_t environ_count_ptr, int32_t environ_buf_size_ptr) {
    // Implement environ_sizes_get
    // This is just a placeholder implementation
    *(int32_t*)environ_count_ptr = 0;
    *(int32_t*)environ_buf_size_ptr = 0;
    return 0;
}

int32_t wasi_environ_get(wasm_exec_env_t exec_env, int32_t environ_ptr, int32_t environ_buf_ptr) {
    // Implement environ_get
    // This is just a placeholder implementation
    return 0;
}

int32_t wasi_clock_time_get(wasm_exec_env_t exec_env, int32_t clock_id, int64_t precision, int64_t *time) {
    // Implement clock_time_get using 64-bit precision and time
    // This is just a placeholder implementation
    *time = 0;
    return 0;
}

int32_t wasi_fd_write(wasm_exec_env_t exec_env, int32_t fd, int64_t iov_ptr, int32_t iov_cnt, int64_t *nwritten_ptr) {
    wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    uint32_t total_bytes = 0;
    FILE *stream = (fd == 1) ? stdout : stderr;

    for (int i = 0; i < iov_cnt; i++) {
        uint64_t *iov = wasm_runtime_addr_app_to_native(module_inst, iov_ptr + i * 16);
        if (!iov) break;

        uint32_t buf_ptr = (uint32_t)iov[0];
        uint32_t buf_len = (uint32_t)iov[1];

        char *buf = wasm_runtime_addr_app_to_native(module_inst, buf_ptr);
        if (!buf) break;

        fwrite(buf, 1, buf_len, stream);
        total_bytes += buf_len;
    }

    fflush(stream);
    *nwritten_ptr = total_bytes;
    return 0;  // Success
}

int32_t wasi_fd_close(wasm_exec_env_t exec_env, int32_t fd) {
    // Stub implementation
    return 0; // Assuming success
}

void wasi_proc_exit(wasm_exec_env_t exec_env, int32_t exit_code) {
    // Stub implementation
    // Note: This function doesn't return, as it's meant to terminate the program
    // In a real implementation, you might want to handle this differently
    //exit(exit_code);
    printf("proc_exit: %d\n", exit_code);
}

// New stubs for the remaining functions
int32_t syscall_unlinkat(wasm_exec_env_t exec_env, int32_t dirfd, int32_t path_ptr, int32_t flags) {
    // Placeholder implementation
    return 0; // Success
}

int32_t emscripten_system(wasm_exec_env_t exec_env, int32_t command_ptr) {
    // Placeholder implementation
    return 0; // Success
}

int32_t syscall_rmdir(wasm_exec_env_t exec_env, int32_t path_ptr) {
    // Placeholder implementation
    return 0; // Success
}

int32_t syscall_renameat(wasm_exec_env_t exec_env, int32_t old_dirfd, int32_t old_path_ptr, int32_t new_dirfd, int32_t new_path_ptr) {
    // Placeholder implementation
    return 0; // Success
}

int32_t syscall_readlinkat(wasm_exec_env_t exec_env, int32_t dirfd, int32_t path_ptr, int32_t buf_ptr, int32_t buf_len, int32_t *nread_ptr) {
    // Placeholder implementation
    if (nread_ptr) {
        *nread_ptr = 0; // No bytes read
    }
    return 0; // Success
}

int32_t syscall_dup3(wasm_exec_env_t exec_env, int32_t oldfd, int32_t newfd, int32_t flags) {
    // Placeholder implementation
    return newfd; // Return the new file descriptor
}

// Define the array of native symbols
static NativeSymbol wasi_native_symbols[] = {
    {"fd_seek", wasi_fd_seek, "(iIiI)i", NULL},
    {"fd_read", wasi_fd_read, "(iIII)i", NULL},
    {"environ_sizes_get", wasi_environ_sizes_get, "(II)i", NULL},
    {"environ_get", wasi_environ_get, "(II)i", NULL},
    {"clock_time_get", wasi_clock_time_get, "(iII)i", NULL},
    {"fd_write", wasi_fd_write, "(iIII)i", NULL},
    {"fd_close", wasi_fd_close, "(i)i", NULL},
    {"proc_exit", wasi_proc_exit, "(i)", NULL},
};

static NativeSymbol env_native_symbols[] = {
    {"__syscall_unlinkat", syscall_unlinkat, "(iIi)i", NULL},
    {"__syscall_readlinkat", syscall_readlinkat, "(iIII)i", NULL},
    {"_emscripten_system", emscripten_system, "(I)i", NULL},
    {"__syscall_rmdir", syscall_rmdir, "(I)i", NULL},
    {"__syscall_renameat", syscall_renameat, "(iIiI)i", NULL},
    {"__syscall_dup3", syscall_dup3, "(iii)i", NULL},
};

// Function to register our WASI functions
void register_wasi_functions(wasm_instance_t* module_inst) {
    int n_native_symbols = sizeof(wasi_native_symbols) / sizeof(NativeSymbol);
    if (!wasm_runtime_register_natives("wasi_snapshot_preview1",
                                       wasi_native_symbols,
                                       n_native_symbols)) {
        // Handle error
        printf("Failed to register WASI functions\n");
    }
    int n_env_native_symbols = sizeof(env_native_symbols) / sizeof(NativeSymbol);
    if (!wasm_runtime_register_natives("env",
                                       env_native_symbols,
                                       n_env_native_symbols)) {
        // Handle error
        printf("Failed to register env functions\n");
    }
}
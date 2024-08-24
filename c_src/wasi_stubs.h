#ifndef WASI_STUBS_H
#define WASI_STUBS_H

#include <wasm_export.h>

// Function declarations
int32_t wasi_fd_seek(wasm_exec_env_t exec_env, int32_t fd, int64_t offset, int32_t whence, int64_t *newoffset);
int32_t wasi_fd_read(wasm_exec_env_t exec_env, int32_t fd, int32_t iov_ptr, int32_t iov_cnt, int64_t *nread);
int32_t wasi_environ_sizes_get(wasm_exec_env_t exec_env, int32_t environ_count_ptr, int32_t environ_buf_size_ptr);
int32_t wasi_environ_get(wasm_exec_env_t exec_env, int32_t environ_ptr, int32_t environ_buf_ptr);
int32_t wasi_clock_time_get(wasm_exec_env_t exec_env, int32_t clock_id, int64_t precision, int64_t *time);

// Function to register WASI functions
void register_wasi_functions(wasm_instance_t* instance);

#endif // WASI_STUBS_H
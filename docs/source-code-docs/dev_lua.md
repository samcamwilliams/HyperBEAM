

# Module dev_lua #
* [Description](#description)

A device that calls a Lua script upon a request and returns the result.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aos_process_benchmark_test_-0">aos_process_benchmark_test_/0*</a></td><td>Benchmark the performance of Lua executions.</td></tr><tr><td valign="top"><a href="#compute-4">compute/4*</a></td><td>Call the Lua script with the given arguments.</td></tr><tr><td valign="top"><a href="#decode-1">decode/1*</a></td><td>Decode a Lua result into a HyperBEAM <code>structured@1.0</code> message.</td></tr><tr><td valign="top"><a href="#direct_benchmark_test-0">direct_benchmark_test/0*</a></td><td>Benchmark the performance of Lua executions.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1*</a></td><td>Encode a HyperBEAM <code>structured@1.0</code> message into a Lua result.</td></tr><tr><td valign="top"><a href="#ensure_initialized-3">ensure_initialized/3*</a></td><td>Initialize the Lua VM if it is not already initialized.</td></tr><tr><td valign="top"><a href="#execute_aos_call-1">execute_aos_call/1*</a></td><td></td></tr><tr><td valign="top"><a href="#execute_aos_call-2">execute_aos_call/2*</a></td><td></td></tr><tr><td valign="top"><a href="#find_script-2">find_script/2*</a></td><td>Find the script in the base message, either by ID or by string.</td></tr><tr><td valign="top"><a href="#generate_lua_process-1">generate_lua_process/1*</a></td><td>Generate a Lua process message.</td></tr><tr><td valign="top"><a href="#generate_stack-1">generate_stack/1*</a></td><td>Generate a stack message for the Lua process.</td></tr><tr><td valign="top"><a href="#generate_test_message-1">generate_test_message/1*</a></td><td>Generate a test message for a Lua process.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>All keys that are not directly available in the base message are
resolved by calling the Lua function in the script of the same name.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Initialize the device state, loading the script into memory if it is
a reference.</td></tr><tr><td valign="top"><a href="#initialize-3">initialize/3*</a></td><td>Initialize a new Lua state with a given base message and script.</td></tr><tr><td valign="top"><a href="#invoke_aos_test-0">invoke_aos_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#invoke_non_compute_key_test-0">invoke_non_compute_key_test/0*</a></td><td>Call a non-compute key on a Lua device message and ensure that the
function of the same name in the script is called.</td></tr><tr><td valign="top"><a href="#lua_http_preprocessor_test-0">lua_http_preprocessor_test/0*</a></td><td>Use a Lua script as a preprocessor on the HTTP server via <code>~meta@1.0</code>.</td></tr><tr><td valign="top"><a href="#normalize-3">normalize/3</a></td><td>Restore the Lua state from a snapshot, if it exists.</td></tr><tr><td valign="top"><a href="#pure_lua_process_benchmark_test_-0">pure_lua_process_benchmark_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#pure_lua_process_test-0">pure_lua_process_test/0*</a></td><td>Call a process whose <code>execution-device</code> is set to <code>lua@5.3a</code>.</td></tr><tr><td valign="top"><a href="#sandbox-3">sandbox/3*</a></td><td>Sandbox (render inoperable) a set of Lua functions.</td></tr><tr><td valign="top"><a href="#sandboxed_failure_test-0">sandboxed_failure_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_invocation_test-0">simple_invocation_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td>Snapshot the Lua state from a live computation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="aos_process_benchmark_test_-0"></a>

### aos_process_benchmark_test_/0 * ###

`aos_process_benchmark_test_() -> any()`

Benchmark the performance of Lua executions.

<a name="compute-4"></a>

### compute/4 * ###

`compute(Key, RawBase, Req, Opts) -> any()`

Call the Lua script with the given arguments.

<a name="decode-1"></a>

### decode/1 * ###

`decode(Map) -> any()`

Decode a Lua result into a HyperBEAM `structured@1.0` message.

<a name="direct_benchmark_test-0"></a>

### direct_benchmark_test/0 * ###

`direct_benchmark_test() -> any()`

Benchmark the performance of Lua executions.

<a name="encode-1"></a>

### encode/1 * ###

`encode(Map) -> any()`

Encode a HyperBEAM `structured@1.0` message into a Lua result.

<a name="ensure_initialized-3"></a>

### ensure_initialized/3 * ###

`ensure_initialized(Base, Req, Opts) -> any()`

Initialize the Lua VM if it is not already initialized. Optionally takes
the script as a  Binary string. If not provided, the script will be loaded
from the base message.

<a name="execute_aos_call-1"></a>

### execute_aos_call/1 * ###

`execute_aos_call(Base) -> any()`

<a name="execute_aos_call-2"></a>

### execute_aos_call/2 * ###

`execute_aos_call(Base, Req) -> any()`

<a name="find_script-2"></a>

### find_script/2 * ###

`find_script(Base, Opts) -> any()`

Find the script in the base message, either by ID or by string.

<a name="generate_lua_process-1"></a>

### generate_lua_process/1 * ###

`generate_lua_process(File) -> any()`

Generate a Lua process message.

<a name="generate_stack-1"></a>

### generate_stack/1 * ###

`generate_stack(File) -> any()`

Generate a stack message for the Lua process.

<a name="generate_test_message-1"></a>

### generate_test_message/1 * ###

`generate_test_message(Process) -> any()`

Generate a test message for a Lua process.

<a name="info-1"></a>

### info/1 ###

`info(Base) -> any()`

All keys that are not directly available in the base message are
resolved by calling the Lua function in the script of the same name.

<a name="init-3"></a>

### init/3 ###

`init(Base, Req, Opts) -> any()`

Initialize the device state, loading the script into memory if it is
a reference.

<a name="initialize-3"></a>

### initialize/3 * ###

`initialize(Base, Script, Opts) -> any()`

Initialize a new Lua state with a given base message and script.

<a name="invoke_aos_test-0"></a>

### invoke_aos_test/0 * ###

`invoke_aos_test() -> any()`

<a name="invoke_non_compute_key_test-0"></a>

### invoke_non_compute_key_test/0 * ###

`invoke_non_compute_key_test() -> any()`

Call a non-compute key on a Lua device message and ensure that the
function of the same name in the script is called.

<a name="lua_http_preprocessor_test-0"></a>

### lua_http_preprocessor_test/0 * ###

`lua_http_preprocessor_test() -> any()`

Use a Lua script as a preprocessor on the HTTP server via `~meta@1.0`.

<a name="normalize-3"></a>

### normalize/3 ###

`normalize(Base, Req, RawOpts) -> any()`

Restore the Lua state from a snapshot, if it exists.

<a name="pure_lua_process_benchmark_test_-0"></a>

### pure_lua_process_benchmark_test_/0 * ###

`pure_lua_process_benchmark_test_() -> any()`

<a name="pure_lua_process_test-0"></a>

### pure_lua_process_test/0 * ###

`pure_lua_process_test() -> any()`

Call a process whose `execution-device` is set to `lua@5.3a`.

<a name="sandbox-3"></a>

### sandbox/3 * ###

`sandbox(State, Map, Opts) -> any()`

Sandbox (render inoperable) a set of Lua functions. Each function is
referred to as if it is a path in AO-Core, with its value being what to
return to the caller. For example, 'os.exit' would be referred to as
referred to as `os/exit`. If preferred, a list rather than a map may be
provided, in which case the functions all return `sandboxed`.

<a name="sandboxed_failure_test-0"></a>

### sandboxed_failure_test/0 * ###

`sandboxed_failure_test() -> any()`

<a name="simple_invocation_test-0"></a>

### simple_invocation_test/0 * ###

`simple_invocation_test() -> any()`

<a name="snapshot-3"></a>

### snapshot/3 ###

`snapshot(Base, Req, Opts) -> any()`

Snapshot the Lua state from a live computation. Normalizes its `priv`
state element, then serializes the state to a binary.


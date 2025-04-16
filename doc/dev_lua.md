

# Module dev_lua
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A device that calls a Lua script upon a request and returns the result.

<a name="index"></a>

## Function Index


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aos_exec_binary-1">aos_exec_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#aos_process_binary-0">aos_process_binary/0*</a></td><td></td></tr><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td>Call the Lua script with the given arguments.</td></tr><tr><td valign="top"><a href="#decode-1">decode/1*</a></td><td>Decode a Lua result into a HyperBEAM <code>structured@1.0</code> message.</td></tr><tr><td valign="top"><a href="#direct_benchmark_test-0">direct_benchmark_test/0*</a></td><td>Benchmark the performance of Lua executions.</td></tr><tr><td valign="top"><a href="#do_compute-3">do_compute/3*</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1*</a></td><td>Encode a HyperBEAM <code>structured@1.0</code> message into a Lua result.</td></tr><tr><td valign="top"><a href="#ensure_initialized-3">ensure_initialized/3*</a></td><td>Initialize the Lua VM if it is not already initialized.</td></tr><tr><td valign="top"><a href="#ensure_initialized-4">ensure_initialized/4*</a></td><td></td></tr><tr><td valign="top"><a href="#execute_aos_call-1">execute_aos_call/1*</a></td><td></td></tr><tr><td valign="top"><a href="#execute_aos_call-2">execute_aos_call/2*</a></td><td></td></tr><tr><td valign="top"><a href="#find_script-2">find_script/2*</a></td><td>Find the script in the base message, either by ID or by string.</td></tr><tr><td valign="top"><a href="#generate_lua_process-1">generate_lua_process/1*</a></td><td>Generate a Lua process message.</td></tr><tr><td valign="top"><a href="#generate_stack-1">generate_stack/1*</a></td><td>Generate a stack message for the Lua process.</td></tr><tr><td valign="top"><a href="#generate_test_message-1">generate_test_message/1*</a></td><td>Generate a test message for a Lua process.</td></tr><tr><td valign="top"><a href="#handler-4">handler/4*</a></td><td>The handler of all non-message and non-device keys.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>All keys that are not directly available in the base message are
resolved by calling the Lua function in the script of the same name.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Initialize the device state, loading the script into memory if it is
a reference.</td></tr><tr><td valign="top"><a href="#invoke_aos_test_disabled-0">invoke_aos_test_disabled/0*</a></td><td>Call AOS with an eval command.</td></tr><tr><td valign="top"><a href="#invoke_non_compute_key_test-0">invoke_non_compute_key_test/0*</a></td><td>Call a non-compute key on a Lua device message and ensure that the
function of the same name in the script is called.</td></tr><tr><td valign="top"><a href="#lua_http_preprocessor_test-0">lua_http_preprocessor_test/0*</a></td><td>Use a Lua script as a preprocessor on the HTTP server via <code>~meta@1.0</code>.</td></tr><tr><td valign="top"><a href="#lua_json_interface_benchmark_test-0">lua_json_interface_benchmark_test/0*</a></td><td>Benchmark execution of a Lua stack with a JSON interface.</td></tr><tr><td valign="top"><a href="#lua_json_interface_test-0">lua_json_interface_test/0*</a></td><td>Ensure that we can call a Lua process using the JSON interface.</td></tr><tr><td valign="top"><a href="#normalize-3">normalize/3</a></td><td>Restore the Lua state from a snapshot, if it exists.</td></tr><tr><td valign="top"><a href="#pure_lua_process_benchmark_test_-0">pure_lua_process_benchmark_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#pure_lua_process_test-0">pure_lua_process_test/0*</a></td><td>Call a process whose <code>execution-device</code> is set to <code>lua@5.3a</code>.</td></tr><tr><td valign="top"><a href="#simple_invocation_test-0">simple_invocation_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td>Snapshot the Lua state from a live computation.</td></tr></table>


<a name="functions"></a>

## Function Details

<a name="aos_exec_binary-1"></a>

### aos_exec_binary/1 *

`aos_exec_binary(Command) -> any()`

<a name="aos_process_binary-0"></a>

### aos_process_binary/0 *

`aos_process_binary() -> any()`

<a name="compute-3"></a>

### compute/3

`compute(Base, Req, RawOpts) -> any()`

Call the Lua script with the given arguments. This key is `multipass@1.0`
compatible: It will only execute the call on the first pass. On subsequent
passes, it will return the base message unchanged.

<a name="decode-1"></a>

### decode/1 *

`decode(Map) -> any()`

Decode a Lua result into a HyperBEAM `structured@1.0` message.

<a name="direct_benchmark_test-0"></a>

### direct_benchmark_test/0 *

`direct_benchmark_test() -> any()`

Benchmark the performance of Lua executions.

<a name="do_compute-3"></a>

### do_compute/3 *

`do_compute(RawBase, Req, Opts) -> any()`

<a name="encode-1"></a>

### encode/1 *

`encode(Map) -> any()`

Encode a HyperBEAM `structured@1.0` message into a Lua result.

<a name="ensure_initialized-3"></a>

### ensure_initialized/3 *

`ensure_initialized(Base, Req, Opts) -> any()`

Initialize the Lua VM if it is not already initialized. Optionally takes
the script as a  Binary string. If not provided, the script will be loaded
from the base message.

<a name="ensure_initialized-4"></a>

### ensure_initialized/4 *

`ensure_initialized(Base, Req, Script, Opts) -> any()`

<a name="execute_aos_call-1"></a>

### execute_aos_call/1 *

`execute_aos_call(Base) -> any()`

<a name="execute_aos_call-2"></a>

### execute_aos_call/2 *

`execute_aos_call(Base, Req) -> any()`

<a name="find_script-2"></a>

### find_script/2 *

`find_script(Base, Opts) -> any()`

Find the script in the base message, either by ID or by string.

<a name="generate_lua_process-1"></a>

### generate_lua_process/1 *

`generate_lua_process(File) -> any()`

Generate a Lua process message.

<a name="generate_stack-1"></a>

### generate_stack/1 *

`generate_stack(File) -> any()`

Generate a stack message for the Lua process.

<a name="generate_test_message-1"></a>

### generate_test_message/1 *

`generate_test_message(Process) -> any()`

Generate a test message for a Lua process.

<a name="handler-4"></a>

### handler/4 *

`handler(Key, Base, Req, Opts) -> any()`

The handler of all non-message and non-device keys. We call the Lua
function in the script of the same name, passing the request as the
parameter.

<a name="info-1"></a>

### info/1

`info(Base) -> any()`

All keys that are not directly available in the base message are
resolved by calling the Lua function in the script of the same name.

<a name="init-3"></a>

### init/3

`init(Base, Req, Opts) -> any()`

Initialize the device state, loading the script into memory if it is
a reference.

<a name="invoke_aos_test_disabled-0"></a>

### invoke_aos_test_disabled/0 *

`invoke_aos_test_disabled() -> any()`

Call AOS with an eval command.

<a name="invoke_non_compute_key_test-0"></a>

### invoke_non_compute_key_test/0 *

`invoke_non_compute_key_test() -> any()`

Call a non-compute key on a Lua device message and ensure that the
function of the same name in the script is called.

<a name="lua_http_preprocessor_test-0"></a>

### lua_http_preprocessor_test/0 *

`lua_http_preprocessor_test() -> any()`

Use a Lua script as a preprocessor on the HTTP server via `~meta@1.0`.

<a name="lua_json_interface_benchmark_test-0"></a>

### lua_json_interface_benchmark_test/0 *

`lua_json_interface_benchmark_test() -> any()`

Benchmark execution of a Lua stack with a JSON interface.

<a name="lua_json_interface_test-0"></a>

### lua_json_interface_test/0 *

`lua_json_interface_test() -> any()`

Ensure that we can call a Lua process using the JSON interface.

<a name="normalize-3"></a>

### normalize/3

`normalize(Base, Req, RawOpts) -> any()`

Restore the Lua state from a snapshot, if it exists.

<a name="pure_lua_process_benchmark_test_-0"></a>

### pure_lua_process_benchmark_test_/0 *

`pure_lua_process_benchmark_test_() -> any()`

<a name="pure_lua_process_test-0"></a>

### pure_lua_process_test/0 *

`pure_lua_process_test() -> any()`

Call a process whose `execution-device` is set to `lua@5.3a`.

<a name="simple_invocation_test-0"></a>

### simple_invocation_test/0 *

`simple_invocation_test() -> any()`

<a name="snapshot-3"></a>

### snapshot/3

`snapshot(Base, Req, Opts) -> any()`

Snapshot the Lua state from a live computation. Normalizes its `priv`
state element, then serializes the state to a binary.


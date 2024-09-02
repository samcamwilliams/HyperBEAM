-module(cu_erwamr).
-export([start/1, call/3]).

-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

load_driver() ->
    case erl_ddll:load("./priv", ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

start(WasmBinary) ->
    ok = load_driver(),
    Port = open_port({spawn, "cu_erwamr"}, []),
    Port ! {self(), {command, term_to_binary({init, WasmBinary})}},
    ao:c({waiting_for_init_from, Port}),
    receive
        {Port, X} ->
            ao:c({wasm_driver_output, X}),
            {ok, Port};
        Other ->
            ao:c({unexpected_result, Other}),
            Other
    end.

stdlib(Module, Func, Args) ->
    ao:c({stdlib_called, Module, Func, Args}),
    1.

call(Port, FunctionName, Args) ->
    call(Port, FunctionName, Args, fun stdlib/3).
call(Port, FunctionName, Args, ImportFunc) ->
    ao:c({call_started, Port, FunctionName, Args}),
    Parent = self(),
    Port ! {self(), {self(), {call, FunctionName, Args}}},
    exec_call(ImportFunc, Port).

exec_call(ImportFunc, Worker) ->
    receive
        {result, Result} ->
            {ok, Result};
        {import, Module, Func, Args} ->
            ao:c({import_called, Module, Func, Args}),
            ErlRes = ImportFunc(Module, Func, Args),
            ao:c({import_returned, ErlRes}),
            Worker ! {resume, ErlRes},
            exec_call(ImportFunc, Worker);
        Error ->
            ao:c({unexpected_result, Error}),
            Error
    end.

read(_Port, _Offset, _Size) ->
    not_implemented.

write(_Port, _Offset, _Data) ->
    not_implemented.

%% Tests

nif_loads_test() ->
    ?MODULE:module_info().

simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, Port, _ImportMap} = start(File),
    Bin = read(Port, 0, 1),
    ao:c({bin, Bin}),
    write(Port, 0, Bin),
    ao:c(wrote),
    {ok, Result} = call(Port, fac, [5.0]),
    ?assertEqual(120.0, Result).

simple_wasm64_test() ->
    ao:c(simple_wasm64_test),
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, Port, _ImportMap} = start(File),
    {ok, Result} = call(Port, fac, [5.0]),
    ?assertEqual(120.0, Result).

aos64_wasm_exceptions_test() ->
    {ok, File} = file:read_file("test/test-standalone-wex-aos.wasm"),
    {ok, Port, _ImportMap} = start(File),
    {ok, Result} = call(Port, main, [0, 0]),
    ?assertEqual(120.0, Result).
-module(cu_erwamr).
-export([start/1, call/3, test/0, write/3, read/3]).

-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

test() ->
    aos64_wasm_exceptions_test(),
    erlang:halt().

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
        {ok, Imports, Exports} ->
            ao:c({wasm_init_success, Imports, Exports}),
            {ok, Port, Imports, Exports};
        Other ->
            ao:c({unexpected_result, Other}),
            Other
    end.

stdlib(_Module, _Func, _Args, _Signature) ->
    [1].

call(Port, FunctionName, Args) ->
    call(Port, FunctionName, Args, fun stdlib/4).
call(Port, FunctionName, Args, ImportFunc) ->
    ao:c({call_started, Port, FunctionName, Args}),
    Port ! {self(), {command, term_to_binary({call, FunctionName, Args})}},
    exec_call(ImportFunc, Port).

exec_call(ImportFunc, Port) ->
    receive
        {ok, Result} ->
            ao:c({call_result, Result}),
            {ok, Result};
        {import, Module, Func, Args, Signature} ->
            ao:c({import_called, Module, Func, Args, Signature}),
            ErlRes = ImportFunc(Module, Func, Args, Signature),
            ao:c({import_returned, ErlRes}),
            Port ! {self(), {command, term_to_binary({import_response, ErlRes})}},
            exec_call(ImportFunc, Port);
        {error, Error} ->
            ao:c({wasm_error, Error}),
            {error, Error};
        Error ->
            ao:c({unexpected_result, Error}),
            Error
    end.

write(Port, Offset, Data) ->
    Port ! {self(), {command, term_to_binary({write, Offset, Data})}},
    receive
        ok ->
            ok;
        Error ->
            Error
    end.

read(Port, Offset, Size) ->
    Port ! {self(), {command, term_to_binary({read, Offset, Size})}},
    receive
        {ok, Result} ->
            {ok, Result};
        Error ->
            Error
    end.

%% Tests

nif_loads_test() ->
    ?MODULE:module_info().

simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    Bin = read(Port, 0, 1),
    ao:c({bin, Bin}),
    write(Port, 0, Bin),
    ao:c(wrote),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

simple_wasm64_test() ->
    ao:c(simple_wasm64_test),
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

aos64_wasm_exceptions_test() ->
    {ok, File} = file:read_file("test/test-standalone-wex-aos.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, [_Result]} = call(Port, "main", [1, 0]),
    {ok, [Ptr1]} = call(Port, "malloc", [100]),
    {ok, [Ptr2]} = call(Port, "malloc", [100]),
    write(Port, Ptr1, <<"{ssdfasdfasdfs}a">>),
    write(Port, Ptr2, <<"{asdfasdfasdf}aa">>),
    {ok, [Ptr3]} = call(Port, "handle", [Ptr1, Ptr2]),
    {ok, Bin} = read(Port, Ptr1, 5),
    ?assertEqual(Bin, <<"{ssdf">>),
    ao:c(success).
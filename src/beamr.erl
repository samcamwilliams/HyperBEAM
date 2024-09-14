-module(beamr).
-export([start/1, call/3, test/0, write/3, read/3]).

-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

test() ->
    aos64_standalone_wex_test(),
    erlang:halt().

load_driver() ->
    case erl_ddll:load("./priv", ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

start(WasmBinary) ->
    ok = load_driver(),
    Port = open_port({spawn, "beamr"}, []),
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

read_iovecs(Port, Ptr, Vecs) ->
    Bin = iolist_to_binary(do_read_iovecs(Port, Ptr, Vecs)),
    {ok, Bin}.

do_read_iovecs(_Port, _Ptr, 0) -> [];
do_read_iovecs(Port, Ptr, Vecs) ->
    {ok, VecStruct} = read(Port, Ptr, 16),
    <<BinPtr:64/little-unsigned-integer, Len:64/little-unsigned-integer>> = VecStruct,
    {ok, VecData} = read(Port, BinPtr, Len),
    [ VecData | do_read_iovecs(Port, Ptr + 16, Vecs - 1) ].

stdlib(Port, "wasi_snapshot_preview1","fd_write", [Fd, Ptr, Vecs, RetPtr], _Signature) ->
    ao:c({fwd_write, Fd, Ptr, Vecs, RetPtr}),
    {ok, VecData} = read_iovecs(Port, Ptr, Vecs),
    BytesWritten = byte_size(VecData),
    error_logger:info_report(VecData),
    ao:c({processed, BytesWritten}),
    % Set return pointer to number of bytes written
    write(Port, RetPtr, <<BytesWritten:64/little-unsigned-integer>>),
    [0];
stdlib(_Port, _Module, "clock_time_get", _Args, _Signature) ->
    ao:c({stub_called, wasi_clock_time_get, 1}),
    [1];
stdlib(_Port, _Module, _Func, _Args, _Signature) ->
    [0].

call(Port, FunctionName, Args) ->
    call(Port, FunctionName, Args, fun stdlib/5).
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
            ErlRes = ImportFunc(Port, Module, Func, Args, Signature),
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
    %ao:c({write_started, Port, Offset, byte_size(Data)}),
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

read_string(Port, Offset) ->
    {ok, iolist_to_binary(do_read_string(Port, Offset, 8))}.

do_read_string(Port, Offset, ChunkSize) ->
    {ok, Data} = read(Port, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(Port, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.

malloc(Port, Size) ->
    case call(Port, "malloc", [Size]) of
        {ok, [0]} ->
            ao:c({malloc_failed, Size}),
            {error, malloc_failed};
        {ok, [Ptr]} ->
            ao:c({malloc_success, Ptr, Size}),
            {ok, Ptr};
        Error ->
            Error
    end.

%% Tests

nif_loads_test() ->
    ?MODULE:module_info().

simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

simple_wasm_calling_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(Port, "main", [1,1]),
    ?assertEqual(1, Result),
    Arg0 = <<"Test string arg 000000000000000\0">>,
    Arg1 = <<"Test string arg 111111111111111\0">>,
    {ok, Ptr0} = malloc(Port, byte_size(Arg0)),
    ?assertNotEqual(0, Ptr0),
    write(Port, Ptr0, Arg0),
    {ok, Ptr1} = malloc(Port, byte_size(Arg1)),
    ?assertNotEqual(0, Ptr1),
    write(Port, Ptr1, Arg1),
    {ok, []} = call(Port, "print_args", [Ptr0, Ptr1]).

wasm64_test() ->
    ao:c(simple_wasm64_test),
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

wasm_exceptions_test_skip() ->
    {ok, File} = file:read_file("test/test-ex.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(Port, "main", [1, 0]),
    ?assertEqual(1, Result).

aos64_standalone_wex_test() ->
    Env = <<"{\"Process\":{\"Id\":\"AOS\",\"Owner\":\"FOOBAR\",\"Tags\":[{\"name\":\"Name\",\"value\":\"Thomas\"}, {\"name\":\"Authority\",\"value\":\"FOOBAR\"}]}}\0">>,
    Msg = <<"{\"From\":\"FOOBAR\",\"Block-Height\":\"1\",\"Target\":\"AOS\",\"Owner\":\"FOOBAR\",\"Id\":\"1\",\"Module\":\"W\",\"Tags\":[{\"name\":\"Action\",\"value\":\"Eval\"}],\"Data\":\"return 1+1\"}\0">>,
    {ok, File} = file:read_file("test/aos-2-pure.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, Ptr1} = malloc(Port, byte_size(Msg)),
    ?assertNotEqual(0, Ptr1),
    write(Port, Ptr1, Msg),
    {ok, Ptr2} = malloc(Port, byte_size(Env)),
    ?assertNotEqual(0, Ptr2),
    write(Port, Ptr2, Env),
    % Read the strings to validate they are correctly passed
    {ok, MsgBin} = read(Port, Ptr1, byte_size(Msg)),
    {ok, EnvBin} = read(Port, Ptr2, byte_size(Env)),
    ?assertEqual(Env, EnvBin),
    ?assertEqual(Msg, MsgBin),
    {ok, [Ptr3]} = call(Port, "handle", [Ptr1, Ptr2]),
    {ok, ResBin} = read_string(Port, Ptr3),
    #{<<"ok">> := true, <<"response">> := Resp} = jiffy:decode(ResBin, [return_maps]),
    #{<<"Output">> := #{ <<"data">> := Data }} = Resp,
    ?assertEqual(<<"2">>, Data).
-module(cu_beamr).
-export([start/1, call/3, call/4, call/5, stop/1, test/0]).
-export([serialize/1, deserialize/2]).
-export([stub_stdlib/6]).

-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

test() ->
    %aos64_standalone_wex_test(),
    %checkpoint_and_resume_test().
    %timed_calls_test().
    ok.

load_driver() ->
    case erl_ddll:load("./priv", ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

start(WasmBinary) ->
    ok = load_driver(),
    Port = open_port({spawn, "cu_beamr"}, []),
    Port ! {self(), {command, term_to_binary({init, WasmBinary})}},
    ao:c({waiting_for_init_from, Port}),
    receive
        {ok, Imports, Exports} ->
            ao:c({wasm_init_success, {imports, length(Imports)}, {exports, length(Exports)}}),
            {ok, Port, Imports, Exports};
        Other ->
            ao:c({unexpected_wasm_init_result, Other}),
            Other
    end.

stop(Port) ->
    ?c(stop_called_on_dev_wasm),
    port_close(Port),
    ok.

call(Port, FunctionName, Args) ->
    call(Port, FunctionName, Args, fun dev_json_iface:lib/6).
call(Port, FunctionName, Args, Stdlib) ->
    {ResType, Res, _} = call(#{ stdout => [] }, Port, FunctionName, Args, Stdlib),
    {ResType, Res}.
call(S, Port, FunctionName, Args, ImportFunc) ->
    ao:c({call_started, Port, FunctionName, Args, ImportFunc}),
    Port ! {self(), {command, term_to_binary({call, FunctionName, Args})}},
    exec_call(S, ImportFunc, Port).

stub_stdlib(S, _Port, _Module, _Func, _Args, _Signature) -> {S, [0]}.

exec_call(S, ImportFunc, Port) ->
    receive
        {ok, Result} ->
            ao:c({call_result, Result}),
            {ok, Result, S};
        {import, Module, Func, Args, Signature} ->
            ao:c({import_called, Module, Func, Args, Signature}),
            {S2, ErlRes} = ImportFunc(S, Port, Module, Func, Args, Signature),
            ao:c({import_returned, Module, Func, Args, ErlRes}),
            Port ! {self(), {command, term_to_binary({import_response, ErlRes})}},
            exec_call(S2, ImportFunc, Port);
        {error, Error} ->
            ao:c({wasm_error, Error}),
            {error, Error, S};
        Error ->
            ao:c({unexpected_result, Error}),
            Error
    end.

serialize(Port) ->
    ?c(starting_serialize),
    {ok, Size} = cu_beamr_io:size(Port),
    {ok, Mem} = cu_beamr_io:read(Port, 0, Size),
    ?c({finished_serialize, byte_size(Mem)}),
    {ok, Mem}.

deserialize(Port, Bin) ->
    % TODO: Be careful of memory growth!
    ?c(starting_deserialize),
    ok = cu_beamr_io:write(Port, 0, Bin),
    ?c(finished_deserialize),
    ok.

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
    {ok, Ptr0} = cu_beamr_io:malloc(Port, byte_size(Arg0)),
    ?assertNotEqual(0, Ptr0),
    cu_beamr_io:write(Port, Ptr0, Arg0),
    {ok, Ptr1} = cu_beamr_io:malloc(Port, byte_size(Arg1)),
    ?assertNotEqual(0, Ptr1),
    cu_beamr_io:write(Port, Ptr1, Arg1),
    {ok, []} = call(Port, "print_args", [Ptr0, Ptr1]).

wasm64_test() ->
    ao:c(simple_wasm64_test),
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

% wasm_exceptions_test_skip() ->
%     {ok, File} = file:read_file("test/test-ex.wasm"),
%     {ok, Port, _Imports, _Exports} = start(File),
%     {ok, [Result]} = call(Port, "main", [1, 0]),
%     ?assertEqual(1, Result).

gen_test_env() ->
    <<"{\"Process\":{\"Id\":\"AOS\",\"Owner\":\"FOOBAR\",\"Tags\":[{\"name\":\"Name\",\"value\":\"Thomas\"}, {\"name\":\"Authority\",\"value\":\"FOOBAR\"}]}}\0">>.

gen_test_aos_msg(Command) ->
    <<"{\"From\":\"FOOBAR\",\"Block-Height\":\"1\",\"Target\":\"AOS\",\"Owner\":\"FOOBAR\",\"Id\":\"1\",\"Module\":\"W\",\"Tags\":[{\"name\":\"Action\",\"value\":\"Eval\"}],\"Data\":\"", (list_to_binary(Command))/binary, "\"}\0">>.

aos64_standalone_wex_test() ->
    Msg = gen_test_aos_msg("return 1+1"),
    Env = gen_test_env(),
    {ok, File} = file:read_file("test/aos-2-pure.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, Ptr1} = cu_beamr_io:malloc(Port, byte_size(Msg)),
    ?assertNotEqual(0, Ptr1),
    cu_beamr_io:write(Port, Ptr1, Msg),
    {ok, Ptr2} = cu_beamr_io:malloc(Port, byte_size(Env)),
    ?assertNotEqual(0, Ptr2),
    cu_beamr_io:write(Port, Ptr2, Env),
    % Read the strings to validate they are correctly passed
    {ok, MsgBin} = cu_beamr_io:read(Port, Ptr1, byte_size(Msg)),
    {ok, EnvBin} = cu_beamr_io:read(Port, Ptr2, byte_size(Env)),
    ?assertEqual(Env, EnvBin),
    ?assertEqual(Msg, MsgBin),
    {ok, [Ptr3], _} = call(Port, "handle", [Ptr1, Ptr2]),
    {ok, ResBin} = cu_beamr_io:read_string(Port, Ptr3),
    #{<<"ok">> := true, <<"response">> := Resp} = jiffy:decode(ResBin, [return_maps]),
    #{<<"Output">> := #{ <<"data">> := Data }} = Resp,
    ?assertEqual(<<"2">>, Data).

checkpoint_and_resume_test() ->
    Env = gen_test_env(),
    Msg1 = gen_test_aos_msg("TestVar = 0"),
    Msg2 = gen_test_aos_msg("TestVar = 1"),
    Msg3 = gen_test_aos_msg("return TestVar"),
    {ok, File} = file:read_file("test/aos-2-pure.wasm"),
    {ok, Port1, _ImportMap, _Exports} = start(File),
    {ok, EnvPtr} = cu_beamr_io:write_string(Port1, Env),
    {ok, Msg1Ptr} = cu_beamr_io:write_string(Port1, Msg1),
    {ok, Msg2Ptr} = cu_beamr_io:write_string(Port1, Msg2),
    {ok, Msg3Ptr} = cu_beamr_io:write_string(Port1, Msg3),
    {ok, [_]} = call(Port1, "main", [0, 0]),
    {ok, [_]} = call(Port1, "handle", [Msg1Ptr, EnvPtr]),
    {ok, MemCheckpoint} = serialize(Port1),
    {ok, [_]} = call(Port1, "handle", [Msg2Ptr, EnvPtr]),
    {ok, [OutPtr1]} = call(Port1, "handle", [Msg3Ptr, EnvPtr]),
    {ok, Port2, _, _} = start(File),
    deserialize(Port2, MemCheckpoint),
    {ok, [OutPtr2]} = call(Port2, "handle", [Msg3Ptr, EnvPtr]),
    Str1 = cu_beamr_io:read_string(Port1, OutPtr1),
    Str2 = cu_beamr_io:read_string(Port2, OutPtr2),
    ?assertNotEqual(Str1, Str2).

timed_calls_test() ->
    Env = gen_test_env(),
    Msg1 = gen_test_aos_msg("return 1+1"),
    {ok, File} = file:read_file("test/aos-2-pure.wasm"),
    {ok, Port1, _ImportMap, _Exports} = start(File),
    {ok, EnvPtr} = cu_beamr_io:write_string(Port1, Env),
    {ok, Msg1Ptr} = cu_beamr_io:write_string(Port1, Msg1),
    {Time, _Res} = timer:tc(?MODULE, call, [Port1, "handle", [Msg1Ptr, EnvPtr]]),
    ?c({'1_run_in', Time, 'microseconds'}),
    ?assert(Time < 10000000),
    StartTime = erlang:system_time(millisecond),
    lists:foreach(fun(_) ->
        ?c(timer:tc(?MODULE, call, [Port1, "handle", [Msg1Ptr, EnvPtr]]))
    end, lists:seq(1, 1000)),
    EndTime = erlang:system_time(millisecond),
    ?c({'1000_runs_in', Secs = (EndTime - StartTime) / 1000, 'seconds'}),
    ?assert(Secs < 10).

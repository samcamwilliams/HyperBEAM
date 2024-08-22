-module(cu_wasm).
-export([start/0, load/1, run/2]).
-compile(export_all).
-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Helper macros for dealing with the Elixir wasmex interface.
-define(CONSTRUCT, '__struct__').
-define(W, 'Elixir.Wasmex').
-define(W_STORE, 'Elixir.Wasmex.Store').
-define(W_STORELIMITS, 'Elixir.Wasmex.StoreLimits').
-define(W_STOREORCALLER, 'Elixir.Wasmex.StoreOrCaller').
-define(W_INSTANCE, 'Elixir.Wasmex.Instance').
-define(W_MODULE, 'Elixir.Wasmex.Module').
-define(W_MEMORY, 'Elixir.Wasmex.Memory').
-define(W_ENGINE, 'Elixir.Wasmex.Engine').
-define(W_ENGINECONFIG, 'Elixir.Wasmex.EngineConfig').

type(T) ->
    Types = #{
        $i => i32,
        $p => i64,
        $j => i64,
        $f => f32,
        $d => f64,
        $e => externref
    },
    maps:get(T, Types, undefined).

start() ->
    load_and_run_aos_test().

load(_Module) ->
    ok.

run(_Msg, Instance) ->
    Instance.

%% Private functions

new(Bin) ->
    new(Bin, #{}).
new(Bin, Opts) ->
    {ok, Engine} = new_engine(Opts),
    new(Bin, Opts, Engine).
new(Bin, Opts, Engine) ->
    {ok, Store} = new_store(
        filter_keys(
            Opts,
            [memory_size, table_elements, instances, tables, memories]
        ),
        Engine
    ),
    {ok, Module} = new_module(Store, Bin),
    Imports = create_stubs(Module),
    %{ok, Instance} = ?W:start_link(#{ store => Store, module => Module, engine => Engine, imports => Stubs }),
    {ok, Instance} = ?W_INSTANCE:new(Store, Module, Imports, []),
    #{ store => Store, module => Module, engine => Engine, instance => Instance, imports => Imports}.

call(Env = #{ instance := Instance, caller := Caller }, Func, Args) ->
    ?W_INSTANCE:call_exported_function(Caller, Instance, Func, Args, self()),
    call_server(Env);
call(Env = #{ instance := Instance, store := Store }, Func, Args) ->
    ?W_INSTANCE:call_exported_function(Store, Instance, Func, Args, self()),
    call_server(Env).

call_server(State = #{ imports := Imports }) ->
    receive
        {returned_function_call, {error, Reason},_} ->
            io:format("### WASM ERROR ENCOUNTERED ###~n~s~n", [Reason]),
            {error, Reason};
        {returned_function_call, Result,_} ->
            io:format("### WASM RESULT ENCOUNTERED ###~n~p~n", [Result]),
            Result;
        {invoke_callback, Namespace, Name, CurrContext, CallbackArgs, Token} ->
            io:format("### WASM CALLBACK ENCOUNTERED ###~n~s:~s (~p) ~p~n", [Namespace, Name, CurrContext, CallbackArgs]),
            {Success, Res} =
                try
                    TempEnv = maps:merge(
                        State,
                        maps:merge(
                            CurrContext,
                            #{
                                memory => 'Elixir.Wasmex.Memory':'__wrap_resource__'(maps:get(memory, CurrContext)),
                                caller => 'Elixir.Wasmex.StoreOrCaller':'__wrap_resource__'(maps:get(caller, CurrContext))
                            }
                        )
                    ),
                    ImportsNS = maps:get(Namespace, Imports),
                    {fn, _, ResType, Callback} = maps:get(Name, ImportsNS),
                    RawRes = apply(Callback, [TempEnv|CallbackArgs]),
                    case ResType of
                        [] -> {true, []};
                        [_] -> {true, [RawRes]};
                        Else -> {true, Else}
                    end
                catch
                    A:B:C ->
                        ao:c({failed_to_run_import, {A, B, C}}),
                        {false, [<<"Error running imported function!">>]}
                end,
            ao:c({raw_res, Name, CallbackArgs, {Success, Res}}),
            'Elixir.Wasmex.Native':instance_receive_callback_result(Token, Success, Res),
            call_server(State);
        Else ->
            ao:c({received_other, Else}),
            Else
    end.

new_engine() -> new_engine(#{}).
new_engine(Opts) ->
    ?W_ENGINE:new(?W_ENGINECONFIG:'__struct__'(
        filter_keys(Opts, [consume_fuel, cranelift_opt_level, wasm_backtrace_details, memory64])
    )).

new_store() -> new_store(#{}).
new_store(Opts) ->
    new_store(Opts, new_engine(Opts)).
new_store(Opts, Engine) ->
    ?W_STORE:
        (case maps:find(wasi, Opts) of true -> new_wasi; error -> new end)
            (?W_STORELIMITS:?CONSTRUCT(Opts), Engine).

new_module(Store, WasmBin) ->
    ?W_MODULE:compile(Store, WasmBin).

set_fuel(#{ store := Store }, Limit) ->
    set_fuel(Store, Limit);
set_fuel(Store, Limit) ->
    ?W_STOREORCALLER:set_fuel(Store, Limit).

get_fuel(#{ store := Store }) ->
    get_fuel(Store);
get_fuel(Store) ->
    ?W_STOREORCALLER:get_fuel(Store).

filter_keys(Map, Keys) ->
    maps:filter(fun(K, _) -> lists:member(K, Keys) end, Map).

imports(#{ module := Module }) ->
    imports(Module);
imports(Module) ->
    ?W_MODULE:imports(Module).

exports(#{ module := Module }) ->
    exports(Module);
exports(Module) ->
    ?W_MODULE:exports(Module).

create_stubs(Env) ->
    Imports = imports(Env),
    ao:c({imports, Imports}),
    maps:map(fun(_GroupName, Funcs) ->
        maps:map(fun(FuncName, {Type, Inputs, Output}) ->
            {Type, Inputs, Output, stub(Env, FuncName, Type, Inputs, Output)}
        end, Funcs)
    end, Imports).

invoke_name_to_func_def(FuncName) when is_binary(FuncName) ->
    invoke_name_to_func_def(binary_to_list(FuncName));
invoke_name_to_func_def([RetType|ArgTypes]) when is_list(ArgTypes) ->
    {fn,
        case RetType of
            $v -> [];
            _ -> [other]
        end,
        lists:map(fun type/1, ArgTypes)
    }.

stub(_Env, <<"invoke_", FuncName/binary>>, Type, Inputs, Output) ->
    fun(Context = #{ caller := Caller }, FuncIndex, _Arg1, _Arg2) ->
        try
            {fn, RetType, ArgTypes} = invoke_name_to_func_def(FuncName),
            {ok, [StackPtr]} = call(Context, <<"emscripten_stack_get_current">>, []),
            ao:c({stack_pointer, StackPtr}),
            ao:c({func_index, FuncIndex}),
            ao:c({func_type, {RetType, ArgTypes}}),
            ao:c({imports, imports(Context)}),
            #{ <<"__indirect_function_table">> := {table, Table} } = exports(Context),
            ao:c({table, Table})
            %ao:c({func_to_call, FuncToCall}),
            %{ok, [FuncName]} = call(Context, FuncToCall, [])
        catch
            A:B ->
                ao:c({exception_triggered, {A, B}}),
                {false, [<<"Error running imported function!">>]}
        end,
        receive after 5000 -> ok end,
        todo_call_the_function_in_try_catch
    end;
stub(_Env, <<"fd_write">>, fn, _, _) ->
    fun(#{ caller := Caller, memory := Memory }, _fd, IO_Vecs, Count, RetPtr) ->
        % Implement fdwrite function from WASI spec
        % IO_Vecs is a pointer to an array of IO vectors
        % Count is the number of IO vectors
        % RetPtr is where to store the number of bytes written

        % Helper function to read a 64-bit integer from memory
        ReadU64 = fun(Ptr) ->
            <<Value:64/little-unsigned-integer>> = ?W_MEMORY:read_binary(Caller, Memory, Ptr, 8),
            Value
        end,

        % Read IO vectors
        ReadIOVec = fun(Ptr) ->
            BufPtr = ReadU64(Ptr),
            BufLen = ReadU64(Ptr + 8),
            {BufPtr, BufLen}
        end,

        % Process all IO vectors
        {TotalBytesWritten, _} = lists:foldl(
            fun(I, {BytesWritten, CurrPtr}) ->
                {BufPtr, BufLen} = ReadIOVec(CurrPtr),
                Data = ?W_MEMORY:read_binary(Caller, Memory, BufPtr, BufLen),
                io:format("~s", [Data]),
                {BytesWritten + BufLen, CurrPtr + 16}
            end,
            {0, IO_Vecs},
            lists:seq(1, Count)
        ),

        % Write total bytes written to RetPtr
        ?W_MEMORY:write_binary(Caller, Memory, RetPtr, <<TotalBytesWritten:32/little-unsigned-integer>>),

        % Return success (0) as per WASI spec
        {ok, [0]}
    end;
stub(_Env, FuncName, Type, Inputs, Output) ->
    % Generate a stub function that matches on the exact number of arguments.
    % Unforuntately, Erlang lacks a nicer way to do this, aside generating and
    % and compiling an AST live. That might even be dirtier than this.
    case length(Inputs) of
        0 -> fun(_) -> stub_call(FuncName, Output) end;
        1 -> fun(_, _) -> stub_call(FuncName, Output) end;
        2 -> fun(_, _, _) -> stub_call(FuncName, Output) end;
        3 -> fun(_, _, _, _) -> stub_call(FuncName, Output) end;
        4 -> fun(_, _, _, _, _) -> stub_call(FuncName, Output) end;
        5 -> fun(_, _, _, _, _, _) -> stub_call(FuncName, Output) end;
        6 -> fun(_, _, _, _, _, _, _) -> stub_call(FuncName, Output) end;
        7 -> fun(_, _, _, _, _, _, _, _) -> stub_call(FuncName, Output) end;
        8 -> fun(_, _, _, _, _, _, _, _, _) -> stub_call(FuncName, Output) end;
        9 -> fun(_, _, _, _, _, _, _, _, _, _) -> stub_call(FuncName, Output) end;
        10 -> fun(_, _, _, _, _, _, _, _, _, _, _) -> stub_call(FuncName, Output) end;
        _ -> erlang:error({unsupported_stub_arity, length(Inputs)})
    end.

stub_call(FuncName, Output) ->
    ao:c({called_stub, FuncName}),
    case Output of
        [f32] -> 0.0;
        [f64] -> 0.0;
        _ -> 0
    end.

load_and_run_basic_test() ->
    % Initialize wasmex
    {ok, WasmBytes} = file:read_file("test.wasm"),
    {ok, Instance} = ?W:start_link(#{ bytes => WasmBytes }),
    % Call the fac function in the wasm module
    {ok, [Result]} = ?W:call_function(Instance, <<"fac">>, [3.0]),
    ?assertEqual(6.0, Result).

load_and_run_aos_test() ->
    % Initialize wasmex
    {ok, WasmBytes} = file:read_file("test-standalone-wex-aos.wasm"),
    Env = new(WasmBytes, #{ memory64 => true }),
    % Call the fac function in the wasm module
    {ok, [Result]} = call(Env, <<"main">>, [0, 0]),
    ao:c({result, Result}),
    ?assertEqual(6.0, Result).

minimal_exceptions_test() ->
    {ok, WasmBytes} = file:read_file("test-ex.wasm"),
    Env = new(WasmBytes, #{ memory64 => true }),
    {ok, [Result]} = call(Env, <<"main">>, [0, 0]),
    ao:c({result, Result}),
    ok.

wasmex_minimal_exceptions_test() ->
    {ok, WasmBytes} = file:read_file("test-standalone-wex-aos.wasm"),
    Env = new(WasmBytes, #{ memory64 => true }),
    #{ store := Store, module := Module, engine := Engine, imports := Imports} = Env,
    {ok, Instance} = ?W:start_link(#{ store => Store, module => Module, engine => Engine, imports => Imports }),
    {ok, [Result]} = ?W:call_function(Instance, <<"main">>, [0, 0]),
    ao:c({result, Result}),
    ok.

% run_with_fuel_test() ->
%     {ok, WasmBytes} = file:read_file("test-wasi-aos.wasm"),
%     Env = new(WasmBytes, #{ consume_fuel => true, memory64 => true }),
%     set_fuel(Env, 100),
%     {ok, [Result]} = call(Env, <<"main">>, [0, 0]),
%     {ok, RemainingFuel} = get_fuel(Env),
%     ?assertEqual(6.0, Result),
%     ?assertNotEqual(RemainingFuel, 100).


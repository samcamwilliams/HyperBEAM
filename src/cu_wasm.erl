-module(cu_wasm).
-export([load/1, run/2]).
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
-define(W_ENGINE, 'Elixir.Wasmex.Engine').
-define(W_ENGINECONFIG, 'Elixir.Wasmex.EngineConfig').

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

call(Env = #{ instance := Instance, store := Store }, Func, Args) ->
    ?W_INSTANCE:call_exported_function(Store, Instance, Func, Args, self()),
    call_server(Env, Func, Args).

call_server(Env = #{ imports := Imports }, Func, Args) ->
    receive
        {returned_function_call, Result,_} -> Result;
        {invoke_callback, Namespace, Name, Context, CallbackArgs, Token} ->
            {Success, Res} =
                try
                    TempEnv = maps:merge(Env, Context),
                    ImportsNS = maps:get(Namespace, Imports),
                    {fn, _, ResType, Callback} = maps:get(Name, ImportsNS),
                    RawRes = apply(Callback, [TempEnv|CallbackArgs]),
                    ao:c({raw_res, Name, CallbackArgs, RawRes}),
                    case ResType of
                        [] -> {true, []};
                        [_] -> {true, [RawRes]};
                        Else -> {true, Else}
                    end
                catch
                    _:E ->
                        ao:c({failed_to_run_import, E}),
                        {false, E}
                end,
            'Elixir.Wasmex.Native':instance_receive_callback_result(Token, Success, Res),
            call_server(Env, Func, Args);
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
    maps:map(fun(_GroupName, Funcs) ->
        maps:map(fun(FuncName, {Type, Inputs, Output}) ->
            stub(Env, FuncName, Type, Inputs, Output)
        end, Funcs)
    end, Imports).

stub(Env = #{ store := Store }, <<"invoke_", _/binary>>, Type, Inputs, Output) ->
    {
        Type,
        Inputs,
        Output,
        fun(#{ caller := Caller }, _FuncIndex, _Arg1, _Arg2) ->
            %ao:c([{i_am, self()}, {exports, exports(Env)}, {caller, Caller}, {instance, get(instance)}]),
            %{ok, SP} = ?W_INSTANCE:call_exported_function(Caller, get(instance), <<"emscripten_stack_get_current">>, [], self()),
            %ao:c(SP),
            %{ok, [FuncName]} = call(SP, <<"get_func_name">>, []),
            todo_call_the_function_in_try_catch
        end
    };
stub(_Env, FuncName, Type, Inputs, Output) ->
    % Generate a stub function that matches on the exact number of arguments.
    % Unforuntately, Erlang lacks a nicer way to do this, aside generating and
    % and compiling an AST live. That might even be dirtier than this.
    {
        Type,
        Inputs,
        Output,
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
        end
    }.

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

load_and_run_basic_mem64_test() ->
    % Initialize wasmex
    {ok, WasmBytes} = file:read_file("test-64.wasm"),
    Env = new(WasmBytes, #{ memory64 => true }),
    % Call the fac function in the wasm module
    {ok, [Result]} = call(Env, <<"fac">>, [3.0]),
    ao:c({result, Result}),
    ?assertEqual(6.0, Result).

run_with_fuel_test() ->
    {ok, WasmBytes} = file:read_file("test-wasi-aos.wasm"),
    Env = new(WasmBytes, #{ consume_fuel => true, memory64 => true }),
    set_fuel(Env, 100),
    {ok, [Result]} = call(Env, <<"main">>, [0, 0]),
    {ok, RemainingFuel} = get_fuel(Env),
    ?assertEqual(6.0, Result),
    ?assertNotEqual(RemainingFuel, 100).


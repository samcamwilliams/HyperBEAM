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
    Stubs = create_stubs(Module),
    {ok, Instance} = ?W:start_link(#{ store => Store, module => Module, engine => Engine, imports => Stubs }),
    #{ store => Store, module => Module, engine => Engine, stubs => Stubs, instance => Instance}.

call(Env, Func, Args) ->
    ?W:call_function(maps:get(instance, Env), Func, Args).

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
    maps:map(fun(_GroupName, Funcs) ->
        maps:map(fun(FuncName, {Type, Inputs, Output}) ->
            stub(FuncName, Type, Inputs, Output)
        end, Funcs)
    end, imports(Env)).

stub(FuncName, Type, Inputs, Output) ->
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
    {ok, WasmBytes} = file:read_file("test-aos.wasm"),
    {ok, Instance} = ?W:start_link(#{ bytes => WasmBytes, memory64 => true }),
    % Call the fac function in the wasm module
    {ok, [Result]} = ?W:call_function(Instance, <<"ccall">>, [3.0]),
    ao:c(Instance),
    ?assertEqual(6.0, Result).

run_with_fuel_test() ->
    {ok, WasmBytes} = file:read_file("test.wasm"),
    Env = new(WasmBytes, #{ consume_fuel => true }),
    set_fuel(Env, 100),
    {ok, [Result]} = call(Env, <<"fac">>, [3.0]),
    {ok, RemainingFuel} = get_fuel(Env),
    ?assertEqual(6.0, Result),
    ?assertNotEqual(RemainingFuel, 100).


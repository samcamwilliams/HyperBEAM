-module(cu_erwamr).
-export([load/1, instantiate/2, call/3]).

-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

init() ->
    SoName = case code:priv_dir(hyperbeam) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    ok = erlang:load_nif(SoName, 0).

instantiate(ModuleResource, ImportMap) ->
    instantiate_nif(ModuleResource, ImportMap).

load(WasmBinary) ->
    case load_nif(WasmBinary) of
        {ok, InstanceResource, ImportMap} ->
            ProcessedImportMap = maps:map(
                fun(K, V) ->
                    {fn, Args, Results} = V,
                    {fn, [binary_to_atom(A, utf8) || A <- Args], [binary_to_atom(R, utf8) || R <- Results]}
                end,
                ImportMap
            ),
            {ok, InstanceResource, ProcessedImportMap};
        Error ->
            Error
    end.

call(InstanceResource, FunctionName, Args) when is_list(Args) ->
    call_nif(InstanceResource, FunctionName, Args).

load_nif(_WasmBinary) ->
    erlang:nif_error("NIF library not loaded").

instantiate_nif(_ModuleResource, _ImportMap) ->
    erlang:nif_error("NIF library not loaded").

call_nif(_InstanceResource, _FunctionName, _Args) ->
    erlang:nif_error("NIF library not loaded").

%% Tests

nif_loads_test() ->
    ?MODULE:module_info().

simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, Mod} = load(File),
    {ok, Instance} = instantiate(Mod, #{}),
    {ok, Result} = call(Instance, "fac", [5.0]),
    ?assertEqual(120.0, Result).
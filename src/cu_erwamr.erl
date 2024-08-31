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

stdlib(InstanceResource, Module, Func, Args) ->
    ao:c({stdlib_called, InstanceResource, Module, Func, Args}),
    1.

call(InstanceResource, FunctionName, Args) ->
    call(InstanceResource, FunctionName, Args, fun stdlib/4).
call(InstanceResource, FunctionName, Args, ImportFunc) ->
    exec_call(InstanceResource, ImportFunc, call_nif(InstanceResource, FunctionName, Args)).

exec_call(InstanceResource, ImportFunc, Res) ->
    ao:c({exec_call, Res}),
    case Res of
        {ok, Result} ->
            {ok, Result};
        {import, Module, Func, Args} ->
            ao:c({import_called, Module, Func, Args}),
            ErlRes = ImportFunc(InstanceResource, Module, Func, Args),
            ao:c({import_returned, ErlRes}),
            exec_call(InstanceResource, ImportFunc, resume_nif(InstanceResource, ErlRes));
        Error ->
            ao:c({exception, Error}),
            Error
    end.

load_nif(_WasmBinary) ->
    erlang:nif_error("NIF library not loaded").

instantiate_nif(_ModuleResource, _ImportMap) ->
    erlang:nif_error("NIF library not loaded").

call_nif(_InstanceResource, _FunctionName, _Args) ->
    erlang:nif_error("NIF library not loaded").

resume_nif(_InstanceResource, _Res) ->
    erlang:nif_error("NIF library not loaded").

read_nif(_InstanceResource, _Offset, _Size) ->
    erlang:nif_error("NIF library not loaded").

write_nif(_InstanceResource, _Offset, _Data) ->
    erlang:nif_error("NIF library not loaded").

%% Tests

nif_loads_test() ->
    ?MODULE:module_info().

simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, Mod, _ImportMap, _ExportMap} = load(File),
    {ok, Instance} = instantiate(Mod, #{}),
    Bin = read_nif(Instance, 0, 1),
    ao:c({bin, Bin}),
    write_nif(Instance, 0, Bin),
    ao:c(wrote),
    {ok, Result} = call(Instance, <<"fac">>, [5.0]),
    ?assertEqual(120.0, Result).

simple_wasm64_test() ->
    ao:c(simple_wasm64_test),
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, Mod, _ImportMap, _ExportMap} = load(File),
    {ok, Instance} = instantiate(Mod, #{}),
    {ok, Result} = call(Instance, <<"fac">>, [5.0]),
    ?assertEqual(120.0, Result).

aos64_wasm_exceptions_test() ->
    {ok, File} = file:read_file("test/test-standalone-wex-aos.wasm"),
    {ok, Mod, ImportMap, ExportMap} = load(File),
    ao:c({import_map, ImportMap}),
    ao:c({export_map, ExportMap}),
    {ok, Instance} = instantiate(Mod, #{}),
    {ok, Result} = call(Instance, <<"main">>, [0, 0]),
    ?assertEqual(120.0, Result).
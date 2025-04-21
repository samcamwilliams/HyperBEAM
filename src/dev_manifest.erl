%%% @doc An Arweave path manifest resolution device. Follows the v1 schema:
%%% https://specs.ar.io/?tx=lXLd0OPwo-dJLB_Amz5jgIeDhiOkjXuM3-r0H_aiNj0
-module(dev_manifest).
-export([info/0]).
-include("include/hb.hrl").

%% @doc Use the `route/4' function as the handler for all requests, aside 
%% from `keys' and `set', which are handled by the default resolver.
info() ->
    #{
        default => fun route/4,
        excludes => [keys, set]
    }.

%% @doc Route a request to the associated data via its manifest.
route(<<"index">>, M1, M2, Opts) ->
    ?event({manifest_index, M1, M2}),
    case manifest(M1, M2, Opts) of
        {ok, JSONStruct} ->
            ?event({manifest_json_struct, JSONStruct}),
            case hb_ao:resolve(JSONStruct, [<<"index">>, <<"path">>], Opts) of
                {ok, Path} ->
                    ?event({manifest_path, Path}),
                    route(Path, M1, M2, Opts);
                _ -> {error, not_found}
            end;
        {error, not_found} ->
            {error, not_found}
    end;
route(Key, M1, M2, Opts) ->
    ?event({manifest_lookup, Key}),
    {ok, JSONStruct} = manifest(M1, M2, Opts),
    ?event({manifest_json_struct, JSONStruct}),
    case hb_ao:resolve(JSONStruct, [<<"paths">>, Key], Opts) of
        {ok, Entry} ->
            ID = maps:get(<<"id">>, Entry),
            ?event({manifest_serving, ID}),
            case hb_cache:read(ID, Opts) of
                {ok, Data} ->
                    ?event({manifest_data, Data}),
                    {ok, Data};
                {error, not_found} ->
                    Fallback = hb_ao:get(JSONStruct, <<"fallback">>, Opts),
                    FallbackID = maps:get(<<"id">>, Fallback),
                    ?event({manifest_serving_fallback, FallbackID}),
                    hb_cache:read(FallbackID, Opts)
            end;
        _ -> {error, not_found}
    end.

%% @doc Find and deserialize a manifest from the given base.
manifest(Base, _Req, Opts) ->
    JSON =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Base}, [<<"data">>]},
                {{as, <<"message@1.0">>, Base}, [<<"body">>]}
            ],
            Opts
        ),
    ?event({manifest_json, JSON}),
    hb_ao:resolve(
        #{ <<"device">> => <<"json@1.0">>, <<"body">> => JSON },
        <<"deserialize">>,
        Opts
    ).
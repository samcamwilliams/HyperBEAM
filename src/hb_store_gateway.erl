%%% @doc A store module that reads data from the nodes Arweave gateway and 
%%% GraphQL routes, additionally including additional store-specific routes.
-module(hb_store_gateway).
-export([scope/1, type/2, read/2, resolve/2, list/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The scope of a GraphQL store is always remote, due to performance.
scope(_) -> remote.
resolve(_, Key) -> Key.

list(StoreOpts, Key) ->
    case read(StoreOpts, Key) of
        {error, _} -> not_found;
        {ok, Message} -> {ok, maps:keys(Message)}
    end.

%% @doc Normalize the store options, adding the routes if specified.
%% If no routes are specified, the default routes are used.
normalize_opts(StoreOpts) when is_map(StoreOpts) ->
    case maps:get(routes, StoreOpts, not_found) of
        not_found -> StoreOpts#{ routes => hb_opts:get(routes, [], #{}) };
        {only, Routes} ->
            StoreOpts#{ routes => Routes };
        Routes ->
            StoreOpts#{ routes => Routes ++ hb_opts:get(routes, [], #{}) }
    end;
normalize_opts(StoreOpts) ->
    StoreOpts.

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(StoreOpts, Key) ->
    ?event({type, StoreOpts, Key}),
    case read(StoreOpts, Key) of
        not_found -> not_found;
        {ok, Data} ->
            maybe_cache(StoreOpts, Data),
            ?event({type, hb_private:reset(hb_message:unattested(Data))}),
            IsFlat = lists:all(
                fun({_, Value}) -> not is_map(Value) end,
                maps:to_list(hb_private:reset(hb_message:unattested(Data)))
            ),
            if
                IsFlat -> simple;
                true -> composite
            end
    end.

%% @doc Read the data at the given key from the GraphQL route. Will only attempt
%% to read the data if the key is an ID.
read(StoreOpts, Key) ->
    case hb_path:term_to_path_parts(Key) of
        [ID] when ?IS_ID(ID) ->
            ?event({read, StoreOpts, Key}),
            case hb_gateway_client:read(Key, normalize_opts(StoreOpts)) of
                {error, _} -> not_found;
                {ok, Message} -> {ok, Message}
            end;
        _ ->
            ?event({ignoring_non_id, Key}),
            not_found
    end.

%% @doc Cache the data if the cache is enabled. The `cache` option may either
%% be `false` to disable local caching, or a store definition to use as the
%% cache.
maybe_cache(StoreOpts, Data) ->
    ?event({maybe_cache, StoreOpts, Data}),
    case hb_opts:get(cache, false, StoreOpts) of
        false -> do_nothing;
        Store ->
            case hb_cache:write(#{ store => Store}, Data) of
                ok -> Data;
                Other -> Other
            end
    end.

%%% Tests

%% @doc Store is accessible via the default options.
graphql_as_store_test() ->
    hb_http_server:start_node(#{}),
    ?assertMatch(
        {ok, #{ <<"type">> := <<"Assignment">> }},
        hb_store:read(
            [{hb_store_gateway, #{}}],
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>
        )
    ).

%% @doc Stored messages are accessible via `hb_cache` accesses.
graphql_from_cache_test() ->
    hb_http_server:start_node(#{}),
    Opts = #{ store => [{hb_store_gateway, #{}}] },
    ?assertMatch(
        {ok, #{ <<"type">> := <<"Assignment">> }},
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            Opts
        )
    ).

%% @doc Routes can be specified in the options, overriding the default routes.
%% We test this by inversion: If the above cache read test works, then we know 
%% that the default routes allow access to the item. If the test below were to
%% produce the same result, despite an empty 'only' route list, then we would
%% know that the module is not respecting the route list.
specific_route_test() ->
    hb_http_server:start_node(#{}),
    Opts = #{
        store =>
            [
                {hb_store_gateway, #{ routes => {only, []}}}
            ]
    },
    ?assertMatch(
        not_found,
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            Opts
        )
    ).

%% @doc Test that the default node config allows for data to be accessed.
external_http_access_test() ->
    Node = hb_http_server:start_node(
        #{
            store => [{hb_store_fs, #{ prefix => "test-cache" }}, {hb_store_gateway, #{}}],
            http_extra_opts => #{ force_message => true, cache_control => [<<"always">>] }
        }
    ),
    ?assertMatch(
        {ok, #{ <<"type">> := <<"Assignment">> }},
        hb_http:get(
            Node,
            <<"/0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            #{}
        )
    ).

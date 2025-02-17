%%% @doc A store module that reads data from the nodes Arweave gateway and 
%%% GraphQL routes, additionally including additional store-specific routes.
-module(hb_store_graphql).
-export([scope/1, type/2, read/2, resolve/2]).
-include("include/hb.hrl").

%% @doc The scope of a GraphQL store is always remote, due to performance.
scope(_) -> remote.
resolve(_, Key) -> Key.

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
    case read(StoreOpts, Key) of
        not_found -> not_found;
        {ok, Data} ->
            maybe_cache(StoreOpts, Data),
            IsFlat = lists:all(
                fun({_, Value}) -> not is_map(Value) end,
                maps:to_list(hb_private:reset(hb_message:unattested(Data)))
            ),
            if
                IsFlat -> simple;
                true -> composite
            end
    end.

%% @doc Read the data at the given key from the GraphQL route.
read(StoreOpts, Key) ->
    Opts = normalize_opts(StoreOpts),
    case hb_graphql:read(Opts, Key) of
        {error, _} -> not_found;
        {ok, Message} -> {ok, Message}
    end.

%% @doc Cache the data if the cache is enabled. The `cache` option may either
%% be `false` to disable local caching, or a store definition to use as the
%% cache.
maybe_cache(StoreOpts, Data) ->
    case hb_opts:get(cache, false, StoreOpts) of
        false -> do_nothing;
        Store ->
            case hb_cache:write(#{ store => Store}, Data) of
                ok -> Data;
                Other -> Other
            end
    end.

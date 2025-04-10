%%% @doc An abstraction for working with maps in HyperBEAM, matching the
%%% generic `maps' module, but additionally supporting the resolution of
%%% links as they are encountered. These functions must be used extremely
%%% carefully. In virtually all circumstances, the `hb_ao:resolve/3' or
%%% `hb_ao:get/3' functions should be used instead, as they will execute the
%%% full AO-Core protocol upon requests (normalizing keys, applying the
%%% appropriate device's functions, as well as resolving links). By using this
%%% module's functions, you are implicitly making the assumption that the message
%%% in question is of the `~message@1.0' form, ignoring any other keys that its
%%% actual device may present. This module is intended for the extremely rare
%%% circumstances in which the additional overhead of the full AO-Core
%%% execution cycle is not acceptable, and the data in question is known to
%%% conform to the `~message@1.0' form.
%%%
%%% If you do not understand any/all of the above, you are in the wrong place!
%%% Utilise the `hb_ao' module and read the documentation therein, saving
%%% yourself from the inevitable issues that will arise from using this
%%% module without understanding the full implications. You have been warned.
-module(hb_maps).
-export([get/2, get/3, get/4, find/2, find/3, is_key/2, keys/1, values/1, size/1]).
-export([put/3, map/2, filter/2, filtermap/2, fold/3, take/2]).
-export([merge/2, remove/2, with/2, without/2, update_with/3, update_with/4]).
-export([from_list/1, to_list/1]).
-include_lib("eunit/include/eunit.hrl").

-spec get(Key :: term(), Map :: map()) -> term().
get(Key, Map) ->
    get(Key, Map, undefined).

-spec get(Key :: term(), Map :: map(), Default :: term()) -> term().
get(Key, Map, Default) ->
    get(Key, Map, Default, #{}).

-spec get(Key :: term(), Map :: map(), Default :: term(), Opts :: map()) -> term().
get(Key, Map, Default, Opts) ->
    hb_ao:ensure_loaded(maps:get(Key, Map, Default), Opts).

-spec find(Key :: term(), Map :: map()) -> {ok, term()} | error.
find(Key, Map) ->
    find(Key, Map, #{}).

-spec find(Key :: term(), Map :: map(), Opts :: map()) -> {ok, term()} | error.
find(Key, Map, Opts) ->
    hb_ao:ensure_loaded(maps:find(Key, Map), Opts).

-spec put(Key :: term(), Value :: term(), Map :: map()) -> map().
put(Key, Value, Map) ->
    maps:put(Key, Value, Map).

-spec is_key(Key :: term(), Map :: map()) -> boolean().
is_key(Key, Map) ->
    maps:is_key(Key, Map).

-spec keys(Map :: map()) -> [term()].
keys(Map) ->
    maps:keys(Map).

-spec values(Map :: map()) -> [term()].
values(Map) ->
    maps:values(Map).

-spec size(Map :: map()) -> non_neg_integer().
size(Map) ->
    maps:size(Map).

-spec map(fun((Key :: term(), Value :: term()) -> term()), Map :: map()) -> map().
map(Fun, Map) ->
    map(Fun, Map, #{}).

-spec map(fun((Key :: term(), Value :: term()) -> term()), Map :: map(), Opts :: map()) -> map().
map(Fun, Map, Opts) ->
    maps:map(fun(K, V) -> Fun(K, hb_ao:ensure_loaded(V, Opts)) end, Map).

-spec merge(map(), map()) -> map().
merge(Map1, Map2) ->
    maps:merge(Map1, Map2).

-spec remove(term(), map()) -> map().
remove(Key, Map) ->
    maps:remove(Key, Map).

-spec with([term()], Map :: map()) -> map().
with(Keys, Map) ->
    maps:with(Keys, Map).

-spec without([term()], Map :: map()) -> map().
without(Keys, Map) ->
    maps:without(Keys, Map).

-spec filter(fun((Key :: term(), Value :: term()) -> boolean()), Map :: map()) -> map().
filter(Fun, Map) ->
    filter(Fun, Map, #{}).

-spec filter(fun((Key :: term(), Value :: term()) -> boolean()), Map :: map(), Opts :: map()) -> map().
filter(Fun, Map, Opts) ->
    maps:filter(fun(K, V) -> Fun(K, hb_ao:ensure_loaded(V, Opts)) end, Map).

-spec filtermap(fun((Key :: term(), Value :: term()) -> {boolean(), term()}), Map :: map()) -> map().
filtermap(Fun, Map) ->
    maps:filtermap(Fun, Map).

-spec fold(fun((Key :: term(), Value :: term(), Acc :: term()) -> term()), Acc :: term(), Map :: map()) -> term().
fold(Fun, Acc, Map) ->
    maps:fold(Fun, Acc, Map).

-spec take(non_neg_integer(), map()) -> map().
take(N, Map) ->
    maps:take(N, Map).

-spec update_with(Key :: term(), Fun :: fun((Value :: term()) -> term()), Map :: map()) -> map().
update_with(Key, Fun, Map) ->
    maps:update_with(Key, Fun, Map).

-spec update_with(Key :: term(), Fun :: fun((Value :: term()) -> term()), Map :: map(), Opts :: map()) -> map().
update_with(Key, Fun, Map, Opts) ->
    maps:update_with(Key, Fun, Map, Opts).

-spec from_list(List :: [{Key :: term(), Value :: term()}]) -> map().
from_list(List) ->
    maps:from_list(List).

-spec to_list(Map :: map()) -> [{Key :: term(), Value :: term()}].
to_list(Map) ->
    maps:to_list(Map).

%%% Tests

get_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(Bin, get(2, Map)).

map_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(#{1 => 1, 2 => Bin, 3 => 3}, map(fun(_K, V) -> V end, Map)).

get_with_typed_link_test() ->
    Bin = <<"123">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{ <<"type">> => integer }}, 3 => 3 },
    ?assertEqual(123, get(2, Map, undefined)).

resolve_on_link_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">> },
    Opts = #{},
    {ok, ID} = hb_cache:write(Msg, Opts),
    ?assertEqual(
        {ok, <<"test-value">>},
        hb_ao:resolve({link, ID, #{}}, <<"test-key">>, #{})
    ).
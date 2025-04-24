%%% @doc In-Memory store that implements a simple least-recently-used cache, offloading to a specified non-volatile store over time.
%%% This cache is registered under `{in_memory, HTTPServerID}`, so that all processes that are executing using the HTTP server’s Opts can find it quickly.
%%%
%%% The Last Recent Used strategy (first is the most recent used, last is the least recent used) is implemented by keeping track of the order and bytes on ets tables:
%%% - A cache table containing all the entries along with the value size and key index.
%%% - A cache indexing table containing all the index pointing to the keys. The IDs are then sorted to ease the eviction policy.
%%% - A cache statistics table containing all the information about the cache size, capacity, and indexing.
-module(hb_cache_lru).

-export([start/2, put/3, get/2]).

-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").

%%% @doc Start the LRU cache
start(NodeMsg, Opts) ->
    spawn(fun() ->
             State = init(NodeMsg, Opts),
             server_loop(State, Opts)
          end).

init(NodeMsg, Opts) ->
    ServerID = hb_ao:get(http_server, NodeMsg, Opts),
    ?event(cache_lru, {start_server, ServerID}),
    % Create LRU tables
    CacheTable = ets:new(hb_cache_lru, [set, public, {read_concurrency, true}]),
    CacheStatsTable = ets:new(hb_cache_lru_stats, [set]),
    CacheIndexTable = ets:new(hb_cache_lru_index, [ordered_set]),
    hb_name:register({in_memory, ServerID}),
    persistent_term:put({in_memory_lru_cache, ServerID}, CacheTable),
    #{cache_table => CacheTable,
      stats_table => CacheStatsTable,
      index_table => CacheIndexTable}.

server_loop(State = #{cache_table := Table}, Opts) ->
    receive
        {get_cache_table, From} ->
            From ! Table;
        {put, Key, Value, From} ->
            put_cache_entry(State, Key, Value, Opts),
            From ! ok;
        {update_recent, Key, Entry} ->
            update_recently_used(State, Key, Entry)
    end,
    server_loop(State, Opts).

%%% @doc Write an entry in the cache
%%% After writing, the LRU is updated by moving the key in the most recent used key to cycle and re-prioritize cache entry
put(Key, Value, Opts) ->
    ServerID = hb_ao:get(http_server, Opts),
    CacheServer =
        case erlang:get({cache_lru, ServerID}) of
            undefined ->
                Server = get_cache_server(ServerID),
                erlang:put({cache_lru, ServerID}, Server),
                Server;
            Pid ->
                Pid
        end,
    CacheServer ! {put, Key, Value, self()},
    receive
        ok ->
            ok
    end.

%%% @doc Retrieve value in the cache from the given key
%%% Because the cache uses LRU, the key is moved on the most recent used key to cycle and re-prioritize cache entry
get(Key, Opts) ->
    ServerID = hb_ao:get(http_server, Opts),
    CacheTable = persistent_term:get({in_memory_lru_cache, ServerID}),
    case get_cache_entry(#{cache_table => CacheTable}, Key) of
        nil ->
            nil;
        Entry = #{value := Value} ->
            CacheServer = get_cache_server(ServerID),
            CacheServer ! {update_recent, Key, Entry},
            Value
    end.

get_cache_server(ServerID) ->
    hb_name:lookup({in_memory, ServerID}).

get_cache_entry(#{cache_table := Table}, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            nil;
        [{_, Entry}] ->
            Entry
    end.

put_cache_entry(State, Key, Value) ->
    ValueSize = erlang:external_size(Value),
    % Default to 1MB (FIXME: to be defined more properly)
    Capacity = hb_opts:get(lru_capacity, 1000000, Opts),
    case ValueSize =< Capacity of
        true ->
            case get_cache_entry(State, Key) of
                nil ->
                    ?event(cache_lru, {assign_entry, Key, Value}),
                    assign_new_entry(State, Key, Value, ValueSize, Capacity);
                Entry ->
                    ?event(cache_lru, {replace_entry, Key, Value}),
                    replace_entry(State, Key, Value, ValueSize, Entry)
            end;
        false ->
            ok
    end.

assign_new_entry(State, Key, Value, ValueSize, Capacity) ->
    case cache_size(State) + ValueSize >= Capacity of
        true ->
            ?event(cache_lru, eviction_required),
            evict_oldest_entry(State, ValueSize);
        false ->
            ok
    end,
    ID = get_index_id(State),
    add_cache_index(State, ID, Key),
    add_cache_entry(State,
                    Key,
                    #{value => Value,
                      id => ID,
                      size => ValueSize}),
    increase_cache_size(State, ValueSize).

cache_size(#{stats_table := Table}) ->
    case ets:lookup(Table, size) of
        [{_, Size}] ->
            Size;
        _ ->
            0
    end.

get_index_id(#{stats_table := StatsTable}) ->
    ets:update_counter(StatsTable, id, {2, 1}, {0, 0}).

add_cache_entry(#{cache_table := Table}, Key, Value) ->
    ets:insert(Table, {Key, Value}).

add_cache_index(#{index_table := Table}, ID, Key) ->
    ets:insert(Table, {ID, Key}).

increase_cache_size(#{stats_table := StatsTable}, ValueSize) ->
    ets:update_counter(StatsTable, size, {2, ValueSize}, {0, 0}).

evict_oldest_entry(State, ValueSize) ->
    evict_oldest_entry(State, ValueSize, 0).

evict_oldest_entry(_State, ValueSize, FreeSize) when FreeSize >= ValueSize ->
    ok;
evict_oldest_entry(State, ValueSize, FreeSize) ->
    case cache_tail_key(State) of
        nil ->
            ok;
        TailKey ->
            #{size := ReclaimedSize, id := ID} = get_cache_entry(State, TailKey),
            ?event(cache_lru, {evict, TailKey, claiming_size, ReclaimedSize}),
            delete_cache_index(State, ID),
            delete_cache_entry(State, TailKey),
            decrease_cache_size(State, ReclaimedSize),
            evict_oldest_entry(State, ValueSize, FreeSize + ReclaimedSize)
    end.

cache_tail_key(#{index_table := Table}) ->
    case ets:first(Table) of
        '$end_of_table' ->
            nil;
        FirstID ->
            [{_, Key}] = ets:lookup(Table, FirstID),
            Key
    end.

delete_cache_index(#{index_table := IndexTable}, ID) ->
    ets:delete(IndexTable, ID).

delete_cache_entry(#{cache_table := Table}, Key) ->
    ets:delete(Table, Key).

decrease_cache_size(#{stats_table := Table}, Size) ->
    ets:update_counter(Table, size, {2, -Size, 0, 0}).

replace_entry(State, Key, Value, ValueSize, OldEntry) ->
    % Update entry and move the keys in the front of the cache as the most used Key
    #{size := PreviousSize} = OldEntry,
    NewEntry = OldEntry#{value := Value, size := ValueSize},
    add_cache_entry(State, Key, NewEntry),
    update_recently_used(State, Key, NewEntry),
    update_cache_size(State, PreviousSize, ValueSize).

update_recently_used(State, Key, Entry) ->
    % Acquire a new ID
    NewID = get_index_id(State),
    #{id := PreviousID} = Entry,
    % Delete previous ID to priorize the new NewID
    delete_cache_index(State, PreviousID),
    add_cache_index(State, NewID, Key),
    % Update the entry's ID
    add_cache_entry(State, Key, Entry#{id := NewID}).

update_cache_size(#{stats_table := Table}, PreviousSize, NewSize) ->
    ets:update_counter(Table, size, [{2, -PreviousSize}, {2, NewSize}]).

%%% Tests

unknown_value_test() ->
    NodeMsg = hb_http_server:set_default_opts(#{}),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, NodeMsg))),
    NodeMsgWithID = maps:put(http_server, ServerID, NodeMsg),
    start(NodeMsgWithID, #{}),
    timer:sleep(100),
    ?assertEqual(nil, get(<<"Key1">>, NodeMsgWithID)).

cache_term_test() ->
    NodeMsg = hb_http_server:set_default_opts(#{}),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, NodeMsg))),
    NodeMsgWithID = maps:put(http_server, ServerID, NodeMsg),
    start(NodeMsgWithID, #{}),
    timer:sleep(100),
    put(<<"Key1">>, <<"Hello">>, NodeMsgWithID),
    ?assertEqual(<<"Hello">>, get(<<"Key1">>, NodeMsgWithID)).

evict_oldest_items_test() ->
    NodeMsg = hb_http_server:set_default_opts(#{}),
    ServerID =
         hb_util:human_id(
             ar_wallet:to_address(
                 hb_opts:get(
                     priv_wallet,
                     no_wallet,
                     NodeMsg
                 )
             )
         ),
    NodeMsgWithID = maps:put(http_server, ServerID, NodeMsg),
    start(NodeMsgWithID, #{lru_capacity => 500}),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    put(<<"Key1">>, Binary, NodeMsgWithID),
    put(<<"Key2">>, Binary, NodeMsgWithID),
    get(<<"Key1">>, NodeMsgWithID),
    put(<<"Key3">>, Binary, NodeMsgWithID),
    ?assertEqual(Binary, get(<<"Key1">>, NodeMsgWithID)),
    ?assertEqual(nil, get(<<"Key2">>, NodeMsgWithID)).

evict_items_with_unsufficient_space_test() ->
    NodeMsg = hb_http_server:set_default_opts(#{}),
    ServerID =
         hb_util:human_id(
             ar_wallet:to_address(
                 hb_opts:get(
                     priv_wallet,
                     no_wallet,
                     NodeMsg
                 )
             )
         ),
    NodeMsgWithID = maps:put(http_server, ServerID, NodeMsg),
    start(NodeMsgWithID, #{lru_capacity => 500}),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    put(<<"Key1">>, Binary, NodeMsgWithID),
    put(<<"Key2">>, Binary, NodeMsgWithID),
    put(<<"Key3">>, crypto:strong_rand_bytes(400), NodeMsgWithID),
    ?assertEqual(nil, get(<<"Key1">>, NodeMsgWithID)),
    ?assertEqual(nil, get(<<"Key2">>, NodeMsgWithID)).

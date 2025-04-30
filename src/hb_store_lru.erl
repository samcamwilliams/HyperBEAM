%%% @doc An in-memory store implementation, following the `hb_store` behavior
%%% and interface. This implementation uses a least-recently-used cache first,
%%% and offloads evicted data to a specified non-volatile store over time.
%%%
%%% This cache is registered under `{in_memory, HTTPServerID}`, in `hb_name`
%%% so that all processes that are executing using the HTTP server’s Opts
%%% can find it quickly.
%%%
%%% The least-recently-used strategy (first is the most recent used, last is the
%%% least recently used) is implemented by keeping track of the order and bytes
%%%  on ets tables:
%%% - A cache table containing all the entries along with the value size and
%%%   key index.
%%% - A cache indexing table containing all the index pointing to the keys. The
%%%   IDs are then sorted to ease the eviction policy.
%%% - A cache statistics table containing all the information about the cache
%%%   size, capacity, and indexing.
-module(hb_store_lru).

-export([start/1, stop/1, reset/1, scope/0]).
-export([write/3, read/2, list/2, type/2, make_link/3, make_group/2]).

-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").

%% @doc Start the LRU cache.
start(Opts) ->
    spawn(fun() ->
             State = init(Opts),
             server_loop(State, Opts)
          end).

%% @doc Stop the LRU in memory by offloading the keys in the ETS tables
%% before exiting the process.
stop(Opts) ->
    ServerID = hb_opts:get(http_server, undefined, Opts),
    CacheServer =
        case erlang:get({cache_lru, ServerID}) of
            undefined ->
                get_cache_server(ServerID);
            Pid ->
                Pid
        end,
    CacheServer ! stop,
    ok.

%% @doc The LRU store is always local, for now.
scope() ->
    local.

%% @doc Reset the store by completely cleaning the ETS tables and
%% delegate the reset to the underlying offloading store.
reset(Opts) ->
    ServerID = hb_opts:get(http_server, undefined, Opts),
    CacheServer =
        case erlang:get({cache_lru, ServerID}) of
            undefined ->
                get_cache_server(ServerID);
            Pid ->
                Pid
        end,
    CacheServer ! {reset, self()},
    receive
        ok ->
            ?event({reset_store, {in_memory, ServerID}}),
            case get_lru_store(Opts) of
                no_store ->
                    ok;
                Store ->
                    hb_store:reset(Store)
            end
    end.

%% @doc Create the `ets' tables for the LRU cache:
%% - The cache of data itself (public, with read concurrency enabled)
%% - A set for the LRU's stats.
%% - An ordered set for the cache's index.
init(Opts) ->
    ServerID = hb_opts:get(http_server, undefined, Opts),
    ?event(cache_lru, {start_server, ServerID}),
    % Create LRU tables
    CacheTable = ets:new(hb_cache_lru, [set, protected, {read_concurrency, true}]),
    CacheStatsTable = ets:new(hb_cache_lru_stats, [set]),
    CacheIndexTable = ets:new(hb_cache_lru_index, [ordered_set]),
    hb_name:register({in_memory, ServerID}),
    persistent_term:put({in_memory_lru_cache, ServerID}, #{cache_table => CacheTable}),
    #{cache_table => CacheTable,
      stats_table => CacheStatsTable,
      index_table => CacheIndexTable}.

server_loop(State =
                #{cache_table := CacheTable,
                  stats_table := StatsTable,
                  index_table := IndexTable},
            Opts) ->
    receive
        {get_cache_table, From} ->
            From ! CacheTable;
        {put, Key, Value, From} ->
            put_cache_entry(State, Key, Value, Opts),
            From ! ok;
        {link, Existing, New, From} ->
            link_cache_entry(State, Existing, New),
            From ! ok;
        {make_group, Key, From} ->
            ?event(cache_lru, {make_group, Key}),
            ensure_dir(State, Key),
            From ! ok;
        {update_recent, Key, Entry} ->
            update_recently_used(State, Key, Entry);
        {reset, From} ->
            ets:delete_all_objects(CacheTable),
            ets:delete_all_objects(StatsTable),
            ets:delete_all_objects(IndexTable),
            From ! ok;
        stop ->
            evict_all_entries(State, Opts),
            exit(self(), ok)
    end,
    server_loop(State, Opts).

%% @doc Write an entry in the cache.
%%
%% After writing, the LRU is updated by moving the key in the most-recently-used
%% key to cycle and re-prioritize cache entry.
write(Opts, RawKey, Value) ->
    Key = hb_store:join(RawKey),
    ServerID = hb_opts:get(http_server, undefined, Opts),
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

%% @doc Retrieve value in the cache from the given key.
%% Because the cache uses LRU, the key is moved on the most recent used key to
%% cycle and re-prioritize cache entry.
read(Opts, RawKey) ->
    ServerID = hb_opts:get(http_server, undefined, Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    Key = resolve(CacheTables, RawKey),
    case get_cache_entry(CacheTables, Key) of
        nil ->
            case get_lru_store(Opts) of
                no_store ->
                    not_found;
                Store ->
                    hb_cache:read(Key, #{store => Store})
            end;
        {raw, Entry = #{value := Value}} ->
            CacheServer = get_cache_server(ServerID),
            CacheServer ! {update_recent, Key, Entry},
            {ok, Value};
        _ ->
            not_found
    end.

resolve(CacheTables, Key) ->
    PathList =
        hb_path:term_to_path_parts(
            hb_store:join(Key)),
    ResolvedParts = lists:map(fun(Path) -> do_resolve(CacheTables, Path) end, PathList),
    hb_store:join(ResolvedParts).

do_resolve(CacheTables, Path) ->
    case get_cache_entry(CacheTables, Path) of
        {link, Link} ->
            do_resolve(CacheTables, Link);
        _ ->
            Path
    end.

%% @doc Make a link from a key to another in the store.
make_link(_, Link, Link) ->
    ok;
make_link(Opts, RawExisting, New) ->
    Existing = hb_store:join(RawExisting),
    ServerID = hb_opts:get(http_server, undefined, Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    case get_cache_entry(CacheTables, Existing) of
        nil ->
            case get_lru_store(Opts) of
                no_store ->
                    no_viable_store;
                Store ->
                    hb_store:make_link(Store, RawExisting, New)
            end;
        _ ->
            CacheServer =
                case erlang:get({cache_lru, ServerID}) of
                    undefined ->
                        Server = get_cache_server(ServerID),
                        erlang:put({cache_lru, ServerID}, Server),
                        Server;
                    Pid ->
                        Pid
                end,
            CacheServer ! {link, Existing, New, self()},
            receive
                ok ->
                    ok
            end
    end.

%% @doc List all the keys registered.
list(Opts, Path) ->
    ServerID = hb_opts:get(http_server, undefined, Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    InMemoryKeys =
        case get_cache_entry(CacheTables, Path) of
            {group, Set} ->
                sets:to_list(Set);
            {link, Link} ->
                list(Opts, Link);
            {raw, #{value := Value}} when is_map(Value) ->
                maps:keys(Value);
            {raw, #{value := Value}} when is_list(Value) ->
                Value;
            nil ->
                []
        end,
    case get_lru_store(Opts) of
        no_store ->
            InMemoryKeys;
        Store ->
            OffloadedKeys = lists:map(fun hb_util:bin/1, hb_cache:list(Path, [Store])),
            InMemoryKeys ++ OffloadedKeys
    end.

%% @doc Determine the type of a key in the store.
type(Opts, Key) ->
    ServerID = hb_opts:get(http_server, undefined, Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    case get_cache_entry(CacheTables, Key) of
        nil ->
            case get_lru_store(Opts) of
                no_store ->
                    not_found;
                Store ->
                    hb_store:type(Store, Key)
            end;
        {raw, _} ->
            simple;
        {link, NewKey} ->
            type(Opts, NewKey);
        {group, _Item} ->
            composite
    end.

%% @doc Create a directory inside the store.
make_group(Opts, Key) ->
    ServerID = hb_opts:get(http_server, undefined, Opts),
    CacheServer =
        case erlang:get({cache_lru, ServerID}) of
            undefined ->
                Server = get_cache_server(ServerID),
                erlang:put({cache_lru, ServerID}, Server),
                Server;
            Pid ->
                Pid
        end,
    CacheServer ! {make_group, Key, self()},
    receive
        ok ->
            ok
    end.

table_keys(TableName) ->
    table_keys(TableName, undefined).

table_keys(TableName, Prefix) ->
    FirstKey = ets:first(TableName),
    table_keys(TableName, FirstKey, Prefix, []).

table_keys(_TableName, '$end_of_table', _Prefix, Acc) ->
    Acc;
table_keys(TableName, CurrentKey, Prefix, Acc) ->
    NextKey = ets:next(TableName, CurrentKey),
    case Prefix of
        undefined ->
            table_keys(TableName, NextKey, Prefix, [CurrentKey | Acc]);
        _ ->
            PrefixParts = hb_path:term_to_path_parts(Prefix),
            Key = hb_path:term_to_path_parts(CurrentKey),
            case lists:prefix(PrefixParts, Key) of
                true ->
                    Extracted = lists:nthtail(length(PrefixParts), Key),
                    table_keys(TableName, NextKey, Prefix, [hb_path:to_binary(Extracted) | Acc]);
                false ->
                    table_keys(TableName, NextKey, Prefix, Acc)
            end
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

put_cache_entry(State, Key, Value, Opts) ->
    ValueSize = erlang:external_size(Value),
    % Default to 1MB (FIXME: to be defined more properly)
    Capacity = hb_opts:get(lru_capacity, 1000000, Opts),
    case ValueSize =< Capacity of
        true ->
            case get_cache_entry(State, Key) of
                nil ->
                    ?event(cache_lru, {assign_entry, Key, Value}),
                    case filename:dirname(Key) of
                        <<".">> ->
                            assign_new_entry(State,
                                             Key,
                                             Value,
                                             ValueSize,
                                             Capacity,
                                             undefined,
                                             Opts);
                        BaseDir ->
                            ensure_dir(State, BaseDir),
                            {group, Entry} = get_cache_entry(State, BaseDir),
                            BaseName = filename:basename(Key),
                            NewGroup = append_key_to_group(BaseName, Entry),
                            add_cache_entry(State, BaseDir, {group, NewGroup}),
                            assign_new_entry(State, Key, Value, ValueSize, Capacity, BaseDir, Opts)
                    end;
                Entry ->
                    ?event(cache_lru, {replace_entry, Key, Value}),
                    replace_entry(State, Key, Value, ValueSize, Entry)
            end;
        false ->
            offload_to_store(Key, Value, [], undefined, Opts)
    end.

ensure_dir(State, Path) ->
    PathParts = hb_path:term_to_path_parts(Path),
    [First | Rest] = PathParts,
    Result = ensure_dir(State, First, Rest),
    Result.

ensure_dir(State, CurrentPath, []) ->
    maybe_create_dir(State, CurrentPath, nil);
ensure_dir(State, CurrentPath, [Next]) ->
    maybe_create_dir(State, CurrentPath, Next),
    ensure_dir(State, hb_store:join([CurrentPath, Next]), []);
ensure_dir(State, CurrentPath, [Next | Rest]) ->
    maybe_create_dir(State, CurrentPath, Next),
    ensure_dir(State, hb_store:join([CurrentPath, Next]), Rest).

maybe_create_dir(State, DirPath, Value) ->
    CurrentValueSet =
        case get_cache_entry(State, DirPath) of
            nil ->
                sets:new();
            {group, CurrentValue} ->
                CurrentValue
        end,
    NewValueSet =
        case Value of
            nil ->
                CurrentValueSet;
            _ ->
                sets:add_element(Value, CurrentValueSet)
        end,
    ?event(cache_lru, {create_group, DirPath, sets:to_list(NewValueSet)}),
    add_cache_entry(State, DirPath, {group, NewValueSet}).

append_key_to_group(Key, Group) ->
    BaseName = filename:basename(Key),
    sets:add_element(BaseName, Group).

assign_new_entry(State, Key, Value, ValueSize, Capacity, Group, Opts) ->
    case cache_size(State) + ValueSize >= Capacity of
        true ->
            ?event(cache_lru, eviction_required),
            evict_oldest_entry(State, ValueSize, Opts);
        false ->
            ok
    end,
    ID = get_index_id(State),
    add_cache_index(State, ID, Key),
    add_cache_entry(State,
                    Key,
                    {raw,
                     #{value => Value,
                       id => ID,
                       size => ValueSize,
                       group => Group}}),
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

link_cache_entry(#{cache_table := Table}, Existing, New) ->
    ?event(cache_lru, {link, Existing, New}),
    ets:insert(Table, {New, {link, Existing}}),
    case ets:lookup(Table, Existing) of
        [{_, {raw, Entry}}] ->
            NewEntry =
                case Entry of
                    #{links := Links} ->
                        [New | Links];
                    _ ->
                        Entry#{links => [New]}
                end,
            ets:insert(Table, {Existing, {raw, NewEntry}});
        _ ->
            ignore
    end.

increase_cache_size(#{stats_table := StatsTable}, ValueSize) ->
    ets:update_counter(StatsTable, size, {2, ValueSize}, {0, 0}).

evict_oldest_entry(State, ValueSize, Opts) ->
    evict_oldest_entry(State, ValueSize, 0, Opts).

evict_oldest_entry(_State, ValueSize, FreeSize, _Opts) when FreeSize >= ValueSize ->
    ok;
evict_oldest_entry(State, ValueSize, FreeSize, Opts) ->
    case cache_tail_key(State) of
        nil ->
            ok;
        TailKey ->
            {raw,
             Entry =
                 #{size := ReclaimedSize,
                   id := ID,
                   value := TailValue,
                   group := Group}} =
                get_cache_entry(State, TailKey),
            ?event(cache_lru, {evict, TailKey, claiming_size, ReclaimedSize}),
            delete_cache_index(State, ID),
            delete_cache_entry(State, TailKey),
            decrease_cache_size(State, ReclaimedSize),
            Links = maps:get(links, Entry, []),
            case Group of
                undefined ->
                    ignore;
                _ ->
                    {group, GroupSet} = get_cache_entry(State, Group),
                    BaseName = filename:basename(TailKey),
                    UpdatedGroupSet = sets:del_element(BaseName, GroupSet),
                    case sets:size(UpdatedGroupSet) of
                        0 ->
                            delete_cache_entry(State, Group);
                        _ ->
                            add_cache_entry(State, Group, {group, UpdatedGroupSet})
                    end
            end,
            offload_to_store(TailKey, TailValue, Links, Group, Opts),
            evict_oldest_entry(State, ValueSize, FreeSize + ReclaimedSize, Opts)
    end.

evict_all_entries(#{cache_table := Table}, Opts) ->
    lists:foreach(fun(Key) ->
                     [{_, {raw, Entry}}] = ets:lookup(Table, Key),
                     #{value := Value, group := Group} = Entry,
                     Links = maps:get(links, Entry, []),
                     offload_to_store(Key, Value, Links, Group, Opts)
                  end,
                  table_keys(Table)).

offload_to_store(TailKey, TailValue, Links, Group, Opts) ->
    case get_lru_store(Opts) of
        no_store ->
            ok;
        Store ->
            case Group of
                undefined ->
                    ignore;
                _ ->
                    hb_store:make_group(Store, Group)
            end,
            case hb_cache:write(TailValue, Opts#{store => Store}) of
                {ok, CacheID} ->
                    hb_cache:link(CacheID, TailKey, Opts#{store => Store}),
                    lists:foreach(fun(Link) -> hb_cache:link(Link, TailKey, Opts#{store => Store})
                                  end,
                                  Links),
                    ?event(cache_lru, {offloaded_key, TailKey}),
                    ok;
                {error, Err} ->
                    ?event(warning, {error_offloading_to_local_cache, Err}),
                    {error, Err}
            end
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
    ets:delete(Table, Key),
    ?event(cache_lru, {deleted, Key}).

decrease_cache_size(#{stats_table := Table}, Size) ->
    ets:update_counter(Table, size, {2, -Size, 0, 0}).

replace_entry(State, Key, Value, ValueSize, OldEntry) ->
    % Update entry and move the keys in the front of the cache as the most used Key
    #{size := PreviousSize} = OldEntry,
    NewEntry = OldEntry#{value := Value, size := ValueSize},
    add_cache_entry(State, Key, {raw, NewEntry}),
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
    add_cache_entry(State, Key, {raw, Entry#{id := NewID}}).

update_cache_size(#{stats_table := Table}, PreviousSize, NewSize) ->
    ets:update_counter(Table, size, [{2, -PreviousSize}, {2, NewSize}]).

get_lru_store(Opts) ->
    hb_maps:get(<<"store">>,
                Opts,
                #{<<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet/lru">>}).

%%% Tests

unknown_value_test() ->
    DefaultOpts = hb_http_server:set_default_opts(#{<<"store">> => no_store}),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(Opts),
    timer:sleep(100),
    ?assertEqual(not_found, read(Opts, <<"Key1">>)).

cache_term_test() ->
    DefaultOpts = hb_http_server:set_default_opts(#{<<"store">> => no_store}),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(Opts),
    timer:sleep(100),
    write(Opts, <<"Key1">>, <<"Hello">>),
    ?assertEqual({ok, <<"Hello">>}, read(Opts, <<"Key1">>)).

evict_oldest_items_test() ->
    DefaultOpts = hb_http_server:set_default_opts(#{<<"store">> => no_store}),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(maps:put(lru_capacity, 500, Opts)),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    write(Opts, <<"Key1">>, Binary),
    write(Opts, <<"Key2">>, Binary),
    read(Opts, <<"Key1">>),
    write(Opts, <<"Key3">>, Binary),
    ?assertEqual({ok, Binary}, read(Opts, <<"Key1">>)),
    ?assertEqual(not_found, read(Opts, <<"Key2">>)).

evict_items_with_insufficient_space_test() ->
    DefaultOpts = hb_http_server:set_default_opts(#{<<"store">> => no_store}),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(maps:put(lru_capacity, 500, Opts)),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    write(Opts, <<"Key1">>, Binary),
    write(Opts, <<"Key2">>, Binary),
    write(Opts, <<"Key3">>, crypto:strong_rand_bytes(400)),
    ?assertEqual(not_found, read(Opts, <<"Key1">>)),
    ?assertEqual(not_found, read(Opts, <<"Key2">>)).

evict_but_able_to_read_from_fs_store_test() ->
    StoreOpts = #{<<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST/lru">>},
    DefaultOpts =
        hb_http_server:set_default_opts(#{lru_capacity => 500, <<"store">> => StoreOpts}),
    hb_store:reset(StoreOpts),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(Opts),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    write(Opts, <<"Key1">>, Binary),
    write(Opts, <<"Key2">>, Binary),
    read(Opts, <<"Key1">>),
    write(Opts, <<"Key3">>, Binary),
    ?assertEqual({ok, Binary}, read(Opts, <<"Key1">>)),
    ?assertEqual({ok, Binary}, read(Opts, <<"Key2">>)),
    % Directly offloads if the data is more than the LRU capacity
    write(Opts, <<"Sub/Key">>, crypto:strong_rand_bytes(600)),
    ?assertMatch({ok, _}, read(Opts, <<"Sub">>)).

stop_test() ->
    StoreOpts = #{<<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST/lru">>},
    DefaultOpts = hb_http_server:set_default_opts(#{<<"store">> => StoreOpts}),
    hb_store:reset(StoreOpts),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    ServerPID = start(maps:put(lru_capacity, 500, Opts)),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    write(Opts, <<"Key1">>, Binary),
    write(Opts, <<"Key2">>, Binary),
    stop(Opts),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(ServerPID)),
    ?assertEqual({ok, Binary}, hb_store:read(StoreOpts, <<"Key1">>)),
    ?assertEqual({ok, Binary}, hb_store:read(StoreOpts, <<"Key2">>)).

reset_test() ->
    StoreOpts = #{<<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST/lru">>},
    hb_store:reset(StoreOpts),
    DefaultOpts = hb_http_server:set_default_opts(#{<<"store">> => StoreOpts}),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(Opts),
    timer:sleep(100),
    write(Opts, <<"Key1">>, <<"Hello">>),
    write(Opts, <<"Key2">>, <<"Hi">>),
    reset(Opts),
    ?assertEqual(not_found, read(Opts, <<"Key1">>)),
    #{cache_table := CacheTable} = persistent_term:get({in_memory_lru_cache, ServerID}),
    ?assertEqual([], ets:tab2list(CacheTable)).

list_test() ->
    StoreOpts = #{<<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST/lru">>},
    DefaultOpts =
        hb_http_server:set_default_opts(#{<<"store">> => StoreOpts, lru_capacity => 500}),
    hb_store:reset(StoreOpts),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(Opts),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    make_group(Opts, <<"Sub">>),
    write(Opts, <<"Hello">>, <<"World">>),
    write(Opts, <<"Sub/Key1">>, Binary),
    write(Opts, <<"Sub/Key2">>, Binary),
    ?assertEqual([<<"Key1">>, <<"Key2">>], lists:sort(list(Opts, <<"Sub">>))),
    write(Opts, <<"Sub/Key3">>, Binary),
    ?assertEqual([<<"Key1">>, <<"Key2">>, <<"Key3">>], lists:sort(list(Opts, <<"Sub">>))),
    write(Opts, <<"Sub/Inner/Key1">>, Binary),
    ?assertEqual([<<"Inner">>, <<"Key1">>, <<"Key2">>, <<"Key3">>],
                 lists:sort(list(Opts, <<"Sub">>))),
    write(Opts, <<"Complex">>, #{<<"a">> => 10, <<"b">> => Binary}),
    ?assertEqual([<<"a">>, <<"b">>], list(Opts, <<"Complex">>)).

type_test() ->
    StoreOpts = #{<<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST/lru">>},
    DefaultOpts =
        hb_http_server:set_default_opts(#{<<"store">> => StoreOpts, lru_capacity => 500}),
    hb_store:reset(StoreOpts),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, no_wallet, DefaultOpts))),
    Opts = maps:put(http_server, ServerID, DefaultOpts),
    start(Opts),
    timer:sleep(100),
    Binary = crypto:strong_rand_bytes(200),
    write(Opts, <<"Key1">>, Binary),
    ?assertEqual(simple, type(Opts, <<"Key1">>)),
    write(Opts, <<"Sub/Key1">>, Binary),
    ?assertEqual(composite, type(Opts, <<"Sub">>)),
    make_link(Opts, <<"Key1">>, <<"KeyLink">>),
    ?assertEqual(simple, type(Opts, <<"KeyLink">>)).

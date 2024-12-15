-module(hb_cache).
-export([read/2, write/3, write_result/4]).
-export([list/2, list_numbered/2]).
%%% Exports for modules that utilize hb_cache.
-export([test_opts/0, test_unsigned/1, test_signed/1]).
-include("src/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A cache of Converge Protocol messages and compute results.
%%%
%%% In Converge Protocol, every message is a combinator: The message itself
%%% represents a 'processor' that can be applied to a new message, yielding a
%%% result. As a consequence, a simple way to understand its data model is to
%%% think of it as a dictionary: The keys in the dictionary are pairs of messages
%%% (Msg1, Msg2), which can be combined in order to yield a result -- the
%%% associated value.
%%% 
%%% Given this structure, HyperBEAM stores each of the messages in a key-value
%%% cache on disk. The cache is a simple wrapper that allows us to look up either
%%% the direct key (a message's ID -- either signed or unsigned) or a 'subpath'.
%%% We also store 'links' in this cache between messages. In the backend, we use
%%% the given `hb_store` to persist the actual data.

%% List all items in a directory, assuming they are numbered.
list_numbered(Path, Opts) ->
    SlotDir = hb_store:path(hb_opts:get(store, no_viable_store, Opts), Path),
    [ list_to_integer(Name) || Name <- list(SlotDir, Opts) ].

%% @doc List all items in a directory.
list(Path, Opts) ->
    case hb_store:list(hb_opts:get(store, no_viable_store, Opts), Path) of
        {ok, Names} -> Names;
        {error, _} -> []
    end.

%% @doc Writes a computation result to the cache.
%% The process outputs a series of key values in the cache as follows:
%%
%%      /RootAddr: Either the Msg3ID alone, or `Msg1ID/Msg2ID` if the keys to
%%                 store are not complete. A caller may want to cache only some
%%                 of the keys because serializing the entire state would be 
%%                 expensive.
%%      /ShortComputePath: An optional link to the RootAddr, created if Msg2 
%%                         contains only a path.
%%      /RootAddr/[Key1, Key2, ...]: The values of the keys that are found in
%%                     the resulting message.
%%      /Msg1.signed_id/Msg2.{unsigned_id,signed_id}: Links from the path that
%%                     led to the creation of the resulting message.
write_result(Msg1, Msg2, Msg3, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    % Calculate the 'address' of the resulting message and the list of keys to
    % cache or `all`.
    {[RootAddress|SecondaryAddresses], KeysToCache}
        = result_root_paths(Msg1, Msg2, Msg3, Opts),
    ?event(
        {writing_result,
            {root_address, RootAddress},
            {keys_to_cache, KeysToCache}
        }
    ),
    % Create a path for the root from the root address.
    RootPath = hb_store:path(Store, [RootAddress]),
    % Write the message at the address, with only the keys we are caching.
    ok = write(RootPath, maps:with(KeysToCache, Msg3), Opts),
    % Link the secondary addresses to the root address.
    lists:foreach(
        fun(SecondaryAddress) ->
            SecondaryPath = hb_store:path(Store, [SecondaryAddress]),
            ok = hb_store:make_link(Store, RootPath, SecondaryPath)
        end,
        SecondaryAddresses
    ),
    ok.

%% @doc Given a computation result (Msg2 applied to Msg1) and a HyperBEAM `Opts` 
%% map returns a tuple of the 'root' path that the message should be written to,
%% and a list of the keys to write.
result_root_paths(Msg1, Msg2, Msg3, Opts) ->
    case hb_opts:get(cache_keys, all, Opts) of
        all ->
            % Get the 'id' and the 'unsigned_id' of the resulting message, and
            % the path that led to the creation of the resulting message. We 
            % use 'uniq' to avoid duplicates, but this may be inefficient if
            % ID generation is expensive for the particular message encoding.
            {
                lists:uniq(
                    [
                        hb_converge:get(id, Msg3, Opts),
                        hb_converge:get(unsigned_id, Msg3, Opts),
                        hb_path:compute_path(Msg1, Msg2, Opts)
                    ]
                ),
                maps:keys(Msg3)
            };
        Keys ->
            {
                [hb_path:compute_path(Msg1, Msg2, Opts)],
                Keys
            }
    end.

%% @doc Writes a message to the cache. Optionally, the message is written to a
%% path given by the caller. If it is not given, the message is written to the
%% ID of the message.
write(Item, Opts) ->
    write(
        hb_store:path(hb_opts:get(store, no_viable_store, Opts), ["messages"]),
        Item,
        Opts
    ).
write(Path, Message, Opts) when is_map(Message) ->
    write(
        Path,
        ar_bundles:normalize(hb_message:message_to_tx(Message)),
        Opts
    );
write(Path, Item, Opts) when not is_record(Item, tx) ->
    ?event(writing_non_tx_item),
    write(Path, ar_bundles:normalize(Item), Opts);
write(Path, Item = #tx{ unsigned_id = ?DEFAULT_ID }, Opts) ->
    ?event(write_of_default_id_tx_requested),
    write(Path, ar_bundles:normalize(Item), Opts);
write(Path, Item, Opts) ->
    ?event({writing_item, {path, Path}, {item, Item}}),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case ar_bundles:type(Item) of
        binary ->
            % The item is a raw binary. Write it into the store and make a
            % link for the signed ID if it is different.
            UnsignedID = ar_bundles:id(Item, unsigned),
            SignedID = ar_bundles:id(Item, signed),
            UnsignedPath = hb_store:path(Store, [Path, hb_util:human_id(UnsignedID)]),
            ?event({writing_item, UnsignedPath}),
            ok = hb_store:write(Store, UnsignedPath, ar_bundles:serialize(Item)),
            if SignedID =/= not_signed ->
                SignedPath = hb_store:path(Store, [Path, hb_util:human_id(SignedID)]),
                ?event({linking_item, SignedPath}),
                hb_store:make_link(Store, UnsignedPath, SignedPath);
            true -> link_unnecessary
            end;
        CompositeType ->
            ?event({writing_composite_item, {path, Path}, {item, Item}}),
            write_raw_composite(Store, Path, CompositeType, Item)
    end,
    ok.

write_raw_composite(Store, Path, map, Item) ->
    % The item contains other messages. We should:
    % 1. Write the header item into the store without its children.
    %    The header item is the normal item, but with the manifest placed
    %    in the data field. This is unpacked in `read/2`.
    % 2. Write each child into the store.
    % 3. Make links from the keys in the map to the corresponding messages.
    % This process will recurse as necessary to write grandchild messages.
    UnsignedHeaderID = ar_bundles:id(Item, unsigned),
    ?event({starting_composite_write, hb_util:human_id(UnsignedHeaderID)}),
    ok =
        hb_store:make_group(
            Store,
            Dir = hb_store:path(Store, [Path, hb_util:human_id(UnsignedHeaderID)])
        ),
    SignedHeaderID = ar_bundles:id(Item, signed),
    ?event(
        {writing_composite_header,
            {unsigned, hb_util:human_id(UnsignedHeaderID)},
            {signed, hb_util:human_id(SignedHeaderID)}
        }
    ),
    hb_store:make_link(
        Store,
        hb_store:path(Store, [Path, hb_util:human_id(UnsignedHeaderID)]),
        hb_store:path(Store, [Path, hb_util:human_id(SignedHeaderID)])
    ),
    hb_store:write(
        Store,
        hb_store:path(Store, [Path, hb_util:human_id(UnsignedHeaderID), "item"]),
        ar_bundles:serialize(
            Item#tx{
                data = #{ <<"manifest">> => ar_bundles:manifest_item(Item) }
            }
        )
    ),
    maps:map(fun(Key, Subitem) ->
        % Note: _Not_ relative to the Path! All messages are stored at the
        % same root of the store.
        ok = write(Store, Subitem),
        hb_store:make_link(
            Store,
            hb_store:path(Store, [Path, hb_util:human_id(Subitem)]),
            hb_store:path(Store, [Dir, Key])
        )
    end, ar_bundles:map(Item)),
    ok;
write_raw_composite(Store, Path, list, Item) ->
    write_raw_composite(Store, Path, map, ar_bundles:normalize(Item)).

%% @doc Read a message from the cache given a path.
read(PathPart, Opts) when not is_list(PathPart) ->
    read([PathPart], Opts);
read(Path, Opts) ->
    ?event({looking_up, Path}),
    % Calculate the correct `store` to use for the cache lookup.
    Store = opts_to_store(Opts),
    % Resolve known links through the caller's request path.
    ResolvedPath =
        P2 = hb_store:resolve(
            Store,
            P1 =
                hb_store:path(
                    Store,
                    lists:map(fun hb_util:human_id/1, Path)
                )
        ),
    % Attempt to resolve the path and return to the caller.
    ?event({resolving_path, P1, P2, ResolvedPath}),
    read_raw(Store, ResolvedPath).

%% @doc Internal function for reading a raw message from the cache.
%% This function returns messages as #tx records and should not be used
%% outside of the cache module.
read_raw(Store, RawPath) ->
    ?event({reading_message, RawPath, Store}),
    MessagePath = hb_store:path(Store, RawPath),
    case hb_store:type(Store, MessagePath) of
        composite ->
            ?event({reading_composite_message, MessagePath}),
            % The message is a bundle and we want the whole item.
            % Read the root and reconstruct it.
            RootPath = hb_store:path(Store, [MessagePath, "item"]),
            {ok, Root} = read_raw_simple_message(Store, RootPath),
            % The bundle is a map of its children by ID. Reconstruct
            % the bundle by reading each child.
            ?no_prod("Reconstructing bundle unnecessarily serializes and deserializes"),
            %% This serialization/deserialization normalizes away the manifest wrapping
            %% tags and object, added by `write_composite/3`. We do this so that we can
            %% separate the bundle items from the main object in storage, but if ar_bundles
            %% exposed a function to rebuild the TX object's manifest we could avoid this.
            {ok,
                ar_bundles:deserialize(ar_bundles:normalize(Root#tx {
                    data = maps:map(
                        fun(_, Key) ->
                            {ok, Child} =
                                read_raw(Store, ["messages", hb_util:human_id(Key)]),
                            Child
                        end,
                        ar_bundles:parse_manifest(
                            maps:get(<<"manifest">>, Root#tx.data)
                        )
                    )
                }))
            };
        simple -> read_raw_simple_message(Store, MessagePath);
        not_found -> not_found
    end.

read_raw_simple_message(Store, Path) ->
    {ok, Bin} = hb_store:read(Store, Path),
    {ok, ar_bundles:deserialize(Bin)}.

%% @doc Calculate the `hb_store`s that should be used during
%% cache operation resolution.
opts_to_store(Opts) ->
    case hb_opts:get(cache_lookup, {only, local}, Opts) of
        {only, Scope} ->
            hb_store:scope(
                hb_opts:get(store, no_viable_store, Opts),
                Scope
            );
        UserStore -> UserStore
    end.

%%% Tests

%% Helpers

test_opts() ->
    #{ cache_control => no_cache, store => test_cache() }.

test_cache() ->
    Store = [{hb_store_fs, #{ prefix => "test-cache" }}],
    hb_store:reset(Store),
    Store.

test_unsigned(Data) ->
    #{
        <<"Example">> => <<"Tag">>,
        <<"Data">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) ->
    hb_message:sign(test_unsigned(Data), ar_wallet:new()).

%% @doc Test storing and retrieving a simple unsigned item
store_simple_unsigned_item_test() ->
    Opts = test_opts(),
    Item = test_unsigned(<<"Simple unsigned data item">>),
    %% Write the simple unsigned item
    ok = write(Item, Opts),
    %% Read the item back
    {ok, RetrievedItem} =
        read(
            [
                "messages",
                hb_util:human_id(hb_converge:get(id, Item))
            ],
            Opts
        ),
    ?assertEqual(Item, RetrievedItem).

%% @doc Test storing and retrieving a simple signed item
simple_signed_item_test() ->
    Opts = test_opts(),
    Item = test_signed(<<"Simple signed data item">>),
    %% Write the simple signed item
    ok = write(Item, Opts),
    %% Read the item back
    {ok, RetrievedItemUnsigned} = read(hb_converge:get(unsigned_id, Item), Opts),
    {ok, RetrievedItemSigned} = read(hb_converge:get(id, Item), Opts),
    %% Assert that the retrieved item matches the original and verifies
    ?assertEqual(Item, RetrievedItemUnsigned),
    ?assertEqual(Item, RetrievedItemSigned),
    ?assertEqual(true, ar_bundles:verify_item(RetrievedItemSigned)).

%% @doc Test storing and retrieving a composite unsigned item
composite_unsigned_item_test_ignore() ->
    Opts = test_opts(),
    ItemData = #{
        <<"key1">> => test_unsigned(<<"value1">>),
        <<"key2">> => test_unsigned(<<"value2">>)
    },
    Item = ar_bundles:deserialize(test_unsigned(ItemData)),
    ok = write(Item, Opts),
    {ok, RetrievedItem} = ?event(read(hb_converge:get(id, Item), Opts)),
    ?assertEqual(
        hb_converge:get(unsigned_id, (maps:get(<<"key1">>, Item#tx.data)), unsigned),
        hb_converge:get(unsigned_id, (maps:get(<<"key1">>, RetrievedItem#tx.data)), unsigned)
    ),
    ?assertEqual(
        hb_converge:get(unsigned_id, (maps:get(<<"key2">>, Item#tx.data)), unsigned),
        hb_converge:get(unsigned_id, (maps:get(<<"key2">>, RetrievedItem#tx.data)), unsigned)
    ),
    ?assertEqual(
        ar_bundles:id(Item, unsigned),
        ar_bundles:id(RetrievedItem, unsigned)
    ).

%% @doc Test storing and retrieving a composite signed item
composite_signed_item_test_ignore() ->
    Opts = test_opts(),
    ItemData = #{
        <<"key1">> => test_signed(<<"value1">>),
        <<"key2">> => test_signed(<<"value2">>)
    },
    Item = ar_bundles:deserialize(test_signed(ItemData)),
    ok = write(Item, Opts),
    {ok, RetrievedItem} = ?event(read(ar_bundles:id(Item, signed), Opts)),
    ?assertEqual(
        ar_bundles:id((maps:get(<<"key1">>, Item#tx.data)), unsigned),
        ar_bundles:id((maps:get(<<"key1">>, RetrievedItem#tx.data)), unsigned)
    ),
    ?assertEqual(
        ar_bundles:id((maps:get(<<"key2">>, Item#tx.data)), signed),
        ar_bundles:id((maps:get(<<"key2">>, RetrievedItem#tx.data)), signed)
    ),
    ?assertEqual(ar_bundles:id(Item, unsigned), ar_bundles:id(RetrievedItem, unsigned)),
    ?assertEqual(ar_bundles:id(Item, signed), ar_bundles:id(RetrievedItem, signed)),
    ?assertEqual(true, ar_bundles:verify_item(Item)),
    ?assertEqual(true, ar_bundles:verify_item(RetrievedItem)).

%% @doc Test deeply nested item storage and retrieval
deeply_nested_item_test() ->
    Opts = test_opts(),
    %% Create nested data
    DeepValueTx = test_signed(<<"deep_value">>),
    Level3Tx = test_unsigned(#{
        <<"level3_key">> => DeepValueTx
    }),
    Level2Tx = test_signed(#{
        <<"level2_key">> => Level3Tx
    }),
    Level1Tx = test_unsigned(#{
        <<"level1_key">> => Level2Tx
    }),
    %% Write the nested item
    ok = write(Level1Tx, Opts),
    %% Read the deep value back using subpath
    {ok, RetrievedItem} =
        read(
            [
                hb_util:human_id(Level1Tx),
                "level1_key",
                "level2_key",
                "level3_key"
            ],
            Opts
        ),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual(<<"deep_value">>, RetrievedItem#tx.data),
    ?assertEqual(
        ar_bundles:id(DeepValueTx, unsigned),
        ar_bundles:id(RetrievedItem, unsigned)
    ).

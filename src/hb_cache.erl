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
write(Message, Opts) ->
    write([], Message, Opts).
write(Path, Message, Opts) ->
    %?event({writing_message, {path, Path}}),
    case hb_message:type(Message) of
        binary ->
            % The item is a raw binary. Write it into the store and make a
            % message around it.
            write(
                Path,
                #{ <<"Converge-Form">> => <<"Value">>, data => Message },
                Opts
            );
        shallow ->
            % The item is a raw binary. Write it into the store and make a
            % link for the signed ID if it is different.
            Store = hb_opts:get(store, no_viable_store, Opts),
            UnsignedID = hb_converge:get(unsigned_id, Message),
            SignedID = hb_converge:get(id, Message),
            UnsignedPath = hb_store:path(Store, [Path, hb_util:human_id(UnsignedID)]),
            %?event({writing_single_layer_message, UnsignedPath}),
            TX = hb_message:message_to_tx(Message),
            ok = hb_store:write(Store, UnsignedPath, ar_bundles:serialize(TX)),
            if SignedID =/= not_signed ->
                SignedPath = hb_store:path(Store, [Path, hb_util:human_id(SignedID)]),
                %?event({linking_single_layer_message, SignedPath, UnsignedPath}),
                hb_store:make_link(Store, UnsignedPath, SignedPath);
            true -> link_unnecessary
            end,
            {ok, UnsignedID};
        deep ->
            %?event({writing_composite_message, {path, Path}, {item, Message}}),
            write_raw_composite(Path, Message, Opts)
    end.

write_raw_composite(Path, Msg, Opts) when is_map(Msg) ->
    % The item contains other messages. We should:
    % 1. Write the header item into the store without its children.
    %    The header item is the normal item, but with `Ref:[Key]` tags
    %    for each child that is stored independently.
    % 2. Write each child into the store.
    % 3. Make links from the keys in the map to the corresponding messages.
    % This process will recurse as necessary to write grandchild messages.
    FullTX = hb_message:message_to_tx(hb_message:minimize(Msg)),
    UnsignedHeaderID = ar_bundles:id(FullTX, unsigned),
    %?event({starting_composite_write, hb_util:human_id(UnsignedHeaderID)}),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ok =
        hb_store:make_group(
            Store,
            Group = hb_store:path(Store, [Path, hb_util:human_id(UnsignedHeaderID)])
        ),
    {DeepKeyVals, FlatKeyVals} = 
        lists:partition(
            fun({_, Value}) -> is_map(Value) end,
            lists:filter(
                fun({Key, _}) -> not hb_private:is_private(Key) end,
                maps:to_list(Msg)
            )
        ),
    Subkeys = lists:map(
        fun({Key, Subitem}) ->
            % Note: _Not_ relative to the Path! All messages are stored at the
            % same root of the store.
            {ok, SubitemPath} = write(Path, Subitem, Opts),
            hb_store:make_link(
                Store,
                SubitemPath,
                hb_store:path(Store, [Group, Key])
            ),
            %?event({linked, {key, KeyPath}, {file, SubitemPath}}),
            {Key, hb_path:hd(hb_path:term_to_path(SubitemPath), Opts)}
        end, DeepKeyVals),
    FlatMsg =
        maps:merge(
            maps:from_list(FlatKeyVals),
            maps:from_list(
                lists:map(
                    fun({Key, SubitemPath}) ->
                        {<<"Ref:", Key/binary>>, SubitemPath}
                    end,
                    Subkeys
                )
            )
        ),
    FlatTX = hb_message:message_to_tx(FlatMsg),
    hb_store:write(
        Store,
        hb_store:path(Store, [Path, hb_util:human_id(UnsignedHeaderID), "header"]),
        ar_bundles:serialize(FlatTX)
    ),
    case hb_message:signers(Msg) of
        [] -> do_nothing;
        _ ->
            SignedHeaderID = ar_bundles:id(FullTX, signed),
            hb_store:make_link(
                Store,
                hb_store:path(Store, [Path, hb_util:human_id(UnsignedHeaderID)]),
                hb_store:path(Store, [Path, hb_util:human_id(SignedHeaderID)])
            )
    end,
    {ok, Group};
write_raw_composite(Path, Item, Opts) when is_list(Item) ->
    write_raw_composite(Path, ar_bundles:normalize(Item), Opts).

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
                    lists:map(
                        fun(ID) when ?IS_ID(ID) -> hb_util:human_id(ID);
                           (ID) -> ID
                        end,
                        Path
                    )
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
            % The message is a bundle and we want the whole item.
            % Read the root and reconstruct it.
            RootPath = hb_store:path(Store, [MessagePath, "header"]),
            {ok, Root} = read_raw_simple_message(Store, RootPath),
            % The bundle is a map of its children by ID. Reconstruct
            % the bundle by reading each child.
            {ok,
                maps:from_list(
                    lists:map(
                        fun({<<"Ref:", Key/binary>>, Path}) ->
                            ?event({reading_composite_child, Path}),
                            {ok, Child} = read_raw(Store, Path),
                            {Key, Child};
                           ({Key, Value}) -> {Key, Value}
                        end,
                        maps:to_list(Root)
                    )
                )
            };
        simple -> read_raw_simple_message(Store, MessagePath);
        not_found -> not_found
    end.

read_raw_simple_message(Store, Path) ->
    {ok, Bin} = hb_store:read(Store, Path),
    {ok,
        hb_message:minimize(
            hb_message:tx_to_message(ar_bundles:deserialize(Bin))
        )
    }.

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

-define(TEST_WALLET_NAME, pb_cache_test_wallet).

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
    {ok, _} = write(Item, Opts),
    %% Read the item back
    {ok, RetrievedItem} = read(hb_util:human_id(hb_converge:get(id, Item)), Opts),
    ?assert(hb_message:match(Item, RetrievedItem)).

%% @doc Test storing and retrieving a simple signed item
simple_signed_item_test() ->
    Opts = test_opts(),
    Msg = test_signed(<<"Simple signed data item">>),
    %% Write the simple signed item
    {ok, _} = write(Msg, Opts),
    %% Read the item back
    UnsignedID = hb_converge:get(unsigned_id, Msg),
    SignedID = hb_converge:get(id, Msg),
    ?event({reading_by_unsigned, UnsignedID}),
    {ok, RetrievedItemUnsigned} = read(UnsignedID, Opts),
    ?event({reading_by_signed, SignedID}),
    {ok, RetrievedItemSigned} = read(SignedID, Opts),
    %% Assert that the retrieved item matches the original and verifies
    ?assert(hb_message:match(Msg, RetrievedItemUnsigned)),
    ?assert(hb_message:match(Msg, RetrievedItemSigned)),
    ?assertEqual(true, hb_message:verify(RetrievedItemSigned)).

% %% Test storing and retrieving a composite unsigned item
% composite_unsigned_item_test_ignore() ->
%     ItemData = #{
%         <<"key1">> => create_unsigned_tx(<<"value1">>),
%         <<"key2">> => create_unsigned_tx(<<"value2">>)
%     },
%     Item = ar_bundles:deserialize(create_unsigned_tx(ItemData)),
%     ok = write(TestStore = test_cache(), Item),
%     {ok, RetrievedItem} = ?event(read_message(TestStore, ar_bundles:id(Item))),
%     ?assertEqual(
%         ar_bundles:id((maps:get(<<"key1">>, Item#tx.data)), unsigned),
%         ar_bundles:id((maps:get(<<"key1">>, RetrievedItem#tx.data)), unsigned)
%     ),
%     ?assertEqual(
%         ar_bundles:id((maps:get(<<"key2">>, Item#tx.data)), unsigned),
%         ar_bundles:id((maps:get(<<"key2">>, RetrievedItem#tx.data)), unsigned)
%     ),
%     ?assertEqual(
%         ar_bundles:id(Item, unsigned),
%         ar_bundles:id(RetrievedItem, unsigned)
%     ).

%% Test storing and retrieving a composite signed item
% composite_signed_item_test_ignore() ->
%     ItemData = #{
%         <<"key1">> => create_signed_tx(<<"value1">>),
%         <<"key2">> => create_signed_tx(<<"value2">>)
%     },
%     Item = ar_bundles:deserialize(create_signed_tx(ItemData)),
%     ok = write(TestStore = test_cache(), Item),
%     {ok, RetrievedItem} = ?event(read_message(TestStore, ar_bundles:id(Item, signed))),
%     ?assertEqual(
%         ar_bundles:id((maps:get(<<"key1">>, Item#tx.data)), unsigned),
%         ar_bundles:id((maps:get(<<"key1">>, RetrievedItem#tx.data)), unsigned)
%     ),
%     ?assertEqual(
%         ar_bundles:id((maps:get(<<"key2">>, Item#tx.data)), signed),
%         ar_bundles:id((maps:get(<<"key2">>, RetrievedItem#tx.data)), signed)
%     ),
%     ?assertEqual(ar_bundles:id(Item, unsigned), ar_bundles:id(RetrievedItem, unsigned)),
%     ?assertEqual(ar_bundles:id(Item, signed), ar_bundles:id(RetrievedItem, signed)),
%     ?assertEqual(true, ar_bundles:verify_item(Item)),
%     ?assertEqual(true, ar_bundles:verify_item(RetrievedItem)).

%% @doc Test deeply nested item storage and retrieval
deeply_nested_item_test() ->
    Opts = test_opts(),
    %% Create nested data
    DeepValueMsg = test_signed(<<"deep_value">>),
    Outer =
        #{
            <<"Level1">> =>
                hb_message:sign(
                    #{
                        <<"Level2">> =>
                            #{
                                <<"Level3">> => DeepValueMsg,
                                <<"E">> => <<"F">>
                            },
                        <<"C">> => <<"D">>
                    },
                    ar_wallet:new()
                ),
            <<"A">> => <<"B">>
        },
    %% Write the nested item
    {ok, _} = write(Outer, Opts),

    %% Read the deep value back using subpath
    {ok, DeepMsg} =
        read(
            [
                OuterID = hb_util:human_id(hb_converge:get(id, Outer)),
                "Level1",
                "Level2",
                "Level3"
            ],
            Opts
        ),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual(<<"deep_value">>, hb_converge:get(data, DeepMsg)),
    ?assertEqual(
        hb_converge:get(unsigned_id, DeepValueMsg),
        hb_converge:get(unsigned_id, DeepMsg)
    ),
    {ok, OuterMsg} = read(OuterID, Opts),
    ?assertEqual(OuterID, hb_converge:get(unsigned_id, OuterMsg)).
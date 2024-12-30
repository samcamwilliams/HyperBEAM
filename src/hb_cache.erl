%%% @doc A cache of Converge Protocol messages and compute results.
%%%
%%% HyperBEAM stores each of the messages in a path-value store on disk,
%%% according to the hashpath of the data. Each individual binary is stored at
%%% a path corresponding to the hash of the data. Messages are stored simply as
%%% collections of links to underlying data. See `hb_path` for more details on
%%% the hashpath structure.
%%% 
%%% The cache is a simple wrapper on stores, that allows us to read either a 
%%% direct key (a binary), a set of keys (a message) or any subpath of the
%%% hashpath space.
%%% 
%%% We also store signed messages in this space by creating a link for each of
%%% the keys in the message to its corresponding underlying data.
-module(hb_cache).
-export([read/2, read_output/3, write/2, write/3, write_output/4]).
-export([list/2, list_numbered/2, link/3]).
%%% Exports for modules that utilize hb_cache.
-export([test_unsigned/1, test_signed/1, test_deeply_nested_complex_item/1]).
-include("src/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% List all items in a directory, assuming they are numbered.
list_numbered(Path, Opts) ->
    SlotDir = hb_store:path(hb_opts:get(store, no_viable_store, Opts), Path),
    [ list_to_integer(Name) || Name <- list(SlotDir, Opts) ].

%% @doc List all items under a given path.
list(Path, Opts) ->
    case hb_store:list(hb_opts:get(store, no_viable_store, Opts), Path) of
        {ok, Names} -> Names;
        {error, _} -> []
    end.

%% @doc Write a message to the cache. For raw binaries, we write the 
write(Msg, Opts) -> store_write(Msg, hb_opts:get(store, no_viable_store, Opts), Opts).
store_write(Bin, Store, Opts) when is_binary(Bin) ->
    hb_store:write(Store, Path = hb_path:hashpath(Bin, Opts), Bin),
    {ok, Path};
store_write(Msg, Store, Opts) when is_map(Msg) ->
    KeyPathMap =
        maps:map(
            fun(Key, Value) ->
                {ok, Path} = store_write(Value, Store, Opts),
                hb_store:make_link(Store, Path, [hb_path:hashpath(Msg, Key, Opts)]),
                Path
            end,
            Msg
        ),
    {ok, UnsignedID} = dev_message:unsigned_id(Msg),
    store_link_message(UnsignedID, KeyPathMap, Store, Opts),
    case dev_message:signed_id(Msg) of
        {error, not_found} -> ok;
        {ok, SignedID} -> 
            store_link_message(SignedID, KeyPathMap, Store, Opts)
    end,
    {ok, KeyPathMap}.

%% @doc Recursively make links to underlying data, in the form of a pathmap.
%% This allows us to have the hashpath compute space be shared across all signed 
%% messages from users, while also allowing us to delimit where the signature/
%% message ends. For example, if we had the following hashpath-space:
%% 
%% Hashpath1/Compute/Results/1
%% Hashpath1/Compute/Results/2/Compute/Results/1
%% Hashpath1/Compute/Results/3
%% 
%% But a message that only contains the first layer of Compute results, we would
%% create the following linked structure:
%% 
%% ID1/Compute/Results/1 -> Hashpath1/Compute/Results/1
%% ID1/Compute/Results/2 -> Hashpath1/Compute/Results/2
%% ID1/Compute/Results/3 -> Hashpath1/Compute/Results/3
store_link_message(Root, PathMap, Store, _Opts) ->
    maps:map(
        fun(Key, Path) ->
            hb_store:make_link(Store, Path, [hb_util:human_id(Root), Key])
        end,
        PathMap
    ).

%% @doc List all of the subpaths of a given path, read each in turn, then use the
%% flat map codec to convert the result into a Converge message.
read(Path, Opts) -> store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts).
store_read(Path, Store, Opts) ->
    case hb_store:list(Store, Path) of
        {ok, []} -> not_found;
        {ok, Subpaths} ->
            FlatMap =
                maps:from_list(
                    lists:map(
                        fun(Subpath) ->
                            {
                                Subpath,
                                hb_store:read(
                                    Store, 
                                    hb_store:path(Store, [Path, Subpath])
                                )
                            }
                    end,
                    Subpaths
                )
            ),
            {ok, hb_message:convert(FlatMap, converge, flat, Opts)}
    end.
    
%%------------------------------------------------------------------------------

%% @doc Make a link from one path to another in the store.
%% Note: Argument order is `link(Src, Dst, Opts)'.
link(Existing, New, Opts) ->
    hb_store:make_link(
        hb_opts:get(store, no_viable_store, Opts),
        Existing,
        New
    ).

%% @doc Writes a computation result to the cache.
%% The process outputs a series of key values in the cache as follows:
%%
%%      /RootAddr: Either the Msg3ID alone, or `Msg1ID/Msg2ID' if the keys to
%%                 store are not complete. A caller may want to cache only some
%%                 of the keys because serializing the entire state would be 
%%                 expensive.
%%      /ShortComputePath: An optional link to the RootAddr, created if Msg2 
%%                         contains only a path.
%%      /RootAddr/[Key1, Key2, ...]: The values of the keys that are found in
%%                     the resulting message.
%%      /Msg1.signed_id/Msg2.{unsigned_id,signed_id}: Links from the path that
%%                     led to the creation of the resulting message.
write_output(Msg1, Msg2, Msg3, Opts) ->
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
    {ok, _} = write(RootPath, maps:with(KeysToCache, Msg3), Opts),
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
                        hb_path:hashpath(Msg1, Msg2, Opts)
                    ]
                ),
                maps:keys(Msg3)
            };
        Keys ->
            {
                [hb_path:hashpath(Msg1, Msg2, Opts)],
                Keys
            }
    end.

%% @doc Writes a message to the cache. Optionally, the message is written to a
%% path given by the caller. If it is not given, the message is written to the
%% ID of the message.
write(Message, Opts) ->
    write([], Message, Opts).
write(Path, Message, Opts) ->
    ?event({writing_message, {path, Path}}),
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
            UnsignedPath =
                hb_store:path(Store, [Path, hb_util:human_id(UnsignedID)]),
            %?event({writing_single_layer_message, UnsignedPath}),
            TX = hb_message:convert(Message, tx, converge, #{}),
            ?event(
                {writing_shallow_message,
                    {path, UnsignedPath},
                    {store, Store},
                    {tx, TX}
                }
            ),
            hb_store:write(Store, UnsignedPath, ar_bundles:serialize(TX)),
            if SignedID =/= not_signed ->
                SignedPath =
                    hb_store:path(Store, [Path, hb_util:human_id(SignedID)]),
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
    FullTX = hb_message:convert(
        hb_message:minimize(Msg),
        tx,
        converge,
        #{}),
    UnsignedHeaderID = ar_bundles:id(FullTX, unsigned),
    %?event({starting_composite_write, hb_util:human_id(UnsignedHeaderID)}),
    Store = hb_opts:get(store, no_viable_store, Opts),
    Group = hb_store:path(Store, [Path, hb_util:human_id(UnsignedHeaderID)]),
    ?event({creating_group, {path, Group}, {store, Store}}),
    ok = hb_store:make_group(Store, Group),
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
    ?event(debug, {subkeys, Subkeys}),
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
    FlatTX = hb_message:convert(FlatMsg, tx, converge, #{}),
    hb_store:write(
        Store,
        hb_store:path(
            Store, [Path, hb_util:human_id(UnsignedHeaderID), "header"]),
        ar_bundles:serialize(FlatTX)
    ),
    case hb_message:signers(Msg) of
        [] -> do_nothing;
        _ ->
            SignedHeaderID = ar_bundles:id(FullTX, signed),
            hb_store:make_link(
                Store,
                hb_store:path(
                    Store,
                    [Path, hb_util:human_id(UnsignedHeaderID)]
                ),
                hb_store:path(
                    Store,
                    [Path, hb_util:human_id(SignedHeaderID)]
                )
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
        no_viable_store -> not_found
    end.

read_raw_simple_message(Store, Path) ->
    {ok, Bin} = hb_store:read(Store, Path),
    {ok,
        hb_message:convert(ar_bundles:deserialize(Bin), converge, tx, #{})
    }.

%% @doc Read the output of a computation, given Msg1, Msg2, and some options.
read_output(MsgID1, MsgID2, Opts) when ?IS_ID(MsgID1) and ?IS_ID(MsgID2) ->
    ?event({cache_lookup, {msg1, MsgID1}, {msg2, MsgID2}, {opts, Opts}}),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(MsgID1, Msg2, Opts) when ?IS_ID(MsgID1) and is_map(Msg2) ->
    {ok, MsgID2} = dev_message:id(Msg2),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(Msg1, Msg2, Opts) when is_map(Msg1) ->
    % Check if Msg1 already has an ID generated in its loaded state.
    % Note: We do not generate the ID here because it may be expensive,
    % unless the caller has explicitly requested it.
    case maps:get(id, Msg1, not_signed) of
        not_signed ->
            % If Msg1 does not have an ID, check if the caller has requested
            % that we generate it. If so, generate it and try again.
            case hb_opts:get(generate_msg1_id, false, Opts) of
                true ->
                    {ok, MsgID1} = dev_message:id(Msg1),
                    read_output(MsgID1, Msg2, Opts);
                false ->
                    not_found
            end;
        MsgID1 ->
            % If Msg1 already has an ID, use it to look up the result.
            read_output(MsgID1, Msg2, Opts)
    end.

%% @doc Calculate the `hb_store's that should be used during
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

test_unsigned(Data) ->
    #{
        <<"Example">> => <<"Tag">>,
        <<"Data">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) ->
    hb_message:sign(test_unsigned(Data), ar_wallet:new()).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_item(Opts) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    %% Write the simple unsigned item
    {ok, _} = write(Item, Opts),
    %% Read the item back
    {ok, RetrievedItem} =
        read(hb_util:human_id(hb_converge:get(id, Item)), Opts),
    ?assert(hb_message:match(Item, RetrievedItem)).

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_item(Opts) ->
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
                                <<"E">> => <<"F">>,
                                <<"Z">> => [1,2,3]
                            },
                        <<"C">> => <<"D">>,
                        <<"G">> => [<<"H">>, <<"I">>],
                        <<"J">> => 1337
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

test_message_with_list(Opts) ->
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem)).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store simple unsigned item", fun test_store_simple_unsigned_item/1},
        {"deeply nested complex item", fun test_deeply_nested_complex_item/1},
        {"message with list", fun test_message_with_list/1}
    ]).
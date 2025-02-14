%%% @doc A cache of Converge Protocol messages and compute results.
%%%
%%% HyperBEAM stores all paths in key value stores, abstracted by the `hb_store'
%%% module. Each store has its own storage backend, but each works with simple
%%% key-value pairs. Each store can write binary keys at paths, and link between
%%% paths.
%%%
%%% There are three layers to HyperBEAMs internal data representation on-disk:
%%%
%%% 1. The raw binary data, written to the store at the hash of the content.
%%%    Storing binary paths in this way effectively deduplicates the data.
%%% 2. The hashpath-graph of all content, stored as a set of links between
%%%    hashpaths, their keys, and the data that underlies them. This allows
%%%    all messages to share the same hashpath space, such that all requests
%%%    from users additively fill-in the hashpath space, minimizing duplicated
%%%    compute.
%%% 3. Messages, referrable by their IDs (attested or unattested). These are
%%%    stored as a set of links attestation IDs and the unattested message.
%%%
%%% Before writing a message to the store, we convert it to Type-Annotated
%%% Binary Messages (TABMs), such that each of the keys in the message is
%%% either a map or a direct binary.
-module(hb_cache).
-export([read/2, read_output/3, write/2, write_binary/3, write_hashpath/2, link/3]).
-export([list/2, list_numbered/2]).
-export([test_unsigned/1, test_signed/1]).
-include("src/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% List all items in a directory, assuming they are numbered.
list_numbered(Path, Opts) ->
    SlotDir = hb_store:path(hb_opts:get(store, no_viable_store, Opts), Path),
    [ to_integer(Name) || Name <- list(SlotDir, Opts) ].

%% @doc List all items under a given path.
list(Path, Opts) when is_map(Opts)->
    case hb_opts:get(store, no_viable_store, Opts) of
        no_viable_store -> [];
        Store ->
            list(Path, Store)
    end;
list(Path, Store) ->
    ResolvedPath = hb_store:resolve(Store, Path),
    case hb_store:list(Store, ResolvedPath) of
        {ok, Names} -> Names;
        {error, _} -> [];
        no_viable_store -> []
    end.

%% @doc Write a message to the cache. For raw binaries, we write the data at
%% the hashpath of the data (by default the SHA2-256 hash of the data). We link
%% the unattended ID's hashpath for the keys (including `/attestations') on the
%% message to the underlying data and recurse. We then link each attestation ID
%% to the unattested message, such that any of the attested or unattested IDs
%% can be read, and once in memory all of the attestations are available. For
%% deep messages, the attestations will also be read, such that the ID of the
%% outer message (which does not include its attestations) will be built upon
%% the attestations of the inner messages. We do not, however, store the IDs from
%% attestations on signed _inner_ messages. We may wish to revisit this.
write(RawMsg, Opts) ->
    % Use the _structured_ format for calculating alternative IDs, but the
    % _tabm_ format for writing to the store.
    case hb_message:with_only_attested(RawMsg) of
        {ok, Msg} ->
            ?event(debug_store, {storing, Msg}),
            do_write_message(
                hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
                calculate_alt_ids(RawMsg, Opts),
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            );
        {error, Err} ->
            {error, Err}
    end.
do_write_message(Bin, AltIDs, Store, Opts) when is_binary(Bin) ->
    % Write the binary in the store at its given hash. Return the path.
    Hashpath = hb_path:hashpath(Bin, Opts),
    ok = hb_store:write(Store, Path = <<"data/", Hashpath/binary>>, Bin),
    lists:map(fun(AltID) -> hb_store:make_link(Store, Path, AltID) end, AltIDs),
    {ok, Path};
do_write_message(Msg, AltIDs, Store, Opts) when is_map(Msg) ->
    % Get the ID of the unsigned message.
    {ok, UnattestedID} = dev_message:id(Msg, #{ <<"attestors">> => <<"none">> }, Opts),
    ?event({writing_message_with_unsigned_id, UnattestedID}),
    MsgHashpathAlg = hb_path:hashpath_alg(Msg),
    hb_store:make_group(Store, UnattestedID),
    % Write the keys of the message into the store, rolling the keys into
    % hashpaths (having only two parts) as we do so.
    % We start by writing the group, such that if the message is empty, we
    % still have a group in the store.
    hb_store:make_group(Store, UnattestedID),
    maps:map(
        fun(Key, Value) ->
            ?event({writing_subkey, {key, Key}, {value, Value}}),
            KeyHashPath =
                hb_path:hashpath(
                    UnattestedID,
                    hb_path:to_binary(Key),
                    MsgHashpathAlg,
                    Opts
                ),
            ?event({key_hashpath_from_unsigned, KeyHashPath}),
            % Note: We do not pass the AltIDs here, as we cannot calculate them
            % based on the TABM that we have in-memory at this point. We could
            % turn the TABM back into a structured message, but this is
            % expensive.
            {ok, Path} = do_write_message(Value, [], Store, Opts),
            hb_store:make_link(Store, Path, KeyHashPath),
            ?event(
                {
                    {link, KeyHashPath},
                    {data_path, Path}
                }
            ),
            Path
        end,
        hb_private:reset(Msg)
    ),
    % Write the attestations to the store, linking each attestation ID to the
    % unattested message.
    lists:map(
        fun(AltID) ->
            hb_store:make_link(Store, UnattestedID, AltID)
        end,
        AltIDs
    ),
    {ok, UnattestedID}.

%% @doc Calculate the alternative IDs for a message.
calculate_alt_ids(Bin, _Opts) when is_binary(Bin) -> [];
calculate_alt_ids(Msg, Opts) ->
    Attestors = maps:keys(maps:get(<<"attestors">>, Msg, #{})),
    lists:map(
        fun(Attestor) ->
            hb_converge:get(
                [<<"attestations">>, Attestor, <<"id">>],
                Msg,
                Opts
            )
        end,
        Attestors
    ).

%% @doc Write a hashpath and its message to the store and link it.
write_hashpath(Msg = #{ <<"priv">> := #{ <<"hashpath">> := HP } }, Opts) ->
    write_hashpath(HP, Msg, Opts);
write_hashpath(MsgWithoutHP, Opts) ->
    write(MsgWithoutHP, Opts).
write_hashpath(HP, Msg, Opts) when is_binary(HP) or is_list(HP) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(cache_debug, {writing_hashpath, {hashpath, HP}, {msg, Msg}, {store, Store}}),
    {ok, Path} = write(Msg, Opts),
    hb_store:make_link(Store, Path, HP),
    {ok, Path}.

%% @doc Write a raw binary keys into the store and link it at a given hashpath.
write_binary(Hashpath, Bin, Opts) ->
    write_binary(Hashpath, Bin, hb_opts:get(store, no_viable_store, Opts), Opts).
write_binary(Hashpath, Bin, Store, Opts) ->
    ?event({writing_binary, {hashpath, Hashpath}, {bin, Bin}, {store, Store}}),
    {ok, Path} = do_write_message(Bin, [Hashpath], Store, Opts),
    {ok, Path}.

%% @doc Read the message at a path. Returns in 'structured@1.0' format: Either a
%% richly typed map or a direct binary.
read(Path, Opts) ->
    case store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts) of
        not_found -> not_found;
        {ok, Res} ->
            ?event({read_encoded_message, Res}),
            Structured = dev_codec_structured:to(Res),
            ?event({read_structured_message, Structured}),
            {ok, Structured}
    end.

%% @doc List all of the subpaths of a given path, read each in turn, returning a
%% flat map.
store_read(_Path, no_viable_store, _) ->
    not_found;
store_read(Path, Store, Opts) ->
    ResolvedFullPath = hb_store:resolve(Store, PathToBin = hb_path:to_binary(Path)),
    ?event(read_debug, {reading, {path, PathToBin}, {resolved, ResolvedFullPath}}),
    case hb_store:type(Store, ResolvedFullPath) of
        not_found -> not_found;
        no_viable_store -> not_found;
        simple -> hb_store:read(Store, ResolvedFullPath);
        _ ->
            case hb_store:list(Store, ResolvedFullPath) of
                {ok, Subpaths} ->
                    ?event(
                        {listed,
                            {original_path, Path},
                            {subpaths, {explicit, Subpaths}}
                        }
                    ),
                    Msg =
                        maps:from_list(
                            lists:map(
                                fun(Subpath) ->
                                    ?event(read_debug, {reading_subpath, {path, Subpath}, {store, Store}}),
                                    {ok, Res} = store_read(
                                        [ResolvedFullPath, Subpath],
                                        Store,
                                        Opts
                                    ),
                                    {iolist_to_binary([Subpath]), Res}
                                end,
                                Subpaths
                            )
                        ),
                    ?event({read_message, Msg}),
                    {ok, Msg};
                _ -> not_found
            end
    end.

%% @doc Read the output of a computation, given Msg1, Msg2, and some options.
read_output(MsgID1, MsgID2, Opts) when ?IS_ID(MsgID1) and ?IS_ID(MsgID2) ->
    ?event({cache_lookup, {msg1, MsgID1}, {msg2, MsgID2}, {opts, Opts}}),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(MsgID1, Msg2, Opts) when ?IS_ID(MsgID1) and is_map(Msg2) ->
    {ok, MsgID2} = dev_message:id(Msg2, #{ <<"attestors">> => <<"all">> }, Opts),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(Msg1, Msg2, Opts) when is_map(Msg1) and is_map(Msg2) ->
    read(hb_path:hashpath(Msg1, Msg2, Opts), Opts);
read_output(_, _, _) -> not_found.

%% @doc Make a link from one path to another in the store.
%% Note: Argument order is `link(Src, Dst, Opts)'.
link(Existing, New, Opts) ->
    hb_store:make_link(
        hb_opts:get(store, no_viable_store, Opts),
        Existing,
        New
    ).

to_integer(Value) when is_list(Value) ->
    list_to_integer(Value);
to_integer(Value) when is_binary(Value) ->
    binary_to_integer(Value).

%%% Tests

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"deep-test-key">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) -> test_signed(Data, ar_wallet:new()).
test_signed(Data, Wallet) ->
    hb_message:attest(test_unsigned(Data), Wallet).

test_store_binary(Opts) ->
    Bin = <<"Simple unsigned data item">>,
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin).

test_store_unsigned_empty_message(Opts) ->
    Item = #{},
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Item, RetrievedItem)).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_message(Opts) ->
    Item = test_unsigned(#{ <<"key">> => <<"Simple unsigned data item">> }),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_converge:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem)),
    ok.

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_signed_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }, Wallet),
    ?event({writing_message, Item}),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    {ok, UID} = dev_message:id(Item, #{ <<"attestors">> => <<"none">> }, Opts),
    {ok, RetrievedItemU} = read(UID, Opts),
    ?event({retreived_unsigned_message, {expected, Item}, {got, RetrievedItemU}}),
    ?assert(hb_message:match(Item, RetrievedItemU)),
    {ok, AttestedID} = dev_message:id(Item, #{ <<"attestors">> => [Address] }, Opts),
    {ok, RetrievedItemS} = read(AttestedID, Opts),
    ?assert(hb_message:match(Item, RetrievedItemS)),
    ok.

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Wallet),
    Outer =
        #{
            <<"level1">> =>
                hb_message:attest(
                    #{
                        <<"level2">> =>
                            #{
                                <<"level3">> => Level3SignedSubmessage,
                                <<"e">> => <<"f">>,
                                <<"z">> => [1,2,3]
                            },
                        <<"c">> => <<"d">>,
                        <<"g">> => [<<"h">>, <<"i">>],
                        <<"j">> => 1337
                    },
                    ar_wallet:new()
                ),
            <<"a">> => <<"b">>
        },
    {ok, UID} = dev_message:id(Outer, #{ <<"attestors">> => <<"none">> }, Opts),
    {ok, AttestedID} = dev_message:id(Outer, #{ <<"attestors">> => [Address] }, Opts),
    %% Write the nested item
    {ok, _} = write(Outer, Opts),
    %% Read the deep value back using subpath
    {ok, DeepMsg} =
        read(
            [
                OuterID = hb_util:human_id(UID),
                <<"level1">>,
                <<"level2">>,
                <<"level3">>
            ],
            Opts
        ),
    ?event({deep_message, DeepMsg}),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual([1,2,3], hb_converge:get(<<"deep-test-key">>, DeepMsg)),
    ?event({deep_message_match, {read, DeepMsg}, {write, Level3SignedSubmessage}}),
    ?assert(hb_message:match(Level3SignedSubmessage, DeepMsg)),
    {ok, OuterMsg} = read(OuterID, Opts),
    ?assert(hb_message:match(Outer, OuterMsg)),
    {ok, AttestedMsg} = read(hb_util:human_id(AttestedID), Opts),
    ?assert(hb_message:match(Outer, AttestedMsg)).

test_message_with_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem)).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store unsigned empty message", fun test_store_unsigned_empty_message/1},
        {"store binary", fun test_store_binary/1},
        {"store simple unsigned message", fun test_store_simple_unsigned_message/1},
        {"store simple signed message", fun test_store_simple_signed_message/1},
        {"deeply nested complex message", fun test_deeply_nested_complex_message/1},
        {"message with message", fun test_message_with_message/1}
    ]).
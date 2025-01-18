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
%%% 3. Messages, referrable by their IDs (attested or unsigned). These are 
%%%    stored as a set of links between keys and the hashes of the underlying
%%%    data. We store this set of links in addition to the hashpath-graph, such
%%%    that we can clearly delimit where the messages end, while the graph
%%%    continues to be used for additional shared compute.
%%% 
%%% Before writing a message to the store, we convert it to Type-Annotated 
%%% Binary Messages (TABMs), such that each of the keys in the message is
%%% either a map or a direct binary.
%%% 
%%% For example, imagine we have a computation result (`Msg3') which contains
%%% the following keys:
%%% ```
%%%     /Result/Signature
%%%     /Result/Owner
%%%     /Result/WASM-Output
%%%     /Usage-Report/CPU-Time
%%%     /Usage-Report/Memory-Usage
%%% '''
%%% 
%%% This module will first write raw binaries to the store using their hashes
%%% as keys, then right link trees for the hash path ('hashpath([Msg1, Msg2, Result,
%%% ...])', then write link trees for each of the unsigned and signed messages.
-module(hb_cache).
-export([read/2, read_output/3, write/2, write_binary/3]).
-export([list/2, list_numbered/2, link/3]).
-export([test_unsigned/1, test_signed/1]).
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

%% @doc Write a message to the cache. For raw binaries, we write the data at
%% the hashpath of the data (by default the SHA2-256 hash of the data). For
%% deep messages, we link the hashpath of the keys to the underlying data and
%% recurse. Additionally, we make sets of links such that signed messages can
%% be recalled from the underlying dataset, without associated keys that were
%% written to the hashpath-graph by other messages.
write(RawMsg, Opts) ->
    Msg = hb_message:convert(RawMsg, tabm, converge, Opts),
    write_message(Msg, hb_opts:get(store, no_viable_store, Opts), Opts).
write_message(Bin, Store, Opts) when is_binary(Bin) ->
    % Write the binary in the store at its given hash. Return the path.
    % We add the `Data/` prefix for a weird reason: Arweave data items have 
    % their signed ID based on the hash of the signature. Subsequently, if
    % we store the data items at just their sha256 hash, we get a hash
    % collision between the signed ID of a message, and its signature data.
    Hashpath = hb_path:hashpath(Bin, Opts),
    ok = hb_store:write(Store, Path = <<"messages/", Hashpath/binary>>, Bin),
    {ok, Path};
write_message(Msg, Store, Opts) when is_map(Msg) ->
    % Precalculate the hashpath of the message.
    MsgHashpath = hb_path:hashpath(Msg, Opts),
    MsgHashpathAlg = hb_path:hashpath_alg(Msg),
    ?event({storing_msg_with_hashpath, MsgHashpath}),
    % Get the ID of the unsigned message.
    {ok, UnsignedID} = dev_message:unsigned_id(Msg),
    % Write the keys of the message into the graph of hashpaths, generating a map of
    % keys to paths of the underlying data as we do so.
    UnsignedMsgPathMap =
        maps:map(
            WriteSubkey = 
                fun(Key, Value) ->
                    {ok, Path} = write_message(Value, Store, Opts),
                    KeyHashPath =
                        hb_path:hashpath(
                            MsgHashpath,
                            hb_path:to_binary(Key),
                            MsgHashpathAlg,
                            Opts
                        ),
                    hb_store:make_link(Store, Path, KeyHashPath),
                    MessageLink =
                        case hb_path:term_to_path_parts(KeyHashPath) of
                            [MsgHashpath, _] -> unnecessary;
                            [NewRoot|_] ->
                                hb_store:make_link(Store, NewRoot, MsgHashpath)
                        end,
                    ?event(
                        {
                            {link, KeyHashPath},
                            {data_path, Path},
                            {message_link, MessageLink}
                        }
                    ),
                    Path
                end,
            hb_message:unsigned(Msg)
        ),
    % Write links for each key in the unsigned message to the underlying data.
    write_link_tree(UnsignedID, UnsignedMsgPathMap, Store, Opts),
    % If the message is signed, we need to write links for the attestations.
    case dev_message:signed_id(Msg) of
        {error, not_signed} ->
            {ok, UnsignedID};
        {ok, SignedID} ->
            % Write each attestation-related key in the message to the store.
            AttestedMsgPathMap =
                maps:map(
                    WriteSubkey,
                    hb_message:attestations(Msg)
                ),
            % Merge the unsigned and attested message path maps, such that we have
            % a complete map of all keys in the message and their paths.
            CompleteMsgPathMap = maps:merge(UnsignedMsgPathMap, AttestedMsgPathMap),
            % Generate links for the signed message.
            write_link_tree(
                SignedID, 
                CompleteMsgPathMap,
                Store,
                Opts
            ),
            {ok, SignedID}
    end.

%% @doc Recursively make links to underlying data, in the form of a pathmap.
%% This allows us to have the hashpath compute space be shared across all
%% messages from users, while also allowing us to delimit where the signature/
%% message ends. For example, if we had the following hashpath-space:
%% 
%% Hashpath1/Compute/Results/1
%% Hashpath1/Compute/Results/2/Compute/Results/1
%% Hashpath1/Compute/Results/3
%% 
%% ...but a message that only contains the first layer of Compute results, we would
%% create the following linked structure:
%% 
%% ID1/Compute/Results/1 -> Hashpath1/Compute/Results/1
%% ID1/Compute/Results/2 -> Hashpath1/Compute/Results/2
%% ID1/Compute/Results/3 -> Hashpath1/Compute/Results/3
write_link_tree(RootPath, PathMap, Store, Opts) ->
    ?event({write_link_tree, {root, RootPath}, {store, Store}}),
    maps:map(
        fun(Key, Path) when is_map(Path) ->
            % The key is a map of subkeys and paths, so we adjust our root ID to
            % additionally include the key and recurse.
            write_link_tree(
                hb_path:to_binary([RootPath, Key]),
                Path,
                Store,
                Opts
            );
        (Key, Path) when not is_map(Path) ->
            % The key is a simple binary, so we link ID/key to the path.
            MsgKey = hb_path:to_binary([RootPath, Key]),
            BinPath = hb_path:to_binary(Path),
            ?event({linking, {msg_key, MsgKey}, {path, BinPath}}),
            hb_store:make_link(Store, BinPath, MsgKey)
        end,
        PathMap
    ).

%% @doc Write a raw binary keys into the store and link it at a given hashpath.
write_binary(Hashpath, Bin, Opts) ->
    write_binary(Hashpath, Bin, hb_opts:get(store, no_viable_store, Opts), Opts).
write_binary(Hashpath, Bin, Store, Opts) ->
    ?event({writing_binary, {hashpath, Hashpath}, {bin, Bin}, {store, Store}}),
    {ok, Path} = write_message(Bin, Store, Opts),
    hb_store:make_link(Store, Path, Hashpath),
    {ok, Path}.

%% @doc Read the message at a path. Returns in Converge's format: Either a rich
%% map or a direct binary. Messages are written in the stores as flat maps, so 
%% we convert them to the rich format here after reading.
read(Path, Opts) ->
    case store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts) of
        not_found -> not_found;
        {ok, FlatMsg} when is_map(FlatMsg) ->
            {ok, hb_message:convert(FlatMsg, converge, flat, Opts)};
        {ok, Res} -> {ok, Res}
    end.
        
%% @doc List all of the subpaths of a given path, read each in turn, returning a 
%% flat map.
store_read(Path, Store, Opts) ->
    ResolvedFullPath = hb_store:resolve(Store, PathToBin = hb_path:to_binary(Path)),
    ?event(
        {reading,
            {path, PathToBin},
            {resolved, ResolvedFullPath}
        }
    ),
    case hb_store:type(Store, ResolvedFullPath) of
        simple ->
            {ok, Binary} = hb_store:read(Store, ResolvedFullPath),
            % Try to get the key type, if it exists in the cache.
            case hb_path:term_to_path_parts(Path) of
                [BasePath, Key] when not ?IS_ID(Key) and is_binary(Key) ->
                    ?no_prod("This extra `/` looks dubious."),
                    case hb_store:read(Store, <<BasePath/binary, "/", "/Converge-Type-", Key/binary>>) of
                        no_viable_store ->
                            {ok, Binary};
                        {ok, Type} ->
                            {ok, hb_codec_converge:decode_value(Type, Binary)}
                    end;
                _ ->
                    {ok, Binary}
            end;
        _ ->
            case hb_store:list(Store, ResolvedFullPath) of
                {ok, RawSubpaths} ->
                    Subpaths =
                        lists:map(fun hb_converge:normalize_key/1, RawSubpaths),
                    ?event(
                        {listed,
                            {original_path, Path},
                            {subpaths, {explicit, Subpaths}}
                        }
                    ),
                    FlatMap =
                        maps:from_list(
                            lists:map(
                                fun(Subpath) ->
                                    ?event({subpath, Subpath}),
                                    ResolvedSubpath =
                                        hb_store:resolve(Store,
                                            hb_path:to_binary([
                                                ResolvedFullPath,
                                                Subpath
                                            ])
                                        ),
                                    ?event(
                                        {reading_subpath, Subpath, {resolved, ResolvedSubpath}}
                                    ),
                                    {
                                        Subpath,
                                        hb_util:ok(store_read(ResolvedSubpath, Store, Opts))
                                    }
                            end,
                            Subpaths
                        )
                    ),
                    ?event({flat_map, FlatMap}),
                    {ok, FlatMap};
                _ -> not_found
            end
    end.

%% @doc Read the output of a computation, given Msg1, Msg2, and some options.
read_output(MsgID1, MsgID2, Opts) when ?IS_ID(MsgID1) and ?IS_ID(MsgID2) ->
    ?event({cache_lookup, {msg1, MsgID1}, {msg2, MsgID2}, {opts, Opts}}),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(MsgID1, Msg2, Opts) when ?IS_ID(MsgID1) and is_map(Msg2) ->
    {ok, MsgID2} = dev_message:id(Msg2),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(Msg1, Msg2, Opts) when is_map(Msg1) and is_map(Msg2) ->
    read(hb_path:hashpath(Msg1, Msg2, Opts), Opts);
read_output(_, _, _) -> not_found.
    
%%------------------------------------------------------------------------------

%% @doc Make a link from one path to another in the store.
%% Note: Argument order is `link(Src, Dst, Opts)'.
link(Existing, New, Opts) ->
    hb_store:make_link(
        hb_opts:get(store, no_viable_store, Opts),
        Existing,
        New
    ).

%%% Tests

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"data">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) ->
    hb_message:sign(test_unsigned(Data), ar_wallet:new()).

test_store_binary(Opts) ->
    Bin = <<"Simple unsigned data item">>,
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_item(Opts) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_converge:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem)).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_signed_item(Opts) ->
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    UID = hb_util:human_id(hb_converge:get(unsigned_id, Item)),
    SID = hb_util:human_id(hb_converge:get(signed_id, Item)),
    {ok, RetrievedItemU} = read(UID, Opts),
    ?assert(hb_message:match(Item, RetrievedItemU)),
    {ok, RetrievedItemS} = read(SID, Opts),
    ?assert(hb_message:match(Item, RetrievedItemS)).

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_item(Opts) ->
    %% Create nested data
    DeepValueMsg = test_signed([1,2,3]),
    Outer =
        #{
            <<"level1">> =>
                hb_message:sign(
                    #{
                        <<"level2">> =>
                            #{
                                <<"level3">> => DeepValueMsg,
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
    %% Write the nested item
    {ok, _} = write(Outer, Opts),
    %% Read the deep value back using subpath
    {ok, DeepMsg} =
        read(
            [
                OuterID = hb_util:human_id(hb_converge:get(unsigned_id, Outer)),
                <<"level1">>,
                <<"level2">>,
                <<"level3">>
            ],
            Opts
        ),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual([1,2,3], hb_converge:get(data, DeepMsg)),
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
        {"store binary", fun test_store_binary/1},
        {"store simple unsigned item", fun test_store_simple_unsigned_item/1},
        {"store simple signed item", fun test_store_simple_signed_item/1},
        {"deeply nested complex item", fun test_deeply_nested_complex_item/1},
        {"message with list", fun test_message_with_list/1}
    ]).
%%% @doc A cache of Converge Protocol messages and compute results.
%%%
%%% HyperBEAM stores each of the messages in a path->value store on disk,
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
-export([read/2, read_output/3, write/2]).
-export([list/2, list_numbered/2, link/3]).
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
write(RawMsg, Opts) ->
    Msg = hb_message:convert(RawMsg, tabm, converge, Opts),
    store_write(Msg, hb_opts:get(store, no_viable_store, Opts), Opts).
store_write(Bin, Store, Opts) when is_binary(Bin) ->
    Hashpath = hb_path:hashpath(Bin, Opts),
    PathParts = hb_path:term_to_path(Hashpath, Opts),
    hb_store:write(Store, PathParts, Bin),
    {ok, PathParts};
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
        {error, not_signed} -> ok;
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
                                list_to_binary(Subpath),
                                hb_util:ok(hb_store:read(
                                    Store, 
                                    hb_store:path(Store, [Path, Subpath])
                                ))
                            }
                    end,
                    Subpaths
                )
            ),
            ?event(debug, {explicit, FlatMap}),
            {ok, hb_message:convert(FlatMap, converge, flat, Opts)};
        {error, _} -> not_found
    end.

%% @doc Read the output of a computation, given Msg1, Msg2, and some options.
read_output(MsgID1, MsgID2, Opts) when ?IS_ID(MsgID1) and ?IS_ID(MsgID2) ->
    ?event({cache_lookup, {msg1, MsgID1}, {msg2, MsgID2}, {opts, Opts}}),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(MsgID1, Msg2, Opts) when ?IS_ID(MsgID1) and is_map(Msg2) ->
    {ok, MsgID2} = dev_message:id(Msg2),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_output(Msg1, Msg2, Opts) when is_map(Msg1) and is_map(Msg2) ->
    read(hb_path:hashpath(Msg1, Msg2, Opts), Opts).
    
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
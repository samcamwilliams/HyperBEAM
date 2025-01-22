-module(hb_client).
%% Converge API and HyperBEAM Built-In Devices
-export([resolve/4, routes/2, add_route/3]).
%% Arweave node API
-export([arweave_timestamp/0]).
%% Arweave bundling and data access API
-export([upload/1, download/1]).

-include("include/hb.hrl").

%%% Converge API and HyperBEAM Built-In Devices

%% @doc Resolve a message pair on a remote node.
%% The message pair is first transformed into a singleton request, by
%% prefixing the keys in both messages for the path segment that they relate to,
%% and then adjusting the "Path" field from the second message.
resolve(Node, Msg1, Msg2, Opts) ->
    TABM2 =
        hb_converge:set(
            #{
                <<"path">> => hb_converge:get(<<"path">>, Msg2, <<"/">>, Opts),
                <<"2.path">> => unset
            },
        prefix_keys(<<"2.">>, Msg2, Opts),
        Opts#{ hashpath => ignore }
    ),
    hb_http:post(
        Node,
        maps:merge(prefix_keys(<<"1.">>, Msg1, Opts), TABM2),
        Opts
    ).

prefix_keys(Prefix, Message, Opts) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            maps:put(<<Prefix/binary, Key/binary>>, Val, Acc)
        end,
        #{},
        hb_message:convert(Message, tabm, Opts)
    ).

routes(Node, Opts) ->
    resolve(Node,
        #{
            <<"device">> => <<"Router@1.0">>
        },
        #{
            <<"path">> => <<"routes">>,
            <<"method">> => <<"GET">>
        },
        Opts
    ).

add_route(Node, Route, Opts) ->
    resolve(Node,
        Route#{
            <<"device">> => <<"Router@1.0">>
        },
        #{
            <<"path">> => <<"routes">>,
            <<"method">> => <<"POST">>
        },
        Opts
    ).


%%% Arweave node API

%% @doc Grab the latest block information from the Arweave gateway node.
arweave_timestamp() ->
    case hb_opts:get(mode) of
        debug -> {0, 0, <<0:256>>};
        prod ->
            {ok, {{_, 200, _}, _, Body}} =
                httpc:request(
                    <<(hb_opts:get(gateway))/binary, "/block/current">>
                ),
            {Fields} = jiffy:decode(Body),
            {_, Timestamp} = lists:keyfind(<<"timestamp">>, 1, Fields),
            {_, Hash} = lists:keyfind(<<"indep_hash">>, 1, Fields),
            {_, Height} = lists:keyfind(<<"height">>, 1, Fields),
            {Timestamp, Height, Hash}
    end.

%%% Bundling and data access API

%% @doc Download the data associated with a given ID. See TODO below.
download(ID) ->
    % TODO: Need to recreate full data items, not just data...
    case httpc:request(hb_opts:get(gateway) ++ "/" ++ ID) of
        {ok, {{_, 200, _}, _, Body}} -> #tx{data = Body};
        _Rest -> throw({id_get_failed, ID})
    end.

%% @doc Upload a data item to the bundler node.
upload(Message) when is_map(Message) ->
    upload(hb_message:convert(Message, tx, converge, #{}));
upload(Item) ->
    ?event({uploading_item, Item}),
    case
        httpc:request(
            post,
            {
                <<(hb_opts:get(bundler))/binary, "/tx">>,
                [],
                "application/octet-stream",
                ar_bundles:serialize(Item)
            },
            [],
            []
        )
    of
        {ok, {{_, 200, _}, _, Body}} ->
            ?event(upload_success),
            {ok, jiffy:decode(Body, [return_maps])};
        Response ->
            ?event(upload_error),
            {error, bundler_http_error, Response}
    end.

%%% Utility functions

%% @doc Convert a map of parameters into a query string, starting with the
%% given separator.
path_opts(EmptyMap, _Sep) when map_size(EmptyMap) == 0 -> "";
path_opts(Opts, Sep) ->
    PathParts = tl(lists:flatten(lists:map(
        fun({Key, Val}) ->
            "&" ++ format_path_opt(Key) ++ "=" ++ format_path_opt(Val)
        end,
        maps:to_list(Opts)
    ))),
    Sep ++ PathParts.

format_path_opt(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_path_opt(Val) when is_binary(Val) ->
    binary_to_list(Val);
format_path_opt(Val) when is_integer(Val) ->
    integer_to_list(Val).

parse_result_set(Body) ->
    {JSONStruct} = jiffy:decode(Body),
    {_, {PageInfoStruct}} = lists:keyfind(<<"pageInfo">>, 1, JSONStruct),
    {_, HasNextPage} = lists:keyfind(<<"hasNextPage">>, 1, PageInfoStruct),
    {_, EdgesStruct} = lists:keyfind(<<"edges">>, 1, JSONStruct),
    {HasNextPage, lists:map(fun json_struct_to_result/1, EdgesStruct)}.

%% Parse a CU result into a #result record. If the result is in the form of a
%% stream, then the cursor is returned in the #result record as well.
json_struct_to_result(NodeStruct) ->
    json_struct_to_result(NodeStruct, #result{}).
json_struct_to_result({NodeStruct}, Res) ->
    json_struct_to_result(NodeStruct, Res);
json_struct_to_result(Struct, Res) ->
    case lists:keyfind(<<"node">>, 1, Struct) of
        false ->
            Res#result{
                messages = lists:map(
                    fun ar_bundles:json_struct_to_item/1,
                    hb_util:find_value(<<"messages">>, Struct, [])
                ),
                assignments = hb_util:find_value(<<"assignments">>, Struct, []),
                spawns = lists:map(
                    fun ar_bundles:json_struct_to_item/1,
                    hb_util:find_value(<<"spawns">>, Struct, [])
                ),
                output = hb_util:find_value(<<"output">>, Struct, [])
            };
        {_, {NodeStruct}} ->
            json_struct_to_result(
                NodeStruct,
                Res#result{
                    cursor = hb_util:find_value(<<"cursor">>, Struct, undefined)
                }
            )
    end.
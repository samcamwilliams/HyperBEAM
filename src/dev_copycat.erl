%%% @doc A device for orchestrating indexing of messages from foreign sources
%%% into a HyperBEAM node's caches.
%%% 
%%% Supported sources of messages are as follows:
%%% - A remote Arweave GraphQL endpoint.
%%% 
%%% This module is not production-ready. Do not use it in practice.
-module(dev_copycat).
-export([graphql/3]).
-include("include/hb.hrl").

-define(SUPPORTED_FILTERS,
    [<<"query">>, <<"tag">>, <<"owner">>, <<"recipient">>, <<"all">>]
).

%% @doc Takes a GraphQL query, optionally with a node address, and curses through
%% each of the messages returned by the query, indexing them into the node's
%% caches.
graphql(Base, Req, Opts) ->
    case parse_query(Base, Req, Opts) of
        {ok, Query} ->
            Node = maps:get(<<"node">>, Opts, undefined),
            OpName = hb_maps:get(<<"operationName">>, Req, undefined, Opts),
            Vars = hb_maps:get(<<"variables">>, Req, #{}, Opts),
            index_graphql(0, Query, Vars, Node, OpName, Opts);
        Other ->
            Other
    end.

%% @doc Index a GraphQL query into the node's caches.
index_graphql(Total, Query, Vars, Node, OpName, Opts) ->
    maybe
        ?event(
            {graphql_run_called,
                {query, {string, Query}},
                {operation, OpName},
                {variables, Vars}
            }
        ),
        {ok, RawRes} ?= hb_gateway_client:query(Query, Vars, Node, OpName, Opts),
        Res = hb_util:deep_get(<<"data/transactions">>, RawRes, #{}, Opts),
        NodeStructs = hb_util:deep_get(<<"edges">>, Res, [], Opts),
        ?event(indexer_short, {graphql_request_returned_items, length(NodeStructs)}),
        ?event(
            {graphql_indexing_responses,
                {query, {string, Query}},
                {variables, Vars},
                {result, Res}
            }
        ),
        ParsedMsgs =
            lists:filtermap(
                fun(NodeStruct) ->
                    Struct = hb_maps:get(<<"node">>, NodeStruct, not_found, Opts),
                    try
                        {ok, ParsedMsg} =
                            hb_gateway_client:result_to_message(
                                Struct,
                                Opts
                            ),
                        {true, ParsedMsg}
                    catch
                        error:Reason ->
                            ?event(
                                warning,
                                {indexer_graphql_parse_failed,
                                    {struct, NodeStruct},
                                    {reason, Reason}
                                }
                            ),
                            false
                    end
                end,
                NodeStructs
            ),
        ?event(indexer_short, {graphql_parsed_msgs, length(ParsedMsgs)}),
        WrittenMsgs =
            lists:filter(
                fun(ParsedMsg) ->
                    try
                        {ok, _} = hb_cache:write(ParsedMsg, Opts),
                        true
                    catch
                        error:Reason ->
                            ?event(
                                warning,
                                {indexer_graphql_write_failed,
                                    {reason, Reason},
                                    {msg, ParsedMsg}
                                }
                            ),
                            false
                    end
                end,
                ParsedMsgs
            ),
        ?event(indexer_short,
            {indexer_graphql_wrote,
                {total, Total},
                {batch, length(WrittenMsgs)},
                {batch_failures, length(ParsedMsgs) - length(WrittenMsgs)}
            }
        ),
        HasNextPage = hb_util:deep_get(<<"pageInfo/hasNextPage">>, Res, false, Opts),
        case HasNextPage of
            true ->
                % Get the last cursor from the node structures and recurse.
                {ok, Cursor} =
                    hb_maps:find(
                        <<"cursor">>,
                        lists:last(NodeStructs),
                        Opts
                    ),
                index_graphql(
                    Total + length(ParsedMsgs),
                    Query,
                    Vars#{ <<"after">> => Cursor },
                    Node,
                    OpName,
                    Opts
                );
            false ->
                {ok, Total}
        end
    else
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Find or create a GraphQL query from a given base and request. We expect
%% to find either a `query' field, a `tags' field, a `tag' and `value' field,
%% an `owner' field, or a `recipient' field. If none of these fields are found,
%% we return a query that will match all results known to an Arweave gateway.
parse_query(Base, Req, Opts) ->
    % Merge the keys of the base and request maps, and remove duplicates.
    Merged = hb_maps:merge(Base, Req, Opts),
    Keys = hb_maps:keys(Merged, Opts),
    SupportedKeys = ?SUPPORTED_FILTERS,
    case lists:filter(fun(K) -> lists:member(K, SupportedKeys) end, Keys) of
        [<<"query">>|_] ->
            {ok, hb_maps:get(<<"query">>, Merged, <<>>, Opts)};
        [<<"tag">>|_] ->
            Key = hb_maps:get(<<"tag">>, Merged, <<>>, Opts),
            Value = hb_maps:get(<<"value">>, Merged, <<>>, Opts),
            default_query(<<"tag">>, {Key, Value}, Opts);
        [FilterKey|_] ->
            default_query(FilterKey, Merged, Opts);
        [] ->
            {error,
                #{
                    <<"body">> =>
                        <<"No supported filter fields found. Supported filters: ",
                            (
                                lists:join(
                                    <<", ">>,
                                    lists:map(
                                        fun(K) -> <<"\"", (K)/binary, "\"">> end,
                                        SupportedKeys
                                    )
                                )
                            )/binary
                        >>
                }
            }
    end.

%% @doc Return a default query for a given filter type.
default_query(<<"tag">>, {Key, Value}, _Opts) ->
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, tags: [",
            "{name: \"", Key/binary, "\", values: [\"", Value/binary, "\"]}",
        "]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
default_query(<<"recipient">>, Merged, Opts) ->
    Recipient = hb_maps:get(<<"recipient">>, Merged, <<>>, Opts),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, recipient: \"", Recipient/binary, "\") { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
default_query(<<"owner">>, Merged, Opts) ->
    Owner = hb_maps:get(<<"owner">>, Merged, <<>>, Opts),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, owner: \"", Owner/binary, "\") { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
default_query(<<"all">>, _Merged, _Opts) ->
    {ok, <<"query($after: String) { ",
        "transactions(after: $after) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>}.
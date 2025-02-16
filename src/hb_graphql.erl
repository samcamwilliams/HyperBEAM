%%% @doc Implementation of Arweave's GraphQL API to gain access to specific 
%%% items of data stored on the network.
%%% 
%%% This module must be used to get full HyperBEAM data items by ID, as Arweave
%%% gateways do not presently expose all necessary fields to retrieve this
%%% information outside of the GraphQL API.
-module(hb_graphql).
-export([get_id/2]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc Get a data item by its ID.
get_id(ID, Opts) ->
    Req =
        <<
            "transactions(first: 1, ids: [\"",
                ID/binary,
            "\"]) { edges { node { id tags { name value } } } } }"
        >>,
    get(Req, Opts).

%% @doc Run a GraphQL request encoded as a binary.
get(Req, Opts) ->
    case hb_opts:get(graphql_urls, no_viable_index, Opts) of
        no_viable_index -> {error, no_viable_index};
        URLs -> first_response(URLs, Req, Opts)
    end.

%% @doc Iterate through a list of endpoint URLs, returning the first response.
first_response([], _Req, _Opts) -> {error, no_viable_index};
first_response([URL | URLs], Req, Opts) ->
    Resp = hb_http:request(
            #{
                <<"path">> => URL,
                <<"method">> => <<"POST">>,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => Req
            },
            Opts
        ),
    case Resp of
        {ok, Res} ->
            ?event(graphql, {response, URL, Res}),
            {ok, jiffy:decode(Res, [return_maps])};
        {error, Reason} ->
            ?event(graphql, {request_error, URL, Reason}),
            first_response(URLs, Req, Opts)
    end.

%%% Tests
get_ans104_test() ->
    % Start a random node so that all of the services come up.
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = get_id(<<"-ztVycnfbp_P6zbuBh9NPmAR3Lc0e9MIqtFIbVjFj9E">>, #{}),
    ?event(graphql, {get_ans104_test, Res}),
    ?assert(hb_message:verify(Res)).

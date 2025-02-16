%%% @doc Implementation of Arweave's GraphQL API to gain access to specific 
%%% items of data stored on the network.
%%% 
%%% This module must be used to get full HyperBEAM `structured@1.0' form messages
%%% from data items stored on the network, as Arweave gateways do not presently
%%% expose all necessary fields to retrieve this information outside of the
%%% GraphQL API. When gateways integrate serving in `httpsig@1.0' form, this
%%% module will be deprecated.
-module(hb_graphql).
-export([message_from_id/2]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Get a data item (including data and tags) by its ID, using the node's
%% GraphQL peers.
%% It uses the following GraphQL schema:
%% type Transaction {
%%   id: ID!
%%   anchor: String!
%%   signature: String!
%%   recipient: String!
%%   owner: Owner { address: String! key: String! }!
%%   fee: Amount!
%%   quantity: Amount!
%%   data: MetaData!
%%   tags: [Tag { name: String! value: String! }!]!
%% }
%% type Amount {
%%   winston: String!
%%   ar: String!
%% }
message_from_id(ID, Opts) ->
    Req =
        #{
            <<"query">> =>
                <<
                    "query($transactionIds: [ID!]!) { ",
                        "transactions(ids: $transactionIds, first: 1){ ",
                            "edges { node { ",
                                "id ",
                                "anchor ",
                                "signature ",
                                "recipient ",
                                "owner { key } ",
                                "fee { winston } ",
                                "quantity { winston } ",
                                "tags { name value } ",
                                "data { size } "
                            "} } ",
                        "} ",
                    "} "
                >>,
            <<"variables">> =>
                #{
                    <<"transactionIds">> => [ID]
                }
        },
    case get(Req, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, GqlMsg, Source} ->
            case hb_converge:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found -> {error, not_found};
                Res = #{<<"id">> := ID} ->
                    % Convert the response to an ANS-104 message.
                    GQLOpts = Opts#{ hashpath => ignore },
                    TX =
                        #tx {
                            format = ans104,
                            id = hb_util:decode(ID),
                            last_tx = hb_util:decode(hb_converge:get(<<"anchor">>, Res, GQLOpts)),
                            signature = hb_util:decode(hb_converge:get(<<"signature">>, Res, GQLOpts)),
                            target = hb_util:decode(hb_converge:get(<<"recipient">>, Res, GQLOpts)),
                            owner = hb_util:decode(hb_converge:get(<<"owner/key">>, Res, GQLOpts)),
                            tags =
                                [
                                        {Name, Value}
                                    ||
                                        #{<<"name">> := Name, <<"value">> := Value}
                                            <- hb_converge:get(<<"tags">>, Res, GQLOpts)
                                ],
                            data_size = hb_util:int(hb_converge:get(<<"data/size">>, Res, GQLOpts))
                        },
                    ?event({raw_ans104, {explicit, TX}}),
                    ?event({ans104_form_response, TX}),
                    TABM = dev_codec_ans104:from(TX),
                    ?event({decoded_tabm, TABM}),
                    Structured = dev_codec_structured:to(TABM),
                    ?event({encoded_structured, Structured}),
                    {ok, Structured};
                _ ->
                    % We got a response, but it didn't contain the data we
                    % expected. Remove this location for the purposes of this
                    % request and try again.
                    ?event(graphql,
                        {
                            response_did_not_contain_id,
                            Source,
                            {unexpected_response, GqlMsg}
                        }
                    ),
                    AllLocations = hb_opts:get(graphql_urls, [], Opts),
                    message_from_id(
                        ID,
                        Opts#{
                            graphql_urls => lists:delete(Source, AllLocations)
                        }
                    )
            end
    end.

%% @doc Get the data associated with a transaction by its ID, using the node's
%% Arweave `gateway' peers. The item is expected to be available in its 
%% unmodified (by caches or other proxies) form at the following location:
%%      https://<gateway>/tx/<id>
%% where `<id>' is the base64-url-encoded transaction ID.
data_from_id(ID, Opts) ->
    ?event(graphql, {data_from_id, ID}),
    {error, deprecated}.
        
%% @doc Run a GraphQL request encoded as a binary. The node message may contain 
%% a list of URLs to use, optionally as a tuple with an additional map of options
%% to use for the request.
get(Req, Opts) ->
    case hb_opts:get(graphql_urls, no_viable_store, Opts) of
        no_viable_store -> {error, no_viable_store};
        Locations -> first_response(Locations, Req, Opts)
    end.

%% @doc Iterate through a list of endpoint URLs, returning the first response.
first_response([], _Req, _Opts) -> {error, no_viable_index};
first_response([Location | Locations], Req, Opts) ->
    {URL, OptsForReq} =
        if is_binary(Location) -> {Location, Opts};
        true ->
                {
                    hb_converge:get(url, Location, Opts),
                    hb_converge:get(opts, Location, Opts)
                }
        end,
    ?event(graphql, {request, {location, Location}, {request, Req}}),
    Resp = hb_http:request(
            #{
                <<"path">> => URL,
                <<"method">> => <<"POST">>,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => jiffy:encode(Req)
            },
            maps:merge(Opts, OptsForReq)
        ),
    case Resp of
        {ok, Res} ->
            JSONStruct = jiffy:decode(maps:get(<<"body">>, Res, <<>>), [return_maps]),
            ?event(graphql, {gql_resp, {location, Location}, {response, JSONStruct}}),
            {
                ok,
                JSONStruct,
                Location
            };
        {X, Reason} when X == error; X == unavailable ->
            ?event(graphql, {request_error, Location, {explicit, Reason}}),
            first_response(Locations, Req, Opts)
    end.

%%% Tests
ans104_no_data_item_test() ->
    % Start a random node so that all of the services come up.
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = message_from_id(<<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>, #{}),
    ?event(graphql, {get_ans104_test, Res}),
    ?event(graphql, {signer, hb_message:signers(Res)}),
    ?assert(hb_message:verify(Res)).
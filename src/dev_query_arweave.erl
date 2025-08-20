%%% @doc An implementation of the Arweave GraphQL API, inside the `~query@1.0'
%%% device.
-module(dev_query_arweave).
%%% AO-Core API:
-export([query/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc The arguments that are supported by the Arweave GraphQL API.
-define(SUPPORTED_QUERY_ARGS,
    [
        <<"ids">>,
        <<"tags">>,
        <<"owners">>,
        <<"recipients">>
    ]
).

%% @doc Handle an Arweave GraphQL query.
query(Obj, <<"transactions">>, Args, Opts) ->
    ?event({transactions_query,
        {object, Obj},
        {field, <<"transactions">>},
        {args, Args}
    }),
    Matches = match_args(Args, Opts),
    ?event({transactions_matches, Matches}),
    Messages =
        lists:filtermap(
            fun(Match) ->
                case hb_cache:read(Match, Opts) of
                    {ok, Msg} -> {true, Msg};
                    not_found -> false
                end
            end,
            Matches
        ),
    {ok, Messages};
query(List, <<"edges">>, _Args, _Opts) ->
    {ok, [{ok, Msg} || Msg <- List]};
query(Msg, <<"node">>, _Args, _Opts) ->
    {ok, Msg};
query(Obj, Field, Args, _Opts) ->
    ?event({unimplemented_transactions_query,
        {object, Obj},
        {field, Field},
        {args, Args}
    }),
    {ok, <<"Not implemented.">>}.

%% @doc Progressively generate matches from each argument for a transaction
%% query.
match_args(Args, Opts) when is_map(Args) ->
    match_args(
        maps:to_list(
            maps:with(
                ?SUPPORTED_QUERY_ARGS,
                Args
            )
        ),
        not_started,
        Opts
    ).
match_args([], IDs, _) when is_list(IDs) -> IDs;
match_args([], _NoResults, _) -> not_found;
match_args([{Field, X} | Rest], Acc, Opts) ->
    case match(Field, X, Opts) of
        {ok, Result} when is_list(Acc) ->
            match_args(Rest, hb_util:list_with(Acc, Result), Opts);
        {ok, Result} ->
            match_args(Rest, Result, Opts);
        _ ->
            match_args(Rest, Acc, Opts)
    end.

%% @doc Generate a match upon `tags' in the arguments, if given.
match(_, null, _) -> ignore;
match(<<"ids">>, IDs, _Opts) ->
    {ok, IDs};
match(<<"tags">>, Tags, Opts) ->
    Template = dev_query_graphql:keys_to_template(Tags),
    ?event({tags_template, Template}),
    hb_cache:match(Template, Opts);
match(<<"owners">>, Owners, Opts) ->
    {ok, matching_commitments(<<"committer">>, Owners, Opts)};
match(<<"recipients">>, Recipients, Opts) ->
    {ok, matching_commitments(<<"field-target">>, Recipients, Opts)};
match(UnsupportedFilter, _, _) ->
    throw({unsupported_query_filter, UnsupportedFilter}).

%% @doc Return the base IDs for messages that have a matching commitment.
matching_commitments(Field, Values, Opts) when is_list(Values) ->
    hb_util:unique(lists:flatten(
        lists:map(
            fun(Value) -> matching_commitments(Field, Value, Opts) end,
            Values
        )
    ));
matching_commitments(Field, Value, Opts) when is_binary(Value) ->
    case hb_cache:match(#{ Field => Value }, Opts) of
        {ok, IDs} ->
            lists:map(fun(ID) -> commitment_id_to_base_id(ID, Opts) end, IDs);
        not_found -> not_found
    end.

%% @doc Convert a commitment message's ID to a base ID.
commitment_id_to_base_id(ID, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    ?event({commitment_id_to_base_id, ID}),
    case hb_store:read(Store, << ID/binary, "/signature">>) of
        {ok, EncSig} ->
            Sig = hb_util:decode(EncSig),
            hb_util:encode(hb_crypto:sha256(Sig));
        not_found ->
            not_found
    end.

%% @doc Generate a match upon `ids' in the arguments, if given.

%%% Tests

write_test_message(Opts) ->
    hb_cache:write(
        Msg = hb_message:commit(
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Message">>,
                <<"action">> => <<"Eval">>,
                <<"data">> => <<"test data">>
            },
            Opts,
            #{
                <<"commitment-device">> => <<"ans104@1.0">>
            }
        ),
        Opts
    ),
    {ok, Msg}.

simple_ans104_query_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($owners: [String!]) {
                transactions(
                    tags:
                        [
                            {name: "type" values: ["Message"]},
                            {name: "variant" values: ["ao.N.1"]}
                        ],
                    owners: $owners
                    ) {
                    edges {
                        node {
                            id,
                            tags {
                                name,
                                value
                            }
                        }
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"owners">> => [hb:address()]
            },
            Opts
        ),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?event({expected_id, ExpectedID}),
    ?event({simple_ans104_query_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        [#{
                            <<"node">> :=
                                #{
                                    <<"id">> := ExpectedID,
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).
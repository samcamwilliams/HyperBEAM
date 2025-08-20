%%% @doc An implementation of the Arweave GraphQL API, inside the `~query@1.0'
%%% device.
-module(dev_query_arweave).
%%% AO-Core API:
-export([query/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

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
    {ok, [{ok, Msg} || Msg <- Messages]};
query(_, _, _, _) ->
    {ok, <<"Not implemented.">>}.

%% @doc Progressively generate matches from each argument for a transaction
%% query.
match_args(Args, Opts) when is_map(Args) -> match_args(maps:to_list(Args), Opts);
match_args([], _) -> [];
match_args([{Field, Values} | Rest], Opts) ->
    match_args(Rest, match(Field, Values, Opts), Opts).

match_args([], Acc, _) -> Acc;
match_args([{Field, Values } | Rest], Acc, Opts) ->
    match_args(Rest, hb_util:list_with(Acc, match(Field, Values, Opts)), Opts).

%% @doc Generate a match upon `tags' in the arguments, if given.
match(<<"ids">>, IDs, _Opts) ->
    {<<"ids">>, IDs};
match(<<"tags">>, Tags, Opts) ->
    {ok, IDs} = hb_cache:match(dev_query_graphql:keys_to_template(Tags), Opts),
    {<<"tags">>, IDs};
match(<<"owners">>, Owners, Opts) ->
    {<<"owners">>, matching_commitments(<<"committer">>, Owners, Opts)};
match(<<"recipients">>, Recipients, Opts) ->
    {<<"recipients">>, matching_commitments(<<"field-target">>, Recipients, Opts)};
match(UnsupportedFilter, _, _) ->
    throw({unsupported_query_filter, UnsupportedFilter}).

%% @doc Return the base IDs for messages that have a matching commitment.
matching_commitments(Field, Values, Opts) ->
    hb_util:unique(lists:flatten(
        lists:map(
            fun(Value) ->
                lists:map(
                    fun(ID) -> commitment_id_to_base_id(ID, Opts) end,
                    case hb_cache:match(#{ Field => Value }, Opts) of
                        {ok, IDs} ->
                            lists:map(
                                fun(ID) -> commitment_id_to_base_id(ID, Opts) end,
                                IDs
                            );
                        not_found -> []
                    end
                )
            end,
            Values
        )
    )).

%% @doc Convert a commitment message's ID to a base ID.
commitment_id_to_base_id(ID, Opts) ->
    case hb_store:read(<< ID/binary, "signature">>, Opts) of
        {ok, EncSig} ->
            Sig = hb_util:decode(EncSig),
            hb_util:encode(hb_crypto:sha256(Sig));
        not_found ->
            not_found
    end.

%% @doc Generate a match upon `ids' in the arguments, if given.

%%% Tests

write_test_messages(Opts) ->
    hb_message:commit(
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
    ok.

simple_ans104_query_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    Query =
        <<"""
            query {
                transactions(
                    tags:
                        [
                            {name: "type", value: "Message"},
                            {name: "action", value: "Eval"}
                        ]
                    ) {
                    edges {
                        node {
                            id,
                            tags
                        }
                    }
                }
            }
        """>>,
    ok = write_test_messages(Opts),
    Res = dev_query_graphql:test_query(Node, Query, Opts),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        #{
                            <<"node">> := #{<<"id">> := _},
                            <<"tags">> :=
                                [
                                    {<<"type">>, <<"Message">>},
                                    {<<"action">>, <<"Eval">>}
                                ]
                        }
                }
            }
        },
        Res
    ).
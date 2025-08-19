%%% @doc An implementation of the Arweave GraphQL API, inside the `~query@1.0'
%%% device.
-module(dev_query_arweave).
%%% AO-Core API:
-export([query/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Handle an Arweave GraphQL query.
query(Obj, Field, Args, _Opts) ->
    ?event({arweave_query, {object, Obj}, {field, Field}, {args, Args}}),
    {ok, <<"Not implemented.">>}.

%%% Tests

write_test_messages(Opts) ->
    Item =
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
    Query =
        <<"""
            query {
                transactions(first: 1) {
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
    {ok, Response} =
        hb_http:post(
            <<"http://localhost:3000">>,
            <<"/graphql">>,
            #{
                <<"body">> => Query
            },
            Opts
        ),
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
        Response
    ).
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
query(Obj, <<"transaction">>, #{<<"id">> := ID}, Opts) ->
    ?event({transaction_query, 
            {object, Obj}, 
            {field, <<"transaction">>}, 
            {id, ID}
        }),
    case hb_cache:read(ID, Opts) of
        {ok, Msg} -> 
            ?event({transaction_found, {id, ID}, {msg, Msg}}),
            {ok, Msg};
        not_found -> 
            ?event({transaction_not_found, {id, ID}}),
            {ok, null}
    end;
query(List, <<"edges">>, _Args, _Opts) ->
    {ok, [{ok, Msg} || Msg <- List]};
query(Msg, <<"node">>, _Args, _Opts) ->
    {ok, Msg};
query(Msg, <<"signature">>, _Args, Opts) ->
    case hb_maps:get(<<"commitments">>, Msg, not_found, Opts) of
        not_found -> {ok, null};
        Commitments ->
            case maps:to_list(Commitments) of
                [] -> {ok, null};
                [{_CommitmentID, Commitment} | _] ->
                    {ok, hb_maps:get(<<"signature">>, Commitment, null, Opts)}
            end
    end;
query(Msg, <<"owner">>, _Args, Opts) ->
    ?event({query_owner, Msg}),
    case hb_message:commitments(#{ <<"committer">> => '_' }, Msg, Opts) of
        not_found -> {ok, null};
        Commitments ->
            case hb_maps:keys(Commitments) of
                [] -> {ok, null};
                [CommID | _] ->
                    {ok, Commitment} = hb_maps:find(CommID, Commitments, Opts),
                    {ok, Address} = hb_maps:find(<<"committer">>, Commitment, Opts),
                    {ok, KeyID} = hb_maps:find(<<"keyid">>, Commitment, Opts),
                    Key = dev_codec_httpsig_keyid:remove_scheme_prefix(KeyID),
                    {ok, #{
                        <<"address">> => Address,
                        <<"key">> => Key
                    }}
            end
    end;
query(#{ <<"key">> := Key }, <<"key">>, _Args, _Opts) ->
    {ok, Key};
query(#{ <<"address">> := Address }, <<"address">>, _Args, _Opts) ->
    {ok, Address};
query(Msg, <<"recipient">>, _Args, Opts) ->
    find_field_key(<<"field-target">>, Msg, Opts);
query(Msg, <<"anchor">>, _Args, Opts) ->
    case find_field_key(<<"field-anchor">>, Msg, Opts) of
        {ok, null} -> {ok, null};
        {ok, Anchor} -> {ok, hb_util:human_id(Anchor)}
    end;
query(Msg, <<"data">>, _Args, Opts) ->
    Data =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Msg}, <<"data">>},
                {{as, <<"message@1.0">>, Msg}, <<"body">>}
            ],
            <<>>,
            Opts
        ),
    Type = hb_maps:get(<<"content-type">>, Msg, null, Opts),
    {ok, #{ <<"data">> => Data, <<"type">> => Type }};
query(#{ <<"data">> := Data }, <<"size">>, _Args, _Opts) ->
    {ok, byte_size(Data)};
query(#{ <<"type">> := Type }, <<"type">>, _Args, _Opts) ->
    {ok, Type};
query(Obj, Field, Args, _Opts) ->
    ?event({unimplemented_transactions_query,
        {object, Obj},
        {field, Field},
        {args, Args}
    }),
    {ok, <<"Not implemented.">>}.

%% @doc Find and return a value from the fields of a message (from its
%% commitments).
find_field_key(Field, Msg, Opts) ->
    case hb_message:commitments(#{ Field => '_' }, Msg, Opts) of
        not_found -> {ok, null};
        Commitments ->
            case hb_maps:keys(Commitments) of
                [] -> {ok, null};
                [CommID | _] ->
                    {ok, Commitment} = hb_maps:find(CommID, Commitments, Opts),
                    case hb_maps:find(Field, Commitment, Opts) of
                        {ok, Value} -> {ok, Value};
                        error -> {ok, null}
                    end
            end
    end.

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
        [],
        Opts
    ).
match_args([], Results, Opts) ->
    hb_util:unique(
        lists:flatten(
            [
                all_ids(ID, Opts)
            ||
                ID <- lists:flatten(Results)
            ]
        )
    );
match_args([{Field, X} | Rest], Acc, Opts) ->
    MatchRes = match(Field, X, Opts),
    ?event({match, {field, Field}, {arg, X}, {match_res, MatchRes}}),
    case MatchRes of
        {ok, Result} ->
            match_args(Rest, [Result | Acc], Opts);
        _Error ->
            match_args(Rest, Acc, Opts)
    end.

%% @doc Generate a match upon `tags' in the arguments, if given.
match(_, null, _) -> ignore;
match(<<"ids">>, IDs, _Opts) ->
    {ok, IDs};
match(<<"tags">>, Tags, Opts) ->
    hb_cache:match(dev_query_graphql:keys_to_template(Tags), Opts);
match(<<"owners">>, Owners, Opts) ->
    {ok, matching_commitments(<<"committer">>, Owners, Opts)};
match(<<"owner">>, Owner, Opts) ->
    Res =  matching_commitments(<<"committer">>, Owner, Opts),
    ?event({match_owner, Owner, Res}),
    {ok, Res};
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
            ?event({found_matching_commitments, {field, Field}, {value, Value}, {ids, IDs}}),
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
            ?event({commitment_id_to_base_id_sig, Sig}),
            hb_util:encode(hb_crypto:sha256(Sig));
        not_found -> not_found
    end.

%% @doc Find all IDs for a message, by any of its other IDs.
all_ids(ID, Opts) ->
    Store = hb_opts:get(store, no_store, Opts),
    case hb_store:list(Store, << ID/binary, "/commitments">>) of
        {ok, []} -> [ID];
        {ok, CommitmentIDs} -> CommitmentIDs;
        _ -> []
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

%% Helper function to write test message with Recipient
write_test_message_with_recipient(Recipient, Opts) ->
    hb_cache:write(
        Msg = hb_message:commit(
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Message">>,
                <<"action">> => <<"Eval">>,
                <<"content-type">> => <<"text/plain">>,
                <<"data">> => <<"test data">>,
                <<"target">> => Recipient
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

%% @doc Test transactions query with tags filter
transactions_query_tags_test() ->
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
            query {
                transactions(
                    tags: [
                        {name: "type", values: ["Message"]},
                        {name: "variant", values: ["ao.N.1"]}
                    ]
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
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
            #{},
            Opts
        ),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_tags_test, Res}),
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

%% @doc Test transactions query with owners filter
transactions_query_owners_test() ->
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
                    owners: $owners
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
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
    ?event({transactions_query_owners_test, Res}),
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

%% @doc Test transactions query with recipients filter
transactions_query_recipients_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    Alice = ar_wallet:new(),
    ?event({alice, Alice, {explicit, hb_util:human_id(Alice)}}),
    AliceAddress = hb_util:human_id(Alice),
    {ok, WrittenMsg} = write_test_message_with_recipient(AliceAddress, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($recipients: [String!]) {
                transactions(
                    recipients: $recipients
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
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
                <<"recipients">> => [AliceAddress]
            },
            Opts
        ),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_recipients_test, Res}),
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

%% @doc Test transactions query with ids filter
transactions_query_ids_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($ids: [ID!]) {
                transactions(
                    ids: $ids
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
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
                <<"ids">> => [ExpectedID]
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_ids_test, Res}),
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

%% @doc Test transactions query with combined filters
transactions_query_combined_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($owners: [String!], $ids: [ID!]) {
                transactions(
                    owners: $owners,
                    ids: $ids,
                    tags: [
                        {name: "type", values: ["Message"]}
                    ]
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
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
                <<"owners">> => [hb:address()],
                <<"ids">> => [ExpectedID]
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_combined_test, Res}),
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


%% @doc Test single transaction query by ID
transaction_query_by_id_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($id: ID!) {
                transaction(id: $id) {
                    id
                    tags {
                        name
                        value
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"id">> => ExpectedID
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transaction_query_by_id_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transaction">> := #{
                    <<"id">> := ExpectedID,
                    <<"tags">> :=
                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Test single transaction query with more fields  
transaction_query_full_test() ->
    Opts =
        #{
            priv_wallet => SenderKey = hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    Alice = ar_wallet:new(),
    ?event({alice, Alice, {explicit, hb_util:human_id(Alice)}}),
    AliceAddress = hb_util:human_id(Alice),
    SenderAddress = hb_util:human_id(SenderKey),
    SenderPubKey = hb_util:encode(ar_wallet:to_pubkey(SenderKey)),
    {ok, WrittenMsg} = write_test_message_with_recipient(AliceAddress, Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($id: ID!) {
                transaction(id: $id) {
                    id
                    anchor
                    signature
                    recipient
                    owner {
                        address
                        key
                    }
                    tags {
                        name
                        value
                    }
                    data {
                        size
                        type
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"id">> => ExpectedID
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transaction_query_full_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transaction">> := #{
                    <<"id">> := ExpectedID,
                    <<"recipient">> := AliceAddress,
                    <<"anchor">> := null,
                    <<"owner">> := #{
                        <<"address">> := SenderAddress,
                        <<"key">> := SenderPubKey
                    },
                    <<"data">> := #{
                        <<"size">> := <<"9">>,
                        <<"type">> := <<"text/plain">>
                    },
                    <<"tags">> :=
                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                    % Note: other fields may be "Not implemented." for now
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Test single transaction query with non-existent ID
transaction_query_not_found_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    NonExistentID = hb_util:encode(crypto:strong_rand_bytes(32)),
    Query =
        <<"""
            query($id: ID!) {
                transaction(id: $id) {
                    id
                    tags {
                        name
                        value
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"id">> => NonExistentID
            },
            Opts
        ),
    ?event({non_existent_id, NonExistentID}),
    ?event({transaction_query_not_found_test, Res}),
    % Should return null for non-existent transaction
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transaction">> := null
            }
        },
        Res
    ).
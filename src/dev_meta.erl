%%% @doc The hyperbeam meta device, which is the default entry point
%%% for all messages processed by the machine. This device executes a
%%% Converge singleton request, after first applying the node's 
%%% pre-processor, if set. The pre-processor can halt the request by
%%% returning an error, or return a modified version if it deems necessary --
%%% the result of the pre-processor is used as the request for the Converge
%%% resolver. Additionally, a post-processor can be set, which is executed after
%%% the Converge resolver has returned a result.
-module(dev_meta).
-export([handle/2, info/3]).
%%% Helper functions for processors
-export([all_signers/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Normalize and route messages downstream based on their path. Messages
%% with a `Meta` key are routed to the `handle_meta/2` function, while all
%% other messages are routed to the `handle_converge/2` function.
handle(NodeMsg, RawRequest) ->
    ?event({raw_request, RawRequest, NodeMsg}),
    NormRequest = hb_singleton:from(RawRequest),
    ?event({processing_messages, NormRequest}),
    handle_converge(RawRequest, NormRequest, NodeMsg).

%% @doc Get/set the node message. If the request is a `POST`, we check that the
%% request is signed by the owner of the node. If not, we return the node message
%% as-is, aside all keys that are private (according to `hb_private`).
info(_, Request, NodeMsg) ->
    ?event({config_req, Request, NodeMsg}),
    case hb_converge:get(<<"method">>, Request, NodeMsg) of
        <<"GET">> ->
            embed_status({ok, hb_private:reset(NodeMsg)});
        <<"POST">> ->
            ReqSigners = hb_message:signers(Request),
            Owner =
                hb_opts:get(
                    operator,
                    case hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg) of
                        no_viable_wallet -> unclaimed;
                        Wallet -> ar_wallet:to_address(Wallet)
                    end,
                    NodeMsg
                ),
            case Owner == unclaimed orelse lists:member(Owner, ReqSigners) of
                false ->
                    ?event(auth, {set_node_message_fail, Request}),
                    embed_status({error, <<"Unauthorized">>});
                true ->
                    ?event(auth, {set_node_message_success, Request}),
                    hb_http_server:set_opts(
                        Request#{
                            http_server =>
                                hb_opts:get(http_server, no_server, NodeMsg)
                        }
                    ),
                    embed_status({ok, <<"OK">>})
            end;
        _ -> embed_status({error, <<"Unsupported Method">>})
    end.

%% @doc Handle a Converge request, which is a list of messages. We apply
%% the node's pre-processor to the request first, and then resolve the request
%% using the node's Converge implementation if its response was `ok`.
%% After execution, we run the node's `postprocessor` message on the result of
%% the request before returning the result it grants back to the user.
handle_converge(Req, Msgs, NodeMsg) ->
    % Apply the pre-processor to the request.
    case resolve_processor(<<"preprocess">>, preprocessor, Req, Msgs, NodeMsg) of
        {ok, PreProcMsg} ->
            ?event({result_after_preprocessing, PreProcMsg}),
            AfterPreprocOpts = hb_http_server:get_opts(NodeMsg),
            % Resolve the request message.
            {ok, Res} =
                embed_status(
                    hb_converge:resolve_many(
                        PreProcMsg,
                        AfterPreprocOpts#{ force_message => true }
                    )
                ),
            ?event({res, Res}),
            AfterResolveOpts = hb_http_server:get_opts(NodeMsg),
            % Apply the post-processor to the result.
            maybe_sign(
                embed_status(
                    resolve_processor(
                        <<"postprocess">>,
                        postprocessor,
                        Req,
                        Res,
                        AfterResolveOpts
                    )
                ),
                NodeMsg
            );
        Res -> embed_status(Res)
    end.

%% @doc execute a message from the node message upon the user's request.
resolve_processor(PathKey, Processor, Req, Query, NodeMsg) ->
    case hb_opts:get(Processor, undefined, NodeMsg) of
        undefined -> {ok, Query};
        ProcessorMsg ->
            ?event({resolving_processor, PathKey, ProcessorMsg}),
            Res = hb_converge:resolve(
                ProcessorMsg,
                #{
                    <<"path">> => PathKey,
                    <<"body">> => Query,
                    <<"request">> => Req
                },
                NodeMsg#{ hashpath => ignore }
            ),
            ?event({preprocessor_result, Res}),
            Res
    end.

%% @doc Wrap the result of a device call in a status.
embed_status({Status, Res}) when is_map(Res) ->
    {ok, Res#{ <<"status">> => hb_http:status_code(Status) }};
embed_status({Status, Res}) when is_binary(Res) ->
    {ok, #{ <<"status">> => hb_http:status_code(Status), <<"body">> => Res }}.

%% @doc Sign the result of a device call if the node is configured to do so.
maybe_sign({Status, Res}, NodeMsg) ->
    {Status, maybe_sign(Res, NodeMsg)};
maybe_sign(Res, NodeMsg) ->
    ?event({maybe_sign, Res, NodeMsg}),
    case hb_opts:get(force_signed, false, NodeMsg) of
        true ->
            hb_message:sign(
                Res,
                hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg),
                hb_opts:get(format, http, NodeMsg)
            );
        false -> Res
    end.

%%% External helpers

%% @doc Return the signers of a list of messages.
all_signers(Msgs) ->
    lists:foldl(
        fun(Msg, Acc) ->
            Signers =
                hb_converge:get(<<"signers">>, Msg, #{}, #{ hashpath => ignore }),
            Acc ++ lists:map(fun hb_util:human_id/1, maps:values(Signers))
        end,
        [],
        Msgs
    ).

%%% Tests

%% @doc Test that we can get the node message.
config_test() ->
    Node = hb_http_server:start_test_node(#{ test_config_item => <<"test">> }),
    {ok, Res} = hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test">>, hb_converge:get(<<"test_config_item">>, Res, #{})).

%% @doc Test that we can't get the node message if the requested key is private.
priv_inaccessible_test() ->
    Node = hb_http_server:start_test_node(
        #{
            test_config_item => <<"test">>,
            priv_key => <<"BAD">>
        }
    ),
    {ok, Res} = hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test">>, hb_converge:get(<<"test_config_item">>, Res, #{})),
    ?assertEqual(not_found, hb_converge:get(<<"priv_key">>, Res, #{})).

%% @doc Test that we can't set the node message if the request is not signed by
%% the owner of the node.
unauthorized_set_node_msg_fails_test() ->
    Node = hb_http_server:start_test_node(#{ priv_wallet => ar_wallet:new() }),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:sign(
                #{
                    <<"path">> => <<"/!meta@1.0/info">>,
                    <<"evil_config_item">> => <<"BAD">>
                },
                ar_wallet:new()
            ),
            #{}
        ),
    ?event({res, SetRes}),
    {ok, Res} = hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(not_found, hb_converge:get(<<"evil_config_item">>, Res, #{})).

%% @doc Test that we can set the node message if the request is signed by the
%% owner of the node.
authorized_set_node_msg_succeeds_test() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_test_node(
        #{
            operator => ar_wallet:to_address(Owner),
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:sign(
                #{
                    <<"path">> => <<"/!meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes}),
    {ok, Res} = hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test2">>, hb_converge:get(<<"test_config_item">>, Res, #{})).

%% @doc Test that we can claim the node correctly and set the node message after.
claim_node_test() ->
    Owner = ar_wallet:new(),
    Address = ar_wallet:to_address(Owner),
    Node = hb_http_server:start_test_node(
        #{
            operator => unclaimed,
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:sign(
                #{
                    <<"path">> => <<"/!meta@1.0/info">>,
                    <<"operator">> => Address
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes}),
    {ok, Res} = hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(Address, hb_converge:get(<<"operator">>, Res, #{})),
    {ok, SetRes2} =
        hb_http:post(
            Node,
            hb_message:sign(
                #{
                    <<"path">> => <<"/!meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes2}),
    {ok, Res2} = hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?event({res, Res2}),
    ?assertEqual(<<"test2">>, hb_converge:get(<<"test_config_item">>, Res2, #{})).

%% @doc Test that we can use a preprocessor upon a request.
preprocessor_test() ->
    Parent = self(),
    Node = hb_http_server:start_test_node(
        #{
            preprocessor =>
                #{
                    <<"device">> => #{
                        <<"preprocess">> =>
                            fun(_, #{ <<"body">> := Msgs }, _) ->
                                Parent ! ok,
                                {ok, Msgs}
                            end
                    }
                }
        }),
    hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?assert(receive ok -> true after 1000 -> false end).

%% @doc Test that we can halt a request if the preprocessor returns an error.
halt_request_test() ->
    Node = hb_http_server:start_test_node(
        #{
            preprocessor =>
                #{
                    <<"device">> => #{
                        <<"preprocess">> =>
                            fun(_, _, _) ->
                                {error, <<"Bad">>}
                            end
                    }
                }
        }),
    {error, Res} = hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?assertEqual(<<"Bad">>, Res).

%% @doc Test that a preprocessor can modify a request.
modify_request_test() ->
    Node = hb_http_server:start_test_node(
        #{
            preprocessor =>
                #{
                    <<"device">> => #{
                        <<"preprocess">> =>
                            fun(_, #{ <<"body">> := [M|Ms] }, _) ->
                                {ok, [M#{ <<"added">> => <<"value">> }|Ms]}
                            end
                    }
                }
        }),
    {ok, Res} = hb_http:get(Node, <<"/added">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"value">>, hb_converge:get(<<"body">>, Res, #{})).

%% @doc Test that we can use a postprocessor upon a request.
postprocessor_test() ->
    Parent = self(),
    Node = hb_http_server:start_test_node(
        #{
            postprocessor =>
                #{
                    <<"device">> => #{
                        <<"postprocess">> =>
                            fun(_, #{ <<"body">> := Msgs }, _) ->
                                Parent ! ok,
                                {ok, Msgs}
                            end
                    }
                }
        }),
    hb_http:get(Node, <<"/!meta@1.0/info">>, #{}),
    ?assert(receive ok -> true after 1000 -> false end).
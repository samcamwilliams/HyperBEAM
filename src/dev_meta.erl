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
-export([all_attestors/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Normalize and route messages downstream based on their path. Messages
%% with a `Meta' key are routed to the `handle_meta/2' function, while all
%% other messages are routed to the `handle_converge/2' function.
handle(NodeMsg, RawRequest) ->
    ?event({singleton_request, RawRequest}),
    NormRequest = hb_singleton:from(RawRequest),
    ?event(http_short, {request, hb_converge:normalize_keys(NormRequest)}),
    case hb_opts:get(initialized, false, NodeMsg) of
        false ->
            embed_status(handle_initialize(NormRequest, NodeMsg));
        true ->
            handle_converge(RawRequest, NormRequest, NodeMsg);
        _ ->
            embed_status({error, <<"`initialized` not found in node message.">>})
    end.

handle_initialize([Base = #{ <<"device">> := Device}, Req = #{ <<"path">> := Path }|_], NodeMsg) ->
    ?event({got, {device, Device}, {path, Path}}),
    case {Device, Path} of
        {<<"meta@1.0">>, <<"info">>} -> info(Base, Req, NodeMsg);
        _ -> {error, <<"Node must be initialized before use.">>}
    end;
handle_initialize([{as, <<"meta@1.0">>, _}|Rest], NodeMsg) ->
    handle_initialize([#{ <<"device">> => <<"meta@1.0">>}|Rest], NodeMsg);
handle_initialize([_|Rest], NodeMsg) ->
    handle_initialize(Rest, NodeMsg);
handle_initialize([], _NodeMsg) ->
    {error, <<"Node must be initialized before use.">>}.

%% @doc Get/set the node message. If the request is a `POST', we check that the
%% request is signed by the owner of the node. If not, we return the node message
%% as-is, aside all keys that are private (according to `hb_private').
info(_, Request, NodeMsg) ->
    case hb_converge:get(<<"method">>, Request, NodeMsg) of
        <<"GET">> ->
            ?event({get_config_req, Request, NodeMsg}),
            embed_status({ok, filter_node_msg(add_dynamic_keys(NodeMsg))});
        <<"POST">> ->
            case hb_converge:get(<<"initialized">>, NodeMsg, not_found, NodeMsg) of
                <<"permanent">> ->
                    embed_status(
                        {error,
                            <<"The node message of this machine is already "
                                "permanent. It cannot be changed.">>
                        }
                    );
                _ ->
                    update_node_message(Request, NodeMsg)
            end;
        _ -> embed_status({error, <<"Unsupported Meta/info method.">>})
    end.

%% @doc Remove items from the node message that are not encodable into a
%% message.
filter_node_msg(Msg) when is_map(Msg) ->
    maps:map(fun(_, Value) -> filter_node_msg(Value) end, hb_private:reset(Msg));
filter_node_msg(Msg) when is_list(Msg) ->
    lists:map(fun filter_node_msg/1, Msg);
filter_node_msg(Tuple) when is_tuple(Tuple) ->
    <<"Unencodable value.">>;
filter_node_msg(Other) ->
    Other.

%% @doc Add dynamic keys to the node message.
add_dynamic_keys(NodeMsg) ->
    case hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg) of
        no_viable_wallet ->
            NodeMsg;
        Wallet ->
            maps:merge(NodeMsg, #{
                <<"address">> => hb_util:id(ar_wallet:to_address(Wallet))
            })
    end.

update_node_message(Request, NodeMsg) ->
    {ok, RequestSigners} = dev_message:attestors(Request),
    Operator =
        hb_opts:get(
            operator,
            case hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg) of
                no_viable_wallet -> unclaimed;
                Wallet -> ar_wallet:to_address(Wallet)
            end,
            NodeMsg
        ),
    EncOperator =
        case Operator of
            unclaimed ->
                unclaimed;
            Val ->
                hb_util:human_id(Val)
        end,
    case EncOperator == unclaimed orelse lists:member(EncOperator, RequestSigners) of
        false ->
            ?event({set_node_message_fail, Request}),
            embed_status({error, <<"Unauthorized">>});
        true ->
            ?event({set_node_message_success, Request}),
            hb_http_server:set_opts(
                Request#{
                    http_server =>
                        hb_opts:get(http_server, no_server, NodeMsg)
                }
            ),
            embed_status({ok, <<"OK">>})
    end.

%% @doc Handle a Converge request, which is a list of messages. We apply
%% the node's pre-processor to the request first, and then resolve the request
%% using the node's Converge implementation if its response was `ok'.
%% After execution, we run the node's `postprocessor' message on the result of
%% the request before returning the result it grants back to the user.
handle_converge(Req, Msgs, NodeMsg) ->
    % Apply the pre-processor to the request.
    case resolve_processor(<<"preprocess">>, preprocessor, Req, Msgs, NodeMsg) of
        {ok, PreProcessedMsg} ->
            ?event(
                {result_after_preprocessing,
                    hb_converge:normalize_keys(PreProcessedMsg)}
            ),
            AfterPreprocOpts = hb_http_server:get_opts(NodeMsg),
            % Resolve the request message.
            HTTPOpts = maps:merge(
                AfterPreprocOpts,
                hb_opts:get(http_extra_opts, #{}, NodeMsg)
            ),
            {ok, Res} =
                embed_status(
                    hb_converge:resolve_many(
                        PreProcessedMsg,
                        HTTPOpts#{ force_message => true }
                    )
                ),
            ?event({res, Res}),
            AfterResolveOpts = hb_http_server:get_opts(NodeMsg),
            % Apply the post-processor to the result.
            Output = maybe_sign(
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
            ),
            ?event(http_short, {response, Output}),
            Output;
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
            ?event({processor_result, {type, PathKey}, {res, Res}}),
            Res
    end.

%% @doc Wrap the result of a device call in a status.
embed_status({ErlStatus, Res}) when is_map(Res) ->
    case lists:member(<<"status">>, hb_message:attested(Res)) of
        false ->
            HTTPCode = status_code({ErlStatus, Res}),
            {ok, Res#{ <<"status">> => HTTPCode }};
        true ->
            {ok, Res}
    end;
embed_status({ErlStatus, Res}) ->
    HTTPCode = status_code({ErlStatus, Res}),
    {ok, #{ <<"status">> => HTTPCode, <<"body">> => Res }}.

%% @doc Calculate the appropriate HTTP status code for a Converge result.
%% The order of precedence is:
%% 1. The status code from the message.
%% 2. The HTTP representation of the status code.
%% 3. The default status code.
status_code({ErlStatus, Msg}) ->
    case message_to_status(Msg) of
        default -> status_code(ErlStatus);
        RawStatus -> RawStatus
    end;
status_code(ok) -> 200;
status_code(error) -> 400;
status_code(created) -> 201;
status_code(not_found) -> 404;
status_code(unavailable) -> 503.

%% @doc Get the HTTP status code from a transaction (if it exists).
message_to_status(#{ <<"body">> := Status }) when is_atom(Status) ->
    status_code(Status);
message_to_status(Item) when is_map(Item) ->
    % Note: We use `dev_message` directly here, such that we do not cause 
    % additional Converge calls for every request. This is particularly important
    % if a remote server is being used for all Converge requests by a node.
    case dev_message:get(<<"status">>, Item) of
        {ok, RawStatus} when is_integer(RawStatus) -> RawStatus;
        {ok, RawStatus} when is_atom(RawStatus) -> status_code(RawStatus);
        {ok, RawStatus} -> binary_to_integer(RawStatus);
        _ -> default
    end;
message_to_status(Item) when is_atom(Item) ->
    status_code(Item);
message_to_status(_Item) ->
    default.

%% @doc Sign the result of a device call if the node is configured to do so.
maybe_sign({Status, Res}, NodeMsg) ->
    {Status, maybe_sign(Res, NodeMsg)};
maybe_sign(Res, NodeMsg) ->
    ?event({maybe_sign, Res, NodeMsg}),
    case hb_opts:get(force_signed, false, NodeMsg) of
        true ->
            case hb_message:signers(Res) of
                [] ->
                    DefaultCodec = hb_opts:get(default_codec, <<"httpsig@1.0">>, NodeMsg),
                    hb_message:attest(
                        Res,
                        NodeMsg,
                        hb_converge:get(<<"codec-device">>, Res, DefaultCodec, NodeMsg)
                    );
                _ -> Res
            end;
        false -> Res
    end.

%%% External helpers

%% @doc Return the signers of a list of messages.
all_attestors(Msgs) ->
    lists:foldl(
        fun(Msg, Acc) ->
            Attestors =
                hb_converge:get(<<"attestors">>, Msg, #{}, #{ hashpath => ignore }),
            Acc ++ lists:map(fun hb_util:human_id/1, maps:values(Attestors))
        end,
        [],
        Msgs
    ).

%%% Tests

%% @doc Test that we can get the node message.
config_test() ->
    Node = hb_http_server:start_node(#{ test_config_item => <<"test">> }),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test">>, hb_converge:get(<<"test_config_item">>, Res, #{})).

%% @doc Test that we can't get the node message if the requested key is private.
priv_inaccessible_test() ->
    Node = hb_http_server:start_node(
        #{
            test_config_item => <<"test">>,
            priv_key => <<"BAD">>
        }
    ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test">>, hb_converge:get(<<"test_config_item">>, Res, #{})),
    ?assertEqual(not_found, hb_converge:get(<<"priv_key">>, Res, #{})).

%% @doc Test that we can't set the node message if the request is not signed by
%% the owner of the node.
unauthorized_set_node_msg_fails_test() ->
    Node = hb_http_server:start_node(#{ priv_wallet => ar_wallet:new() }),
    {error, _} =
        hb_http:post(
            Node,
            hb_message:attest(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"evil_config_item">> => <<"BAD">>
                },
                ar_wallet:new()
            ),
            #{}
        ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(not_found, hb_converge:get(<<"evil_config_item">>, Res, #{})).

%% @doc Test that we can set the node message if the request is signed by the
%% owner of the node.
authorized_set_node_msg_succeeds_test() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        #{
            operator => ar_wallet:to_address(Owner),
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:attest(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes}),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test2">>, hb_converge:get(<<"test_config_item">>, Res, #{})).

%% @doc Test that an uninitialized node will not run computation.
uninitialized_node_test() ->
    Node = hb_http_server:start_node(#{ initialized => false }),
    {error, Res} = hb_http:get(Node, <<"/key1?1.key1=value1">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"Node must be initialized before use.">>, Res).

%% @doc Test that a permanent node message cannot be changed.
permanent_node_message_test() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        #{
            operator => unclaimed,
            initialized => false,
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes1} =
        hb_http:post(
            Node,
            hb_message:attest(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>,
                    initialized => <<"permanent">>
                },
                Owner
            ),
            #{}
        ),
    ?event({set_res, SetRes1}),
    {ok, Res} = hb_http:get(Node, #{ <<"path">> => <<"/~meta@1.0/info">> }, #{}),
    ?event({get_res, Res}),
    ?assertEqual(<<"test2">>, hb_converge:get(<<"test_config_item">>, Res, #{})),
    {error, SetRes2} =
        hb_http:post(
            Node,
            hb_message:attest(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"bad_value">>
                },
                Owner
            ),
            #{}
        ),
    ?event({set_res, SetRes2}),
    {ok, Res2} = hb_http:get(Node, #{ <<"path">> => <<"/~meta@1.0/info">> }, #{}),
    ?event({get_res, Res2}),
    ?assertEqual(<<"test2">>, hb_converge:get(<<"test_config_item">>, Res2, #{})).

%% @doc Test that we can claim the node correctly and set the node message after.
claim_node_test() ->
    Owner = ar_wallet:new(),
    Address = ar_wallet:to_address(Owner),
    Node = hb_http_server:start_node(
        #{
            operator => unclaimed,
            test_config_item => <<"test">>
        }
    ),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:attest(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"operator">> => Address
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes}),
    timer:sleep(100),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(Address, hb_converge:get(<<"operator">>, Res, #{})),
    {ok, SetRes2} =
        hb_http:post(
            Node,
            hb_message:attest(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"test_config_item">> => <<"test2">>
                },
                Owner
            ),
            #{}
        ),
    ?event({res, SetRes2}),
    {ok, Res2} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res2}),
    ?assertEqual(<<"test2">>, hb_converge:get(<<"test_config_item">>, Res2, #{})).

%% @doc Test that we can use a preprocessor upon a request.
% preprocessor_test() ->
%     Parent = self(),
%     Node = hb_http_server:start_node(
%         #{
%             preprocessor =>
%                 #{
%                     <<"device">> => #{
%                         <<"preprocess">> =>
%                             fun(_, #{ <<"body">> := Msgs }, _) ->
%                                 Parent ! ok,
%                                 {ok, Msgs}
%                             end
%                     }
%                 }
%         }),
%     hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
%     ?assert(receive ok -> true after 1000 -> false end).

%% @doc Test that we can halt a request if the preprocessor returns an error.
halt_request_test() ->
    Node = hb_http_server:start_node(
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
    {error, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(<<"Bad">>, Res).

%% @doc Test that a preprocessor can modify a request.
modify_request_test() ->
    Node = hb_http_server:start_node(
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

%% @doc Test that we can use a postprocessor upon a request. Calls the `test@1.0'
%% device's postprocessor, which sets the `postprocessor-called' key to true in
%% the HTTP server.
% postprocessor_test() ->
%     Node = hb_http_server:start_node(
%         #{
%             postprocessor => <<"test-device@1.0">>
%         }),
%     hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
%     timer:sleep(100),
%     {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info/postprocessor-called">>, #{}),
%     ?event({res, Res}),
%     ?assertEqual(true, Res).
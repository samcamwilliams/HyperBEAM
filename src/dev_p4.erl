%%% @doc The HyperBEAM core payment ledger. This module allows the operator to
%%% specify another device that can act as a pricing mechanism for transactions
%%% on the node, as well as orchestrating a payment ledger to calculate whether
%%% the node should fulfil services for users.
%%%
%%% The device requires the following node message settings in order to function:
%%%
%%% - `p4_pricing_device': The device that will estimate the cost of a request.
%%% - `p4_ledger_device': The device that will act as a payment ledger.
%%%
%%% The pricing device should implement the following keys:
%%% ```
%%%             GET /estimate?type=pre|post&body=[...]&request=RequestMessage
%%%             GET /price?type=pre|post&body=[...]&request=RequestMessage
%%% '''
%%% 
%%% The `body' key is used to pass either the request or response messages to the
%%% device. The `type' key is used to specify whether the inquiry is for a request
%%% (pre) or a response (post) object. Requests carry lists of messages that will
%%% be executed, while responses carry the results of the execution. The `price'
%%% key may return `infinity' if the node will not serve a user under any
%%% circumstances. Else, the value returned by the `price' key will be passed to
%%% the ledger device as the `amount' key.
%%%
%%% The ledger device should implement the following keys:
%%% ```
%%%             POST /credit?message=PaymentMessage&request=RequestMessage
%%%             POST /debit?amount=PriceMessage&type=pre|post&request=RequestMessage
%%% '''
%%%
%%% The `type' key is optional and defaults to `pre'. If `type' is set to `post',
%%% the debit must be applied to the ledger, whereas the `pre' type is used to
%%% check whether the debit would succeed before execution.
-module(dev_p4).
-export([preprocess/3, postprocess/3, balance/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The default list of routes that should not be charged for.
-define(DEFAULT_NON_CHARGABLE_ROUTES, [
    #{ <<"template">> => <<"/~p4@1.0/balance">> },
    #{ <<"template">> => <<"/~meta@1.0/*">> }
]).

%% @doc Estimate the cost of a transaction and decide whether to proceed with
%% a request. The default behavior if `pricing_device' or `p4_balances' are
%% not set is to proceed, so it is important that a user initialize them.
preprocess(State, Raw, NodeMsg) ->
    PricingDevice = hb_converge:get(<<"pricing_device">>, State, false, NodeMsg),
    LedgerDevice = hb_converge:get(<<"ledger_device">>, State, false, NodeMsg),
    Messages = hb_converge:get(<<"body">>, Raw, NodeMsg#{ hashpath => ignore }),
    Request = hb_converge:get(<<"request">>, Raw, NodeMsg),
    IsChargable = is_chargable_req(Request, NodeMsg),
    ?event(payment, {preprocess_with_devices, PricingDevice, LedgerDevice, {chargable, IsChargable}}),
    case {IsChargable, (PricingDevice =/= false) and (LedgerDevice =/= false)} of
        {false, _} -> {ok, Messages};
        {true, false} -> {ok, Messages};
        {true, true} ->
            PricingMsg = #{ <<"device">> => PricingDevice },
            LedgerMsg = #{ <<"device">> => LedgerDevice },
            PricingReq = #{
                <<"path">> => <<"estimate">>,
                <<"type">> => <<"pre">>,
                <<"request">> => Request,
                <<"body">> => Messages
            },
            ?event({p4_pricing_request, {devmsg, PricingMsg}, {req, PricingReq}}),
            case hb_converge:resolve(PricingMsg, PricingReq, NodeMsg) of
                {error, Error} ->
                    % The device is unable to estimate the cost of the request,
                    % so we don't proceed.
                    {error, {error_calculating_price, Error}};
                {ok, <<"infinity">>} ->
                    % The device states that under no circumstances should we
                    % proceed with the request.
                    ?event(payment, {p4_pre_pricing_response, {error, <<"infinity">>}}),
                    {error,
                        <<"Node will not service this request "
                            "under any circumstances.">>};
                {ok, Price} ->
                    % The device has estimated the cost of the request. We 
                    % forward the request to the ledger device to check if we
                    % have enough funds to service the request.
                    LedgerReq =
                        #{
                            <<"path">> => <<"debit">>,
                            <<"amount">> => Price,
                            <<"type">> => <<"pre">>,
                            <<"request">> => Request
                        },
                    ?event(payment, {p4_pre_pricing_estimate, Price}),
                    case hb_converge:resolve(LedgerMsg, LedgerReq, NodeMsg) of
                        {ok, true} ->
                            % The ledger device has confirmed that the user has
                            % enough funds for the request, so we proceed.
                            {ok, Messages};
                        {ok, false} ->
                            ?event(payment, {pre_ledger_validation, false}),
                            {error, 
                                #{
                                    <<"status">> => 429,
                                    <<"body">> => <<"Insufficient funds">>,
                                    <<"price">> => Price
                                }
                            };
                        {error, Error} ->
                            % The ledger device is unable to process the request,
                            % so we don't proceed.
                            ?event(payment, {pre_ledger_validation, {error, Error}}),
                            {error, {error_checking_ledger, Error}}
                    end
            end
    end.

%% @doc Postprocess the request after it has been fulfilled.
postprocess(State, RawResponse, NodeMsg) ->
    PricingDevice = hb_converge:get(<<"pricing_device">>, State, false, NodeMsg),
    LedgerDevice = hb_converge:get(<<"ledger_device">>, State, false, NodeMsg),
    Response =
        hb_converge:get(
            <<"body">>,
            RawResponse,
            NodeMsg#{ hashpath => ignore }
        ),
    Request = hb_converge:get(<<"request">>, RawResponse, NodeMsg),
    ?event(payment, {post_processing_with_devices, PricingDevice, LedgerDevice}),
    case (PricingDevice =/= false) and (LedgerDevice =/= false) of
        false -> {ok, Response};
        true ->
            PricingMsg = #{ <<"device">> => PricingDevice },
            LedgerMsg = #{ <<"device">> => LedgerDevice },
            PricingReq = #{
                <<"path">> => <<"price">>,
                <<"type">> => <<"post">>,
                <<"request">> => Request,
                <<"body">> => Response
            },
            ?event({post_pricing_request, PricingReq}),
            PricingRes =
                case hb_converge:resolve(PricingMsg, PricingReq, NodeMsg) of
                    {error, _Error} ->
                        % The pricing device is unable to give us a cost for
                        % the request, so we try to estimate it instead.
                        EstimateReq = PricingReq#{ <<"path">> => <<"estimate">> },
                        hb_converge:resolve(PricingMsg, EstimateReq, NodeMsg);
                    {ok, P} -> {ok, P}
                end,
            ?event(payment, {p4_post_pricing_response, PricingRes}),
            case PricingRes of
                {ok, Price} ->
                    % We have successfully estimated the cost of the request,
                    % so we proceed to debit the user's account.
                    LedgerReq =
                        #{
                            <<"path">> => <<"debit">>,
                            <<"type">> => <<"post">>,
                            <<"amount">> => Price,
                            <<"request">> => Request
                        },
                    ?event({p4_ledger_request, LedgerReq}),
                    {ok, Resp} = 
                        hb_converge:resolve(
                            LedgerMsg,
                            LedgerReq,
                            NodeMsg
                        ),
                    ?event(payment, {p4_post_ledger_response, Resp}),
                    % Return the original request.
                    {ok, Response};
                {error, PricingError} ->
                    % The pricing device is unable to process the request,
                    % so we don't proceed.
                    {error, PricingError}
            end
    end.

%% @doc Get the balance of a user in the ledger.
balance(_, Req, NodeMsg) ->
    LedgerDevice =
        hb_converge:get(
            <<"preprocessor/ledger_device">>,
            NodeMsg,
            false,
            NodeMsg
        ),
    LedgerMsg = #{ <<"device">> => LedgerDevice },
    LedgerReq = #{
        <<"path">> => <<"balance">>,
        <<"request">> => Req
    },
    ?event({ledger_message, {ledger_msg, LedgerMsg}}),
    case hb_converge:resolve(LedgerMsg, LedgerReq, NodeMsg) of
        {ok, Balance} ->
            {ok, Balance};
        {error, Error} ->
            {error, Error}
    end.

%% @doc The node operator may elect to make certain routes non-chargable, using 
%% the `routes' syntax also used to declare routes in `router@1.0'.
is_chargable_req(Req, NodeMsg) ->
    NonChargableRoutes =
        hb_opts:get(
            p4_non_chargable_routes,
            ?DEFAULT_NON_CHARGABLE_ROUTES,
            NodeMsg
        ),
    Matches = dev_router:match_routes(Req, NonChargableRoutes, NodeMsg),
    ?event(
        {
            is_chargable,
            {non_chargable_routes, NonChargableRoutes},
            {req, Req},
            {matches, Matches}
        }
    ),
    case Matches of
        no_matches -> true;
        _ -> false
    end.

%%% Tests

test_opts(Opts) ->
    test_opts(Opts, <<"faff@1.0">>).
test_opts(Opts, PricingDev) ->
    test_opts(Opts, PricingDev, <<"faff@1.0">>).
test_opts(Opts, PricingDev, LedgerDev) ->
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"pricing_device">> => PricingDev,
            <<"ledger_device">> => LedgerDev
        },
    Opts#{
        preprocessor => ProcessorMsg,
        postprocessor => ProcessorMsg
    }.

%% @doc Simple test of p4's capabilities with the `faff@1.0' device.
faff_test() ->
    GoodWallet = ar_wallet:new(),
    BadWallet = ar_wallet:new(),
    Node = hb_http_server:start_node(
       test_opts(
            #{
                faff_allow_list =>
                    [hb_util:human_id(ar_wallet:to_address(GoodWallet))]
            }
        )
    ),
    Req = #{
        <<"path">> => <<"/greeting">>,
        <<"greeting">> => <<"Hello, world!">>
    },
    GoodSignedReq = hb_message:attest(Req, GoodWallet),
    ?event({req, GoodSignedReq}),
    BadSignedReq = hb_message:attest(Req, BadWallet),
    ?event({req, BadSignedReq}),
    {ok, Res} = hb_http:get(Node, GoodSignedReq, #{}),
    ?event(payment, {res, Res}),
    ?assertEqual(<<"Hello, world!">>, hb_converge:get(<<"body">>, Res, #{})),
    ?assertMatch({error, _}, hb_http:get(Node, BadSignedReq, #{})).

%% @doc Test that a non-chargable route is not charged for.
non_chargable_route_test() ->
    Wallet = ar_wallet:new(),
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger_device">> => <<"simple-pay@1.0">>,
            <<"pricing_device">> => <<"simple-pay@1.0">>
        },
    Node = hb_http_server:start_node(
        #{
            p4_non_chargable_routes =>
                [
                    #{ <<"template">> => <<"/~p4@1.0/balance">> },
                    #{ <<"template">> => <<"/~meta@1.0/*">> }
                ],
            preprocessor => Processor,
            postprocessor => Processor,
            operator => hb:address()
        }
    ),
    Req = #{
        <<"path">> => <<"/~p4@1.0/balance">>
    },
    GoodSignedReq = hb_message:attest(Req, Wallet),
    Res = hb_http:get(Node, GoodSignedReq, #{}),
    ?event({res1, Res}),
    ?assertMatch({ok, #{ <<"body">> := 0 }}, Res),
    Req2 = #{ <<"path">> => <<"/~meta@1.0/info">> },
    GoodSignedReq2 = hb_message:attest(Req2, Wallet),
    Res2 = hb_http:get(Node, GoodSignedReq2, #{}),
    ?event({res2, Res2}),
    ?assertMatch({ok, #{ <<"operator">> := _ }}, Res2),
    Req3 = #{ <<"path">> => <<"/~scheduler@1.0">> },
    BadSignedReq3 = hb_message:attest(Req3, Wallet),
    Res3 = hb_http:get(Node, BadSignedReq3, #{}),
    ?event({res3, Res3}),
    ?assertMatch({error, _}, Res3).
%%% @doc The HyperBEAM core payment ledger. This module allows the operator to
%%% specify another device that can act as a pricing mechanism for transactions
%%% on the node, as well as orchestrating a payment ledger to calculate whether
%%% the node should fulfil services for users.
%%%
%%% The device requires the following node message settings in order to function:
%%%
%%% - `p4_pricing_device`: The device that will estimate the cost of a request.
%%% - `p4_ledger_device`: The device that will act as a payment ledger.
%%%
%%% The pricing device should implement the following keys:
%%% ```
%%%             GET /estimate?type=request|response&body=[...]&request=RequestMessage
%%%             GET /price?type=request|response&body=[...]&request=RequestMessage
%%% ```
%%% 
%%% The `body` key is used to pass either the request or response messages to the
%%% device. The `type` key is used to specify whether the inquiry is for a request
%%% or a response object -- requests carry lists of messages that will be executed,
%%% while responses carry the results of the execution. The `price` key may return
%%% `infinity` if the node will not serve a user under any circumstances. Else,
%%% the value returned by the `price` key will be passed to the ledger device as
%%% the `amount` key.
%%%
%%% The ledger device should implement the following keys:
%%% ```
%%%             POST /credit?message=PaymentMessage&request=RequestMessage
%%%             POST /debit?quantity=PriceMessage&simulate=true|false
%%%                         &request=RequestMessage
%%% ```
%%%
%%% The `simulate` key is optional and defaults to `false`. If `simulate` is set to
%%% `true`, the debit will not be applied to the ledger, but it should return
%%% whether the debit would have succeeded.
-module(dev_p4).
-export([preprocess/3, postprocess/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Estimate the cost of a transaction and decide whether to proceed with
%% a request. The default behavior if `pricing_device` or `p4_balances` are
%% not set is to proceed, so it is important that a user initialize them.
preprocess(State, Raw, NodeMsg) ->
    PricingDevice = hb_converge:get(<<"pricing_device">>, State, false, NodeMsg),
    LedgerDevice = hb_converge:get(<<"ledger_device">>, State, false, NodeMsg),
    Messages = hb_converge:get(<<"body">>, Raw, NodeMsg#{ hashpath => ignore }),
    Request = hb_converge:get(<<"request">>, Raw, NodeMsg),
    ?event(payment, {p4_processing_with_device, PricingDevice, LedgerDevice}),
    case (PricingDevice =/= false) and (LedgerDevice =/= false) of
        false -> {ok, Messages};
        true ->
            PricingMsg = #{ <<"device">> => PricingDevice },
            LedgerMsg = #{ <<"device">> => LedgerDevice },
            PricingReq = #{
                <<"path">> => <<"estimate">>,
                <<"type">> => <<"request">>,
                <<"request">> => Request,
                <<"body">> => Messages
            },
            ?event(payment,
                {p4_pricing_request, {devmsg, PricingMsg}, {req, PricingReq}}),
            case hb_converge:resolve(PricingMsg, PricingReq, NodeMsg) of
                {error, Error} ->
                    % The device is unable to estimate the cost of the request,
                    % so we don't proceed.
                    {error, {error_calculating_price, Error}};
                {ok, <<"infinity">>} ->
                    % The device states that under no circumstances should we
                    % proceed with the request.
                    ?event(payment, {p4_pricing_response, {error, <<"infinity">>}}),
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
                            <<"simulate">> => true,
                            <<"request">> => Request
                        },
                    ?event(payment, {p4_ledger_request, LedgerReq}),
                    case hb_converge:resolve(LedgerMsg, LedgerReq, NodeMsg) of
                        {ok, true} ->
                            % The ledger device has confirmed that the user has
                            % enough funds for the request, so we proceed.
                            {ok, Messages};
                        {ok, false} ->
                            ?event(payment, {p4_ledger_response, {error, false}}),
                            {error, 
                                #{
                                    <<"status">> => 429,
                                    <<"reason">> => <<"Insufficient funds">>,
                                    <<"price">> => Price
                                }
                            };
                        {error, Error} ->
                            % The ledger device is unable to process the request,
                            % so we don't proceed.
                            ?event(payment, {p4_ledger_response, {error, Error}}),
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
    Request = hb_converge:get(<<"request">>, Response, NodeMsg),
    ?event(payment, {p4_postprocessing_with_device, PricingDevice, LedgerDevice}),
    case (PricingDevice =/= false) and (LedgerDevice =/= false) of
        false -> {ok, Response};
        true ->
            PricingMsg = #{ <<"device">> => PricingDevice },
            LedgerMsg = #{ <<"device">> => LedgerDevice },
            PricingReq = #{
                <<"path">> => <<"price">>,
                <<"type">> => <<"response">>,
                <<"request">> => Request,
                <<"body">> => Response
            },
            ?event(payment, {p4_pricing_request, PricingReq}),
            PricingRes =
                case hb_converge:resolve(PricingMsg, PricingReq, NodeMsg) of
                    {error, _Error} ->
                        % The pricing device is unable to give us a cost for
                        % the request, so we try to estimate it instead.
                        EstimateReq = PricingReq#{ <<"path">> => <<"estimate">> },
                        hb_converge:resolve(PricingMsg, EstimateReq, NodeMsg);
                    {ok, P} -> {ok, P}
                end,
            ?event(payment, {p4_pricing_response, PricingRes}),
            case PricingRes of
                {ok, Price} ->
                    % We have successfully estimated the cost of the request,
                    % so we proceed to debit the user's account.
                    LedgerReq =
                        #{
                            <<"path">> => <<"debit">>,
                            <<"amount">> => Price,
                            <<"simulate">> => false,
                            <<"request">> => Request
                        },
                    ?event(payment, {p4_ledger_request, LedgerReq}),
                    {ok, _} = 
                        hb_converge:resolve(
                            LedgerMsg,
                            LedgerReq,
                            NodeMsg
                        ),
                    % Return the original request.
                    {ok, Response};
                {error, PricingError} ->
                    % The pricing device is unable to process the request,
                    % so we don't proceed.
                    {error, PricingError}
            end
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

faff_test() ->
    GoodWallet = ar_wallet:new(),
    BadWallet = ar_wallet:new(),
    Node = hb_http_server:start_test_node(
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
    GoodSignedReq = hb_message:sign(Req, GoodWallet),
    ?event({req, GoodSignedReq}),
    BadSignedReq = hb_message:sign(Req, BadWallet),
    ?event({req, BadSignedReq}),
    {ok, Res} = hb_http:get(Node, GoodSignedReq, #{}),
    ?event(payment, {res, Res}),
    ?assertEqual(<<"Hello, world!">>, hb_converge:get(<<"body">>, Res, #{})),
    ?assertMatch({error, _}, hb_http:get(Node, BadSignedReq, #{})).
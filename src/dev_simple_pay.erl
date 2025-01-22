%%% @doc A simple device that allows the operator to specify a price for a
%%% request and then charge the user for it, on a per message basis.
%%% The device's ledger is stored in the node message at `simple_pay_ledger`,
%%% and can be topped-up by either the operator, or an external device. The 
%%% price is specified in the node message at `simple_pay_price`.
%%% This device acts as both a pricing device and a ledger device, by p4's
%%% definition.
-module(dev_simple_pay).
-export([estimate/3, debit/3, balance/3, topup/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Estimate the cost of a request by counting the number of messages in
%% the request, then multiplying by the per-message price.
estimate(_, Req, NodeMsg) ->
    case hb_converge:get(<<"type">>, Req, undefined, NodeMsg) of
        <<"post">> -> {ok, 0};
        <<"pre">> ->
            Messages = hb_converge:get(<<"body">>, Req, NodeMsg#{ hashpath => ignore }),
            {ok, length(Messages) * hb_opts:get(simple_pay_price, 1, NodeMsg)}
    end.

%% @doc Preprocess a request by checking the ledger and charging the user. We 
%% can charge the user at this stage because we know statically what the price
%% will be
debit(_, RawReq, NodeMsg) ->
    case hb_converge:get(<<"type">>, RawReq, undefined, NodeMsg) of
        <<"post">> -> {ok, true};
        <<"pre">> ->
            ?event(payment, {debit_preprocessing}),
            Req = hb_converge:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }),
            Signer = hb_converge:get(<<"signers/1">>, Req, undefined, NodeMsg),
            UserBalance = get_balance(Signer, NodeMsg),
            Price = hb_converge:get(<<"amount">>, RawReq, 0, NodeMsg),
            ?event(payment,
                {debit,
                    {user, Signer},
                    {balance, UserBalance},
                    {price, Price}
                }),
            case UserBalance >= Price of
                true ->
                    set_balance(Signer, UserBalance - Price, NodeMsg),
                    {ok, true};
                false -> {ok, false}
            end
    end.

%% @doc Get the balance of a user in the ledger.
balance(_, Req, NodeMsg) ->
    Signer = hb_converge:get(<<"signers/1">>, Req, NodeMsg),
    {ok, get_balance(Signer, NodeMsg)}.

%% @doc Adjust a user's balance, normalizing their wallet ID first.
set_balance(Signer, Amount, NodeMsg) ->
    NormSigner = hb_util:human_id(Signer),
    Ledger = hb_opts:get(simple_pay_ledger, #{}, NodeMsg),
    ?event(payment,
        {modifying_balance,
            {user, NormSigner},
            {amount, Amount},
            {ledger_before, Ledger}
        }
    ),
    hb_http_server:set_opts(
        NewMsg = NodeMsg#{
            simple_pay_ledger =>
                hb_converge:set(
                    Ledger,
                    NormSigner,
                    Amount,
                    NodeMsg
                )
        }
    ),
    {ok, NewMsg}.

%% @doc Get the balance of a user in the ledger.
get_balance(Signer, NodeMsg) ->
    NormSigner = hb_util:human_id(Signer),
    Ledger = hb_opts:get(simple_pay_ledger, #{}, NodeMsg),
    hb_converge:get(NormSigner, Ledger, 0, NodeMsg).

%% @doc Top up the user's balance in the ledger.
topup(_, Req, NodeMsg) ->
    SignerRaw = hb_converge:get(<<"signers/1">>, Req, NodeMsg),
    Signer = hb_util:human_id(SignerRaw),
    case hb_opts:get(operator, undefined, NodeMsg) of
        Unauth when Unauth =/= Signer -> {error, <<"Unauthorized">>};
        Operator ->
            Amount = hb_converge:get(<<"amount">>, Req, 0, NodeMsg),
            Recipient = hb_converge:get(<<"recipient">>, Req, Operator, NodeMsg),
            CurrentBalance = get_balance(Recipient, NodeMsg),
            ?event(payment,
                {topup,
                    {amount, Amount},
                    {recipient, Recipient},
                    {balance, CurrentBalance},
                    {expected_new_balance, CurrentBalance + Amount}
                }),
            {ok, NewNodeMsg} =
                set_balance(
                    Recipient,
                    CurrentBalance + Amount,
                    NodeMsg
                ),
            % Briefly wait for the ledger to be updated.
            receive after 100 -> ok end,
            {ok, get_balance(Recipient, NewNodeMsg)}
    end.

%%% Tests

test_opts() ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger_device">> => <<"simple-pay@1.0">>,
            <<"pricing_device">> => <<"simple-pay@1.0">>
        },
    {
        Address,
        Wallet,
        #{
            simple_pay_ledger => #{Address => 100},
            simple_pay_price => 10,
            operator => Address,
            preprocessor => ProcessorMsg,
            postprocessor => ProcessorMsg
        }
    }.

get_balance_and_top_up_test() ->
    {Address, Wallet, Opts} = test_opts(),
    Node = hb_http_server:start_test_node(Opts),
    {ok, Res} =
        hb_http:get(
            Node,
            hb_message:sign(
                #{<<"path">> => <<"/!simple-pay@1.0/balance">>},
                Wallet
            ),
            #{}
        ),
    ?assertEqual(70, hb_converge:get(<<"body">>, Res, #{})),
    {ok, NewBalance} =
        hb_http:post(
            Node,
            hb_message:sign(
                #{
                    <<"path">> => <<"/!simple-pay@1.0/topup">>,
                    <<"amount">> => 100,
                    <<"recipient">> => Address
                },
                Wallet
            ),
            #{}
        ),
    ?assertEqual(140, hb_converge:get(<<"body">>, NewBalance, #{})),
    {ok, Res2} =
        hb_http:get(
            Node,
            hb_message:sign(
                #{<<"path">> => <<"/!simple-pay@1.0/balance">>},
                Wallet
            ),
            #{}
        ),
    ?assertEqual(110, hb_converge:get(<<"body">>, Res2, #{})).

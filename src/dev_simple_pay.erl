%%% @doc A simple device that allows the operator to specify a price for a
%%% request and then charge the user for it, on a per message basis.
%%% The device's ledger is stored in the node message at `simple_pay_ledger`,
%%% and can be topped-up by either the operator, or an external device. The 
%%% price is specified in the node message at `simple_pay_price`.
%%% This device acts as both a pricing device and a ledger device, by p4's
%%% definition.
-module(dev_simple_pay).
-export([estimate/3, debit/3, balance/3, top_up/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Estimate the cost of a request by counting the number of messages in
%% the request, then multiplying by the per-message price.
estimate(_, Req, NodeMsg) ->
    Messages = hb_converge:get(<<"body">>, Req, NodeMsg#{ hashpath => ignore }),
    {ok, length(Messages) * hb_opts:get(simple_pay_price, 1, NodeMsg)}.

%% @doc Preprocess a request by checking the ledger and charging the user.
debit(_, RawReq, NodeMsg) ->
    Req = hb_converge:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }),
    Signer = hb_converge:get(<<"signers/1">>, Req, undefined, NodeMsg),
    UserBalance = get_balance(Signer, NodeMsg),
    Simulation = hb_converge:get(<<"simulate">>, Req, false, NodeMsg),
    Price = hb_opts:get(simple_pay_price, 1, NodeMsg),
    ?event(payment,
        {debit,
            {signer, Signer},
            {balance, UserBalance},
            {price, Price},
            {simulation, Simulation}
        }),
    case {Simulation, (UserBalance >= Price)} of
        {true, Admissible} -> {ok, Admissible};
        {false, false} -> {error, <<"Insufficient funds">>};
        {false, true} ->
            set_balance(Signer, UserBalance - Price, NodeMsg),
            {ok, true}
    end.

%% @doc Get the balance of a user in the ledger.
balance(_, Req, NodeMsg) ->
    Signer = hb_converge:get(<<"signers/1">>, Req, NodeMsg),
    {ok, get_balance(Signer, NodeMsg)}.

set_balance(Signer, Amount, NodeMsg) ->
    NormSigner = hb_util:human_id(Signer),
    Ledger = hb_converge:get(<<"simple_pay_ledger">>, NodeMsg, #{}, NodeMsg),
    ?event(payment,
        {modifying_balance,
            {user, NormSigner},
            {amount, Amount},
            {ledger, Ledger}
        }
    ),
    hb_http_server:set_opts(
        NodeMsg#{
            simple_pay_ledger =>
                hb_converge:set(
                    Ledger,
                    NormSigner,
                    Amount,
                    NodeMsg
                )
        }
    ).

%% @doc Get the balance of a user in the ledger.
get_balance(Signer, NodeMsg) ->
    NormSigner = hb_util:human_id(Signer),
    Ledger = hb_opts:get(simple_pay_ledger, #{}, NodeMsg),
    hb_converge:get(NormSigner, Ledger, 0, NodeMsg).

%% @doc Top up the user's balance in the ledger.
top_up(_, Req, NodeMsg) ->
    SignerRaw = hb_converge:get(<<"signers/1">>, Req, NodeMsg),
    Signer = hb_util:human_id(SignerRaw),
    case hb_opts:get(operator, undefined, NodeMsg) of
        Unauth when Unauth =/= Signer -> {error, <<"Unauthorized">>};
        Operator ->
            Amount = hb_converge:get(<<"amount">>, Req, 0, NodeMsg),
            Recipient = hb_converge:get(<<"recipient">>, Req, Operator, NodeMsg),
            set_balance(Recipient, Amount, NodeMsg),
            {ok, get_balance(Recipient, NodeMsg)}
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
    ?assertEqual(100, hb_converge:get(<<"body">>, Res, #{})),
    {ok, _} =
        hb_http:post(
            Node,
            hb_message:sign(
                #{
                    <<"path">> => <<"/!simple-pay@1.0/top-up">>,
                    <<"amount">> => 100,
                    <<"recipient">> => Address
                },
                Wallet
            ),
            #{}
        ),
    {ok, Res2} =
        hb_http:get(
            Node,
            hb_message:sign(
                #{<<"path">> => <<"/!simple-pay@1.0/balance">>},
                Wallet
            ),
            #{}
        ),
    ?assertEqual(170, hb_converge:get(<<"body">>, Res2, #{})).

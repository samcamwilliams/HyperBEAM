%%% @doc This module contains end-to-end tests for Hyperbeam, accessing through
%%% the HTTP interface. As well as testing the system, you can use these tests
%%% as examples of how to interact with HyperBEAM nodes.
-module(hb_examples).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% @doc Start a node running the simple pay meta device, and use it to relay
%% a message for a client. We must ensure:
%% 1. When the client has no balance, the relay fails.
%% 2. The operator is able to topup for the client.
%% 3. The client has the correct balance after the topup.
%% 4. The relay succeeds when the client has enough balance.
%% 5. The received message is signed by the host using http-sig and validates
%%    correctly.
relay_with_payments_test() ->
    HostWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    % Start a node with the simple-pay device enabled.
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger_device">> => <<"simple-pay@1.0">>,
            <<"pricing_device">> => <<"simple-pay@1.0">>
        },
    HostNode =
        hb_http_server:start_node(
            #{
                operator => ar_wallet:to_address(HostWallet),
                preprocessor => ProcessorMsg,
                postprocessor => ProcessorMsg
            }
        ),
    % Create a message for the client to relay.
    ClientMessage1 =
        hb_message:attest(
            #{<<"path">> => <<"/~relay@1.0/call?relay-path=https://www.google.com">>},
            ClientWallet
        ),
    % Relay the message.
    Res = hb_http:get(HostNode, ClientMessage1, #{}),
    ?assertEqual({error, <<"Insufficient funds">>}, Res),
    % Topup the client's balance.
    % Note: The fields must be in the headers, for now.
    TopupMessage =
        hb_message:attest(
            #{
                <<"path">> => <<"/~simple-pay@1.0/topup">>,
                <<"recipient">> => ClientAddress,
                <<"amount">> => 100
            },
            HostWallet
        ),
    ?assertMatch({ok, _}, hb_http:get(HostNode, TopupMessage, #{})),
    % Relay the message again.
    Res2 = hb_http:get(HostNode, ClientMessage1, #{}),
    ?assertMatch({ok, #{ <<"body">> := Bin }} when byte_size(Bin) > 10_000, Res2),
    {ok, Resp} = Res2,
    ?assert(length(hb_message:signers(Resp)) > 0),
    ?assert(hb_message:verify(Resp)).

%% @doc Gain signed WASM responses from a node and verify them.
%% 1. Start the client with a small balance.
%% 2. Execute a simple WASM function on the host node.
%% 3. Verify the response is correct and signed by the host node.
%% 4. Get the balance of the client and verify it has been deducted.
paid_wasm_test() ->
    HostWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger_device">> => <<"simple-pay@1.0">>,
            <<"pricing_device">> => <<"simple-pay@1.0">>
        },
    HostNode =
        hb_http_server:start_node(
            #{
                simple_pay_ledger => #{ ClientAddress => 100 },
                simple_pay_price => 10,
                operator => ar_wallet:to_address(HostWallet),
                preprocessor => ProcessorMsg,
                postprocessor => ProcessorMsg
            }
        ),
    % Read the WASM file from disk, post it to the host and execute it.
    {ok, WASMFile} = file:read_file(<<"test/test-64.wasm">>),
    ClientMessage1 =
        hb_message:attest(
            #{
                <<"path">> =>
                    <<"/~wasm-64@1.0/init/compute/results?wasm-function=fac">>,
                <<"body">> => WASMFile,
                <<"wasm-params+list">> => <<"3.0">>
            },
            ClientWallet
        ),
    {ok, Res} = hb_http:post(HostNode, ClientMessage1, #{}),
    % Check that the message is signed by the host node.
    ?assert(length(hb_message:signers(Res)) > 0),
    ?assert(hb_message:verify(Res)),
    % Now we have the results, we can verify them.
    ?assertMatch(6.0, hb_converge:get(<<"output/1">>, Res, #{})),
    % Check that the client's balance has been deducted.
    ClientMessage2 =
        hb_message:attest(
            #{<<"path">> => <<"/~simple-pay@1.0/balance">>},
            ClientWallet
        ),
    {ok, Res2} = hb_http:get(HostNode, ClientMessage2, #{}),
    ?assertMatch(20, hb_converge:get(<<"body">>, Res2, #{})).
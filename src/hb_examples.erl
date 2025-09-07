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
relay_with_payments_test_() ->
    {timeout, 30, fun relay_with_payments_test/0}.
relay_with_payments_test() ->
    HostWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    % Start a node with the simple-pay device enabled.
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    HostNode =
        hb_http_server:start_node(
            #{
                operator => ar_wallet:to_address(HostWallet),
                on => #{
                    <<"request">> => ProcessorMsg,
                    <<"response">> => ProcessorMsg
                }
            }
        ),
    % Create a message for the client to relay.
    ClientMessage1 =
        hb_message:commit(
            #{<<"path">> => <<"/~relay@1.0/call?relay-path=https://www.google.com">>},
            ClientWallet
        ),
    % Relay the message.
    Res = hb_http:get(HostNode, ClientMessage1, #{}),
    ?assertMatch({error, #{ <<"body">> := <<"Insufficient funds">> }}, Res),
    % Topup the client's balance.
    % Note: The fields must be in the headers, for now.
    TopupMessage =
        hb_message:commit(
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
    ?assert(length(hb_message:signers(Resp, #{})) > 0),
    ?assert(hb_message:verify(Resp, all, #{})).

%% @doc Gain signed WASM responses from a node and verify them.
%% 1. Start the client with a small balance.
%% 2. Execute a simple WASM function on the host node.
%% 3. Verify the response is correct and signed by the host node.
%% 4. Get the balance of the client and verify it has been deducted.
paid_wasm_test_() ->
    {timeout, 30, fun paid_wasm/0}.
paid_wasm() ->
    HostWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    HostNode =
        hb_http_server:start_node(
            Opts = #{
				store => [
					#{
						<<"store-module">> => hb_store_fs,
						<<"name">> => <<"cache-TEST">>
					}
				],
                simple_pay_ledger => #{ ClientAddress => 100 },
                simple_pay_price => 10,
                operator => ar_wallet:to_address(HostWallet),
                on => #{
                    <<"request">> => ProcessorMsg,
                    <<"response">> => ProcessorMsg
                }
            }
        ),
    % Read the WASM file from disk, post it to the host and execute it.
    {ok, WASMFile} = file:read_file(<<"test/test-64.wasm">>),
    ClientMessage1 =
        hb_message:commit(
            #{
                <<"path">> =>
                    <<"/~wasm-64@1.0/init/compute/results?function=fac">>,
                <<"body">> => WASMFile,
                <<"parameters+list">> => <<"3.0">>
            },
            Opts#{ priv_wallet => ClientWallet }
        ),
    {ok, Res} = hb_http:post(HostNode, ClientMessage1, Opts),
    % Check that the message is signed by the host node.
    ?assert(length(hb_message:signers(Res, Opts)) > 0),
    ?assert(hb_message:verify(Res, all, Opts)),
    % Now we have the results, we can verify them.
    ?assertMatch(6.0, hb_ao:get(<<"output/1">>, Res, Opts)),
    % Check that the client's balance has been deducted.
    ClientMessage2 =
        hb_message:commit(
            #{<<"path">> => <<"/~p4@1.0/balance">>},
            ClientWallet
        ),
    {ok, Res2} = hb_http:get(HostNode, ClientMessage2, Opts),
    ?assertMatch(60, Res2).

create_schedule_aos2_test_disabled() ->
    % The legacy process format, according to the ao.tn.1 spec:
    % Data-Protocol	The name of the Data-Protocol for this data-item	1-1	ao
    % Variant	The network version that this data-item is for	1-1	ao.TN.1
    % Type	Indicates the shape of this Data-Protocol data-item	1-1	Process
    % Module	Links the process to ao module using the module's unique
    %   Transaction ID (TXID).	1-1	{TXID}
    % Scheduler	Specifies the scheduler unit by Wallet Address or Name, and can
    %   be referenced by a recent Scheduler-Location.	1-1	{ADDRESS}
    % Cron-Interval	An interval at which a particular Cron Message is recevied by the process,
    %   in the format X-Y, where X is a scalar value, and Y is milliseconds,
    %   seconds, minutes, hours, days, months, years, or blocks	0-n	1-second
    % Cron-Tag-{Name}	defines tags for Cron Messages at set intervals,
    %   specifying relevant metadata.	0-1	
    % Memory-Limit	Overrides maximum memory, in megabytes or gigabytes, set by 
    %   Module, can not exceed modules setting	0-1	16-mb
    % Compute-Limit	Caps the compute cycles for a module per evaluation, ensuring
    %   efficient, controlled execution	0-1	1000
    % Pushed-For	Message TXID that this Process is pushed as a result	0-1	{TXID}
    % Cast	Sets message handling: 'True' for do not push, 'False' for normal
    %   pushing	0-1	{True or False}
    % Authority	Defines a trusted wallet address which can send Messages to
    %   the Process	0-1	{ADDRESS}
    % On-Boot	Defines a startup script to run when the process is spawned. If
    %   value "Data" it uses the Data field of the Process Data Item. If it is a
    %   TXID it will load that TX from Arweave and execute it.	0-1	{Data or TXID}
    % {Any-Tags}	Custom Tags specific for the initial input of the Process	0-n
    Node =
        try hb_http_server:start_node(#{ priv_wallet => hb:wallet() })
        catch
            _:_ ->
                <<"http://localhost:8734">>
        end,
    ProcMsg = #{
        <<"data-protocol">> => <<"ao">>,
        <<"type">> => <<"Process">>,
        <<"variant">> => <<"ao.TN.1">>,
        <<"type">> => <<"Process">>,
        <<"module">> => <<"bkjb55i07GUCUSWROtKK4HU1mBS_X0TyH3M5jMV6aPg">>,
        <<"scheduler">> => hb_util:human_id(hb:address()),
        <<"memory-limit">> => <<"1024-mb">>,
        <<"compute-limit">> => <<"10000000">>,
        <<"authority">> => hb_util:human_id(hb:address()),
        <<"scheduler-location">> => hb_util:human_id(hb:address())
    },
    Wallet = hb:wallet(),
    SignedProc = hb_message:commit(ProcMsg, Wallet),
    IDNone = hb_message:id(SignedProc, none),
    IDAll = hb_message:id(SignedProc, all),
    {ok, Res} = schedule(SignedProc, IDNone, Wallet, Node),
    ?event({res, Res}),
    receive after 100 -> ok end,
    ?event({id, IDNone, IDAll}),
    {ok, Res2} = hb_http:get(
        Node,
        <<"/~scheduler@1.0/slot?target=", IDNone/binary>>,
        #{}
    ),
    ?assertMatch(Slot when Slot >= 0, hb_ao:get(<<"at-slot">>, Res2, #{})).

schedule(ProcMsg, Target) ->
    schedule(ProcMsg, Target, hb:wallet()).
schedule(ProcMsg, Target, Wallet) ->
    schedule(ProcMsg, Target, Wallet, <<"http://localhost:8734">>).
schedule(ProcMsg, Target, Wallet, Node) ->
    SignedReq = 
        hb_message:commit(
            #{
                <<"path">> => <<"/~scheduler@1.0/schedule">>,
                <<"target">> => Target,
                <<"body">> => ProcMsg
            },
            Wallet
        ),
    ?event({signed_req, SignedReq}),
    hb_http:post(Node, SignedReq, #{}).


%% @doc Test that we can schedule an ANS-104 data item on a relayed node. The
%% input to the relaying server comes in the form of a serialized ANS-104
%% data item, which should then be correctly deserialized and sent to the
%% scheduler node.
relay_schedule_ans104_test() ->
    SchedulerWallet = ar_wallet:new(),
    ComputeWallet = ar_wallet:new(),
    RelayWallet = ar_wallet:new(),
    ?event(debug_test,
        {wallets,
            {scheduler, hb_util:human_id(SchedulerWallet)},
            {compute, hb_util:human_id(ComputeWallet)},
            {relay, hb_util:human_id(RelayWallet)}
        }
    ),
    Scheduler =
        hb_http_server:start_node(
            #{
                on => #{
                    <<"start">> => #{
                        <<"device">> => <<"scheduler@1.0">>,
                        <<"path">> => <<"location">>,
                        <<"method">> => <<"POST">>,
                        <<"target">> => <<"self">>,
                        <<"require-codec">> => <<"ans104@1.0">>,
                        <<"hook">> => #{
                            <<"result">> => <<"ignore">>,
                            <<"commit-request">> => true
                        }
                    }
                },
                store => [hb_test_utils:test_store()],
                priv_wallet => SchedulerWallet
            }
        ),
    ?event(debug_test, {scheduler, Scheduler}),
    Compute =
        hb_http_server:start_node(
            #{
                priv_wallet => ComputeWallet,
                store =>
                    [
                        ComputeStore = hb_test_utils:test_store(),
                        #{
                            <<"store-module">> => hb_store_remote_node,
                            <<"name">> => <<"cache-TEST/remote-node">>,
                            <<"node">> => Scheduler
                        }
                    ]
            }
        ),
    % Get the scheduler location of the scheduling node and write it to the
    % compute node's store.
    {ok, SchedulerLocation} =
        hb_http:get(
            Scheduler,
            <<"/~scheduler@1.0/location">>,
            #{}
        ),
    ?event({scheduler_location, SchedulerLocation}),
    dev_scheduler_cache:write_location(
        hb_maps:get(<<"body">>, SchedulerLocation, <<"NO BODY">>, #{}),
        #{ store => [ComputeStore] }
    ),
    % Create the relaying server.
    Relay =
        hb_http_server:start_node(#{
            priv_wallet => RelayWallet,
            relay_allow_commit_request => true,
            store => [hb_test_utils:test_store()],
            routes =>
                [
                    #{
                        <<"template">> => <<"^/push">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> => [
                            #{
                                <<"wallet">> => hb_util:human_id(SchedulerWallet),
                                <<"prefix">> => Scheduler
                            }
                        ]
                    },
                    #{
                        <<"template">> => <<"^/.*">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> => [
                            #{
                                <<"wallet">> => hb_util:human_id(ComputeWallet),
                                <<"prefix">> => Compute
                            }
                        ]
                    }
                ],
            on => #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"router@1.0">>,
                        <<"path">> => <<"preprocess">>,
                        <<"commit-request">> => true
                    }
            }
        }),
    ?event(debug_test,
        {nodes,
            {scheduler, {url, Scheduler}, {wallet, hb_util:human_id(SchedulerWallet)}},
            {compute, {url, Compute}, {wallet, hb_util:human_id(ComputeWallet)}},
            {relay, {url, Relay}, {wallet, hb_util:human_id(RelayWallet)}}
        }
    ),
    ClientOpts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => ar_wallet:new()
        },
    % Create process to schedule, then send it to the relaying server as
    % a serialized ANS-104 data item.
    Process =
        hb_message:commit(
            #{
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"test-device@1.0">>,
                <<"push-device">> => <<"push@1.0">>,
                <<"scheduler">> => hb_util:human_id(SchedulerWallet),
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    % Push the initial message via the scheduler node.
    ScheduleRes =
        hb_http:post(
            Relay,
            Process#{
                <<"path">> => <<"push">>,
                <<"codec-device">> => <<"ans104@1.0">>
            },
            ClientOpts
        ),
    ?event(debug_test, {post_result, ScheduleRes}),
    ?assertMatch({ok, #{ <<"status">> := 200, <<"slot">> := 0 }}, ScheduleRes),
    % Push another message via the compute node.
    ProcID = hb_message:id(Process, all, ClientOpts),
    ToPush =
        hb_message:commit(
            #{
                <<"test-key">> => <<"value">>,
                <<"rand-key">> => hb_util:encode(crypto:strong_rand_bytes(32))
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    PushRes =
        hb_http:post(
            Relay,
            ToPush#{
                <<"path">> => <<ProcID/binary, "/push">>,
                <<"codec-device">> => <<"ans104@1.0">>
            },
            ClientOpts
        ),
    ?event(debug_test, {post_result, PushRes}),
    ?assertMatch({ok, #{ <<"status">> := 200, <<"slot">> := 1 }}, PushRes).
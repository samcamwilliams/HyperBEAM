%%% @doc This module contains the device implementation of AO processes
%%% in Converge. The core functionality of the module is in 'routing' requests
%%% for different functionality (scheduling, computing, and pushing messages)
%%% to the appropriate device. This is achieved by swapping out the device 
%%% of the process message with the necessary component in order to run the 
%%% execution, then swapping it back before returning. Computation is supported
%%% as a stack of devices, customizable by the user, while the scheduling
%%% device is (by default) a single device.
%%% 
%%% This allows the devices to share state as needed. Additionally, after each
%%% computation step the device caches the result at a path relative to the
%%% process definition itself, such that the process message's ID can act as an
%%% immutable reference to the process's growing list of interactions. See 
%%% `dev_process_cache' for details.
%%% 
%%% The external API of the device is as follows:
%%% ```
%%% GET /ID/Schedule:                Returns the messages in the schedule
%%% POST /ID/Schedule:               Adds a message to the schedule
%%% 
%%% GET /ID/Compute/[IDorSlotNum]:   Returns the state of the process after 
%%%                                  applying a message
%%% GET /ID/Now:                     Returns the `/Results' key of the latest 
%%%                                  computed message
%%% '''
%%% 
%%% An example process definition will look like this:
%%% ```
%%%     Device: Process/1.0
%%%     Scheduler-Device: Scheduler/1.0
%%%     Execution-Device: Stack/1.0
%%%     Execution-Stack: "Scheduler/1.0", "Cron/1.0", "WASM/1.0", "PoDA/1.0"
%%%     Cron-Frequency: 10-Minutes
%%%     WASM-Image: WASMImageID
%%%     PoDA:
%%%         Device: PoDA/1.0
%%%         Authority: A
%%%         Authority: B
%%%         Authority: C
%%%         Quorum: 2
%%% '''
%%%
%%% Runtime options:
%%%     Cache-Frequency: The number of assignments that will be computed 
%%%                      before the full (restorable) state should be cached.
%%%     Cache-Keys:      A list of the keys that should be cached for all 
%%%                      assignments, in addition to `/Results'.
-module(dev_process).
%%% Public API
-export([info/1, compute/3, schedule/3, slot/3, now/3, push/3, snapshot/3]).
-export([ensure_process_key/2]).
%%% Test helpers
-export([test_aos_process/0, dev_test_process/0, test_wasm_process/1]).
%%% Tests
-export([do_test_restore/0]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% The frequency at which the process state should be cached. Can be overridden
%% with the `cache_frequency` option.
-define(DEFAULT_CACHE_FREQ, 10).

%% @doc When the info key is called, we should return the process exports.
info(_Msg1) ->
    #{
        worker => fun dev_process_worker:server/3,
        grouper => fun dev_process_worker:group/3
    }.


%% @doc Wraps functions in the Scheduler device.
schedule(Msg1, Msg2, Opts) ->
    run_as(<<"scheduler">>, Msg1, Msg2, Opts).

slot(Msg1, Msg2, Opts) ->
    ?event({slot_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    run_as(<<"scheduler">>, Msg1, Msg2, Opts).

next(Msg1, _Msg2, Opts) ->
    run_as(<<"scheduler">>, Msg1, next, Opts).

snapshot(RawMsg1, _Msg2, Opts) ->
    Msg1 = ensure_process_key(RawMsg1, Opts),
    {ok, SnapshotMsg} = run_as(
        <<"Execution">>,
        Msg1,
        #{ <<"path">> => <<"snapshot">>, <<"mode">> => <<"Map">> },
        Opts#{ cache_control => [] }
    ),
    ProcID = hb_converge:get(<<"id">>, Msg1, Opts),
    Slot = hb_converge:get(<<"current-slot">>, Msg1, Opts),
    {ok,
        hb_private:set(
            hb_converge:set(
                SnapshotMsg,
                #{ <<"cache-control">> => [<<"store">>] },
                Opts
            ),
            #{ <<"priv/additional-hashpaths">> =>
                    [
                        hb_path:to_binary([ProcID, <<"Snapshot">>, Slot])
                    ]
            },
            Opts
        )
    }.


%% @doc Before computation begins, a boot phase is required. This phase
%% allows devices on the execution stack to initialize themselves. We set the
%% `Initialized' key to `True' to indicate that the process has been
%% initialized.
init(Msg1, _Msg2, Opts) ->
    ?event({init_called, {msg1, Msg1}, {opts, Opts}}),
    {ok, Initialized} =
        run_as(<<"execution">>, Msg1, #{ <<"path">> => init }, Opts),
    {
        ok,
        hb_converge:set(
            Initialized,
            #{
                <<"initialized">> => <<"true">>,
                <<"current-slot">> => -1
            },
            Opts
        )
    }.

%% @doc Compute the result of an assignment applied to the process state, if it 
%% is the next message.
compute(Msg1, Msg2, Opts) ->
    % If we do not have a live state, restore or initialize one.
    {ok, Loaded} =
        ensure_loaded(
            ensure_process_key(Msg1, Opts),
            Msg2,
            Opts
        ),
    {ok, Normalized} = 
        run_as(
            <<"execution">>,
            Loaded,
            normalize,
            Opts
        ),
    ProcID = hb_converge:get(<<"process/id">>, Loaded, Opts),
    ?event({compute_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    do_compute(
        ProcID,
        Normalized,
        Msg2,
        hb_converge:get(<<"slot">>, Msg2, Opts),
        Opts
    ).

%% @doc Continually get and apply the next assignment from the scheduler until
%% we reach the target slot that the user has requested.
do_compute(ProcID, Msg1, Msg2, TargetSlot, Opts) ->
    ?event({do_compute_called, {target_slot, TargetSlot}, {msg1, Msg1}}),
    case hb_converge:get(<<"current-slot">>, Msg1, Opts) of
        CurrentSlot when CurrentSlot > TargetSlot ->
            throw({error, {already_calculated_slot, TargetSlot}});
        CurrentSlot when CurrentSlot == TargetSlot ->
            % We reached the target height so we return.
            ?event({reached_target_slot_returning_state, TargetSlot}),
            {ok, as_process(Msg1, Opts)};
        CurrentSlot ->
            % Get the next input from the scheduler device.
            {ok, #{ <<"body">> := ToProcess, <<"state">> := State }} =
                next(Msg1, Msg2, Opts),
            ?event(process_compute,
                {
                    executing,
                    {msg1, Msg1},
                    {msg2, ToProcess}
                }
            ),
            {ok, Msg3} =
                run_as(
                    <<"execution">>,
                    State,
                    ToProcess,
                    Opts
                ),
            % Cache the `Memory` key every `Cache-Frequency` slots.
            Freq =
                hb_opts:get(
                    process_cache_frequency,
                    ?DEFAULT_CACHE_FREQ,
                    Opts
                ),
            case CurrentSlot rem Freq of
                0 ->
                    case snapshot(Msg3, Msg2, Opts) of
                        {ok, Snapshot} ->
                            ?event(snapshot,
                                {got_snapshot, 
                                    {storing_as_slot, CurrentSlot}
                                }
                            ),
                            dev_process_cache:write(
                                ProcID,
                                CurrentSlot,
                                Snapshot,
                                Opts
                            );
                        not_found ->
                            ?event(no_result_for_snapshot),
                            nothing_to_store
                    end;
                _ -> nothing_to_do
            end,
            ?event({do_compute_result, {msg3, Msg3}}),
            do_compute(
                ProcID,
                hb_converge:set(
                    Msg3,
                    #{ <<"current-slot">> => CurrentSlot + 1 },
                    Opts
                ),
                Msg2,
                TargetSlot,
                Opts
            )
    end.

%% @doc Returns the `/Results' key of the latest computed message.
now(RawMsg1, _Msg2, Opts) ->
    Msg1 = ensure_process_key(RawMsg1, Opts),
    {ok, CurrentSlot} = hb_converge:resolve(Msg1, #{ <<"path">> => <<"slot/current-slot">> }, Opts),
    ProcessID = hb_converge:get(<<"process/id">>, Msg1, Opts),
    ?event({now_called, {process, ProcessID}, {slot, CurrentSlot}}),
    hb_converge:resolve(
        Msg1,
        #{ <<"path">> => <<"compute/results">>, <<"slot">> => CurrentSlot },
        Opts
    ).

%% @doc Recursively push messages to the scheduler until we find a message
%% that does not lead to any further messages being scheduled.
push(Msg1, Msg2, Opts) ->
    Wallet = hb:wallet(),
    PushMsgSlot = hb_converge:get(<<"Slot">>, Msg2, Opts),
    {ok, Outbox} = hb_converge:resolve(
        Msg1,
        #{ <<"path">> => <<"compute/results/outbox">>, <<"slot">> => PushMsgSlot },
        Opts#{ spawn_worker => true }
    ),
    case ?IS_EMPTY_MESSAGE(Outbox) of
        true ->
            {ok, #{}};
        false ->
            {ok, maps:map(
                fun(Key, MsgToPush) ->
                    case hb_converge:get(<<"target">>, MsgToPush, Opts) of
                        not_found ->
                            ?event(push, {skip_no_target, {key, Key}, MsgToPush}),
                            #{};
                        Target ->
                            ?event(push,
                                {pushing_child,
                                    {originates_from_slot, PushMsgSlot},
                                    {outbox_key, Key}
                                }
                            ),
                            {ok, NextSlotOnProc} = hb_converge:resolve(
                                Msg1,
                                #{
                                    <<"method">> => <<"POST">>,
                                    <<"path">> => <<"schedule/slot">>,
                                    <<"body">> =>
                                        PushedMsg = hb_message:attest(
                                            MsgToPush,
                                            Wallet
                                        )
                                },
                                Opts
                            ),
                            PushedMsgID = hb_converge:get(<<"id">>, PushedMsg, Opts),
                            ?event(push,
                                {push_scheduled,
                                    {assigned_slot, NextSlotOnProc},
                                    {target, Target},
                                    {pushed_msg, PushedMsg}
                                }),
                            {ok, Downstream} = hb_converge:resolve(
                                Msg1,
                                #{ <<"path">> => <<"push">>, <<"slot">> => NextSlotOnProc },
                                Opts
                            ),
                            #{
                                <<"id">> => PushedMsgID,
                                <<"target">> => Target,
                                <<"slot">> => NextSlotOnProc,
                                <<"resulted-in">> => Downstream
                            }
                    end
                end,
                maps:without([<<"hashpath">>], Outbox)
            )}
    end.

%% @doc Ensure that the process message we have in memory is live and
%% up-to-date.
ensure_loaded(Msg1, Msg2, Opts) ->
    % Get the nonce we are currently on and the inbound nonce.
    TargetSlot = hb_converge:get(<<"slot">>, Msg2, undefined, Opts),
    ProcID = 
        case hb_converge:get(<<"process/id">>, {as, dev_message, Msg1}, Opts) of
            not_found ->
                hb_converge:get(<<"id">>, Msg1, Opts);
            P -> P
        end,
    case hb_converge:get(<<"initialized">>, Msg1, <<"false">>, Opts) of
        <<"false">> ->
            % Try to load the latest complete state from disk.
            LoadRes =
                dev_process_cache:latest(
                    ProcID,
                    [],
                    TargetSlot,
                    Opts
                ),
            ?event({snapshot_load_res, {proc_id, ProcID}, {res, LoadRes}, {target, TargetSlot}}),
            case LoadRes of
                {ok, LoadedSlot, SnapshotMsg} ->
                    % Restore the devices in the executor stack with the
                    % loaded state. This allows the devices to load any
                    % necessary 'shadow' state (state not represented in
                    % the public component of a message) into memory.
                    % Do not update the hashpath while we do this.
                    ?event(snapshot, {loaded_state_checkpoint, ProcID, LoadedSlot}),
                    {ok,
                        hb_converge:set(
                            Msg1,
                            #{
                                <<"initialized">> => <<"true">>,
                                <<"current-slot">> => LoadedSlot,
                                <<"snapshot">> => SnapshotMsg
                            },
                            Opts#{ hashpath => ignore }
                        )
                    };
                not_found ->
                    % If we do not have a checkpoint, initialize the
                    % process from scratch.
                    ?event(
                        {no_checkpoint_found,
                            {process, ProcID},
                            {slot, TargetSlot}
                        }
                    ),
                    init(Msg1, Msg2, Opts)
            end;
        <<"true">> -> {ok, Msg1}
    end.

%% @doc Run a message against Msg1, with the device being swapped out for
%% the device found at `Key'. After execution, the device is swapped back
%% to the original device if the device is the same as we left it.
run_as(Key, Msg1, Msg2, Opts) ->
    BaseDevice = hb_converge:get(<<"device">>, {as, dev_message, Msg1}, Opts),
    %?event({running_as, {key, Key}, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    {ok, PreparedMsg} =
        dev_message:set(
            ensure_process_key(Msg1, Opts),
            #{
                <<"device">> => 
                    DeviceSet = hb_converge:get(
                        << Key/binary, "-device">>,
                        {as, dev_message, Msg1},
                        Opts
                    ),
                <<"input-prefix">> =>
                    case hb_converge:get(<<"input-prefix">>, Msg1, Opts) of
                        not_found -> <<"process">>;
                        Prefix -> Prefix
                    end,
                <<"output-prefixes">> =>
                    hb_converge:get(
                        <<Key/binary, "-output-prefixes">>,
                        {as, dev_message, Msg1}, undefined, Opts)
            },
            Opts
        ),
    ?event({resolving_proc, {msg1, PreparedMsg}, {msg2, Msg2}, {opts, Opts}}),
    DeviceAsSet = hb_converge:get(<<"device">>, PreparedMsg, Opts),
    ?event({device_set, DeviceAsSet}),
    {ok, BaseResult} =
        hb_converge:resolve(
            PreparedMsg,
            Msg2,
            Opts
        ),
    case BaseResult of
        #{ <<"device">> := DeviceSet } ->
            {ok, hb_converge:set(BaseResult, #{ <<"device">> => BaseDevice })};
        _ ->
            ?event({returning_base_result, BaseResult}),
            {ok, BaseResult}
    end.

%% @doc Change the message to for that has the device set as this module.
%% In situations where the key that is `run_as` returns a message with a 
%% transformed device, this is useful.
as_process(Msg1, Opts) ->
    {ok, Proc} = dev_message:set(Msg1, #{ <<"device">> => <<"Process@1.0">> }, Opts),
    Proc.

%% @doc Helper function to store a copy of the `process` key in the message.
ensure_process_key(Msg1, Opts) ->
    case hb_converge:get(<<"process">>, {as, dev_message, Msg1}, Opts) of
        not_found ->
            hb_converge:set(
                Msg1, #{ <<"process">> => Msg1 }, Opts#{ hashpath => ignore });
        _ -> Msg1
    end.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    ok.

%% @doc Generate a process message with a random number, and no 
%% executor.
test_base_process() ->
    Wallet = hb:wallet(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    hb_message:attest(#{
        <<"device">> => <<"Process@1.0">>,
        <<"scheduler-device">> => <<"Scheduler@1.0">>,
        <<"scheduler-location">> => Address,
        <<"type">> => <<"Process">>,
        <<"test-random-seed">> => rand:uniform(1337)
    }, Wallet).

test_wasm_process(WASMImage) ->
    Wallet = hb:wallet(),
    #{ <<"image">> := WASMImageID } = dev_wasm:cache_wasm_image(WASMImage),
    hb_message:attest(
        maps:merge(test_base_process(), #{
            <<"execution-device">> => <<"Stack@1.0">>,
            <<"device-stack">> => [<<"WASM-64@1.0">>],
            <<"image">> => WASMImageID
        }),
        Wallet
    ).

%% @doc Generate a process message with a random number, and the 
%% `dev_wasm' device for execution.
test_aos_process() ->
    Wallet = hb:wallet(),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>),
    hb_message:attest(maps:merge(WASMProc, #{
        <<"device-stack">> =>
            [
                <<"WASI@1.0">>,
                <<"JSON-Iface@1.0">>,
                <<"WASM-64@1.0">>,
                <<"Multipass@1.0">>
            ],
        <<"output-prefix">> => <<"wasm">>,
        <<"passes">> => 2,
        <<"stack-keys">> =>
            [
                <<"init">>,
                <<"compute">>,
                <<"snapshot">>,
                <<"normalize">>
            ],
        <<"scheduler">> => hb_util:human_id(hb:address()),
        <<"authority">> => hb_util:human_id(hb:address())
    }), Wallet).

%% @doc Generate a device that has a stack of two `dev_test's for 
%% execution. This should generate a message state has doubled 
%% `Already-Seen' elements for each assigned slot.
dev_test_process() ->
    Wallet = hb:wallet(),
    hb_message:attest(
        maps:merge(test_base_process(), #{
            <<"execution-device">> => <<"Stack@1.0">>,
            <<"device-stack">> => [<<"Test-Device@1.0">>, <<"Test-Device@1.0">>]
        }),
        Wallet
    ).

schedule_test_message(Msg1, Text) ->
    schedule_test_message(Msg1, Text, #{}).
schedule_test_message(Msg1, Text, MsgBase) ->
    Wallet = hb:wallet(),
    Msg2 = hb_message:attest(#{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(
                MsgBase#{
                    <<"type">> => <<"Message">>,
                    <<"test-label">> => Text
                },
                Wallet
            )
    }, Wallet),
    {ok, _} = hb_converge:resolve(Msg1, Msg2, #{}).

schedule_aos_call(Msg1, Code) ->
    Wallet = hb:wallet(),
    ProcID = hb_message:id(Msg1, all),
    Msg2 = hb_message:attest(#{
        <<"action">> => <<"Eval">>,
        <<"data">> => Code,
        <<"target">> => ProcID
    }, Wallet),
    schedule_test_message(Msg1, <<"TEST MSG">>, Msg2).

schedule_wasm_call(Msg1, FuncName, Params) ->
    Wallet = hb:wallet(),
    Msg2 = hb_message:attest(#{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(
                #{
                    <<"type">> => <<"Message">>,
                    <<"wasm-function">> => FuncName,
                    <<"wasm-params">> => Params
                },
                Wallet
            )
    }, Wallet),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})).

schedule_on_process_test() ->
    init(),
    Msg1 = test_aos_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    ?event(messages_scheduled),
    {ok, SchedulerRes} =
        hb_converge:resolve(Msg1, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),
    ?assertMatch(
        <<"TEST TEXT 1">>,
        hb_converge:get(<<"assignments/0/body/Test-Label">>, SchedulerRes)
    ),
    ?assertMatch(
        <<"TEST TEXT 2">>,
        hb_converge:get(<<"assignments/1/body/Test-Label">>, SchedulerRes)
    ).

get_scheduler_slot_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    Msg2 = #{
        <<"path">> => <<"Slot">>,
        <<"method">> => <<"GET">>
    },
    ?assertMatch(
        {ok, #{ <<"current-slot">> := CurrentSlot }} when CurrentSlot > 0,
        hb_converge:resolve(Msg1, Msg2, #{})
    ).

recursive_path_resolution_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    CurrentSlot =
        hb_converge:resolve(
            Msg1,
            #{ <<"path">> => <<"slot/current-slot">> },
            #{ <<"hashpath">> => ignore }
        ),
    ?event({resolved_current_slot, CurrentSlot}),
    ?assertMatch(
        CurrentSlot when CurrentSlot > 0,
        CurrentSlot
    ),
    ok.

test_device_compute_test() ->
    init(),
    Msg1 = dev_test_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    ?assertMatch(
        {ok, <<"TEST TEXT 2">>},
        hb_converge:resolve(
            Msg1,
            <<"schedule/assignments/1/body/test-label">>,
            #{ <<"hashpath">> => ignore }
        )
    ),
    Msg2 = #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
    {ok, Msg3} = hb_converge:resolve(Msg1, Msg2, #{}),
    ?event({computed_message, {msg3, Msg3}}),
    ?assertEqual(1, hb_converge:get(<<"results/assignment-slot">>, Msg3, #{})),
    ?assertEqual([1,1,0,0], hb_converge:get(<<"already-seen">>, Msg3, #{})).

wasm_compute_test() ->
    init(),
    Msg1 = test_wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Msg1, <<"fac">>, [5.0]),
    schedule_wasm_call(Msg1, <<"fac">>, [6.0]),
    {ok, Msg3} = 
        hb_converge:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
            #{ <<"hashpath">> => ignore }
        ),
    ?event({computed_message, {msg3, Msg3}}),
    ?assertEqual([120.0], hb_converge:get(<<"results/output">>, Msg3, #{})),
    {ok, Msg4} = 
        hb_converge:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            #{ <<"hashpath">> => ignore }
        ),
    ?event({computed_message, {msg4, Msg4}}),
    ?assertEqual([720.0], hb_converge:get(<<"results/output">>, Msg4, #{})).

aos_compute_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"return 1+1">>),
        schedule_aos_call(Msg1, <<"return 2+2">>),
        Msg2 = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Msg3} = hb_converge:resolve(Msg1, Msg2, #{}),
        {ok, Res} = hb_converge:resolve(Msg3, <<"results">>, #{}),
        ?event({computed_message, {msg3, Res}}),
        {ok, Data} = hb_converge:resolve(Res, <<"data">>, #{}),
        ?event({computed_data, Data}),
        ?assertEqual(<<"2">>, Data),
        Msg4 = #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
        {ok, Msg5} = hb_converge:resolve(Msg1, Msg4, #{}),
        ?assertEqual(<<"4">>, hb_converge:get(<<"results/data">>, Msg5, #{})),
        {ok, Msg5}
    end}.

%% @doc Manually test state restoration without using the cache.
restore_test_() -> {timeout, 30, fun do_test_restore/0}.

do_test_restore() ->
    % Init the process and schedule 3 messages:
    % 1. Set variables in Lua.
    % 2. Return the variable.
    % Execute the first computation, then the second as a disconnected process.
    Opts = #{ process_cache_frequency => 1 },
    init(),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ResetRes = hb_store:reset(Store),
    ?event({reset_store, {result, ResetRes}, {store, Store}}),
    Msg1 = test_aos_process(),
    schedule_aos_call(Msg1, <<"X = 42">>),
    schedule_aos_call(Msg1, <<"X = 1337">>),
    schedule_aos_call(Msg1, <<"return X">>),
    % Compute the first message.
    {ok, _} =
        hb_converge:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            Opts
        ),
    {ok, ResultB} =
        hb_converge:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 2 },
            Opts
        ),
    ?event({result_b, ResultB}),
    ?assertEqual(<<"1337">>, hb_converge:get(<<"results/data">>, ResultB, #{})).

now_results_test() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"return 1+1">>),
        schedule_aos_call(Msg1, <<"return 2+2">>),
        ?assertEqual({ok, <<"4">>}, hb_converge:resolve(Msg1, <<"now/data">>, #{}))
    end}.

full_push_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        ?event(push, {msg1, Msg1}),
        Script = ping_ping_script(2),
        ?event({script, Script}),
        {ok, Msg2} = schedule_aos_call(Msg1, Script),
        ?event(push, {init_sched_result, Msg2}),
        {ok, StartingMsgSlot} =
            hb_converge:resolve(Msg2, #{ <<"path">> => <<"slot">> }, #{}),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => StartingMsgSlot
            },
        {ok, _} = hb_converge:resolve(Msg1, Msg3, #{}),
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_converge:resolve(Msg1, <<"now/data">>, #{})
        )
    end}.

persistent_process_test() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"X=1">>),
        schedule_aos_call(Msg1, <<"return 2">>),
        schedule_aos_call(Msg1, <<"return X">>),
        T0 = hb:now(),
        FirstSlotMsg2 = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 0
        },
        ?assertMatch(
            {ok, _},
            hb_converge:resolve(Msg1, FirstSlotMsg2, #{ spawn_worker => true })
        ),
        T1 = hb:now(),
        ThirdSlotMsg2 = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 2
        },
        Res = hb_converge:resolve(Msg1, ThirdSlotMsg2, #{}),
        ?event({computed_message, {msg3, Res}}),
        ?assertMatch(
            {ok, _},
            Res
        ),
        T2 = hb:now(),
        ?event(benchmark, {runtimes, {first_run, T1 - T0}, {second_run, T2 - T1}}),
        % The second resolve should be much faster than the first resolve, as the
        % process is already running.
        ?assert(T2 - T1 < ((T1 - T0)/2))
    end}.

simple_wasm_persistent_worker_benchmark_test() ->
    init(),
    BenchTime = 1,
    Msg1 = test_wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Msg1, <<"fac">>, [5.0]),
    schedule_wasm_call(Msg1, <<"fac">>, [6.0]),
    {ok, Initialized} = 
        hb_converge:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            #{ spawn_worker => true }
        ),
    Iterations = hb:benchmark(
        fun(Iteration) ->
            schedule_wasm_call(
                Initialized,
                <<"fac">>,
                [5.0]
            ),
            ?assertMatch(
                {ok, _},
                hb_converge:resolve(
                    Initialized,
                    #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration + 1 },
                    #{}
                )
            )
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_util:eunit_print(
        "Scheduled and evaluated ~p simple wasm process messages in ~p s (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 2),
    ok.

aos_persistent_worker_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchTime = 4,
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"X=1337">>),
        FirstSlotMsg2 = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 0
        },
        ?assertMatch(
            {ok, _},
            hb_converge:resolve(Msg1, FirstSlotMsg2, #{ spawn_worker => true })
        ),
        Iterations = hb:benchmark(
            fun(Iteration) ->
                schedule_aos_call(
                    Msg1,
                    <<"return X + ", (integer_to_binary(Iteration))/binary>>
                ),
                ?assertMatch(
                    {ok, _},
                    hb_converge:resolve(
                        Msg1,
                        #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration },
                        #{}
                    )
                )
            end,
            BenchTime
        ),
        ?event(benchmark, {scheduled, Iterations}),
        hb_util:eunit_print(
            "Scheduled and evaluated ~p AOS process messages in ~p s (~.2f msg/s)",
            [Iterations, BenchTime, Iterations / BenchTime]
        ),
        ?assert(Iterations >= 2),
        ok
    end}.

%%% Test helpers

ping_ping_script(Limit) ->
    % <<
    %     "Handlers.add(\"Ping\",\n"
    %     "   function(m)\n"
    %     "       C = tonumber(m.Count)\n"
    %     "       if C <= ", (integer_to_binary(Limit))/binary, " then\n"
    %     "           Send({ Target = ao.id, Action = \"Ping\", Count = C + 1 })\n"
    %     "           print(\"Ping\", C + 1)\n"
    %     "       else\n"
    %     "           print(\"Done.\")\n"
    %     "       end\n"
    %     "   end\n"
    %     ")\n"
    %     "Send({ Target = ao.id, Action = \"Ping\", Count = 1 })\n"
    % >>.
    <<
        "Handlers.add(\"Ping\",\n"
        "   function (test) return true end,\n"
        "   function(m)\n"
        "       C = tonumber(m.Count)\n"
        "       if C <= ", (integer_to_binary(Limit))/binary, " then\n"
        "           Send({ Target = ao.id, Action = \"Ping\", Count = C + 1 })\n"
        "           print(\"Ping\", C + 1)\n"
        "       else\n"
        "           print(\"Done.\")\n"
        "       end\n"
        "   end\n"
        ")\n"
        "Send({ Target = ao.id, Action = \"Ping\", Count = 1 })\n"
    >>.
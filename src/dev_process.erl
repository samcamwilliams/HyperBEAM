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
-export([info/1, compute/3, schedule/3, slot/3, now/3, push/3, memory/3]).
-export([ensure_process_key/2]).
%%% Test helpers
-export([test_aos_process/0, dev_test_process/0, test_wasm_process/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% The frequency at which the process state should be cached. Can be overridden
%% with the `cache_frequency` option.
-define(DEFAULT_CACHE_FREQ, 2).

%% @doc When the info key is called, we should return the process exports.
info(_Msg1) ->
    #{
        worker => fun dev_process_worker:server/3,
        grouper => fun dev_process_worker:group/3
    }.

%% @doc Wraps functions in the Scheduler device.
schedule(Msg1, Msg2, Opts) ->
    run_as(<<"Scheduler">>, Msg1, Msg2, Opts).

slot(Msg1, Msg2, Opts) ->
    ?event({slot_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    run_as(<<"Scheduler">>, Msg1, Msg2, Opts).

next(Msg1, _Msg2, Opts) ->
    run_as(<<"Scheduler">>, Msg1, next, Opts).

memory(RawMsg1, _Msg2, Opts) ->
    Msg1 = ensure_process_key(RawMsg1, Opts),
    run_as(<<"Execution">>, Msg1, #{ path => <<"Memory">> }, Opts).

%% @doc Before computation begins, a boot phase is required. This phase
%% allows devices on the execution stack to initialize themselves. We set the
%% `Initialized' key to `True' to indicate that the process has been
%% initialized.
init(Msg1, _Msg2, Opts) ->
    ?event({init_called, {msg1, Msg1}, {opts, Opts}}),
    {ok, Initialized} =
        run_as(<<"Execution">>, Msg1, #{ path => init }, Opts),
    {
        ok,
        hb_converge:set(
            Initialized,
            #{
                <<"Initialized">> => <<"True">>,
                <<"Current-Slot">> => -1
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
            <<"Execution">>,
            Loaded,
            normalize,
            Opts#{ hashpath => ignore }
        ),
    ?event({compute_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    do_compute(
        Normalized,
        Msg2,
        hb_converge:get(<<"Slot">>, Msg2, Opts),
        Opts
    ).

%% @doc Continually get and apply the next assignment from the scheduler until
%% we reach the target slot that the user has requested.
do_compute(Msg1, Msg2, TargetSlot, Opts) ->
    ?event({do_compute_called, {target_slot, TargetSlot}, {msg1, Msg1}}),
    case hb_converge:get(<<"Current-Slot">>, Msg1, Opts) of
        CurrentSlot when CurrentSlot > TargetSlot ->
            throw({error, {already_calculated_slot, TargetSlot}});
        CurrentSlot when CurrentSlot == TargetSlot ->
            % We reached the target height so we return.
            ?event({reached_target_slot_returning_state, TargetSlot}),
            {ok, as_process(Msg1, Opts)};
        CurrentSlot ->
            % Get the next input from the scheduler device.
            {ok, #{ <<"Message">> := ToProcess, <<"State">> := State }} =
                next(Msg1, Msg2, Opts),
            % Calculate how much of the state should be cached.
            Freq =
                hb_opts:get(
                    process_cache_frequency,
                    ?DEFAULT_CACHE_FREQ,
                    Opts
                ),
            CacheKeys =
                case CurrentSlot rem Freq of
                    0 -> all;
                    _ -> [<<"Results">>]
                end,
            ?event(process_compute,
                {
                    executing,
                    {msg1, Msg1},
                    {msg2, ToProcess},
                    {caching, CacheKeys}
                }
            ),
            {ok, Msg3} =
                run_as(
                    <<"Execution">>,
                    State,
                    ToProcess,
                    Opts#{ cache_keys => CacheKeys }
                ),
            ?event({do_compute_result, {msg3, Msg3}}),
            do_compute(
                hb_converge:set(
                    Msg3,
                    #{ <<"Current-Slot">> => CurrentSlot + 1 },
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
    {ok, CurrentSlot} = hb_converge:resolve(Msg1, #{ path => <<"Slot/Current-Slot">> }, Opts),
    ProcessID = hb_converge:get(<<"Process/id">>, Msg1, Opts),
    ?event({now_called, {process, ProcessID}, {slot, CurrentSlot}}),
    hb_converge:resolve(
        Msg1,
        #{ path => <<"Compute/Results">>, <<"Slot">> => CurrentSlot },
        Opts
    ).

%% @doc Recursively push messages to the scheduler until we find a message
%% that does not lead to any further messages being scheduled.
push(Msg1, Msg2, Opts) ->
    Wallet = hb:wallet(),
    PushMsgSlot = hb_converge:get(<<"Slot">>, Msg2, Opts),
    {ok, Outbox} = hb_converge:resolve(
        Msg1,
        #{ path => <<"Compute/Results/Outbox">>, <<"Slot">> => PushMsgSlot },
        Opts#{ spawn_worker => true }
    ),
    case ?IS_EMPTY_MESSAGE(Outbox) of
        true ->
            {ok, #{}};
        false ->
            {ok, maps:map(
                fun(Key, MsgToPush) ->
                    case hb_converge:get(<<"Target">>, MsgToPush, Opts) of
                        not_found ->
                            ?event({skip_no_target, {key, Key}, MsgToPush}),
                            {ok, <<"No Target. Did not push.">>};
                        Target ->
                            ?event(
                                {pushing_child,
                                    {originates_from_slot, PushMsgSlot},
                                    {outbox_key, Key}
                                }
                            ),
                            {ok, NextSlotOnProc} = hb_converge:resolve(
                                Msg1,
                                #{
                                    method => <<"POST">>,
                                    path => <<"Schedule/Slot">>,
                                    <<"Message">> =>
                                        PushedMsg = hb_message:sign(
                                            MsgToPush,
                                            Wallet
                                        )
                                },
                                Opts
                            ),
                            PushedMsgID = hb_converge:get(<<"id">>, PushedMsg, Opts),
                            ?event(
                                {push_scheduled,
                                    {assigned_slot, NextSlotOnProc},
                                    {target, Target}
                                }),
                            {ok, Downstream} = hb_converge:resolve(
                                Msg1,
                                #{ path => <<"Push">>, <<"Slot">> => NextSlotOnProc },
                                Opts
                            ),
                            #{
                                <<"id">> => PushedMsgID,
                                <<"Target">> => Target,
                                <<"Slot">> => NextSlotOnProc,
                                <<"Resulted-In">> => Downstream
                            }
                    end
                end,
                maps:without([hashpath], Outbox)
            )}
    end.

%% @doc Ensure that the process message we have in memory is live and
%% up-to-date.
ensure_loaded(Msg1, Msg2, Opts) ->
    % Get the nonce we are currently on and the inbound nonce.
    TargetSlot = hb_converge:get(<<"Slot">>, Msg2, undefined, Opts),
    ProcID = 
        case hb_converge:get(<<"Process/id">>, {as, dev_message, Msg1}, Opts) of
            not_found ->
                hb_converge:get(<<"id">>, Msg1, Opts);
            P -> P
        end,
    case hb_converge:get(<<"Initialized">>, Msg1, <<"False">>, Opts) of
        <<"False">> ->
            % Try to load the latest complete state from disk.
            LoadRes =
                dev_process_cache:latest(
                    ProcID,
                    [<<"Initialized">>],
                    TargetSlot,
                    Opts
                ),
            case LoadRes of
                {ok, LoadedSlot, MsgFromCache} ->
                    % Restore the devices in the executor stack with the
                    % loaded state. This allows the devices to load any
                    % necessary 'shadow' state (state not represented in
                    % the public component of a message) into memory.
                    ?event({loaded_state_checkpoint, ProcID, LoadedSlot}),
                    run_as(
                        <<"Execution">>,
                        MsgFromCache,
                        restore,
                        Opts
                    );
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
        <<"True">> -> {ok, Msg1}
    end.

%% @doc Run a message against Msg1, with the device being swapped out for
%% the device found at `Key'. After execution, the device is swapped back
%% to the original device if the device is the same as we left it.
run_as(Key, Msg1, Msg2, Opts) ->
    BaseDevice = hb_converge:get(<<"Device">>, {as, dev_message, Msg1}, Opts),
    %?event({running_as, {key, Key}, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    {ok, PreparedMsg} =
        dev_message:set(
            ensure_process_key(Msg1, Opts),
            #{
                device => 
                    DeviceSet = hb_converge:get(
                        << Key/binary, "-Device">>,
                        {as, dev_message, Msg1},
                        Opts
                    ),
                <<"Input-Prefix">> =>
                    case hb_converge:get(<<"Input-Prefix">>, Msg1, Opts) of
                        not_found -> <<"Process">>;
                        Prefix -> Prefix
                    end,
                <<"Output-Prefixes">> =>
                    hb_converge:get(
                        <<Key/binary, "-Output-Prefixes">>,
                        {as, dev_message, Msg1}, undefined, Opts)
            },
            Opts
        ),
    %?event({resolving_proc, {msg1, PreparedMsg}, {msg2, Msg2}, {opts, Opts}}),
    {ok, BaseResult} =
        hb_converge:resolve(
            PreparedMsg,
            Msg2,
            Opts
        ),
    case BaseResult of
        #{ device := DeviceSet } ->
            {ok, hb_converge:set(BaseResult, #{ device => BaseDevice })};
        _ ->
            ?event({returning_base_result, BaseResult}),
            {ok, BaseResult}
    end.

%% @doc Change the message to for that has the device set as this module.
%% In situations where the key that is `run_as` returns a message with a 
%% transformed device, this is useful.
as_process(Msg1, Opts) ->
    {ok, Proc} = dev_message:set(Msg1, #{ device => <<"Process/1.0">> }, Opts),
    Proc.

%% @doc Helper function to store a copy of the `process` key in the message.
ensure_process_key(Msg1, Opts) ->
    case hb_converge:get(<<"Process">>, {as, dev_message, Msg1}, Opts) of
        not_found ->
            hb_converge:set(
                Msg1, #{ <<"Process">> => Msg1 }, Opts#{ hashpath => ignore });
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
    #{
        device => <<"Process/1.0">>,
        <<"Scheduler-Device">> => <<"Scheduler/1.0">>,
        <<"Scheduler-Location">> => Address,
        <<"Type">> => <<"Process">>,
        <<"Test-Random-Seed">> => rand:uniform(1337)
    }.

test_wasm_process(WASMImage) ->
    #{ image := WASMImageID } = dev_wasm:cache_wasm_image(WASMImage),
    maps:merge(test_base_process(), #{
        <<"Execution-Device">> => <<"Stack/1.0">>,
        <<"Device-Stack">> => [<<"WASM-64/1.0">>],
        <<"Image">> => WASMImageID
    }).

%% @doc Generate a process message with a random number, and the 
%% `dev_wasm' device for execution.
test_aos_process() ->
    Wallet = hb:wallet(),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>),
    hb_message:sign(maps:merge(WASMProc, #{
        <<"Device-Stack">> =>
            [
                <<"WASI/1.0">>,
                <<"JSON-Iface/1.0">>,
                <<"WASM-64/1.0">>,
                <<"Multipass/1.0">>
            ],
        <<"Output-Prefix">> => <<"WASM">>,
        <<"Passes">> => 2,
        <<"Stack-Keys">> =>
            [
                <<"Init">>,
                <<"Compute">>,
                <<"Memory">>,
                <<"Normalize">>
            ],
        <<"Scheduler">> => hb:address(),
        <<"Authority">> => hb:address()
    }), Wallet).

%% @doc Generate a device that has a stack of two `dev_test's for 
%% execution. This should generate a message state has doubled 
%% `Already-Seen' elements for each assigned slot.
dev_test_process() ->
    maps:merge(test_base_process(), #{
        <<"Execution-Device">> => <<"Stack/1.0">>,
        <<"Device-Stack">> => [<<"Test-Device/1.0">>, <<"Test-Device/1.0">>]
    }).

schedule_test_message(Msg1, Text) ->
    schedule_test_message(Msg1, Text, #{}).
schedule_test_message(Msg1, Text, MsgBase) ->
    Wallet = hb:wallet(),
    Msg2 = hb_message:sign(#{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            MsgBase#{
                <<"Type">> => <<"Message">>,
                <<"Test-Label">> => Text
            }
    }, Wallet),
    {ok, _} = hb_converge:resolve(Msg1, Msg2, #{}).

schedule_aos_call(Msg1, Code) ->
    Wallet = hb:wallet(),
    ProcID = hb_converge:get(id, Msg1, #{}),
    Msg2 = hb_message:sign(#{
        <<"Action">> => <<"Eval">>,
        data => Code,
        target => ProcID
    }, Wallet),
    schedule_test_message(Msg1, <<"TEST MSG">>, Msg2).

schedule_wasm_call(Msg1, FuncName, Params) ->
    Msg2 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"WASM-Function">> => FuncName,
                <<"WASM-Params">> => Params
            }
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})).

schedule_on_process_test() ->
    init(),
    Msg1 = test_aos_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    ?event(messages_scheduled),
    {ok, SchedulerRes} =
        hb_converge:resolve(Msg1, #{
            <<"Method">> => <<"GET">>,
            path => <<"Schedule">>
        }, #{}),
    ?assertMatch(
        <<"TEST TEXT 1">>,
        hb_converge:get(<<"Assignments/0/Message/Test-Label">>, SchedulerRes)
    ),
    ?assertMatch(
        <<"TEST TEXT 2">>,
        hb_converge:get(<<"Assignments/1/Message/Test-Label">>, SchedulerRes)
    ).

get_scheduler_slot_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    Msg2 = #{
        path => <<"Slot">>,
        <<"Method">> => <<"GET">>
    },
    ?assertMatch(
        {ok, #{ <<"Current-Slot">> := CurrentSlot }} when CurrentSlot > 0,
        hb_converge:resolve(Msg1, Msg2, #{})
    ).

recursive_path_resolution_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    CurrentSlot =
        hb_converge:resolve(
            Msg1,
            #{ path => <<"Slot/Current-Slot">> },
            #{ hashpath => ignore }
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
        <<"TEST TEXT 2">>,
        hb_converge:get(
            <<"Schedule/Assignments/1/Message/Test-Label">>,
            Msg1,
            #{ hashpath => ignore }
        )
    ),
    Msg2 = #{ path => <<"Compute">>, <<"Slot">> => 1 },
    {ok, Msg3} = hb_converge:resolve(Msg1, Msg2, #{}),
    ?event({computed_message, {msg3, Msg3}}),
    ?assertEqual(1, hb_converge:get(<<"Results/Assignment-Slot">>, Msg3, #{})),
    ?assertEqual([1,1,0,0], hb_converge:get(<<"Already-Seen">>, Msg3, #{})).

wasm_compute_test() ->
    init(),
    Msg1 = test_wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Msg1, <<"fac">>, [5.0]),
    schedule_wasm_call(Msg1, <<"fac">>, [6.0]),
    {ok, Msg3} = 
        hb_converge:resolve(
            Msg1,
            #{ path => <<"Compute">>, <<"Slot">> => 0 },
            #{ hashpath => ignore }
        ),
    ?event({computed_message, {msg3, Msg3}}),
    ?assertEqual([120.0], hb_converge:get(<<"Results/Output">>, Msg3, #{})),
    {ok, Msg4} = 
        hb_converge:resolve(
            Msg1,
            #{ path => <<"Compute">>, <<"Slot">> => 1 },
            #{ hashpath => ignore }
        ),
    ?event({computed_message, {msg4, Msg4}}),
    ?assertEqual([720.0], hb_converge:get(<<"Results/Output">>, Msg4, #{})).

aos_compute_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"return 1+1">>),
        schedule_aos_call(Msg1, <<"return 2+2">>),
        Msg2 = #{ path => <<"Compute">>, <<"Slot">> => 0 },
        {ok, Msg3} = hb_converge:resolve(Msg1, Msg2, #{}),
        {ok, Res} = hb_converge:resolve(Msg3, <<"Results">>, #{}),
        ?event({computed_message, {msg3, Res}}),
        {ok, Data} = hb_converge:resolve(Res, <<"Data">>, #{}),
        ?event({computed_data, Data}),
        ?assertEqual(<<"2">>, Data),
        Msg4 = #{ path => <<"Compute">>, <<"Slot">> => 1 },
        {ok, Msg5} = hb_converge:resolve(Msg1, Msg4, #{}),
        ?assertEqual(<<"4">>, hb_converge:get(<<"Results/Data">>, Msg5, #{})),
        {ok, Msg5}
    end}.

%% @doc Manually test state restoration without using the cache.
manually_restore_state_test() ->
    % Init the process and schedule 2 messages:
    % 1. Set a variable in Lua.
    % 2. Return the variable.
    init(),
    Msg1 = test_aos_process(),
    schedule_aos_call(Msg1, <<"X = 1337">>),
    schedule_aos_call(Msg1, <<"return X">>),
    % Compute the first message.
    {ok, ForkState} =
        hb_converge:resolve(
            Msg1,
            #{ path => <<"Compute">>, <<"Slot">> => 0 },
            #{}
        ),
    % Get the state after the first message.
    {ok, State} = hb_converge:resolve(ForkState, <<"Memory">>, #{}),
    % % Calculate the second message to ensure it functions correctly.
    % {ok, ResultA} =
    %     hb_converge:resolve(
    %         ForkState,
    %         #{ path => <<"Compute">>, <<"Slot">> => 1 },
    %         #{}
    %     ),
    % ?event({result_a, ResultA}),
    % ?assertEqual(<<"1337">>, hb_converge:get(<<"Results/Data">>, ResultA, #{})),
    % Destroy the private state of the message after the first state, 
    % as will happen when it is serialized.
    Priv = hb_private:from_message(ForkState),
    ?event({destroying_private_state, Priv}),
    NewState = hb_private:reset(ForkState),
    % Compute the second message on the process without its private state.
    {ok, ResultB} =
        hb_converge:resolve(
            NewState#{
                <<"Memory">> => State,
                <<"Results">> => #{}
            },
            #{ path => <<"Compute">>, <<"Slot">> => 1 },
            #{}
        ),
    ?event({result_b, ResultB}),
    ?assertEqual(<<"1337">>, hb_converge:get(<<"Results/Data">>, ResultB, #{})).

now_results_test() ->
    init(),
    Msg1 = test_aos_process(),
    schedule_aos_call(Msg1, <<"return 1+1">>),
    schedule_aos_call(Msg1, <<"return 2+2">>),
    ?assertEqual({ok, <<"4">>}, hb_converge:resolve(Msg1, <<"Now/Data">>, #{})).

full_push_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        hb_cache:write(Msg1, #{}),
        Script = ping_ping_script(3),
        ?event({script, Script}),
        {ok, Msg2} = schedule_aos_call(Msg1, Script),
        ?event({init_sched_result, Msg2}),
        {ok, StartingMsgSlot} =
            hb_converge:resolve(Msg2, #{ path => <<"Slot">> }, #{}),
        Msg3 =
            #{
                path => <<"Push">>,
                <<"Slot">> => StartingMsgSlot
            },
        {ok, _} = hb_converge:resolve(Msg1, Msg3, #{}),
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_converge:resolve(Msg1, <<"Now/Data">>, #{})
        )
    end}.

persistent_process_test() ->
    init(),
    Msg1 = test_aos_process(),
    schedule_aos_call(Msg1, <<"X=1">>),
    schedule_aos_call(Msg1, <<"return 2">>),
    schedule_aos_call(Msg1, <<"return X">>),
    T0 = hb:now(),
    FirstSlotMsg2 = #{
        path => <<"Compute">>,
        <<"Slot">> => 0
    },
    ?assertMatch(
        {ok, _},
        hb_converge:resolve(Msg1, FirstSlotMsg2, #{ spawn_worker => true })
    ),
    T1 = hb:now(),
    ThirdSlotMsg2 = #{
        path => <<"Compute">>,
        <<"Slot">> => 2
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
    ?assert(T2 - T1 < ((T1 - T0)/2)).

simple_wasm_persistent_worker_benchmark_test() ->
    init(),
    BenchTime = 1,
    Msg1 = test_wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Msg1, <<"fac">>, [5.0]),
    schedule_wasm_call(Msg1, <<"fac">>, [6.0]),
    {ok, Initialized} = 
        hb_converge:resolve(
            Msg1,
            #{ path => <<"Compute">>, <<"Slot">> => 1 },
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
                    #{ path => <<"Compute">>, <<"Slot">> => Iteration + 1 },
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
            path => <<"Compute">>,
            <<"Slot">> => 0
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
                        #{ path => <<"Compute">>, <<"Slot">> => Iteration },
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
        ?assert(Iterations >= 5),
        ok
    end}.

%%% Test helpers

ping_ping_script(Limit) ->
    <<
        "Handlers.add(\"Ping\",\n"
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

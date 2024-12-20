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
%%% GET /ID/Scheduler/Schedule <- Returns the messages in the schedule
%%% POST /ID/Scheduler/Schedule <- Adds a message to the schedule
%%% 
%%% GET /ID/Computed/[IDorSlotNum]/Result <- Returns the state of the process
%%%                                         after applying a message
%%% GET /ID/Now <- Returns the `/Results` key of the latest computed message
%%% 
%%% Process definition example:
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
%%%         Quorum: 2'''
%%%
%%% Runtime options:
%%%     Cache-Frequency: The number of assignments that can pass before
%%%                      the full state should be cached.
%%%     Cache-Keys:      A list of the keys that should be cached, in
%%%                      addition to `/Results'.
-module(dev_process).
%%% Public API
-export([info/2, compute/3, schedule/3, slot/3, now/3]).
%%% Test helpers
-export([test_process/0, test_device_process/0, test_wasm_process/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% The frequency at which the process state should be cached. Can be overridden
%% with the `cache_frequency` option.
-define(DEFAULT_CACHE_FREQ, 2).

%% @doc When the info key is called, we should return the process exports.
info(_Msg1, _Opts) ->
    #{
        exports => [compute, schedule, slot],
        worker => fun dev_process_worker:server/3,
        group => fun dev_process_worker:group/3
    }.

%% @doc Wraps functions in the Scheduler device.
schedule(Msg1, Msg2, Opts) ->
    run_as(<<"Scheduler-Device">>, Msg1, Msg2, Opts).

slot(Msg1, Msg2, Opts) ->
    run_as(<<"Scheduler-Device">>, Msg1, Msg2, Opts).

next(Msg1, _Msg2, Opts) ->
    run_as(<<"Scheduler-Device">>, Msg1, next, Opts).

%% @doc Before computation begins, a boot phase is required. This phase
%% allows devices on the execution stack to initialize themselves. We set the
%% `Initialized' key to `True' to indicate that the process has been
%% initialized.
init(Msg1, _Msg2, Opts) ->
    ?event({init_called, {msg1, Msg1}, {opts, Opts}}),
    {ok, Initialized} =
        run_as(<<"Execution-Device">>, Msg1, #{ path => init }, Opts),
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
    do_compute(
        Loaded,
        Msg2,
        hb_converge:get(<<"Slot">>, Msg2, Opts),
        Opts
    ).

%% @doc Continually get and apply the next assignment from the scheduler until
%% we reach the target slot that the user has requested.
do_compute(Msg1, Msg2, TargetSlot, Opts) ->
    ?event({do_compute_called, {target_slot, TargetSlot}, {msg1, Msg1}}),
    case hb_converge:get(<<"Current-Slot">>, Msg1, Opts) of
        CurrentSlot when CurrentSlot == TargetSlot ->
            % We reached the target height so we return.
            ?event({reached_target_slot_returning_state, TargetSlot}),
            {ok, process(Msg1, Opts)};
        CurrentSlot ->
            % Get the next input from the scheduler device.
            ?no_prod("Must update hashpath!"),
            {ok, #{ <<"Message">> := ToProcess, <<"State">> := State }} =
                next(Msg1, Msg2, Opts#{ hashpath => ignore }),
            % Calculate how much of the state should be cached.
            Freq = hb_opts:get(process_cache_frequency, ?DEFAULT_CACHE_FREQ, Opts),
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
                    <<"Execution-Device">>,
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
now(Msg1, _Msg2, Opts) ->
    CurrentSlot = hb_converge:get(<<"Current-Slot">>, Msg1, Opts),
    ProcessID = hb_converge:get(<<"Process/id">>, Msg1, Opts),
    ?event({now_called, {process, ProcessID}, {slot, CurrentSlot}}),
    {ok, Msg3} = dev_process_cache:read(ProcessID, CurrentSlot, Opts),
    {ok, hb_converge:get(<<"Results">>, Msg3, Opts)}.

%% @doc Ensure that the process message we have in memory is live and
%% up-to-date.
ensure_loaded(Msg1, Msg2, Opts) ->
    % Get the nonce we are currently on and the inbound nonce.
    ?event({ensure_loaded, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    TargetSlot = hb_converge:get(<<"Slot">>, Msg2, undefined, Opts),
    ProcID = hb_converge:get(<<"Process/id">>, {as, dev_message, Msg1}, Opts),
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
                        <<"Execution-Device">>,
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
%% to the original device.
run_as(Key, Msg1, Msg2, Opts) ->
    BaseDevice = hb_converge:get(<<"Device">>, {as, dev_message, Msg1}, Opts),
    ?event({running_as, {key, Key}, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    {ok, PreparedMsg} =
        dev_message:set(
            ensure_process_key(Msg1, Opts),
            #{
                device => 
                    DeviceSet = hb_converge:get(
                        Key,
                        {as, dev_message, Msg1},
                        Opts
                    )
            },
            Opts
        ),
    ?event({prepared_msg, {msg1, PreparedMsg}, {msg2, Msg2}}),
    {ok, BaseResult} =
        hb_converge:resolve(
            PreparedMsg,
            Msg2,
            Opts
        ),
    ?event({base_result, BaseResult}),
    ?event({base_result_before_device_swap_back, BaseResult}),
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
process(Msg1, Opts) ->
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

%% @doc Generate a process message with a random number, and the 
%% `dev_wasm' device for execution.
test_process() ->
    test_wasm_process(<<"test/aos-2-pure.wasm">>).

test_wasm_process(WASMImage) ->
    maps:merge(test_base_process(), #{
        <<"Execution-Device">> => <<"Stack/1.0">>,
        <<"Device-Stack">> => [dev_vfs, dev_wasm],
        <<"WASM-Image">> => WASMImage
    }).

%% @doc Generate a device that has a stack of two `dev_test's for 
%% execution. This should generate a message state has doubled 
%% `Already-Seen' elements for each assigned slot.
test_device_process() ->
    maps:merge(test_base_process(), #{
        <<"Execution-Device">> => <<"Stack/1.0">>,
        <<"Device-Stack">> => [<<"Test-Device/1.0">>, <<"Test-Device/1.0">>]
    }).

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

schedule_test_message(Msg1, Text) ->
    Msg2 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Test-Key">> => Text
            }
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    ok.

schedule_on_process_test() ->
    Msg1 = test_process(),
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
        hb_converge:get(<<"Assignments/0/Message/Test-Key">>, SchedulerRes)
    ),
    ?assertMatch(
        <<"TEST TEXT 2">>,
        hb_converge:get(<<"Assignments/1/Message/Test-Key">>, SchedulerRes)
    ).

get_scheduler_slot_test() ->
    Msg1 = test_process(),
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

recursive_resolve_test() ->
    Msg1 = test_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    CurrentSlot =
        hb_converge:resolve(
            Msg1,
            #{ path => [<<"Slot">>, <<"Current-Slot">>] },
            #{ hashpath => ignore }
        ),
    ?event({resolved_current_slot, CurrentSlot}),
    ?assertMatch(
        CurrentSlot when CurrentSlot > 0,
        CurrentSlot
    ),
    ok.

test_device_compute_test() ->
    hb:init(),
    Msg1 = test_device_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    ?assertMatch(
        <<"TEST TEXT 2">>,
        hb_converge:get(
            <<"Schedule/Assignments/1/Message/Test-Key">>,
            Msg1,
            #{ hashpath => ignore }
        )
    ),
    Msg2 = #{ path => <<"Compute">>, <<"Slot">> => 1 },
    {ok, Msg3} = hb_converge:resolve(Msg1, Msg2, #{}),
    ?event({computed_message, {msg3, Msg3}}),
    ?assertEqual(1, hb_converge:get(<<"Results/Assignment-Slot">>, Msg3, #{})),
    ?assertEqual([1,1,0,0], hb_converge:get(<<"Already-Seen">>, Msg3, #{})).

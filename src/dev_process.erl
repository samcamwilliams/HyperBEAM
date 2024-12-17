-module(dev_process).
%%% Public API
-export([info/2, compute/3, schedule/3, slot/3, now/3]).
%%% Test helpers
-export([test_process/0, test_wasm_process/0]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%%% @moduledoc This module contains the device implementation of AO processes
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
%%% `dev_process_cache` for details.
%%% 
%%% The external API of the device is as follows:
%%% 
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
%%%         Quorum: 2
%%%
%%% Runtime options:
%%%     Cache-Frequency: The number of assignments that can pass before
%%%                      the full state should be cached.
%%%     Cache-Keys:      A list of the keys that should be cached, in
%%%                      addition to `/Results`.

%% The frequency at which the process state should be cached. Can be overridden
%% with the `cache_frequency` option.
-define(DEFAULT_CACHE_FREQ, 10).

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
%% allows devices on the execution stack to initialize themselves.
init(Msg1, Msg2, Opts) ->
    {ok, Res} = run_as(<<"Execution-Device">>, Msg1, init, Opts),
    compute(Res, Msg2, Opts).

%% @doc Compute the result of an assignment applied to the process state, if it 
%% is the next message.
compute(Msg1, Msg2, Opts) ->
    ProcID =
        hb_converge:get(<<"Process/id">>, Msg1, Opts#{ hashpath := ignore }),
    % Get the nonce we are currently on and the inbound nonce.
    {ok, CurrentSlot} = hb_converge:get(<<"Current-Slot">>, Msg2, Opts, -1),
    {ok, TargetSlot} = hb_converge:get(<<"Assignment/Slot">>, Msg2, Opts),
    % If we do not have a live state, load or initialize one.
    {ok, Loaded} =
        case CurrentSlot of
            -1 ->
                % Try to load the latest complete state from disk.
                case dev_process_cache:latest(ProcID, TargetSlot, Opts) of
                    {ok, LoadedSlot, MsgFromCache} ->
                        % Boot the devices in the executor stack with the
                        % loaded state.
                        ?event({loaded_state_checkpoint, ProcID, LoadedSlot}),
                        run_as(<<"Execution-Device">>, MsgFromCache, boot, Opts);
                    not_found ->
                        % If we do not have a checkpoint, initialize the
                        % process from scratch.
                        init(Msg1, Msg2, Opts)
                end;
            _ -> {ok, Msg1}
        end,
    do_compute(Loaded, Msg2, TargetSlot, Opts).

%% @doc Continually get the next assignment from the scheduler until we
%% are up-to-date.
do_compute(Msg1, Msg2, TargetSlot, Opts) ->
    case hb_converge:get(<<"Slot">>, Msg2, Opts) of
        CurrentSlot when CurrentSlot == TargetSlot ->
            % We reached the target height so we return.
            {ok, Msg1};
        CurrentSlot ->
            % Get the next input from the scheduler device.
            {ok, #{ <<"Message">> := ToProcess, <<"State">> := State }} =
                next(Msg1, Msg2, Opts),
            % Calculate how much of the state should be cached.
            Freq = hb_opts:get(cache_frequency, Opts, ?DEFAULT_CACHE_FREQ),
            CacheKeys =
                case CurrentSlot rem Freq of
                    0 -> all;
                    _ -> [<<"Results">>]
                end,
            {ok, Msg3} =
                run_as(
                    <<"Execution-Device">>,
                    State,
                    ToProcess,
                    Opts#{ cache_keys := CacheKeys }
                ),
            do_compute(Msg3, Msg2, TargetSlot, Opts)
    end.

%% @doc Returns the `/Results` key of the latest computed message.
now(Msg1, _Msg2, Opts) ->
    CurrentSlot = hb_converge:get(<<"Current-Slot">>, Msg1, Opts),
    ProcessID = hb_converge:get(<<"Process/id">>, Msg1, Opts),
    ?event({now_called, {process, ProcessID}, {slot, CurrentSlot}}),
    {ok, Msg3} = dev_process_cache:read(ProcessID, CurrentSlot, Opts),
    {ok, hb_converge:get(<<"Results">>, Msg3, Opts)}.

%% @doc Run a message against Msg1, with the device being swapped out for
%% the device found at `Key`. After execution, the device is swapped back
%% to the original device.
run_as(Key, Msg1, Msg2, Opts) ->
    BaseDevice = hb_converge:get(<<"Device">>, {as, dev_message, Msg1}, Opts),
    ?event({running_as, {key, Key}, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    PreparedMsg =
        hb_converge:set(
            Msg1,
            #{
                device => 
                    DeviceSet = hb_converge:get(
                        Key,
                        {as, dev_message, Msg1},
                        Opts
                    ),
                <<"Process">> =>
                    case hb_converge:get(
                        <<"Process">>,
                        {as, dev_message, Msg1},
                        Opts#{ hashpath => ignore }
                    ) of
                        not_found ->
                            Msg1;
                        Process ->
                            Process
                    end
            }
        ),
    ?event({prepared_msg, {msg1, PreparedMsg}, {msg2, Msg2}}),
    {ok, BaseResult} =
        hb_converge:resolve(
            PreparedMsg,
            Msg2,
            Opts
        ),
    ?event({base_result_before_device_swap_back, BaseResult}),
    case BaseResult of
        #{ device := DeviceSet } ->
            {ok, hb_converge:set(BaseResult, #{ device => BaseDevice })};
        _ ->
            ?event({returning_base_result, BaseResult}),
            {ok, BaseResult}
    end.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    ok.

test_process() ->
    Wallet = hb:wallet(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    #{
        device => ?MODULE,
        <<"Execution-Device">> => <<"Stack/1.0">>,
        <<"Scheduler-Device">> => <<"Scheduler/1.0">>,
        <<"Scheduler-Location">> => Address,
        <<"Device-Stack">> => [dev_wasm],
        <<"WASM-Image">> => <<"wasm-image-id">>,
        <<"Type">> => <<"Process">>,
        <<"Test-Key-Random-Number">> => rand:uniform(1337),
        <<"Scheduler-Authority">> => <<"scheduler-id">>
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
    init(),
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
    init(),
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

test_wasm_process() ->
    maps:merge(test_process(), #{
        <<"Device-Stack">> => [dev_vfs, dev_wasm],
        <<"WASM-Image">> => <<"test/test-standalone-wex-aos.wasm">>
    }).

recursive_resolve_test() ->
    init(),
    Msg1 = test_wasm_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    CurrentSlot = hb_converge:resolve(Msg1, #{ path => [<<"Slot">>, <<"Current-Slot">>] }, #{}),
    ?event({resolved_current_slot, CurrentSlot}),
    ?assertMatch(
        CurrentSlot when CurrentSlot > 0,
        CurrentSlot
    ),
    ok.
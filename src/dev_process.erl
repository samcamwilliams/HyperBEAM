-module(dev_process).
-export([info/2, compute/3, schedule/3, slot/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%%% @moduledoc This module contains notes for the implmenetation of AO processes
%%% in Converge.
%%% 
%%% The ideal API for an AO process would be:
%%% 
%%% GET /ID/scheduler <- Returns the messages in the schedule
%%% POST /ID/scheduler <- Adds a message to the schedule
%%% 
%%% GET /ID/Computed/[IDorSlotNum]/Result <- Returns the state of the process
%%%                                         after applying a message
%%% POST /ID/Computed/ <- Compute and cache a message on the process.
%%% 
%%% Process definition:
%%%     Device: Process/1.0
%%%     Scheduler: [Message]
%%%         Device: Scheduler/1.0
%%%         Authority: SchedulerID
%%%     Cron: Cron/1.0
%%%     WASM:
%%%         Device: WASM/1.0
%%%         Image: WASMImageID
%%%     PoDA:
%%%         Device: PoDA/1.0
%%%         Authority: A
%%%         Authority: B
%%%         Authority: C
%%%         Quorum: 2
%%%     Execution-Stack: Scheduler, Cron, WASM, PoDA
%%%     

%% @doc When the info key is called, we should return the process exports.
info(_Msg1, _Opts) ->
    #{
        exports => [compute, schedule, slot]
    }.

%% @doc Wraps the schedule function in the Scheduler device.
schedule(Msg1, Msg2, Opts) ->
    run_as(<<"Scheduler">>, Msg1, Msg2, Opts).

%% @doc Wraps the slot function in the Scheduler device.
slot(Msg1, Msg2, Opts) ->
    run_as(<<"Scheduler">>, Msg1, Msg2, Opts).

%% @doc Before computation begins, a boot phase is required. This phase
%% allows devices on the execution stack to initialize themselves.
init(Msg1, Msg2, Opts) ->
    run_as(<<"Execution-Stack">>, Msg1, Msg2, Opts).

%% @doc When a computed result is requested, we should check the cache for the
%% result. If it is not present, we should compute the result and cache it.
compute(Msg1, Msg2, Opts) ->
    {ok, Initialized} =
        case hb_converge:get(<<"Initialized">>, {as, dev_message, Msg1}, Opts) of
            not_found ->
                init(Msg1, Msg2, Opts);
            Result ->
                Result
        end,
    hb_converge:resolve(
        hb_converge:set(Initialized, #{ device => <<"Stack/1.0">> }),
        Msg2,
        Opts
    ).

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
            {ok, BaseResult}
    end.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    ok.

basic_test_process() ->
    #{
        device => ?MODULE,
        <<"Executor">> => <<"Stack/1.0">>,
        <<"Scheduler">> => <<"Scheduler/1.0">>,
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
    Msg1 = basic_test_process(),
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
    Msg1 = basic_test_process(),
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

basic_wasm_test_process() ->
    maps:merge(basic_test_process(), #{
        <<"Device-Stack">> => [dev_vfs, dev_wasm],
        <<"WASM-Image">> => <<"test/test-standalone-wex-aos.wasm">>
    }).

basic_wasm_process_test() ->
    init(),
    Msg1 = basic_wasm_test_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    schedule_test_message(Msg1, <<"TEST TEXT 3">>),
    ?assertMatch(
        CurrentSlot when CurrentSlot > 0,
        hb_converge:get(<<"Slot/Current-Slot">>, Msg1)
    ),
    ok.
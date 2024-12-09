-module(dev_process).
-export([info/2, scheduler/3, compute/3]).
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

%% @doc When the info key is called, we should return the process definition.
info(_Msg1, _Opts) ->
    #{
        exports => [scheduler, compute],
        default_handler => dev_message
    }.

%% @doc When the scheduler key is called, we should run the request through
%% the scheduler.
scheduler(Msg1, Msg2, Opts) ->
    hb_converge:resolve(
        #{
            device =>
                hb_converge:get_as(
                    dev_message, <<"Scheduler">>, Msg1, Opts
                ),
            process => Msg1
        },
        Msg2,
        Opts
    ).

%% @doc Before computation begins, a boot phase is required. This phase
%% allows devices on the execution stack to initialize themselves.
init(Msg1, Msg2, Opts) ->
    run_as(<<"Execution-Stack">>, Msg1, Msg2, Opts).

%% @doc When a computed result is requested, we should check the cache for the
%% result. If it is not present, we should compute the result and cache it.
compute(Msg1, Msg2, Opts) ->
    {ok, Initialized} =
        case hb_converge:get_as(dev_message, <<"Initialized">>, Msg1, Opts) of
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
    BaseDevice = hb_converge:get_as(dev_message, <<"Device">>, Msg1, Opts),
    {ok, BaseResult} =
        hb_converge:resolve(
            hb_converge:set(
                Msg1,
                #{
                    device => <<"Stack/1.0">>,
                    <<"Device-Stack">> =>
                        hb_converge:get_as(
                            dev_message,
                            Key,
                            Msg1,
                            Opts
                        ),
                    path => <<"Init">>,
                    <<"Process">> =>
                        case hb_converge:get_as(
                            dev_message, <<"Process">>, Msg1, Opts
                        ) of
                            not_found ->
                                Msg1;
                            Process ->
                                Process
                        end
                }
            ),
            Msg2,
            Opts
        ),
    {ok, hb_converge:set(BaseResult, #{ device => BaseDevice })}.

%%% Tests

test_process() ->
    #{
        device => ?MODULE,
        process => #{
            <<"Execution-Stack">> => [dev_cron, dev_wasm],
            <<"WASM-Image">> => <<"wasm-image-id">>,
            <<"Type">> => <<"Process">>,
            <<"Exciting-Random-Number">> => rand:uniform(1337)
        }
    }.

schedule_on_process_test() ->
    Msg1 = test_process(),
    Msg2 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Exciting">> => <<"Reasonably">>
            }
    },
    Msg3 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Exciting">> => <<"Getting old.">>
            }
    },
    ?assertMatch(
        {ok, _},
        hb_converge:resolve(Msg1, Msg2, #{})
    ),
    ?assertMatch(
        {ok, _},
        hb_converge:resolve(Msg1, Msg3, #{})
    ),
    ?assertMatch(
        {ok, _},
        ?event(hb_converge:resolve(Msg1, #{
            <<"Method">> => <<"GET">>,
            <<"Path">> => <<"Schedule">>
        }, #{}))
    ).

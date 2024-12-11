%%% @doc This module contains notes for the implmenetation of AO processes
%%% in Converge.
%%% 
%%% The ideal API for an AO process would be:
%%% ```
%%% GET /ID/scheduler <- Returns the messages in the schedule
%%% POST /ID/scheduler <- Adds a message to the schedule
%%% 
%%% GET /ID/Computed/[IDorSlotNum]/Result <- Returns the state of the process
%%%                                         after applying a message
%%% POST /ID/Computed/ <- Compute and cache a message on the process.'''
%%% 
%%% Process definition:
%%% ```
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
%%%     Stack: Scheduler, Cron, WASM, PoDA'''
%%%     
-module(dev_process).
-export([info/2, scheduler/3, compute/3]).

%% @doc When the info key is called, we should return the process definition.
info(_Msg1, _Opts) ->
    #{
        exports => [scheduler, compute],
        default_handler => dev_message
    }.

%% @doc When the scheduler key is called, we should run the request through
%% the scheduler.
scheduler(Msg1, Msg2, Opts) ->
    {ok, Scheduler} = dev_message:get(scheduler, Msg1, Opts),
    hb_converge:resolve(
        #{
            device => Scheduler,
            process => Msg1
        },
        Msg2,
        Opts
    ).

%% @doc When a computed result is requested, we should check the cache for the
%% result. If it is not present, we should compute the result and cache it.
compute(Msg1, Msg2, Opts) ->
    %case hb_cache:output(Msg1, Msg2, Opts) of
    case not_found of
        {ok, Result} -> {ok, Result};
        not_found ->
            Initialized =
                case hb_converge:get(initialized, Msg1, Opts) of
                    {ok, _Initialized} ->
                        Msg1;
                    not_found ->
                        {ok, Init} = hb_converge:resolve(
                            hb_converge:set(Msg1, #{ device => stack }),
                            #{ path => <<"Init">> },
                            Opts
                        ),
                        Init
                end,
            hb_converge:resolve(
                hb_converge:set(Initialized, #{ device => <<"Stack/1.0">> }),
                Msg2,
                Opts
            )
    end.
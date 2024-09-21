-module(cu_process).
-export([simple_stack_test/0]).
-export([start/2, start/3]).
-export([state/1, result/2]).

-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A process is represented as a queue of messages and a stack of components.
%%% Each AO process runs as an Erlang process consuming messages from -- and placing items
%%% into -- its schedule.
%%% 
%%% Components in a process's stack may return either `ok` or `pass` as their output.
%%% In the case of `ok`, the process will continue to the next device in its stack, or
%%% begin to process the next message in its schedule.
%%% In the case of `pass`, the process will start again from the first device in its stack,
%%% with the state transitions from the device executions thus far will be persisted and the 
%%% `pass` count in the state incremented.
%%% 
%%% The core components of this framework are:
%%% 
%%% DevMod:init(Params, State) -> {ok, State}
%%% 
%%% start(ProcMsg, Schedule) -> ErlangProcessID
%%% 
%%% process(Message, State#{ schedule := Schedule, devices := Devices }) -> 
%%%     {Result, Schedule, Devices}
%%% 
%%% DeviceMod:execute(Message, State) -> {ok, State} | {break, State} | {pass, State}
%%%     | {stop, Reason, State}
%%%
%%% This architecture also allows for parralelization of device execution. Each device can
%%% exposes a `DevMod:uses()` function that returns a list of state components that it
%%% employs in its execution:
%%% 
%%% DeviceMod:uses() -> all | [StateComponentNameAtom | {StateComponentNameAtom, read | write }]
%%% 
%%% When no specifier is given, it is assumed that the device will read from and write to
%%% the given state key. When no `DeviceMod:uses()` function is provided, it is assumed
%%% that the device will read and write to all state components (as with the `all` specifier).
%%% 
%%% An example process may look something like this:
%%%     Device: Scheduler
%%%     Location: [SchedulerAddress]
%%%     Device: Dedup
%%%     Variant: 1.0
%%%     Device: JSON-Interface
%%%     Variant: wasm64-aos
%%%     Device: wasm
%%%     Variant: wasm64-wasi_preview1-unknown
%%% 
%%% The host environment may then additionally add Checkpoint and Messenging devices in order
%%% to operate like a MU or CU interface.

%% Start a new Erlang process for the AO process, optionally giving the assignments so far.
start(Process) -> start(Process, #{}).
start(Process, Opts) ->
    spawn(fun() -> boot(Process, Opts) end).

run(Process) -> run(Process, Opts)
run(Process, RawOpts) ->
    Self = self(),
    Opts = RawOpts#{
        post => maps:get(post, RawOpts, []) ++ [
                {
                    dev_monitor,
                    [
                        fun(end_of_schedule, S) ->
                            Self ! {end_of_sechedule, ao_message:id(S#{ process })};
                        (Msg, S) ->
                            Self ! {result, self(), ao_message:id(Msg), S#{ result } }
                        end
                    ]
                }
            ]
    },
    ProcPID = spawn_monitor(fun() -> boot(Process, Opts) end),
    monitor(ProcID)

monitor(ProcID) ->
    receive
        {result, ProcID, MsgID, Result} ->
            ao:c({intermediate_result, ID, Result}),
            [{MsgID, Result}|monitor(Process, Wallet, ProcPID, Result)];
        {end_of_schedule, ProcID} ->
            ProcID ! {self(), shutdown(ProcID)},
            [];
        Else -> [Else]
    end.

boot(Process, Opts) ->
    InitState = #{
        process := Process,
        wallet := maps:get(wallet, Opts, ao:wallet()),
        schedule := maps:get(schedule, Opts, []),
        devices :=
            cu_device:normalize_devs(
                maps:get(pre, Opts, []),
                Process,
                maps:get(post, Opts, [])
            )
    },
    case cu_device_stack:call(InitState, init) of
        {ok, State} ->
            execute_sched(State);
        {error, N, DevMod, Info} ->
            throw({error, boot, N, DevMod, Info})
    end.

execute_schedule(State) ->
    case State of
        #{ schedule := [] } ->
            case execute_eos(State) of
                NS#{ schedule := [] } ->
                    await_command(State);
                NS ->
                    execute_schedule(NS)
            end;
        #{ schedule := Sched = [Msg | NextSched] } ->
            case execute_msg(Msg, State) of
                {ok, NewState = #{ schedule := [Msg|NextSched]}} ->
                    execute_schedule(NewState#{ schedule := NextSched });
                {ok, NewState} ->
                    ao:c({schedule_updated, not_popping}),
                    execute_schedule(NewState)
                Err = {error, Err} ->
                    shutdown(State),
                    throw(Err)
            end
    end.

execute_msg(Msg, State) ->
    cu_device_stack:call(State, execute, #{ pre => [Msg] }).
   
%% After execution of the current schedule has finished the Erlang process should
%% enter a hibernation state, waiting for either more work or termination.
await_command(State) ->
    receive
        {add, Msg} ->
            execute_sched(State#{ schedule := [Msg | State#{ schedule } ] });
        stop -> shutdown(State)
    end.

%%% Tests

simple_stack_test() ->
    Proc =
        #tx {
            tags = [
                {<<"Device">>, <<"JSON-Interface">>},
                {<<"Device">>, <<"WASM64-pure">>},
                {<<"Image">>, <<"aos64-pure.wasm">>}
                {<<"Device">>, <<"Cron">>},
                {<<"Time">>, <<"100-Milliseconds">>}
                {<<"Device">>, <<"Checkpoint">>}
            ]
        },
    Schedule =
        [
            #tx {
                tags = [
                    {<<"Action">>, <<"Eval">>}
                ],
                Data = <<"return 1+1">>
            }
        ]
    Res = run(Proc, #{ schedule => Schedule }),
    Res.
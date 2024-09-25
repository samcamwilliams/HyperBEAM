-module(cu_process).
-export([start/1, start/2]).
-export([run/1, run/2]).
-export([test/0]).

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

run(Process) -> run(Process, #{}).
run(Process, RawOpts) ->
    Self = self(),
    Opts = RawOpts#{
        post => maps:get(post, RawOpts, []) ++ [
                { dev_monitor, [ fun(S, Event) -> Self ! {monitor, S, Event} end ] }
            ]
    },
    monitor(element(1, spawn_monitor(fun() -> boot(Process, Opts) end))).

monitor(ProcID) ->
    receive
        {monitor, State, end_of_schedule} ->
            ProcID ! {self(), shutdown},
            [State];
        {monitor, State, {message, Message}} ->
            ao:c({intermediate_result, maps:get(result, State, no_result)}),
            [{message_processed, ao_message:id(Message), maps:get(result, State, {no_result, State})}|monitor(ProcID)];
        Else -> [Else]
    end.

boot(Process, Opts) ->
    InitState = #{
        process => Process,
        wallet => maps:get(wallet, Opts, ao:wallet()),
        schedule => maps:get(schedule, Opts, []),
        devices =>
            cu_device_stack:normalize(
                maps:get(pre, Opts, []),
                Process,
                maps:get(post, Opts, [])
            )
    },
    case cu_device_stack:call(InitState, init) of
        {ok, State} ->
            execute_schedule(State, Opts);
        {error, N, DevMod, Info} ->
            throw({error, boot, N, DevMod, Info})
    end.

execute_schedule(State, Opts) ->
    case State of
        #{ schedule := [] } ->
            case execute_eos(State, Opts) of
                {ok, #{ schedule := [] }} ->
                    await_command(State, Opts);
                {ok, NS} ->
                    execute_schedule(NS, Opts);
                {error, DevNum, DevMod, Info} ->
                    execute_terminate(State#{ errors := maps:get(errors, State, []) ++ [{DevNum, DevMod, Info}] }, Opts)
            end;
        #{ schedule := [Msg | NextSched] } ->
            case execute_message(Msg, State, Opts) of
                {ok, NewState = #{ schedule := [Msg|NextSched]}} ->
                    execute_schedule(NewState#{ schedule := NextSched }, Opts);
                {ok, NewState} ->
                    ao:c({schedule_updated, not_popping}),
                    execute_schedule(NewState, Opts);
                {error, DevNum, DevMod, Info} ->
                    ao:c({error, {DevNum, DevMod, Info}}),
                    execute_terminate(State#{ errors := maps:get(errors, State, []) ++ [{DevNum, DevMod, Info}] }, Opts)
            end
    end.

execute_message(Msg, State, Opts) ->
    cu_device_stack:call(State, execute, Opts#{ arg_prefix => [Msg] }).

execute_terminate(S, Opts) ->
    cu_device_stack:call(S, terminate, Opts).

execute_eos(S, Opts) ->
    cu_device_stack:call(S, end_of_schedule, Opts).

%% After execution of the current schedule has finished the Erlang process should
%% enter a hibernation state, waiting for either more work or termination.
await_command(State = #{ schedule := Sched }, Opts) ->
    receive
        {add, Msg} ->
            execute_schedule(State#{ schedule := [Msg | Sched ] }, Opts);
        stop -> execute_terminate(State, Opts)
    end.

%%% Tests

test() -> 
    simple_stack_test().

simple_stack_test() ->
    Wallet = ao:wallet(),
    Authority = ar_wallet:to_address(Wallet),
    RawProc =
        ar_bundles:sign_item(#tx {
            tags = [
                {<<"Protocol">>, <<"ao">>},
                {<<"Variant">>, <<"ao.tn.1">>},
                {<<"Module">>, <<"aos-2-pure">>},
                {<<"Authority">>, ar_util:encode(Authority)},
                {<<"Device">>, <<"JSON-Interface">>},
                {<<"Device">>, <<"WASM64-pure">>},
                {<<"Image">>, <<"aos-2-pure.wasm">>},
                {<<"Device">>, <<"Cron">>},
                {<<"Time">>, <<"100-Milliseconds">>},
                {<<"Device">>, <<"Checkpoint">>}
            ]
        }, Wallet),
    Proc = RawProc#tx {id = <<"TEST_ID">>},
    Schedule =
        [
            ar_bundles:sign_item(#tx {
                target = <<"TEST_ID">>,
                tags = [
                    {<<"Action">>, <<"Eval">>}
                ],
                data = <<"return 1+1">>
            }, Wallet)
        ],
    ao:c({schedule, jiffy:encode(ar_bundles:item_to_json_struct(hd(Schedule)))}),
    Res = run(Proc, #{ schedule => Schedule, error_strategy => stop }),
    Res,
    ok.
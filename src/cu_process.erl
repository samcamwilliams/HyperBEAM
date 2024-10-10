-module(cu_process).
-export([start/1, start/2]).
-export([run/1, run/2]).
-export([generate_test_data/0]).

-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A process is a specific type of AO combinator, represented as a stack of components.
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

%% The default frequency for checkpointing is 2 slots.
-define(DEFAULT_FREQ, 2).

%% Start a new Erlang process for the AO process, optionally giving the assignments so far.
start(Process) -> start(Process, #{}).
start(Process, Opts) ->
    spawn(fun() -> boot(Process, Opts) end).

run(Process) -> run(Process, #{}).
run(Process, RawOpts) ->
    Self = self(),
    Opts = RawOpts#{
        post => maps:get(post, RawOpts, []) ++
            [
                {dev_monitor, [fun(S, Event) -> Self ! {monitor, S, Event} end]}
            ]
    },
    monitor(element(1, spawn_monitor(fun() -> boot(Process, Opts) end))).

monitor(ProcID) ->
    receive
        {monitor, State, end_of_schedule} ->
            ProcID ! {self(), shutdown},
            [];
        {monitor, State, {message, Message}} ->
            [
                {message_processed, ao_message:id(Message),
                    maps:get(result, State, {no_result, State})}
                | monitor(ProcID)
            ];
        Else ->
            [Else]
    end.

%% Process execution flow =>
%% Boot:    Read from cache, build init state, run init() on all devices.
%% Execute: Run execute_schedule for the slot.
%%          Run execute_message for each device.
%%          Run checkpoint on all devices if the current slot is a checkpoint slot.
%%          Cache the result of the computation if the caller requested it.
%%          Repeat for the next slot.
%% EOS:     Run end_of_schedule() on all devices to see if there is more work to be done.
%% Waiting: Either wait for a new message to arrive, or exit as requested.
boot(Process, Opts) ->
    {Slot, Checkpoint} =
        case ao_cache:latest(maps:get(store, Opts, ao:get(store)), Process#tx.id) of
            not_found ->
                {0, #{}};
            {LatestSlot, State} ->
                {LatestSlot, State#tx.data}
    end,
    InitState =
        (Checkpoint)#{
            process => Process,
            slot => Slot,
            to_message => maps:get(to_message, Opts, inf),
            to => maps:get(to, Opts, inf),
            wallet => maps:get(wallet, Opts, ao:wallet()),
            store => maps:get(store, Opts, ao:get(store)),
            schedule => maps:get(schedule, Opts, []),
            devices =>
                cu_device_stack:normalize(
                    maps:get(pre, Opts, []),
                    Process,
                    maps:get(post, Opts, [])
                )
        },
    case cu_device_stack:call(InitState, init) of
        {ok, StateAfterInit} ->
            execute_schedule(StateAfterInit, Opts);
        {error, N, DevMod, Info} ->
            throw({error, boot, N, DevMod, Info})
    end.

execute_schedule(State, Opts) ->
    case State of
        #{schedule := []} ->
            case execute_eos(State, Opts) of
                {ok, #{schedule := []}} ->
                    await_command(State, Opts);
                {ok, NS} ->
                    execute_schedule(NS, Opts);
                {error, DevNum, DevMod, Info} ->
                    ao:c({error, {DevNum, DevMod, Info}}),
                    execute_terminate(
                        State#{errors := maps:get(errors, State, []) ++ [{DevNum, DevMod, Info}]},
                        Opts
                    )
            end;
        #{schedule := [Msg | NextSched]} ->
            case execute_message(Msg, State, Opts) of
                {ok, NewState = #{schedule := [Msg | NextSched]}} ->
                    execute_checkpoint(Msg, NewState#{schedule := NextSched}, Opts);
                {ok, NewState} ->
                    ao:c({schedule_updated, not_popping}),
                    execute_checkpoint(Msg, NewState, Opts);
                {error, DevNum, DevMod, Info} ->
                    ao:c({error, {DevNum, DevMod, Info}}),
                    execute_terminate(
                        State#{errors := maps:get(errors, State, []) ++ [{DevNum, DevMod, Info}]},
                        Opts
                    )
            end
    end.

execute_checkpoint(
    Msg,
    State =
        #{
            store := Store,
            result := Result,
            process := Process,
            slot := Slot,
            wallet := Wallet
        },
    Opts) ->
    ?c({checkpointing, Slot}),
    case is_checkpoint_slot(State, Opts) of
        true ->
            % Run checkpoint on the device stack, but we do not propagate the result.
            {ok, CheckpointState} =
                cu_device_stack:call(
                    State#{ save_keys => [] },
                    checkpoint,
                    Opts
                ),
            ?c({checkpoint_result, maps:keys(CheckpointState)}),
            Checkpoint =
                maps:from_list(lists:map(
                    fun(Key) ->
                        case is_record(maps:get(Key, CheckpointState), tx) of
                            true ->
                                {Key, maps:get(Key, CheckpointState)};
                            false -> throw({error, checkpoint_result_not_tx, Key})
                        end
                    end,
                    ?c(maps:get(save_keys, CheckpointState, []))
                )),
            ?c({checkpoint_result_normalized, maps:keys(Checkpoint)}),
            ao_cache:write_output(
                Store,
                Process#tx.id,
                Slot,
                ar_bundles:sign_item(
                    ar_bundles:normalize(#tx { data = Checkpoint#{ <<"Result">> => Result } }),
                    Wallet
                )
            );
        false ->
            ao_cache:write_output(
                Store,
                Process#tx.id,
                Slot,
                ar_bundles:sign_item(#{ <<"Result">> => Result }, Wallet)
            )
    end,
    execute_schedule(State, Opts).

execute_message(Msg, State, Opts) ->
    cu_device_stack:call(State, execute, Opts#{arg_prefix => [Msg]}).

execute_terminate(S, Opts) ->
    cu_device_stack:call(S, terminate, Opts).

execute_eos(S, Opts) ->
    cu_device_stack:call(S, end_of_schedule, Opts).

is_checkpoint_slot(State, Opts) ->
    (maps:get(checkpoint, Opts, fun(_) -> false end))(State)
        orelse maps:get(slot, State) rem maps:get(freq, Opts, ?DEFAULT_FREQ) == 0.

%% After execution of the current schedule has finished the Erlang process should
%% enter a hibernation state, waiting for either more work or termination.
await_command(State = #{schedule := Sched}, Opts) ->
    receive
        {add, Msg} ->
            % TODO: Should we run `end_of_schedule` or `new_item` (or something)
            % here?
            execute_schedule(State#{schedule := [Msg | Sched]}, Opts);
        stop ->
            execute_terminate(State, Opts)
    end.

%%% TESTS

simple_stack_test_ignore() ->
    Wallet = ao:wallet(),
    Authority = ar_wallet:to_address(Wallet),
    Proc =
        ar_bundles:sign_item(
            #tx{
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
            },
            Wallet
        ),
    Msg = ar_bundles:sign_item(
        #tx{
            target = ar_util:encode(Proc#tx.id),
            tags = [
                {<<"Action">>, <<"Eval">>}
            ],
            data = <<"return 1+1">>
        },
        Wallet
    ),
    Schedule =
        [
            #tx{
                data = #{
                    <<"Message">> => Msg,
                    <<"Assignment">> => ar_bundles:sign_item(
                        #tx{
                            target = ar_util:encode(Proc#tx.id),
                            tags = [
                                {<<"Slot">>, <<"0">>},
                                {<<"Message">>, ar_util:encode(Msg#tx.id)},
                                {<<"Block-Height">>, <<"0">>},
                                {<<"Timestamp">>, <<"1234567890">>},
                                {<<"Process">>, ar_util:encode(Proc#tx.id)}
                            ]
                        },
                        Wallet
                    )
                }
            }
        ],
    [{message_processed, _, TX} | _] = 
        run(Proc, #{schedule => Schedule, error_strategy => stop, wallet => Wallet}),
    ao:c({simple_stack_test_result, TX#tx.data}),
    ok.

full_push_test_() ->
    {timeout, 150, ?_assert(full_push_test())}.

full_push_test() ->
    application:ensure_all_started(ao),
    Msg = generate_test_data(),
    ao_cache:write(ao:get(store), Msg),
    ao_client:push(Msg, none).

generate_test_data() ->
    Store = ao:get(store),
    Wallet = ao:wallet(),
    ID = ar_wallet:to_address(Wallet),
    {ok, Module} = file:read_file("test/aos-2-pure.wasm"),
    ao_cache:write(
        Store,
        Img = ar_bundles:sign_item(
            #tx {
                tags = [
                    {<<"Protocol">>, <<"ao">>},
                    {<<"Variant">>, <<"ao.tn.2">>},
                    {<<"Type">>, <<"Image">>}
                ],
                data = Module
            },
            Wallet
        )
    ),
    ao_cache:write(
        Store,
        Signed = ar_bundles:sign_item(
            #tx{
                tags = [
                    {<<"Protocol">>, <<"ao">>},
                    {<<"Variant">>, <<"ao.tn.2">>},
                    {<<"Type">>, <<"Process">>},
                    {<<"Authority">>, ar_util:encode(ID)},
                    {<<"Device">>, <<"Scheduler">>},
                    {<<"Location">>, ar_util:encode(ID)},
                    {<<"Device">>, <<"JSON-Interface">>},
                    {<<"Device">>, <<"WASM64-pure">>},
                    {<<"Image">>, ar_util:encode(Img#tx.id)},
                    {<<"Module">>, <<"aos-2-pure">>},
                    {<<"Device">>, <<"Cron">>},
                    {<<"Time">>, <<"100-Milliseconds">>}
                ]
            },
            Wallet
        )
    ),
    Msg = ar_bundles:sign_item(
        #tx{
            target = Signed#tx.id,
            tags = [
                {<<"Protocol">>, <<"ao">>},
                {<<"Variant">>, <<"ao.tn.2">>},
                {<<"Type">>, <<"Message">>},
                {<<"Action">>, <<"Eval">>}
            ],
            data =
                <<
                    "\n"
                    "Handlers.add(\"Ping\", function(m) Send({ Target = ao.id, Action = \"Ping\" }); print(m.From); print(\"Sent Ping\"); end)\n"
                    "Send({ Target = ao.id, Action = \"Ping\" })\n"
                    "print(\"Setup Handler and pushed message\" .. ao.id )\n"
                >>
        },
        Wallet
    ),
    ao_cache:write(Store, Msg),
    ?c({test_data_written, {proc, ar_util:encode(Signed#tx.id)}, {msg, ar_util:encode(Msg#tx.id)}}),
    Msg.

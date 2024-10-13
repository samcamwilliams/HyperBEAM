-module(cu_process).
-export([start/1, start/2, result/4]).
-export([run/1, run/2, run/3]).
-export([generate_test_data/0]).

-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").
-ao_debug(print).

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
-define(DEFAULT_FREQ, 10).

result(ProcID, MsgRef, Store, Wallet) ->
    case ao_cache:read_output(Store, ProcID, MsgRef) of
        not_found ->
            ?c({proc_id, ar_util:encode(ProcID)}),
            case gproc:lookup_pids({n, l, {cu, 1}}) of
                [] ->
                    ?c({no_cu_for_proc, ar_util:encode(ProcID)}),
                    Proc = ao_cache:read(Store, ProcID),
                    await_results(
                        cu_process:run(
                            Proc,
                            #{to => MsgRef, store => Store, wallet => Wallet, on_idle => wait},
                            create_monitor_for_message(MsgRef)
                        )
                    );
                [Pid|_] ->
                    ?c({found_cu_for_proc, ar_util:encode(ProcID)}),
                    Pid ! {on_idle, run, add_monitor, [create_monitor_for_message(MsgRef)]},
                    Pid ! {on_idle, message, MsgRef},
                    ?c({added_listener_and_message, Pid}),
                    await_results(Pid)
            end;
        Result -> Result
    end.

%% Start a new Erlang process for the AO process, optionally giving the assignments so far.
start(Process) -> start(Process, #{}).
start(Process, Opts) ->
    spawn(fun() -> boot(Process, Opts) end).

run(Process) -> run(Process, #{}).
run(Process, Opts) ->
    run(Process, Opts, create_persistent_monitor()).
run(Process, Opts, Monitor) when not is_list(Monitor) ->
    run(Process, Opts, [Monitor]);
run(Process, RawOpts, Monitors) ->
    Opts = RawOpts#{
        post => maps:get(post, RawOpts, []) ++ [{dev_monitor, Monitors}]
    },
    element(1, spawn_monitor(fun() -> boot(Process, Opts) end)).

await_results(Pid) ->
    receive
        {result, Pid, _Msg, State} -> maps:get(results, State)
    end.

create_monitor_for_message(Msg) when is_record(Msg, tx) ->
    create_monitor_for_message(Msg#tx.id);
create_monitor_for_message(MsgID) ->
    Listener = self(),
    fun(S, {message, Inbound}) ->
        Assignment = maps:get(<<"Assignment">>, Inbound#tx.data),
        Slot =
            case lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags) of
                {<<"Slot">>, RawSlot} -> list_to_integer(binary_to_list(RawSlot));
                false -> no_slot
            end,
        ?c({slot, Slot}),
        case (Slot == MsgID) or (Assignment#tx.id == MsgID) of
            true -> Listener ! {result, self(), Inbound, S}, done;
            false -> ignored
        end;
        (_, _) -> ignored
    end.

create_persistent_monitor() ->
    Listener = self(),
    fun(S, {message, Msg}) ->
        Listener ! {result, self(), Msg, S};
        (_, _) -> ok
    end.

%% Process execution flow =>
%% Boot:    Check which devices need checkpoints and which they require
%%          Read the checkpoint data from the cache if available
%%          Build init state, mixing checkpoint data with baseline state
%%          Run init() on all devices
%% Execute: Run execute_schedule for the slot.
%%          Run execute_message for each device.
%%          Run checkpoint on all devices if the current slot is a checkpoint slot.
%%          Cache the result of the computation if the caller requested it.
%%          Repeat for the next slot.
%% EOS:     Run end_of_schedule() on all devices to see if there is more work to be done.
%% Waiting: Either wait for a new message to arrive, or exit as requested.
boot(Process, Opts) ->
    % Register the process with gproc so that it can be found by its ID.
    gproc:reg({n, l, {cu, 1}}),
    ?c({booting_process, ar_util:encode(Process#tx.id)}),
    % Build the device stack.
    Devs =
        cu_device_stack:normalize(
            maps:get(pre, Opts, []),
            Process,
            maps:get(post, Opts, [])
        ),
    % Get the store we are using for this execution.
    Store = maps:get(store, Opts, ao:get(store)),
    % Get checkpoint key names from all devices.
    {ok,
        #{keys := [Key|_]}} =
            cu_device_stack:call(#{devices => Devs, keys => []}, checkpoint_uses),
    % We don't support partial checkpoints (perhaps we never will?), so just take one key
    % and use that to find the latest full checkpoint.
    CheckpointOption =
        ao_cache:latest(
            Store,
            Process#tx.id,
            maps:get(to, Opts, inf),
            [Key]
        ),
    {Slot, Checkpoint} =
        case CheckpointOption of
            not_found ->
                ?c({wasm_no_checkpoint, maps:get(to, Opts, inf)}),
                {-1, #{}};
            {LatestSlot, State} ->
                ?c(wasm_checkpoint),
                {LatestSlot, State#tx.data}
    end,
    InitState =
        (Checkpoint)#{
            process => Process,
            slot => Slot + 1,
            to => maps:get(to, Opts, inf),
            wallet => maps:get(wallet, Opts, ao:wallet()),
            store => maps:get(store, Opts, ao:get(store)),
            schedule => maps:get(schedule, Opts, []),
            devices => Devs
                
        },
    ?c({running_init_on_slot, Slot + 1, maps:get(to, Opts, inf), maps:keys(Checkpoint)}),
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
                    ?c({error, {DevNum, DevMod, Info}}),
                    execute_terminate(
                        State#{errors := maps:get(errors, State, []) ++ [{DevNum, DevMod, Info}]},
                        Opts
                    )
            end;
        #{schedule := [Msg | NextSched]} ->
            case execute_message(Msg, State, Opts) of
                {ok, NewState = #{schedule := [Msg | NextSched]}} ->
                    post_execute(Msg, NewState#{schedule := NextSched}, Opts);
                {ok, NewState} ->
                    ?c({schedule_updated, not_popping}),
                    post_execute(Msg, NewState#{schedule := NextSched}, Opts);
                {error, DevNum, DevMod, Info} ->
                    ?c({error, {DevNum, DevMod, Info}}),
                    execute_terminate(
                        State#{errors := maps:get(errors, State, []) ++ [{DevNum, DevMod, Info}]},
                        Opts
                    )
            end
    end.

post_execute(
    Msg,
    State =
        #{
            store := Store,
            results := Results,
            process := Process,
            slot := Slot,
            wallet := Wallet
        },
    Opts) ->
    ?c({handling_post_execute_for_slot, Slot}),
    case is_checkpoint_slot(State, Opts) of
        true ->
            % Run checkpoint on the device stack, but we do not propagate the result.
            ?c({checkpointing_for_slot, Slot}),
            {ok, CheckpointState} =
                cu_device_stack:call(
                    State#{ save_keys => [] },
                    checkpoint,
                    Opts
                ),
            Checkpoint =
                ar_bundles:normalize(
                    #tx {
                        data =
                            maps:merge(
                                Results,
                                maps:from_list(lists:map(
                                    fun(Key) ->
                                        Item = maps:get(Key, CheckpointState),
                                        case is_record(Item, tx) of
                                            true -> {Key, Item};
                                            false -> throw({error, checkpoint_result_not_tx, Key})
                                        end
                                    end,
                                    maps:get(save_keys, CheckpointState, [])
                                ))
                            )
                    }
                ),
            ?c({checkpoint_normalized_for_slot, Slot}),
            ao_cache:write_output(
                Store,
                Process#tx.id,
                Slot,
                ar_bundles:sign_item(Checkpoint, Wallet)
            ),
            ?c({checkpoint_written_for_slot, Slot});
        false ->
            NormalizedResult = ar_bundles:deserialize(ar_bundles:serialize(Results)),
            ao_cache:write_output(
                Store,
                Process#tx.id,
                Slot,
                NormalizedResult
            ),
            ?c({result_written_for_slot, Slot})
    end,
    execute_schedule(initialize_slot(State), Opts).

initialize_slot(State = #{slot := Slot}) ->
    ?c({initializing_slot, Slot + 1}),
    State#{slot := Slot + 1, pass := 0, results := undefined }.

execute_message(Msg, State, Opts) ->
    cu_device_stack:call(State, execute, Opts#{arg_prefix => [Msg]}).

execute_terminate(S, Opts) ->
    cu_device_stack:call(S, terminate, Opts).

execute_eos(S, Opts) ->
    cu_device_stack:call(S, end_of_schedule, Opts).

is_checkpoint_slot(State, Opts) ->
    (maps:get(is_checkpoint, Opts, fun(_) -> false end))(State)
        orelse maps:get(slot, State) rem maps:get(freq, Opts, ?DEFAULT_FREQ) == 0.

%% After execution of the current schedule has finished the Erlang process should
%% enter a hibernation state, waiting for either more work or termination.
await_command(State, Opts = #{ on_idle := terminate }) ->
    execute_terminate(State, Opts);
await_command(State, Opts = #{ on_idle := wait }) ->
    ?c({awaiting_command, self()}),
    receive
        {on_idle, run, Function, Args} ->
            ?c({running_command, Function, Args}),
            {ok, NewState} =
                cu_device_stack:call(
                    State,
                    Function,
                    Opts#{arg_prefix => Args}
                ),
            await_command(NewState, Opts);
        {on_idle, message, MsgRef} ->
            ?c({received_message, MsgRef}),
            % TODO: Should we run `end_of_schedule` or `new_item` (or something)
            % here?
            {ok, NewState} = execute_eos(State#{ to => MsgRef }, Opts),
            execute_schedule(NewState, Opts);
        {on_idle, stop} ->
            ?c({received_stop_command}),
            execute_terminate(State, Opts);
        Other ->
            ?c({received_unknown_message, Other}),
            await_command(State, Opts)
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
    ?c({simple_stack_test_result, TX#tx.data}),
    ok.

full_push_test_() ->
    {timeout, 150, ?_assert(full_push_test())}.

full_push_test() ->
    ?c(full_push_test_started),
    Msg = generate_test_data(),
    ao_cache:write(ao:get(store), Msg),
    ao_client:push(Msg, none).

simple_load_test() ->
    ?c(scheduling_many_items),
    Messages = 30,
    Msg = generate_test_data(),
    ao_cache:write(ao:get(store), Msg),
    Start = ao:now(),
    Assignments = lists:map(
        fun(_) -> ao_client:schedule(Msg) end,
        lists:seq(1, Messages)
    ),
    Scheduled = ao:now(),
    {ok, LastAssignment} = lists:last(Assignments),
    ?c({scheduling_many_items_done_s, ((Scheduled - Start) / Messages) / 1000}),
    ao_client:compute(LastAssignment),
    Computed = ao:now(),
    ?c({compute_time_s, ((Computed - Scheduled) / Messages) / 1000}),
    ?c({total_time_s, ((Computed - Start) / Messages) / 1000}),
    ?c({processed_messages, Messages}).

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
                    "Handlers.add(\"Ping\", function(m) Send({ Target = ao.id, Action = \"Ping\" }); print(\"Sent Ping\"); end)\n"
                    "Send({ Target = ao.id, Action = \"Ping\" })\n"
                >>
        },
        Wallet
    ),
    ao_cache:write(Store, Msg),
    ?c({test_data_written, {proc, ar_util:encode(Signed#tx.id)}, {msg, ar_util:encode(Msg#tx.id)}}),
    Msg.

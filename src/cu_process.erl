-module(cu_process).
-export([start/2, start/3]).
-export([state/1, result/2]).

-include("include/ao.hrl").

%% Start a new Erlang process for the AO process, optionally giving the assignments so far.
start(Process, Wallet) ->
    BootReplyPID = self(),
    spawn(fun() -> boot(Process, Wallet, BootReplyPID) end).

start_wait(Process, Wallet) ->
    ProcPID = start(Process, Wallet),
    receive
        {boot, Process#tx.id, failed, Reason} ->
            {error, Reason};
        {boot, Process#tx.id, ok} ->
            {ok, ProcPID}
    end.

%% A recursive function that runs through each boot stage in turn, building the pstate
%% data for a new process.
boot(Process, Wallet, BootReplyPID) ->
    boot(register,
        #pstate {
            process = Process,
            wallet = Wallet,
            assignments = []
        },
        BootReplyPID
    ).

boot(register, S, ReplyID) ->
    not_implemented,
    boot(id, S, ReplyPID);
boot(id, S, ReplyPID) ->
    % Extract the PID for quick access
    boot({components, create}, S#pstate { id = (S#id.process)#tx.id }, ReplyPID);
boot({components, create}, S, ReplyPID) ->
    % Extract component modules from a process definition
    MachineMod = cu_component:tag_to_mod(Proc, <<"Machine">>),
    InterfaceMod = cu_component:tag_to_mod(Proc, <<"Interface">>),
    Transforms = cu_component:tags_to_mods(Proc, <<"Transform">>),
    Devices = cu_component:tags_to_mods(Proc, <<"Device">>),
    % Check that all components are implemented by the node.
    case lists:filter(
            fun({not_supported, Tag}) -> true; (_) -> false end,
            [MachineMod, InterfaceMod] ++ Transforms ++ Devices) of
        [] ->
            boot(
                validate_component_params,
                S#pstate {
                    vm_mod = MachineMod,
                    iface_mod = InterfaceMod,
                    transforms =
                        [ {TransMod, undefined} || TransMod <- Transforms ],
                    devices =
                        [ {DevMod, undefined} || DevMod <- Transforms ],
                },
                ReplyPID
            );
        UnsupportComponents ->
            boot_failed(S, ReplyPID, {components, create}, UnsupportedComponents),
    end;
boot({components, validate}, S, ReplyPID) ->
    % Ensure that each component believes that the process definition is viable
    case lists:filter(fun(Mod) -> not Mod:viable(S#pstate.process) end, components(S)) of
        [] -> boot(checkpoint, S, ReplyPID);
        InviableMods ->
            boot_failed(S, ReplyPID, {components, validate}, {inviable, InviableMods})
    end;
boot(checkpoint, S, ReplyPID) ->
    % Load a process-scope checkpoint, if available.
    case cu_checkpoint:latest(
            process,
            S#pstate.id,
            trusted_checkpointers(S),
            S#proc.transforms, S#proc.devices) of
        not_found ->
            % There are no checkpoints, boot fully
            boot(init_components, S, ReplyPID);
        {Epoch, Slot, Mach, Iface, Trans, Devs} ->
            boot(
                finished,
                S#pstate {
                    epoch = Epoch,
                    slot = Slot,
                    vm_s = MachineMod,
                    iface_s = InterfaceMod,
                    transforms = Trans,
                    devices = Devs
                },
                ReplyPID
            );
boot(init_components, S, ReplyPID) ->
    case cu_component:init_all(S) of
        {ok, NewS} -> boot(finished, NewS, ReplyPID);
        {error, E} ->
            boot_failed(S, ReplyPID, init_components, E)
    end;
boot(finished, S, ReplyPID) ->
    ReplyPID ! {boot, S#tx.id, ok},
    server(S).

server(S) ->
    receive
        stop -> ok
    end.

boot_failed(S, ReplyPID, Reason) ->
    ReplyPID ! {boot, (S#pstate.process)#tx.id, failed, Reason},
    failed.

components(S) ->
    ComponentMods = [ S#pstate.vm_mod, S#pstate.iface_mod]
        ++ [ TransMod || {TransMod, _} < - S#pstate.transforms ]
        ++ [ DevMod || {DevMod, _} <- S#pstate.devices ].
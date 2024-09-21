-module(cu_component).
-export([init_all/1]).

-include("include/ao.hrl").

-define(AO_DEFAULTS, #{
   <<"Transform">> => cu_dedup,
   <<"Interface">> => cu_interface_json
}).

%% A map of component type atoms to their protocol-defined tags
-define(TYPES, #{
    transformers => <<"Transform">>,
    interfaces => <<"Interface">>,
    machines => <<"Machine">>,
    devices => <<"Device">>
}).

init_all(S) ->
    init(init(S, machine),


apply_



boot({init, machine}, S, ReplyPID) ->
    case (S#pstate.vm_mod)(S) of
        {ok, MachS} ->
            boot({init, interface}, S#pstate { vm_s = MachS }, ReplyPID);
        {error, E} ->
            boot_failed(S, ReplyPID, machine_init, E)
    end;
boot({init, interface}, S, ReplyPID) ->
    case (S#pstate.iface_mod)(S) of
        {ok, IfaceS} ->
            boot({init, transforms}, S#pstate { iface_s = IfaceS }, ReplyPID);
        not_supported ->
            boot_failed(S, ReplyPID, interface_init, E)
    end;
boot({init, transformers}, S, ReplyPID) ->
    NewTrans =
        lists:foldl(
            fun({Mod, _}, Acc) ->
                % TODO: Check the foldl vs foldr is correct here.
                Mod:init()
            end,
            [],
            S#pstate.)
        ),
a

type_to_tag(Atom) ->
    case maps:find(Atom, ?TYPES) of
        {ok, TypeTag} -> TypeTag;
        not_found -> error(unsupported_type)
    end.
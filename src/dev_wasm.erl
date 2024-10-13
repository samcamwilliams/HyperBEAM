-module(dev_wasm).
-export([uses/0, init/2, execute/3, terminate/1]).
-export([checkpoint/1, checkpoint_uses/1]).

-include("include/ao.hrl").

%%% A device that executes a WASM image on messages.

init(State, Params) ->
    {<<"Image">>, ImageID} = lists:keyfind(<<"Image">>, 1, Params),
    Image = ao_cache:lookup(maps:get(store, State, ao:get(store)), ImageID),
    {ok, Port, _ImportMap, _Exports} = cu_beamr:start(Image#tx.data),
    ?c(started_wasm),
    % Apply the checkpoint if it is in the initial state.
    ?c({initial_state, maps:keys(State)}),
    case maps:get(<<"WASM-State">>, State, undefined) of
        undefined ->
            State;
        Checkpoint ->
            ?c(wasm_checkpoint_found),
            ?c({is_tx, is_record(Checkpoint, tx)}),
            ?c({wasm_deserializing, byte_size(Checkpoint#tx.data)}),
            cu_beamr:deserialize(Port, Checkpoint#tx.data),
            ?c(wasm_deserialized)
    end,
    {ok, State#{
        wasm => Port,
        phase => pre_exec,
        call => undefined
    }}.

execute(
    M,
    State = #{wasm := Port, phase := pre_exec, call := {Func, Params}, wasm_stdlib := Stdlib},
    LastExec
) ->
    case ao_message:id(M) of
        LastExec ->
            {ok, State};
        MsgID ->
            {ResType, Res, State2} = cu_beamr:call(State, Port, Func, Params, Stdlib),
            {pass, State2#{phase := post_exec, results => {ResType, Res}}, MsgID}
    end;
execute(_M, State = #{phase := post_exec}, _) ->
    % Reset the phase indicator for the next run.
    {ok, State#{phase := pre_exec}}.

checkpoint(State = #{wasm := Port, save_keys := SaveKeys}) ->
    {ok, Serialized} = cu_beamr:serialize(Port),
    TX = ar_bundles:normalize(#tx{ data = Serialized }),
    {ok, State#{
        <<"WASM-State">> => TX,
        save_keys => [<<"WASM-State">> | SaveKeys]
    }}.

checkpoint_uses(S = #{keys := Keys}) ->
    {ok, S#{keys => [<<"WASM-State">> | Keys]}}.

terminate(State = #{wasm := Port}) ->
    ?c(terminate_called_on_dev_wasm),
    cu_beamr:stop(Port),
    {ok, State#{wasm := undefined}}.

uses() -> all.

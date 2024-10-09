-module(dev_wasm).
-export([uses/0, init/2, execute/3, terminate/1]).
-export([checkpoint/1]).

-include("include/ao.hrl").

%%% A device that executes a WASM image on messages.

init(State, Params) ->
    {<<"Image">>, ImageID} = lists:keyfind(<<"Image">>, 1, Params),
    Image = ao_cache:lookup(maps:get(store, State, ao:get(store)), ImageID),
    {ok, Port, _ImportMap, _Exports} = cu_beamr:start(Image#tx.data),
    ?c(started_wasm),
    % Apply the checkpoint if it is in the initial state.
    case maps:get(<<"WASM-State">>, State, undefined) of
        undefined ->
            ?c(wasm_no_checkpoint),
            State;
        Checkpoint ->
            ?c(wasm_deserializing),
            cu_beamr:deserialize(Port, Checkpoint),
            ?c(wasm_deserialized)
    end,
    {ok, State#{
        wasm => Port,
        phase => pre_exec,
        call => undefined
    }}.

execute(
    M,
    State = #{wasm := Port, phase := pre_exec, call := {Func, Params}, stdlib := Stdlib},
    LastExec
) ->
    case ao_message:id(M) of
        LastExec ->
            {ok, State};
        MsgID ->
            {ResType, Res, State2} = cu_beamr:call(State, Port, Func, Params, Stdlib),
            {pass, State2#{phase := post_exec, result := {ResType, Res}}, MsgID}
    end;
execute(_M, State = #{phase := post_exec}, _) ->
    % Reset the phase indicator for the next run.
    {ok, State#{phase := pre_exec}}.

checkpoint(State = #{wasm := Port, save_keys := SaveKeys}) ->
    {ok, State#{
        <<"WASM-State">> => cu_beamr:serialize(Port),
        save_keys => [<<"WASM-State">> | SaveKeys]
    }}.

terminate(State = #{wasm := Port}) ->
    cu_beamr:stop(Port),
    {ok, State#{wasm := undefined}}.

uses() -> all.

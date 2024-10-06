-module(dev_wasm).
-export([uses/0, init/2, execute/3, terminate/1]).

-include("include/ao.hrl").

%%% A device that executes a WASM image on messages.

init(State, Params) ->
    {<<"Image">>, ImageID} = lists:keyfind(<<"Image">>, 1, Params),
    Image = ao_message:get(ImageID),
    {ok, Port, _ImportMap, _Exports} = cu_beamr:start(Image#tx.data),
    NState =
        case maps:get(checkpoint, State, undefined) of
            undefined -> State;
            Checkpoint -> cu_beamr:deserialize(Port, Checkpoint)
        end,
    {ok, NState#{
        wasm => Port,
        phase => pre_exec,
        call => undefined,
        serialize => fun() -> cu_beamr:serialize(Port) end,
        deserialize => fun(Bin) -> cu_beamr:deserialize(Port, Bin) end
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

terminate(State = #{wasm := Port}) ->
    cu_beamr:stop(Port),
    {ok, State#{wasm := undefined}}.

uses() -> all.

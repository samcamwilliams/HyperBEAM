-module(dev_wasm).
-export([uses/0, init/2, execute/3, terminate/1]).
-export([checkpoint/1, checkpoint_uses/1]).

-include("include/hb.hrl").

%%% A device that executes a WASM image on messages.

init(State, Params) ->
    {<<"Image">>, ImageID} = lists:keyfind(<<"Image">>, 1, Params),
    {ok, Image} = hb_cache:read_message(maps:get(store, State, hb_opts:get(store)), ImageID),
    {ok, Port, _ImportMap, _Exports} = hb_beamr:start(Image#tx.data),
    % Apply the checkpoint if it is in the initial state.
    case maps:get(<<"WASM-State">>, State, undefined) of
        undefined ->
            State;
        Checkpoint ->
            ?event(wasm_checkpoint_found),
            ?event({is_tx, is_record(Checkpoint, tx)}),
            ?event({wasm_deserializing, byte_size(Checkpoint#tx.data)}),
            hb_beamr:deserialize(Port, Checkpoint#tx.data),
            ?event(wasm_deserialized)
    end,
    {ok, State#{
        wasm => Port,
        phase => pre_exec,
        call => undefined
    }}.

execute(
    M,
    State = #{pass := 1, wasm := Port, phase := pre_exec, call := {Func, Params}, wasm_stdlib := Stdlib},
    LastExec
) ->
    case hb_util:id(M) of
        LastExec ->
            {ok, State};
        MsgID ->
            {ResType, Res, State2} = hb_beamr:call(State, Port, Func, Params, Stdlib),
            {ok, State2#{ phase := post_exec, results => {ResType, Res} }, MsgID}
    end;
execute(_M, State = #{ pass := 2, phase := post_exec }, _) ->
    % Reset the phase indicator for the next run.
    {ok, State#{ phase := pre_exec }};
execute(_, S, _) ->
    {ok, S}.

checkpoint(State = #{ wasm := Port, save_keys := SaveKeys }) ->
    {ok, Serialized} = hb_beamr:serialize(Port),
    TX = ar_bundles:normalize(#tx{ data = Serialized }),
    {ok, State#{
        <<"WASM-State">> => TX,
        save_keys => [ <<"WASM-State">> | SaveKeys ]
    }};
checkpoint(InvalidS) ->
    throw({wat, InvalidS}),
    {ok, InvalidS}.

checkpoint_uses(S = #{ results := Results }) ->
    Keys = maps:get(keys, Results, []),
    {ok, S#{ results => Results#{ keys => [ <<"WASM-State">> | Keys ] } }}.

terminate(State = #{wasm := Port}) ->
    ?event(terminate_called_on_dev_wasm),
    hb_beamr:stop(Port),
    {ok, State#{wasm := undefined}}.

uses() -> all.
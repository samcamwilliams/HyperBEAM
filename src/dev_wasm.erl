-module(dev_wasm).
-export([init/2, execute/2, uses/0]).
-export([stdlib/6]).

%%% A device that executes a WASM image on messages.

-record(state, {
    port,
    last
}).

init(Params, State) ->
    {<<"Image">>, ImageID} = lists:keyfind(<<"Image">>, 1, Params),
    Image = ao_message:get(ImageID),
    {ok, Port, _ImportMap, _Exports} = cu_beamr:start(Image#tx.data),
    {ok, State#{ wasm := Port, phase := pre_exec } }.

execute(M, State#{ phase := pre_exec, wasm := WASM, call := {Func, Params }}) ->
    case ao_message:id(M) of
        WASM#state.last ->
            {ok, State};
        _ ->
            {ResType, Res, State2} = cu_beamr:call(WASM#state.port, Func, Params, fun stdlib/6),
            {pass, State2#{
                phase := post_exec,
                wasm := S#state { last = ao_message:id(M) },
                result := {ResType, Res}
            }}
    end;
execute(M, State#{ phase := post_exec}) ->
    % Reset the phase indicator for the next run.
    {ok, State#{ phase := pre_exec }}.

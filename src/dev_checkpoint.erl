-module(dev_checkpoint).
-export([uses/0, init/2, execute/2]).

uses() -> all.

init(Params, State) ->
    % TODO: Read the latest checkpoint if it exists.
    {ok, State}.

execute(Msg, State#{ phase := post_exec }) ->
    % TODO: Optionally checkpoint the state here.
    % We should always cache the result at this stage.
    {ok, State};
execute(Msg, State) ->
    {ok, State}.

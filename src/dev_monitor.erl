-module(dev_monitor).
-export([init/3, execute/2, uses/0]).

%%% A simple device that allows flexible monitoring of a process execution.
%%% Adding a dev_monitor device to a process will cause the listed functions
%%% to be called with the current process state during each pass. The monitor
%%% functions must not mutate state.

init(State, PrivParams, Params) ->
    {ok,  State#{ monitors := PrivParams }}.

execute(Message, State) ->
    % TODO: Pmap this instead?
    Monitors =
        lists:filter(
            fun(Mon) ->
                case Mon(State) of
                    done -> false;
                    _ -> true
                end
            end,
            State#{ monitors }
        ),
    {ok, State#{ monitors := Monitors }}.

uses() -> all.
-module(ao_logger).
-export([start/0, log/2, register/1, report/1]).

-include("include/ao.hrl").

-record(state, {
    activity = [],
    processes = [],
    console = true
}).

start() ->
    spawn(fun() ->
        loop(#state{})
    end).

log(Monitor, Data) ->
    Monitor ! {log, Data}.

register(Monitor) ->
    Monitor ! {register, self()}.

report(Monitor) ->
    Monitor ! {report, self()},
    receive
        {report, Activity} ->
            Activity
    end.

loop(State) ->
    receive
        {log, Activity} ->
            console(State, Activity),
            loop(State#state{ activity = [Activity | State#state.activity] });
        {register, PID} ->
            erlang:monitor(process, PID),
            console(State, Act = {ok, registered, PID}),
            loop(State#state{
                processes = [PID | State#state.processes],
                activity = [Act | State#state.activity]
            });
        {'DOWN', _MonitorRef, process, PID, Reason} ->
            console(State, Act = {terminated, Reason, PID}),
            loop(State#state{
                processes = State#state.processes -- [PID],
                activity = [Act | State#state.activity]
            });
        {report, PID} ->
            PID ! {report, State#state.activity},
            loop(State)
    end.

console(#state { console = false }, _) ->
    not_printing;
console(S, {Status, Type, Details}) when is_record(Details, tx) ->
    console(S, {Status, Type, Details#tx.id});
console(_S, {Status, Type, Details}) ->
    io:format("### MU PUSH REPORT ~p ###~n~p: ~p~n~p~n~n",
        [self(), Status, Type, Details]);
console(_S, Act) ->
    io:format("### MU PUSH UNEXPECTED ~p ###~n~p~n~n", [self(), Act]).
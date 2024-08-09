-module(su_registry).
-export([start/0, find/1, server/1]).

start() ->
    register(?MODULE, spawn(fun() -> server(#{}) end)).

find(ProcID) ->
    ReplyPID = self(),
    ?MODULE ! {find, ProcID, ReplyPID},
    receive
        {process, Process} -> Process
    end.

server(Registry) ->
    receive
        {find, ProcID, ReplyPID} ->
            Process =
                case maps:find(ProcID, Registry) of
                    {ok, ExistingProcess} ->
                        case is_process_alive(ExistingProcess) of
                            true ->
                                ExistingProcess;
                            false ->
                                new_proc(ProcID)
                        end;
                    error ->
                        new_proc(ProcID)
                end,
            ReplyPID ! {process, Process},
            server(Registry#{ProcID => Process})
    end.

new_proc(ProcID) ->
    spawn(fun() -> su_process:start(ProcID) end).
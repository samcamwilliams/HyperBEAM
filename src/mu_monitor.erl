-module(mu_monitor).
-export([start/1, start/2, start/3, stop/1]).

-include("include/ao.hrl").

-record(state, {
    proc_id,
    cursor,
    logger
}).

start(ProcID) ->
    start(ProcID, ao:get(default_cron_rate)).
start(ProcID, Rate) ->
    start(ProcID, Rate, ao_client:cron_cursor(ProcID)).
start(ProcID, Rate, Cursor) ->
    Logger = ao_logger:start(),
    Monitor = spawn(
        fun() ->
            server(
                #state{
                    proc_id = ProcID,
                    cursor = Cursor,
                    logger = Logger
                }
            )
        end),
    Ticker = spawn(fun() -> ticker(Monitor, Rate) end),
    ao_logger:register(Monitor),
    ao_logger:log(Monitor, {ok, started_monitor, {ProcID, Rate, Cursor}}),
    ao_logger:register(Ticker),
    {Monitor, Logger}.

stop(PID) ->
    PID ! stop.

server(State) ->
    receive
        stop -> ok;
        tick ->server(handle_crons(State))
    end.

handle_crons(State) ->
    case ao_client:cron(State#state.proc_id, State#state.cursor) of
        {ok, HasNextPage, Results, Cursor} ->
            lists:map(
                fun(Res) -> mu_push:start(Res, State#state.logger) end,
                Results
            ),
            NS = State#state{cursor = Cursor},
            case HasNextPage of
                true -> NS;
                false -> handle_crons(NS)
            end;
        Error ->
            ao_logger:log(State#state.logger, Error),
            State
    end.

ticker(Monitor, Rate) ->
    case erlang:is_process_alive(Monitor) of
        true ->
            timer:sleep(Rate),
            Monitor ! tick,
            ticker(Monitor, Rate);
        false ->
            ok
    end.
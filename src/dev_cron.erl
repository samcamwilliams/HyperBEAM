-module(dev_cron).
-export([init/2, execute/2, uses/0]).

%%% A device that inserts new messages into the schedule to allow processes
%%% to passively 'call' themselves without user interaction.

-include("include/ao.hrl").

-record(state, {
    time,
    last_run
}).

init(State = #{ process := ProcM }, Params) ->
    case lists:keyfind(<<"Time">>, 1, Params) of
        {<<"Time">>, CronTime} ->
            MilliSecs = timestamp(CronTime),
            %% TODO: What's the most sensible way to initialize the last_run?
            %% Current behavior: Timer starts after _first_ message.
            {ok, State#{ cron := #state { time = MilliSecs, last_run = timestamp(ProcM) } }};
        false ->
            {ok, State#{ cron := inactive }}
    end.

execute(_M, State = #{ cron := inactive }) ->
    {ok, State};
execute(M, State = #{ cron := #state { last_run = undefined } }) ->

    {ok, State#{ cron := #state { last_run = timestamp(M) } }};
execute(Message, State = #{ cron := #state { time = MilliSecs, last_run = LastRun }, schedule := Sched }) ->
    case timestamp(Message) - LastRun of
        Time when Time > MilliSecs ->
            NextCronMsg = create_cron(State, CronTime = timestamp(Message) + MilliSecs),
            {restack,
                State#{
                    cron := #state { last_run = CronTime },
                    schedule := [NextCronMsg | Sched]
                }
            };
        _ ->
            {ok, State}
    end.

timestamp(M) ->
    {<<"Timestamp">>, TSBin} = lists:keyfind(<<"Timestamp">>, 1, M#tx.tags),
    list_to_integer(binary_to_list(TSBin)).

create_cron(_State, CronTime) ->
    #tx{
        tags = [
            {<<"Action">>, <<"Cron">>},
            {<<"Timestamp">>, list_to_binary(integer_to_list(CronTime))}
        ]
    }.

uses() -> all.
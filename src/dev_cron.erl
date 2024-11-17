-module(dev_cron).
-export([init/2, execute/2, uses/0]).

%%% A device that inserts new messages into the schedule to allow processes
%%% to passively 'call' themselves without user interaction.

-include("include/hb.hrl").

-record(state, {
    time,
    last_run
}).

init(State = #{ process := ProcM }, Params) ->
    case lists:keyfind(<<"Time">>, 1, Params) of
        {<<"Time">>, CronTime} ->
            MilliSecs = parse_time(CronTime),
            %% TODO: What's the most sensible way to initialize the last_run?
            %% Current behavior: Timer starts after _first_ message.
            {ok, State#{ cron => #state { time = MilliSecs, last_run = timestamp(ProcM) } }};
        false ->
            {ok, State#{ cron => inactive }}
    end.

parse_time(BinString) ->
    [AmountStr, UnitStr] = binary:split(BinString, <<"-">>),
    Amount = binary_to_integer(AmountStr),
    Unit = string:lowercase(binary_to_list(UnitStr)),
    case Unit of
        "millisecond" ++ _ -> Amount;
        "second" ++ _ -> Amount * 1000;
        "minute" ++ _ -> Amount * 60 * 1000;
        "hour" ++ _ -> Amount * 60 * 60 * 1000;
        "day" ++ _ -> Amount * 24 * 60 * 60 * 1000;
        _ -> throw({error, invalid_time_unit, UnitStr})
    end.

execute(_M, State = #{ cron := inactive }) ->
    {ok, State};
execute(M, State = #{ pass := 1, cron := #state { last_run = undefined } }) ->
    {ok, State#{ cron := #state { last_run = timestamp(M) } }};
execute(Message, State = #{ pass := 1, cron := #state { time = MilliSecs, last_run = LastRun }, schedule := Sched }) ->
    case timestamp(Message) - LastRun of
        Time when Time > MilliSecs ->
            NextCronMsg = create_cron(State, CronTime = timestamp(Message) + MilliSecs),
            {pass,
                State#{
                    cron := #state { last_run = CronTime },
                    schedule := [NextCronMsg | Sched]
                }
            };
        _ ->
            {ok, State}
    end;
execute(_, S) ->
    {ok, S}.

timestamp(M) ->
    % TODO: Process this properly
    case lists:keyfind(<<"Timestamp">>, 1, M#tx.tags) of
        {<<"Timestamp">>, TSBin} ->
            list_to_integer(binary_to_list(TSBin));
        false ->
            0
    end.

create_cron(_State, CronTime) ->
    #tx{
        tags = [
            {<<"Action">>, <<"Cron">>},
            {<<"Timestamp">>, list_to_binary(integer_to_list(CronTime))}
        ]
    }.

uses() -> all.
-module(mu_push).
-export([start/1, start/2]).

-include("include/ao.hrl").

start(Item) -> start(Item, ao_logger:start()).

start(Res, undefined) ->
    start(Res);
start(Res, Monitor) when is_record(Res, result) ->
    spawn(fun() ->
        lists:map(
            fun(Spawn) ->
                mu_push:start(Spawn, Monitor)
            end,
            maybe_to_list(Res#result.spawns)
        ),
        lists:map(
            fun(Message) ->
                mu_push:start(Message, Monitor)
            end,
            maybe_to_list(Res#result.messages)
        ),
        lists:map(
            fun(Assignment) ->
                ao_client:assign(Assignment)
            end,
            maybe_to_list(Res#result.assignments)
        )
    end);
start(Item, Monitor) ->
    spawn(
        fun() ->
            ao_logger:log(Monitor, {ok, start, Item}),
            case ar_bundles:verify_item(Item) of
                true ->
                    spawn(
                        fun() ->
                            ao_logger:register(self()),
                            push(Item, Monitor)
                        end
                    ),
                    Monitor;
                false ->
                    ao_logger:log(Monitor, {error, invalid_item}),
                    {error, invalid_item}
            end
        end
    ).

maybe_to_list(Map) when is_map(Map) -> [V || {_K, V} <- maps:to_list(Map)];
maybe_to_list(undefined) -> [];
maybe_to_list(Else) -> Else.

push(Item, Monitor) ->
    % Send message to scheduler
    case ao_client:schedule(Item) of
        % Get assignment response
        {ok, Assignment} ->
            ao_logger:log(Monitor, {ok, scheduled, Assignment}),
            % Get result from cu
            case ao_client:compute(Assignment) of
                {ok, #tx{data = Res}} ->
                    ao_logger:log(Monitor, {ok, computed, Assignment}),
                    Result = #result{
                        messages =
                            (maps:get(<<"/Outbox/Message">>, Res, #tx{data = []}))#tx.data,
                        assignments = maps:get(<<"/Outbox/Assignment">>, Res, []),
                        spawns = maps:get(<<"/Outbox/Spawn">>, Res, [])
                    },
                    start(Result, Monitor);
                Error ->
                    ao_logger:log(Monitor, Error)
            end;
        Error ->
            ao_logger:log(Monitor, Error)
    end.

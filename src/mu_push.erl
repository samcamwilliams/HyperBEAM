-module(mu_push).
-export([start/1, start/2]).

-include("include/ao.hrl").

% create logger and call start
start(Item) -> start(Item, ao_logger:start()).

start(Res, Monitor) when is_record(Res, result) ->
    lists:map(
        fun(Spawn) ->
            mu_push:start(Spawn, Monitor)
        end,
        Res#result.spawns
    ),
    lists:map(
        fun(Message) ->
            mu_push:start(Message, Monitor)
        end,
        Res#result.messages
    ),
    lists:map(
        fun(Assignment) ->
            ao_client:assign(Assignment)
        end,
        Res#result.assignments
    );
% log start time
start(Item, Monitor) ->
    ao_logger:log(Monitor, {ok, start, Item#tx.id}),
    case ar_bundles:verify_item(Item) of
        true ->
            % is valid launch process
            spawn(
                fun() ->
                    ao_logger:register(self()),
                    push(Item, Monitor)
                end),
            Monitor;
        false -> {error, invalid_item}
    end.

push(Item, Monitor) ->
    % send message to su
    case ao_client:schedule(Item) of
        % get assignment response
        {ok, Assignment} ->
            ao_logger:log(Monitor, {ok, scheduled, Assignment}),
            % get result from cu
            case ao_client:compute(Assignment) of
                {ok, Result} ->
                    ao_logger:log(Monitor, {ok, computed, Result}),
                    start(Result, Monitor);
                Error -> ao_logger:log(Monitor, Error)
            end;
        Error -> ao_logger:log(Monitor, Error)
    end.
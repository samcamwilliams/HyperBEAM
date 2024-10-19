-module(dev_cu).
-export([push/2]).

push(Item, S = #{ monitor := Monitor }) ->
    case ao_client:compute(Item) of
        {ok, Results} ->
            {ok, S#{ results => Results }};
        Error ->
            ao_logger:log(Monitor, Error),
            {error, Error}
    end.
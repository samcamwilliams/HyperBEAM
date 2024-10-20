-module(dev_cu).
-export([push/2]).

push(_Item, S = #{ assignment := Assignment, logger := Logger }) ->
    case ao_client:compute(Assignment) of
        {ok, Results} ->
            {ok, S#{ results => Results }};
        Error ->
            ao_logger:log(Logger, Error),
            {error, Error}
    end.
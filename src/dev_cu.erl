-module(dev_cu).
-export([push/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/ao.hrl").

push(_Item, S = #{ assignment := Assignment, logger := Logger }) ->
    case ao_client:compute(Assignment) of
        {ok, Results} ->
            ar_bundles:print(Results),
            {ok, S#{ results => Results }};
        Error ->
            ao_logger:log(Logger, Error),
            {error, Error}
    end.
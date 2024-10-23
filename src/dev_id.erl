-module(dev_id).
-export([execute/1]).

%%% The identity device: Simply return the message as it is.

execute(M) ->
    {ok, M}.
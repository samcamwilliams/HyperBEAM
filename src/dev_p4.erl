-module(dev_p4).
-export([push/2]).
-include("include/ao.hrl").

push(Item, S) ->
    % TODO: Check payment.
    {ok, S}.

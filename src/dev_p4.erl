-module(dev_p4).
-export([push/2]).
-include("include/hb.hrl").

push(Item, S) ->
    % TODO: Check payment.
    {ok, S}.

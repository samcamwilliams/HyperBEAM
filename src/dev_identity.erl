-module(dev_identity).
-export([execute/1]).
-include("include/ao.hrl").

%%% The identity device: Simply return the message as it is.

execute(Arg) ->
    ?c({identity_device_called, Arg}),
    {ok, Arg}.

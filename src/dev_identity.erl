-module(dev_identity).
-export([execute/1]).
-include("include/hb.hrl").

%%% The identity device: Simply return the message as it is.

execute(Arg) ->
    ?event({identity_device_called, Arg}),
    {ok, Arg}.

-module(ao_router).
-export([find/2, find/3]).
-include_lib("include/ao.hrl").
%%% Locate a service in the AO network, routing to local instances if available.

find(Service, ID) ->
    find(Service, ID, any).

find(Service, ID, Type) ->
    ?c({find, Service, ID, Type}),
    ok.


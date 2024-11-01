-module(ao_router).
-export([find/2, find/3]).
-include_lib("include/ao.hrl").

%%% Locate a service in the AO network. This module uses
%%% URLs to locate services, so it can be used to locate
%%% nodes using IP addresses or domain names. This also 
%%% allows us to use different protocols later, potentially.

find(Type, ID) ->
    find(Type, ID, '_').

find(Type, ID, Address) ->
    ?c({find, Type, ID, Address}),
    case maps:get(Type, ao:get(nodes), undefined) of
        #{ Address := Node } -> {ok, Node};
        undefined -> {error, service_type_not_found}
    end.

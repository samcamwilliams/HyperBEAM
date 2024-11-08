-module(dev_lookup).
-export([read/1]).
-include("include/ao.hrl").

%%% The lookup device: Look up an ID by name and return it.

read(#tx { tags = Tags }) ->
    % Note: Use the local store -- do not attempt to reach remotely.
    % ao_cache:read should return {ok, Val} or an error tuple, so we can return
    % the value directly.
    {<<"Subpath">>, Subpath} = lists:keyfind(<<"Subpath">>, 1, Tags),
    ?c({looking_up_for_remote_peer, Subpath}),
    case ao_cache:lookup(ao:get(local_store), Subpath) of
        not_found -> {ok, #tx { tags = [{<<"Error">>, <<"Not found">>}, {<<"Status">>, <<"404">>}] }};
        Val -> {ok, Val}
    end.

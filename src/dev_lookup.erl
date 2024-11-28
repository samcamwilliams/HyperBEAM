-module(dev_lookup).
-export([read/1]).
-include("include/hb.hrl").

%%% The lookup device: Look up an ID by name and return it.

read(#tx { tags = Tags }) ->
    % Note: Use the local store -- do not attempt to reach remotely.
    % hb_cache:read should return {ok, Val} or an error tuple, so we can return
    % the value directly.
    {<<"Subpath">>, Subpath} = lists:keyfind(<<"Subpath">>, 1, Tags),
    ?event({looking_up_for_remote_peer, Subpath}),
    hb_cache:read_message(hb_store:scope(hb_opts:get(store), local), Subpath).
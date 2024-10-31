-module(dev_lookup).
-export([read/1]).

%%% The lookup device: Look up an ID by name and return it.

read(#{ id := ID }) ->
    % Use the local store -- do not attempt to reach remotely.
    % ao_cache:read should return {ok, Val} or an error tuple, so we can return
    % the value directly.
    ao_cache:read(ao:get(local_store), ID).
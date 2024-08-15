-module(su_timestamp).
-export([start/0, get/0]).
-define(TIMEOUT, 1000 * 15).
-define(GATEWAY_URL, "https://arweave.net").

start() ->
    TSServer = spawn(fun() -> cache(ao_client:arweave_timestamp()) end),
    spawn(fun() -> refresher(TSServer) end),
    register(?MODULE, TSServer),
    TSServer.

get() ->
    ?MODULE ! {get, self()},
    receive
        {timestamp, Timestamp} ->
            Timestamp
    end.

cache(Current) ->
    receive
        {get, Pid} ->
            Pid ! {timestamp, Current},
            cache(Current);
        {refresh, New} ->
            cache(New)
    end.

refresher(TSServer) ->
    timer:sleep(?TIMEOUT),
    TSServer ! {refresh, ao_client:arweave_timestamp()},
    refresher(TSServer).
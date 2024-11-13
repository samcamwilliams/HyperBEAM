-module(ar_timestamp).
-export([start/0, get/0]).
-define(TIMEOUT, 1000 * 15).

%%% A simple Erlang server that caches the current Arweave timestamp and
%%% refreshes it periodically.

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
    TS =
        case ao:get(mode) of
            debug -> { 0, 0, << 0:256 >> };
            prod -> ao_client:arweave_timestamp()
        end,
    TSServer ! {refresh, TS},
    refresher(TSServer).
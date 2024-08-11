-module(su_timestamp).
-export([start/0, get/0]).
-define(TIMEOUT, 1000 * 15).
-define(GATEWAY_URL, "https://arweave.net").

start() ->
    TSServer = spawn(fun() -> cache(get_from_arweave()) end),
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
    TSServer ! {refresh, get_from_arweave()},
    refresher(TSServer).

get_from_arweave() ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(?GATEWAY_URL ++ "/block/current"),
    {Fields} = jiffy:decode(Body),
    {_, Timestamp} = lists:keyfind(<<"timestamp">>, 1, Fields),
    {_, Hash} = lists:keyfind(<<"indep_hash">>, 1, Fields),
    {_, Height} = lists:keyfind(<<"height">>, 1, Fields),    
    {Timestamp, Height, Hash}.
-module(ar_timestamp).
-export([start/0, get/0]).
-define(TIMEOUT, 1000 * 15).

%%% A simple Erlang server that caches the current Arweave timestamp and
%%% refreshes it periodically.

%% @doc Check if the server is already running, and if not, start it.
start() ->
    case whereis(?MODULE) of
        undefined -> spawn_server();
        PID ->
            case is_process_alive(PID) of
                true -> PID;
                false -> spawn_server()
            end
    end.

%% @doc Spawn a new server and its refresher.
spawn_server() ->
    TSServer =
        spawn(fun() -> cache(hb_client:arweave_timestamp()) end),
    spawn(fun() -> refresher(TSServer) end),
    register(?MODULE, TSServer),
    TSServer.

%% @doc Get the current timestamp from the server, starting the server if it
%% isn't already running.
get() ->
    PID = start(),
    PID ! {get, self()},
    receive
        {timestamp, Timestamp} ->
            Timestamp
    end.

%% @doc Cache the current timestamp from Arweave.
cache(Current) ->
    receive
        {get, Pid} ->
            Pid ! {timestamp, Current},
            cache(Current);
        {refresh, New} ->
            cache(New)
    end.

%% @doc Refresh the timestamp cache periodically.
refresher(TSServer) ->
    timer:sleep(?TIMEOUT),
    TS =
        case hb_opts:get(mode) of
            debug -> { 0, 0, << 0:256 >> };
            prod -> hb_client:arweave_timestamp()
        end,
    TSServer ! {refresh, TS},
    refresher(TSServer).
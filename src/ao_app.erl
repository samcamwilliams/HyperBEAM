%%%-------------------------------------------------------------------
%% @doc supersu public API
%% @end
%%%-------------------------------------------------------------------

-module(ao_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ao_sup:start_link(),
    ok = su_registry:start(),
    _TimestampServer = su_timestamp:start(),
    {ok, _} = ao_http_router:start().

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%% @doc supersu public API
%% @end
%%%-------------------------------------------------------------------

-module(ao_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("include/ao.hrl").

start(_StartType, _StartArgs) ->
    ao_sup:start_link(),
    ok = su_registry:start(),
    _TimestampServer = su_timestamp:start(),
    {ok, HTTP} = ao_http_router:start(),
    {ok, HTTP}.

stop(_State) ->
    ok.

%% internal functions

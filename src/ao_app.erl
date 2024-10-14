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
    su_data:init(),
    ok = su_registry:start(),
    TS = su_timestamp:start(),
    _HTTP = ao_http_router:start([su_http, mu_http, cu_http]),
    {ok, TS}.

stop(_State) ->
    ok.

%% internal functions

%%%-------------------------------------------------------------------
%% @doc supersu public API
%% @end
%%%-------------------------------------------------------------------

-module(ao_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ao_sup:start_link(),
    su_data:init(),
    su_registry:start(),
    su_timestamp:start(),
    ao_http_router:start([su_http, mu_http]).

    % cu_process:test().

stop(_State) ->
    ok.

%% internal functions

%%%-------------------------------------------------------------------
%% @doc supersu public API
%% @end
%%%-------------------------------------------------------------------

-module(supersu_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    supersu_sup:start_link(),
    su_data:init(),
    su_registry:start(),
    su_timestamp:start(),
    su_http:start().

stop(_State) ->
    ok.

%% internal functions

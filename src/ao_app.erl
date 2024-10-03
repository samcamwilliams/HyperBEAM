%%%-------------------------------------------------------------------
%% @doc supersu public API
%% @end
%%%-------------------------------------------------------------------

-module(ao_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case ao:get(profiling) of
        true ->
            eprof:start_profiling([self()]);
        false -> do_nothing
    end,
    ao_sup:start_link(),
    su_data:init(),
    Reg = su_registry:start(),
    _TS = su_timestamp:start(),
    _HTTP = ao_http_router:start([su_http, mu_http, cu_http]),
    cu_process:full_push_test(),
    {ok, Reg}.

stop(_State) ->
    ok.

%% internal functions

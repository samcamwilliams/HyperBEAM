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
    {ok, _} = ao_http_router:start().

stop(_State) ->
    ok.

attest_key() ->
    W = ao:wallet(),
    Addr = ar_wallet:to_address(W),
    Cmd = lists:flatten(io_lib:format("...", [])),
    case os:cmd(Cmd) of
        {ok, Res} ->
            Signed = ar_bundle:sign_item(
                #tx{
                    tags = [
                        {<<"Type">>, <<"TEE-Attestation">>},
                        {<<"Address">>, ar_util:id(Addr)}
                    ],
                    data = Res
                },
                W
            ),
            ao_client:upload(Signed),
            ok;
        {error, _} ->
            skip
    end.

%% internal functions

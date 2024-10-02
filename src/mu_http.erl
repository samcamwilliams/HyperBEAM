-module(mu_http).
-export([routes/0, handle/3]).
-include("src/include/ar.hrl").

routes() ->
    {"/mu", [ "/" ]}.

handle(<<"POST">>, [], Req) ->
    {ok, ReqBin} = ao_http_router:read_body(Req),
    _MonitorPID = mu_push:start(ar_bundles:deserialize(ReqBin)),
    ao_http:reply(
        Req,
        ar_bundles:sign_item(
            #tx { tags = [
                {<< "Pushing">>, ar_util:encode(crypto:strong_rand_bytes(32))},
                {<< "Status">>, << "Running">> }
            ]},
            ao:wallet()
        )
    ),
    {ok, Req}.

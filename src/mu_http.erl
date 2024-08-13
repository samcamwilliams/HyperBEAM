-module(mu_http).

-export([routes/0, handle/3]).

routes() ->
    {"/mu", [ "/" ]}.

handle(<<"POST">>, [], Req) ->
    {ok, ReqBin} = ao_http_router:read_body(Req),
    mu_push:start(ar_bundles:deserialize(ReqBin)),
    {ok, Req}.
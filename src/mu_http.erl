-module(mu_http).

-export([routes/0, handle/3]).

routes() ->
    {"/mu", [ "/" ]}.

handle(<<"POST">>, [], Req) ->
    ReqBin = ao_http_router:read_body(Req),
    {JSONStruct} = jiffy:decode(ReqBin),
    mu_push:start(ar_bundles:deserialize(JSONStruct)),
    {ok, Req}.
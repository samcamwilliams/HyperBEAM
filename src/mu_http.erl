-module(mu_http).

-export([routes/0, handle/3]).

-include("src/include/ar.hrl").

routes() ->
    {"/mu", ["/"]}.

handle(<<"GET">>, [], Req) ->
    cowboy_req:reply(200,
                     #{<<"Content-Type">> => <<"application/json">>},
                     jiffy:encode({[{<<"Name">>, <<"AO Messenger Unit">>}]}),
                     Req),
    {ok, Req};
% receive message to process
handle(<<"POST">>, [], Req) ->
    {ok, ReqBin} = ao_http_router:read_body(Req),
    % send message to su and get result from cu
    _MonitorPID = mu_push:start(TX = ar_bundles:deserialize(ReqBin)),
    cowboy_req:reply(201,
                     #{<<"Content-Type">> => <<"application/json">>},
                     jiffy:encode({[{id, ar_util:encode(TX#tx.id)}]}),
                     Req),
    {ok, Req}.

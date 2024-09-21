-module(cu_http).
-export([routes/0, handle/3]).
-include("src/include/ar.hrl").

routes() ->
    {"/cu", [ "/" ]}.

handle(<<"GET">>, [], Req) ->
    % Return node info the client.
    Wallet = su_registry:get_wallet(),
    cowboy_req:reply(200,
        #{<<"Content-Type">> => <<"application/json">>},
        jiffy:encode({[
            {<<"Unit">>, <<"Compute">>},
            {<<"Address">>, ar_util:encode(ar_wallet:to_address(Wallet))},
            {<<"Timestamp">>, list_to_binary(integer_to_list(erlang:system_time(millisecond)))}
        ]}),
        Req),
    {ok, Req};
handle(_, [ID], Req) when is_binary(ID) and byte_size(ID) == 43 ->
    % Handle a request for the outcome of an interaction on a process.
    % There are 3 different core forms/styles of these requests that can be intermingled:
    % - Requests for state output
    {ok, ReqBin} = ao_http_router:read_body(Req),
    _MonitorPID = mu_push:start(TX = ar_bundles:deserialize(ReqBin)),
    cowboy_req:reply(201,
        #{<<"Content-Type">> => <<"application/json">>},
        jiffy:encode({[{id, ar_util:encode(TX#tx.id)}]}),
        Req
    ),
    {ok, Req}.
-module(cu_http).
-export([routes/0, handle/3]).
-include("src/include/ao.hrl").

routes() ->
    {"/cu", ["/", "/:id", "/:id/:slot"]}.

handle(<<"GET">>, [], Req) ->
    % Return node info the client.
    Wallet = su_registry:get_wallet(),
    cowboy_req:reply(
        200,
        #{<<"Content-Type">> => <<"application/json">>},
        jiffy:encode(
            {[
                {<<"Unit">>, <<"Compute">>},
                {<<"Address">>, ar_util:encode(ar_wallet:to_address(Wallet))},
                {<<"Timestamp">>, list_to_binary(integer_to_list(erlang:system_time(millisecond)))}
            ]}
        ),
        Req
    ),
    {ok, Req};
handle(<<"GET">>, [ProcID, Msg], Req) ->
    Slot = parse_slot(Msg),
    Res = cu_process:result(ProcID, Slot, ao:get(store), ao:wallet()),
    ao_http:reply(Req, Res);
handle(<<"GET">>, [ProcID], Req) ->
    handle(<<"GET">>, [ProcID, undefined], Req);
handle(_, _, Req) ->
    cowboy_req:reply(404, #{}, <<"Not Implemented">>, Req),
    {ok, Req}.

parse_slot(undefined) -> undefined;
parse_slot(<<>>) -> undefined;
parse_slot(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 -> Bin;
parse_slot(Slot) -> list_to_integer(binary_to_list(Slot)).

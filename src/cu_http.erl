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
handle(<<"GET">>, [ProcID, Slot], Req) ->
    case dev_checkpoint:read(ProcID, Slot) of
        unavailable ->
            ResultLog =
                cu_process:run(
                    % TODO: Scheduler must only run until slot!
                    su_data:read_message(ProcID),
                    #{error_strategy => throw, to_slot => list_to_integer(binary_to_list(Slot))}
                ),
            {message_processed, _ID, Res} = lists:last(ResultLog),
            ar_bundles:print(Res),
            ao_http:reply(Req, Res);
        {ok, Result} ->
            Req2 = ao_http:reply(Req, Result),
            {ok, Req2}
    end;
handle(<<"GET">>, [ProcID], Req) ->
    handle(<<"GET">>, [ProcID, undefined], Req);
handle(_, _, Req) ->
    cowboy_req:reply(404, #{}, <<"Not Implemented">>, Req),
    {ok, Req}.

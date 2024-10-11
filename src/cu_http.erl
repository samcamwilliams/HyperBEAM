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
    case ao_cache:read_output(Store = ao:get(store), ProcID, parse_end(Msg)) of
        not_found ->
            ResultLog =
                cu_process:run(
                    ao_cache:read(Store, ProcID),
                    #{
                        error_strategy => throw,
                        to => Msg,
                        store => Store,
                        wallet => ao:wallet(),
                        terminate_on_idle => true
                    }
                ),
            {message_processed, _ID, Res} = lists:last(ResultLog),
            ao_http:reply(Req, Res);
        Res -> ao_http:reply(Req, Res)
    end;
handle(<<"GET">>, [ProcID], Req) ->
    handle(<<"GET">>, [ProcID, undefined], Req);
handle(_, _, Req) ->
    cowboy_req:reply(404, #{}, <<"Not Implemented">>, Req),
    {ok, Req}.

parse_end(undefined) -> undefined;
parse_end(<<>>) -> undefined;
parse_end(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 -> Bin;
parse_end(Slot) -> list_to_integer(binary_to_list(Slot)).

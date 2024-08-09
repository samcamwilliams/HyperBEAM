-module(su_http).
-export([init/2, start/0, allowed_methods/2]).
-define(WORKER_POOL, 100).
-define(PORT, 8081).

-include("include/ar.hrl").

start() ->
    application:ensure_all_started(cowboy),
    Dispatcher = cowboy_router:compile(
        [
            {'_', [
                {"/:proc_id/slot", ?MODULE, handle},
                {"/:proc_id", ?MODULE, handle},
                {"/", ?MODULE, handle}
            ]}
        ]
    ),
    cowboy:start_clear(?MODULE, [{port, ?PORT}], #{env => #{dispatch => Dispatcher}}).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    SplitPath = split_path(cowboy_req:path(Req)),
    handle(Method, SplitPath, Req, State).

split_path(Path) ->
    binary:split(Path, <<"/">>, [global, trim_all]).

read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

handle(<<"GET">>, [ProcID, <<"slot">>], Req, State) ->
    CurrentSlot = su_process:get_current_slot(su_registry:find(binary_to_list(ProcID))),
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        integer_to_list(CurrentSlot),
        Req),
    {ok, Req, State};
handle(<<"POST">>, [], Req, State) ->
    {ok, Body, Req2} = read_body(Req),
    Message = su_data:decode(Body),
    su:c(Message#tx.tags),
    ProcID = su_registry:find(binary_to_list(Message#tx.target)),
    Assignment = su_process:schedule(ProcID, Message),
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        su_data:encode(Assignment),
        Req2),
    {ok, Req2, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.
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
                {"/:id", ?MODULE, handle},
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
        #{<<"Content-Type">> => <<"text/plain">>},
        integer_to_list(CurrentSlot),
        Req),
    {ok, Req, State};
handle(<<"GET">>, [ID], Req, State) when byte_size(ID) =:= 43 ->
    Message = su_data:read_message(ID),
    cowboy_req:reply(200,
        #{<<"Content-Type">> => <<"application/binary">>},
        ar_bundles:serialize(Message),
        Req),
    {ok, Req, State};
handle(<<"POST">>, [], Req, State) ->
    {ok, Body, Req2} = read_body(Req),
    Message = ar_bundles:deserialize(Body),
    true = ar_bundles:verify_item(Message),
    ProcID = su_registry:find(binary_to_list(ar_util:encode(Message#tx.target))),
    Assignment = su_process:schedule(ProcID, Message),
    cowboy_req:reply(200,
        #{<<"Content-Type">> => <<"application/json">>},
        jiffy:encode({[
            {id, ar_util:encode(Assignment#tx.id)},
            {timestamp, list_to_binary(element(2, lists:keyfind("Timestamp", 1, Assignment#tx.tags)))}
        ]}),
        Req2),
    {ok, Req2, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.
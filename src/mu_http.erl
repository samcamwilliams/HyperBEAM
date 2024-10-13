-module(mu_http).

-export([routes/0, handle/3]).

-include("src/include/ao.hrl").

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
    #{ trace := Trace } = cowboy_req:match_qs([{trace, [], <<"none">>}], Req),
    ?c({push_start, reading_body, Trace}),
    {ok, ReqBin} = ao_http_router:read_body(Req),
    Logger = ao_logger:start(self()),
    ?c({logger_started, Logger}),
    MonitorPID = mu_push:start(Item = ar_bundles:deserialize(ReqBin), Logger),
    ?c({push_started, MonitorPID}),
    case Trace of
        <<"none">> ->
            ao_http:reply(
                Req,
                ar_bundles:sign_item(
                    #tx { tags = [
                        {<< "Pushing">>, ar_util:encode(Item#tx.id)},
                        {<< "Status">>, << "Running">> }
                    ]},
                    ao:wallet()
                )
            );
        TraceOpt -> trace(Req, TraceOpt, MonitorPID)
    end.

trace(Req, <<"all">>, MonitorPID) ->
    ?c({trace_waiting, MonitorPID}),
    receive
        {ao_logger, MonitorPID, done, Activities} ->
            ?c({trace_done, MonitorPID}),
            ao_http:reply(
                Req,
                lists:map(fun log_to_tx/1, Activities)
            )
    end.

log_to_tx({Status, Type, Details}) ->
    #tx {
        tags = [
            {<<"Action">>, <<"Log-Event">>},
            {<<"Status">>, to_binary(Status)},
            {<<"Type">>, to_binary(Type)}
        ],
        data = [Details]
    }.

to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
to_binary(Other) ->
    list_to_binary(lists:flatten(io_lib:format("~p", [Other]))).
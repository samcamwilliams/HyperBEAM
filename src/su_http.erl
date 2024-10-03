-module(su_http).

-export([handle/3, routes/0]).

-include("include/ao.hrl").

%%% SU HTTP Server API

routes() ->
    {"/su", % Namespace
     ["/timestamp",
      "/:proc_id/slot", % Routes to match
      "/:id",
      "/"]}.

handle(<<"GET">>, [], Req) ->
    Wallet = su_registry:get_wallet(),
    cowboy_req:reply(200,
                     #{<<"Content-Type">> => <<"application/json">>},
                     jiffy:encode({[{<<"Unit">>, <<"Scheduler">>},
                                    {<<"Address">>,
                                     ar_util:encode(
                                         ar_wallet:to_address(Wallet))},
                                    {<<"Timestamp">>,
                                     list_to_binary(integer_to_list(erlang:system_time(millisecond)))},
                                    {<<"Processes">>,
                                     lists:map(fun ar_util:encode/1,
                                               su_registry:get_processes())}]}),
                     Req),
    {ok, Req};
handle(<<"GET">>, [<<"timestamp">>], Req) ->
    {Timestamp, Height, Hash} = ao_client:arweave_timestamp(),
    cowboy_req:reply(200,
                     #{<<"Content-Type">> => <<"application/json">>},
                     jiffy:encode({[{<<"Timestamp">>, list_to_binary(integer_to_list(Timestamp))},
                                    {<<"Height">>, list_to_binary(integer_to_list(Height))},
                                    {<<"Hash">>, Hash}]}),
                     Req),
    {ok, Req};
handle(<<"GET">>, [ProcID, <<"slot">>], Req) ->
    CurrentSlot =
        su_process:get_current_slot(
            su_registry:find(binary_to_list(ProcID))),
    cowboy_req:reply(200,
                     #{<<"Content-Type">> => <<"text/plain">>},
                     integer_to_list(CurrentSlot),
                     Req),
    {ok, Req};
handle(<<"GET">>, [BinID], Req) ->
    ID = binary_to_list(BinID),
    #{to := To, from := From} =
        cowboy_req:match_qs([{from, [], undefined}, {to, [], undefined}], Req),
    % If neither 'to' nor 'from' is specified, send the message for the given ID
    % Else, send a stream of the assignments for the given range
    case {To, From} of
        {undefined, undefined} ->
            send_data(ID, Req);
        {_, _} ->
            send_stream(ID, From, To, Req)
    end,
    {ok, Req};
handle(<<"POST">>, [], Req) ->
    {ok, Body} = ao_http_router:read_body(Req),
    Message = ar_bundles:deserialize(Body),
    case {ar_bundles:verify_item(Message), lists:keyfind(<<"Type">>, 1, Message#tx.tags)} of
        {false, _} ->
            cowboy_req:reply(400,
                             #{<<"Content-Type">> => <<"application/json">>},
                             jiffy:encode({[{error, <<"Data item is not valid.">>}]}),
                             Req),
            {ok, Req};
        {true, {<<"Type">>, <<"Process">>}} ->
            su_data:write_message(Message),
            ao_client:upload(Message),
            cowboy_req:reply(201,
                             #{<<"Content-Type">> => <<"application/json">>},
                             jiffy:encode({[{id, ar_util:encode(Message#tx.id)},
                                            {timestamp, erlang:system_time(millisecond)}]}),
                             Req),
            {ok, Req};
        {true, _} ->
            % If the process-id is not specified, use the target of the message as the process-id
            AOProcID =
                case cowboy_req:match_qs([{'process', [], undefined}], Req) of
                    #{process := undefined} -> binary_to_list(ar_util:encode(Message#tx.target));
                    #{process := ProcessID} -> ProcessID
                end,
            ao_http:reply(
                Req,
                su_process:schedule(su_registry:find(AOProcID, true), Message)
            )
    end.

%% Private methods

% Send existing-SU GraphQL compatible results
send_stream(ProcID, undefined, To, Req) ->
    send_stream(ProcID, 0, To, Req);
send_stream(ProcID, From, undefined, Req) ->
    send_stream(ProcID, From, su_process:get_current_slot(su_registry:find(ProcID)), Req);
send_stream(ProcID, From, To, Req) when is_binary(From) ->
    send_stream(ProcID, list_to_integer(binary_to_list(From)), To, Req);
send_stream(ProcID, From, To, Req) when is_binary(To) ->
    send_stream(ProcID, From, list_to_integer(binary_to_list(To)), Req);
send_stream(ProcID, From, To, Req) ->
    % TODO: Add feedback on further assignments to the bundle
    {Assignments, More} = su_process:get_assignments(
            ProcID,
            From,
            To
        ),
    ao_http:reply(
        Req,
        ar_bundles:sign_item(#tx{
                tags = [
                    {<<"Type">>, <<"Assignments">>},
                    {<<"Process">>, ProcID},
                    {<<"Continues">>, atom_to_binary(More, utf8)}
                ],
                data = assignments_to_bundle(Assignments)
            },
            ao:wallet()
        )
    ).

send_data(ID, Req) ->
    Message = su_data:read_message(ID),
    ao_http:reply(
        Req,
        Message
    ).

assignments_to_bundle(Assignments) ->
    assignments_to_bundle(Assignments, #{}).
assignments_to_bundle([], Bundle) ->
    Bundle;
assignments_to_bundle([Assignment | Assignments], Bundle) ->
    {_, Slot} = lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags),
    {_, MessageID} = lists:keyfind(<<"Message">>, 1, Assignment#tx.tags),
    Message = su_data:read_message(MessageID),
    assignments_to_bundle(
        Assignments,
        Bundle#{
            Slot =>
                #{
                    <<"Assignment">> => Assignment,
                    <<"Message">> => Message
                }
        }
    ).

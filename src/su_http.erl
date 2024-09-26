-module(su_http).

-export([handle/3, routes/0]).

-include("include/ar.hrl").

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
            send_stream(tn1, ID, From, To, Req)
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
                case cowboy_req:match_qs([{'process-id', [], undefined}], Req) of
                    #{process_id := ProcessID} ->
                        ProcessID;
                    _ ->
                        binary_to_list(ar_util:encode(Message#tx.target))
                end,
            Assignment =
                su_process:schedule(
                    su_registry:find(AOProcID), Message),
            {JSONStruct} = ar_bundles:item_to_json_struct(Assignment),
            cowboy_req:reply(201,
                             #{<<"Content-Type">> => <<"application/json">>},
                             % TN.2: This should be the whole assignment.
                             jiffy:encode({[{timestamp,
                                             list_to_integer(element(2,
                                                                     lists:keyfind("Timestamp",
                                                                                   1,
                                                                                   Assignment#tx.tags)))},
                                            {id, ar_util:encode(Assignment#tx.id)}]}),
                             %|JSONStruct
                             Req),
            {ok, Req}
    end.

%% Private methods

% Send existing-SU GraphQL compatible results
send_stream(Version, ProcID, undefined, To, Req) ->
    send_stream(Version, ProcID, 0, To, Req);
send_stream(Version, ProcID, From, undefined, Req) ->
    send_stream(Version,
                ProcID,
                From,
                su_process:get_current_slot(
                    su_registry:find(ProcID)),
                Req);
send_stream(Version, ProcID, From, To, Req) when is_binary(From) ->
    send_stream(Version, ProcID, list_to_integer(binary_to_list(From)), To, Req);
send_stream(Version, ProcID, From, To, Req) when is_binary(To) ->
    send_stream(Version, ProcID, From, list_to_integer(binary_to_list(To)), Req);
send_stream(tn1, ProcID, From, To, Req) ->
    cowboy_req:reply(200,
                     #{<<"Content-Type">> => <<"application/json">>},
                     jiffy:encode(assignments_to_json(su_process:get_assignments(ProcID,
                                                                                 From,
                                                                                 To))),
                     Req).

assignments_to_json({Assignments, HasNextPage}) ->
    {[{<<"page_info">>, {[{<<"has_next_page">>, HasNextPage}]}},
      {<<"edges">>,
       [{[{<<"cursor">>, element(2, lists:keyfind(<<"Nonce">>, 1, Assignment#tx.tags))},
          {<<"node">>, assignment_to_json(Assignment)}]}
        || Assignment <- Assignments]}]}.

assignment_to_json(Assignment) ->
    {_, MessageID} = lists:keyfind(<<"Message">>, 1, Assignment#tx.tags),
    Message = su_data:read_message(MessageID),
    {[{<<"message">>, ar_bundles:item_to_json_struct(Message)},
      {<<"assignment">>, ar_bundles:item_to_json_struct(Assignment)}]}.

send_data(ID, Req) ->
    Message = su_data:read_message(ID),
    cowboy_req:reply(200,
                     #{<<"Content-Type">> => <<"application/binary">>},
                     ar_bundles:serialize(Message),
                     Req).

%%% SU HTTP Client
get_assignments(Process) ->
    case su_registry:find(Process#tx.id, false) of
        not_found ->
            % Process is not held locally. Find SU-Locator record and get from remote.
            not_implemented;
        PID ->
            su_process:get_assignments(PID, false)
    end.

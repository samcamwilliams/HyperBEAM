-module(su_http).
-export([handle/3, routes/0]).
-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% SU HTTP Server API

routes() ->
    % Namespace
    {"/su", [
        "/timestamp",
        % Routes to match
        "/:proc_id/slot",
        "/:id",
        "/"
    ]}.

handle(<<"GET">>, [], Req) ->
    Wallet = su_registry:get_wallet(),
    cowboy_req:reply(
        200,
        #{<<"Content-Type">> => <<"application/json">>},
        jiffy:encode(
            {[
                {<<"Unit">>, <<"Scheduler">>},
                {<<"Address">>,
                    ar_util:encode(
                        ar_wallet:to_address(Wallet)
                    )},
                {<<"Processes">>,
                    lists:map(
                        fun ar_util:encode/1,
                        su_registry:get_processes()
                    )}
            ]}
        ),
        Req
    ),
    {ok, Req};
handle(<<"GET">>, [ProcID, <<"slot">>], Req) ->
    CurrentSlot =
        su_process:get_current_slot(
            su_registry:find(binary_to_list(ProcID))
        ),
    cowboy_req:reply(
        200,
        #{<<"Content-Type">> => <<"text/plain">>},
        integer_to_list(CurrentSlot),
        Req
    ),
    {ok, Req};
handle(<<"GET">>, [BinID], Req) ->
    Store = ao:get(store),
    ID = binary_to_list(BinID),
    ?c({getting_assignments, ID}),
    #{to := To, from := From} =
        cowboy_req:match_qs([{from, [], 0}, {to, [], infinity}], Req),
    ?c({range, {To, From}}),
    send_stream(Store, ID, From, To, Req),
    {ok, Req};
handle(<<"POST">>, [], Req) ->
    Store = ao:get(store),
    {ok, Body} = ao_http_router:read_body(Req),
    Message = ar_bundles:deserialize(Body),
    case {ar_bundles:verify_item(Message), lists:keyfind(<<"Type">>, 1, Message#tx.tags)} of
        {false, _} ->
            cowboy_req:reply(
                400,
                #{<<"Content-Type">> => <<"application/json">>},
                jiffy:encode({[{error, <<"Data item is not valid.">>}]}),
                Req
            ),
            {ok, Req};
        {true, {<<"Type">>, <<"Process">>}} ->
            ao_cache:write(Store, Message),
            ao_client:upload(Message),
            ?c(old_style_reply),
            cowboy_req:reply(
                201,
                #{<<"Content-Type">> => <<"application/json">>},
                jiffy:encode(
                    {[
                        {id, ar_util:encode(Message#tx.id)},
                        {timestamp, erlang:system_time(millisecond)}
                    ]}
                ),
                Req
            ),
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
send_stream(Store, ProcID, undefined, To, Req) ->
    send_stream(Store, ProcID, 0, To, Req);
send_stream(Store, ProcID, From, undefined, Req) ->
    send_stream(Store, ProcID, From, su_process:get_current_slot(su_registry:find(ProcID)), Req);
send_stream(Store, ProcID, From, To, Req) when is_binary(From) ->
    send_stream(Store, ProcID, list_to_integer(binary_to_list(From)), To, Req);
send_stream(Store, ProcID, From, To, Req) when is_binary(To) ->
    send_stream(Store, ProcID, From, list_to_integer(binary_to_list(To)), Req);
send_stream(Store, ProcID, From, To, Req) ->
    {Timestamp, Height, Hash} = su_timestamp:get(),
    {Assignments, More} = su_process:get_assignments(
        ProcID,
        From,
        To
    ),
    ?c({got_assignments, length(Assignments)}),
    % TODO: Find out why tags are not getting included in bundle
    Bundle = #tx{
        tags =
            [
                {<<"Type">>, <<"Schedule">>},
                {<<"Process">>, list_to_binary(ProcID)},
                {<<"Continues">>, atom_to_binary(More, utf8)},
                {<<"Timestamp">>, list_to_binary(integer_to_list(Timestamp))},
                {<<"Block-Height">>, list_to_binary(integer_to_list(Height))},
                {<<"Block-Hash">>, Hash}
            ] ++
                case Assignments of
                    [] ->
                        [];
                    _ ->
                        {_, FromSlot} = lists:keyfind(
                            <<"Slot">>, 1, (hd(Assignments))#tx.tags
                        ),
                        {_, ToSlot} = lists:keyfind(
                            <<"Slot">>, 1, (lists:last(Assignments))#tx.tags
                        ),
                        [
                            {<<"From">>, FromSlot},
                            {<<"To">>, ToSlot}
                        ]
                end,
        data = assignments_to_bundle(Store, Assignments)
    },
    ?c({bundle, ar_util:encode(Bundle#tx.id)}),
    ao_http:reply(
        Req,
        ar_bundles:sign_item(Bundle, ao:wallet())
    ).

send_data(Store, ID, Req) ->
    Message = ao_cache:read(Store, ID),
    ao_http:reply(
        Req,
        Message
    ).

assignments_to_bundle(Store, Assignments) ->
    assignments_to_bundle(Store, Assignments, #{}).
assignments_to_bundle(_, [], Bundle) ->
    Bundle;
assignments_to_bundle(Store, [Assignment | Assignments], Bundle) ->
    {_, Slot} = lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags),
    {_, MessageID} = lists:keyfind(<<"Message">>, 1, Assignment#tx.tags),
    Message = ao_cache:read(Store, MessageID),
    assignments_to_bundle(
        Store,
        Assignments,
        Bundle#{
            Slot =>
                #{
                    <<"Assignment">> => Assignment,
                    <<"Message">> => Message
                }
        }
    ).

%%% Tests

% schedule_many_test() ->
%     application:ensure_all_started(ao),
%     su_data:reset_data(),
%     Wallet = ar_wallet:new(),
%     Proc = ar_bundles:sign_item(#tx{ data = <<"test-proc">> }, Wallet),
%     su_registry:find(binary_to_list(ar_util:encode(Proc#tx.id)), true),
%     SignedItem = ar_bundles:sign_item(#tx{ tags = [{<<"Target">>, ar_util:encode(Proc#tx.id)}], data = <<"test">> }, Wallet),
%     ?c(signed_item),
%     lists:foreach(
%         fun(X) ->
%             ?c({assigning, X}),
%             Res = ao_client:schedule(SignedItem),
%             ?c({assigned, X, Res})
%         end,
%         lists:seq(1, 100)
%     ),
%     true.
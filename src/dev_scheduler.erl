-module(dev_scheduler).
%%% Local scheduling functions:
-export([schedule/1]).
%%% CU-flow functions:
-export([init/2, end_of_schedule/1, uses/0, checkpoint/1]).
%%% MU-flow functions:
-export([push/2]).
-ao_debug(print).

-include("include/ao.hrl").

%%% A simple scheduler scheme for AO.
%%% The module is composed of three parts:
%%% 1. The HTTP interface for the scheduler itself.
%%% 2. The 'push' client function needed for dispatching messages to the SU.
%%% 3. The device client functions needed in order to manage a schedule
%%%    for the execution of processes.

%%% HTTP API functions:
schedule(Item) ->
    {ok, Output} = su_http:handle(Item),
    {ok, Output}.

%%% MU pushing client functions:
push(CarrierMsg, State = #{ logger := Logger }) ->
    Msg = ar_bundles:hd(CarrierMsg),
    ?c(pushing_message),
    ar_bundles:print(Msg),
    case ao_client:schedule(Msg) of
        {_, Assignment} ->
            {ok, State#{assignment => Assignment}};
        Error ->
            ao_logger:log(Logger, Error),
            {error, Error}
    end.

%%% Process/device client functions:
init(State, [{<<"Location">>, Location} | _]) ->
    case State of
        #{schedule := Schedule} when Schedule =/= [] ->
            {ok, State};
        _ ->
            {ok, update_schedule(State#{su_location => Location})}
    end;
init(State, _) ->
    {ok, State}.

end_of_schedule(State) -> {ok, update_schedule(State)}.

update_schedule(State = #{ process := Proc }) ->
    Store = maps:get(store, State, ao:get(store)),
    CurrentSlot = maps:get(slot, State, 0),
    ToSlot = maps:get(to, State),
    ?c({updating_schedule_current, CurrentSlot, to, ToSlot}),
    % TODO: Get from slot via checkpoint
    Assignments = ao_client:get_assignments(Proc#tx.id, CurrentSlot, ToSlot),
    ?c({got_assignments_from_su, length(Assignments)}),
    lists:foreach(
        fun(Assignment) ->
            ?c({writing_recvd_assignment, ar_util:id(Assignment#tx.id)}),
            ao_cache:write(Store, Assignment)
        end,
        Assignments
    ),
    State#{schedule => Assignments}.

checkpoint(State) -> {ok, State}.

uses() -> all.
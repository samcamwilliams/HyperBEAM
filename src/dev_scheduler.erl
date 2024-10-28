-module(dev_scheduler).
%%% Local scheduling functions:
-export([schedule/1]).
%%% CU-flow functions:
-export([init/3, end_of_schedule/1, uses/0, checkpoint/1]).
%%% MU-flow functions:
-export([push/2]).

-include("include/ao.hrl").

%%% A simple scheduler scheme for AO.
%%% The module is composed of three parts:
%%% 1. The HTTP interface for the scheduler itself.
%%% 2. The 'push' client function needed for dispatching messages to the SU.
%%% 3. The device client functions needed in order to manage a schedule
%%%    for the execution of processes.

%%% HTTP API functions:
schedule(Item) -> su_http:handle(Item).

%%% MU pushing client functions:
push(Item, State = #{ logger := Logger }) ->
    case ao_client:schedule(Item) of
        {_, Assignment} ->
            {ok, State#{assignment => Assignment}};
        Error ->
            ao_logger:log(Logger, Error),
            {error, Error}
    end.

%%% Process/device client functions:
init(State, [{<<"Location">>, Location} | _], _) ->
    case State of
        #{schedule := []} ->
            {ok, update_schedule(State#{su_location => Location})};
        _ ->
            {ok, State}
    end;
init(State, _, _) ->
    {ok, State}.

end_of_schedule(State) -> {ok, update_schedule(State)}.

update_schedule(State = #{store := Store, process := Proc, schedule := []}) ->
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
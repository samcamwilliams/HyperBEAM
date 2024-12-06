-module(dev_scheduler).
%%% Local scheduling functions:
-export([schedule/1]).
%%% CU-flow functions:
-export([init/2, end_of_schedule/1, uses/0, checkpoint/1]).
%%% MU-flow functions:
-export([push/2]).

-include("include/hb.hrl").

%%% A simple scheduler scheme for AO.
%%% The module is composed of three parts:
%%% 1. The HTTP interface for the scheduler itself.
%%% 2. The 'push' client function needed for dispatching messages to the SU.
%%% 3. The device client functions needed in order to manage a schedule
%%%    for the execution of processes.

%%% HTTP API functions:
schedule(Item) ->
    {ok, Output} = dev_scheduler_interface:handle(Item),
    %?debug_wait(1000),
    {ok, Output}.

%%% MU pushing client functions:
push(Msg, State = #{ logger := Logger }) ->
    ?event(su_scheduling_message_for_push),
    case hb_client:schedule(Msg) of
        {ok, Assignment} ->
            ?event({scheduled_message, hb_util:id(Assignment, unsigned)}),
            {ok, State#{assignment => Assignment}};
        Error ->
            ?event({error_scheduling_message, Error}),
            hb_logger:log(Logger, Error),
            {error, Error}
    end.

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
    Store = maps:get(store, State, hb_opts:get(store)),
    CurrentSlot = maps:get(slot, State, 0),
    ToSlot = maps:get(to, State),
    ?event({updating_schedule_current, CurrentSlot, to, ToSlot}),
    % TODO: Get from slot via checkpoint. (Done, right?)
    Assignments = hb_client:get_assignments(hb_util:id(Proc, signed), CurrentSlot, ToSlot),
    ?event({got_assignments_from_su,
        [
            {
                element(2, lists:keyfind(<<"Assignment">>, 1, A#tx.tags)),
                hb_util:id(A, signed),
                hb_util:id(A, unsigned)
            }
        || A <- Assignments ]}),
    lists:foreach(
        fun(Assignment) ->
            ?event({writing_assignment_to_cache, hb_util:id(Assignment, unsigned)}),
            hb_cache:write(Store, Assignment)
        end,
        Assignments
    ),
    State#{schedule => Assignments}.

checkpoint(State) -> {ok, State}.

uses() -> all.
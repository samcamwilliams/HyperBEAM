-module(dev_scheduler).
-export([init/3, end_of_schedule/1, uses/0]).

-include("include/ao.hrl").

%%% A simple scheduler scheme for AO.

init(State, [{<<"Location">>, Location} | _], _) ->
    update_schedule(State#{su_location => Location});
init(State, _, _) ->
    {ok, State}.

end_of_schedule(State) -> update_schedule(State).

update_schedule(State = #{ process := Proc, slot := Slot, schedule := [] }) ->
    ToSlot = maps:get(to_slot, State, undefined),
    % TODO: Get from slot via checkpoint
    Assignments = ao_client:get_assignments(Proc#tx.id, Slot, ToSlot),
    {ok, State#{schedule => Assignments, next_slot => Slot + length(Assignments)}};
update_schedule(State) -> {ok, State}.

uses() -> all.

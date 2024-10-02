-module(dev_scheduler).
-export([init/3, end_of_schedule/1, uses/0]).

-include("include/ao.hrl").

%%% A simple scheduler scheme for AO.

init(State, [{<<"Location">>, Location}|_], _) ->
    ao:c(init_scheduler),
    case State of
        #{ schedule := [] } -> {ok, update_schedule(State#{ su_location => Location })};
        _ -> {ok, State}
    end;
init(State, _, _) ->
    ao:c(scheduler_ignoring_init),
    {ok, State}.

end_of_schedule(State) -> update_schedule(State).

update_schedule(State = #{ su_location := _Location, process := Proc }) ->
    {ok, State#{ schedule => ao_client:get_assignments(Proc#tx.id) }}.

uses() -> all.
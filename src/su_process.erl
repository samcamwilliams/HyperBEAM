-module(su_process).
-export([start/1, schedule/2, get_current_slot/1]).
-export([new_proc_test/0]).
-record(state, {id, current, wallet = ar_wallet:new() }).

-include("include/ar.hrl").

start(ProcID) ->
    server(#state{id = ProcID, current = su_data:get_current_slot(ProcID)}).

schedule(ProcID, Message) when is_list(ProcID) ->
    schedule(su_registry:find(ProcID), Message);
schedule(ProcID, Message) ->
    ProcID ! {schedule, Message, self()},
    receive
        {scheduled, Message, Assignment} ->
            Assignment
    end.

get_current_slot(ProcID) ->
    ProcID ! {get_current_slot, self()},
    receive
        {current_slot, CurrentSlot} ->
            CurrentSlot
    end.

server(State) ->
    receive
        {schedule, Message, Reply} ->
            {Assignment, NextState} = assign(State, Message),
            Reply ! {scheduled, Message, Assignment},
            server(NextState);
        {get_current_slot, Reply} ->
            Reply ! {current_slot, State#state.current},
            server(State)
    end.

assign(State, Message) ->
    su:c(su_data:write_message(State#state.id, State#state.current + 1, Message)),
    TXWithoutTags = ar_tx:new(list_to_binary(State#state.id), 0, 0, <<>>),
    TX = TXWithoutTags#tx { tags = [
            {<<"type">>, <<"su">>},
            {<<"slot">>, State#state.current + 1}
        ]
    },
    TXSigned = ar_tx:sign(TX, State#state.wallet),
    su:c(su_data:write_assignment(State#state.id, State#state.current + 1, TXSigned)),
    {TXSigned, State#state{current = State#state.current + 1}}.

%% TESTS

new_proc_test() ->
    ProcID = "test",
    schedule(ProcID, #{id => 1}),
    schedule(ProcID, #{id => 2}),
    schedule(ProcID, #{id => 3}),
    3 = su_data:get_current_slot(ProcID),
    ok.
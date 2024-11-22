-module(dev_dedup).
-export([init/2, execute/2, uses/0]).

%%% A device that deduplicates messages to a process.
%%% Only runs on the first pass.

init(State, [<<"Variant">>, <<"1.0">>]) ->
    {ok, State#{ dedup => [] }}.

execute(Message, State = #{ pass := 1, dedup := Dedup }) ->
    case lists:member(ID = hb_util:id(Message), Dedup) of
        true ->
            {break, State};
        false ->
            {ok, State#{ dedup := [ID | Dedup] }}
    end;
execute(_Message, State) ->
    {ok, State}.

uses() -> all.
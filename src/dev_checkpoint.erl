-module(dev_checkpoint).
-export([uses/0, init/3, execute/3, read/2]).

-include("src/include/ao.hrl").

-define(DEFAULT_DATA_DIR, "data").
-define(DEFAULT_FREQ, 2).

read(_ProcID, _Slot) ->
    case read_result(_ProcID, _Slot) of
        not_found -> unavailable;
        Result -> {ok, Result}
    end.

uses() -> all.

init(State, Params, undefined) ->
    init(State, Params, #{});
init(State = #{to_slot := To, process := Proc}, _Params, PrivParams) ->
    Dir = maps:get(data_dir, PrivParams, ?DEFAULT_DATA_DIR),
    StateDir = Dir ++ "/checkpoints/" ++ binary_to_list(ar_util:encode(Proc#tx.id)) ++ "/state/",
    NewPrivS = #{frequency => maps:get(frequency, PrivParams, ?DEFAULT_FREQ), data_dir => Dir},
    case file:list_dir(StateDir) of
        {ok, Names} when Names =/= [] ->
            ?c(Names),
            ClosestSlot =
                lists:max(
                    [X || X <- lists:map(fun erlang:list_to_integer/1, Names), X =< To]
                ),
            {ok, RawBin} = file:read_file(StateDir ++ integer_to_list(ClosestSlot)),
            TX = #tx{data = Memory} = ar_bundles:deserialize(RawBin),
            ar_bundles:print(TX),
            ?c([{to, To}, {closest_slot, ClosestSlot}]),
            {ok,
                (maps:get(deserialize, State, fun(S, _) -> S#{checkpoint => Memory} end))(
                    State#{slot => ClosestSlot}, Memory
                ),
                NewPrivS};
        _ ->
            {ok, State, NewPrivS}
    end.

execute(Msg, State, PrivS) ->
    case isCheckpointSlot(Msg, State, PrivS) of
        true -> write_checkpoint(Msg, State, PrivS);
        false -> ok
    end,
    write_result(Msg, State, PrivS),
    {ok, State#{fs => #{}}};
execute(_Msg, State, _) ->
    {ok, State}.

isCheckpointSlot(#tx{data = #{<<"Assignment">> := Assignment}}, _State, #{frequency := Freq}) ->
    slot(Assignment) rem Freq == 0.

slot(Assignment) ->
    ar_bundles:print(Assignment),
    {_, SlotBin} = lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags),
    list_to_integer(binary_to_list(SlotBin)).

write_checkpoint(
    #tx{data = #{<<"Assignment">> := Assignment}}, #{process := Proc, serialize := Serialize}, #{
        data_dir := Dir
    }
) ->
    % Slot = slot(Assignment),
    % ProcID = binary_to_list(ar_util:encode(Proc#tx.id)),
    % {ok, Memory} = Serialize(),
    % su_data:write_message(
    %     #tx{
    %         tags =
    %             [
    %                 {<<"Type">>, <<"Checkpoint">>},
    %                 {<<"Process">>, ProcID}
    %             ],
    %         data = Memory
    %     },
    %     Dir ++ "/checkpoints/" ++
    %         ProcID ++ "/state/" ++ integer_to_list(Slot)
    % ),
    ok.

read_result(ProcID, Slot) ->
    Dir = ?DEFAULT_DATA_DIR,
    Path = Dir ++ "/checkpoints/" ++ binary_to_list(ar_util:encode(ProcID)),
    su_data:read_message(
        Slot,
        Path
    ).

write_result(
    #tx{data = #{<<"Assignment">> := Assignment}},
    #{result := Res, process := Proc},
    #{data_dir := Dir}
) ->
    Slot = slot(Assignment),
    ProcID = binary_to_list(ar_util:encode(Proc#tx.id)),
    su_data:write_message(
        Res,
        Dir ++ "/checkpoints/" ++
            ProcID ++ "/results/" ++ integer_to_list(Slot)
    ),
    ok.

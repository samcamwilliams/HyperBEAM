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
init(State, _Params, PrivParams) ->
    % TODO: Read the latest checkpoint if it exists.
    {ok,
        State#{
            fs => #{}
        },
        #{
            frequency => maps:get(frequency, PrivParams, ?DEFAULT_FREQ),
            data_dir => maps:get(data_dir, PrivParams, ?DEFAULT_DATA_DIR)
        }}.

execute(Msg, State, PrivS) ->
    case isCheckpointSlot(Msg, State, PrivS) of
        true -> write_checkpoint(State);
        false -> ok
    end,
    write_result(Msg, State, PrivS),
    {ok, State#{fs => #{}}};
execute(_Msg, State, _) ->
    {ok, State}.

isCheckpointSlot(#tx{data = #{<<"Assignment">> := Assignment}}, _State, #{frequency := Freq}) ->
    slot(Assignment) rem Freq == 0.

slot(Assignment) ->
    {_, SlotBin} = lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags),
    list_to_integer(binary_to_list(SlotBin)).

write_checkpoint(_State) -> ok.

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
            ProcID ++ "/" ++ integer_to_list(Slot)
    ),
    ok.

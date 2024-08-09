-module(su_data).
-export([init/0, get_current_slot/1]).
-export([read_message/2, write_message/3]).
-export([read_assignment/2, write_assignment/3]).
-export([encode/1, decode/1]).
-define(ROOT, "data").

init() ->
    filelib:ensure_dir(?ROOT),
    ok.

get_current_slot(ProcID) ->
    case file:list_dir(?ROOT ++ "/" ++ ProcID ++ "/assignments") of
        {ok, []} -> 0;
        {ok, Files} ->
            lists:max([list_to_integer(filename:rootname(F)) || F <- Files]);
        {error, _} -> 0
    end.

read_message(ProcID, Slot) ->
    case file:read_file(?ROOT ++ "/" ++ ProcID ++ "/messages/" ++ integer_to_list(Slot) ++ ".json") of
        {ok, Message} -> decode(Message);
        {error, _} -> not_found
    end.

write_message(ProcID, Slot, Message) ->
    FN = ?ROOT ++ "/" ++ ProcID ++ "/messages/" ++ integer_to_list(Slot) ++ ".json",
    filelib:ensure_dir(FN),
    file:write_file(FN, encode(Message)).

read_assignment(ProcID, Slot) ->
    case file:read_file(?ROOT ++ "/" ++ ProcID ++ "/assignments/" ++ integer_to_list(Slot) ++ ".json") of
        {ok, Assignment} -> decode(Assignment);
        {error, _} -> not_found
    end.

write_assignment(ProcID, Slot, Assignment) ->
    FN = ?ROOT ++ "/" ++ ProcID ++ "/assignments/" ++ integer_to_list(Slot) ++ ".json",
    filelib:ensure_dir(FN),
    file:write_file(FN, encode(Assignment)).

encode(Message) when is_list(Message) ->
    encode(maps:from_list(Message));
encode(Message) ->
    jiffy:encode(ar_tx:tx_to_json_struct(Message)).

decode(Message) ->
    {List} = jiffy:decode(Message),
    ar_tx:json_struct_to_tx(maps:from_list(List)).
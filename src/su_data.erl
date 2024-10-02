-module(su_data).
-export([init/0, get_current_slot/1]).
-export([read_message/1, write_message/1]).
-export([read_assignment/2, write_assignment/2]).
-export([encode/1, decode/1]).
-export([reset_data/0]).
-export([store_and_retrieve_test/0]).

-define(ROOT, "data").

-include("include/ar.hrl").

init() ->
    filelib:ensure_dir(?ROOT),
    ok.

get_current_slot("") -> error;
get_current_slot(ProcID) ->
    case file:list_dir(?ROOT ++ "/assignments/" ++ ProcID) of
        {ok, []} -> {-1, <<>>};
        {ok, Files} ->
            AssignmentFile = lists:max([list_to_integer(filename:rootname(F)) || F <- Files]),
            Assignment = read_assignment(ProcID, AssignmentFile),
            {
                list_to_integer(binary_to_list(element(2, lists:keyfind(<<"Nonce">>, 1, Assignment#tx.tags)))),
                ar_util:decode(element(2, lists:keyfind(<<"Hash-Chain">>, 1, Assignment#tx.tags)))
            };
        {error, _} -> {-1, <<>>}
    end.

read_message(MessageID) when is_binary(MessageID) ->
    read_message(binary_to_list(MessageID));
read_message(MessageID) ->
    case file:read_file(?ROOT ++ "/messages/" ++ MessageID) of
        {ok, Message} -> decode(Message);
        {error, _} -> not_found
    end.

write_message(Message) ->
    FN = ?ROOT ++ "/messages/" ++ binary_to_list(ar_util:encode(Message#tx.id)),
    write_message(Message, FN).

write_message(Message, FN) ->
    filelib:ensure_dir(FN),
    file:write_file(FN, encode(Message)).

read_assignment(ProcID, Slot) ->
    case file:read_file(?ROOT ++ "/assignments/" ++ ProcID ++ "/" ++ integer_to_list(Slot)) of
        {ok, Assignment} -> decode(Assignment);
        {error, _} -> not_found
    end.

write_assignment(ProcID, Assignment) ->
    Slot = element(2, lists:keyfind("Slot", 1, Assignment#tx.tags)),
    FN = ?ROOT ++ "/assignments/" ++ ProcID ++ "/" ++ Slot,
    filelib:ensure_dir(FN),
    file:write_file(FN, encode(Assignment)).

encode(Message) ->
    ar_bundles:serialize(Message).

decode(Message) ->
    ar_bundles:deserialize(Message).

%% NEVER, EVER, _EVER_ RUN THIS IN PROD.
%% You will lose money.
reset_data() ->
    os:cmd("rm -Rf " ++ ?ROOT),
    init().

store_and_retrieve_test() ->
    su_data:write_message(Msg = ar_bundles:sign_item(#tx {}, ar_wallet:new())),
    Msg = su_data:read_message(ar_util:encode(Msg#tx.id)).
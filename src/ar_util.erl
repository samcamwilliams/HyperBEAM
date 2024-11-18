-module(ar_util).
-export([encode/1, decode/1, safe_encode/1, safe_decode/1, find_value/2,
         find_value/3]).
-export([remove_common/2]).

%% @doc Encode a binary to URL safe base64 binary string.
encode(Bin) ->
  b64fast:encode(Bin).

%% @doc Try to decode a URL safe base64 into a binary or throw an error when
%% invalid.
decode(Input) ->
  b64fast:decode(Input).

safe_encode(Bin) when is_binary(Bin) ->
  encode(Bin);
safe_encode(Bin) ->
  Bin.

%% @doc Safely decode a URL safe base64 into a binary returning an ok or error
%% tuple.
safe_decode(E) ->
  try
    D = decode(E),
    {ok, D}
  catch
    _:_ ->
      {error, invalid}
  end.

%% @doc Find the value associated with a key in parsed a JSON structure list.
find_value(Key, List) ->
  ar_util:find_value(Key, List, undefined).

find_value(Key, Map, Default) when is_map(Map) ->
  case maps:find(Key, Map) of
    {ok, Value} ->
      Value;
    error ->
      Default
  end;
find_value(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Val} ->
      Val;
    false ->
      Default
  end.

%% @doc Remove the common prefix from two strings, returning the remainder of the
%% first string. This function also coerces lists to binaries where appropriate,
%% returning the type of the first argument.
remove_common(MainStr, SubStr) when is_binary(MainStr) and is_list(SubStr) ->
    remove_common(MainStr, list_to_binary(SubStr));
remove_common(MainStr, SubStr) when is_list(MainStr) and is_binary(SubStr) ->
    binary_to_list(remove_common(list_to_binary(MainStr), SubStr));
remove_common(<< X:8, Rest1/binary>>, << X:8, Rest2/binary>>) ->
    remove_common(Rest1, Rest2);
remove_common([X|Rest1], [X|Rest2]) ->
    remove_common(Rest1, Rest2);
remove_common([$/|Path], _) -> Path;
remove_common(Rest, _) -> Rest.
-module(ar_util).
-export([id/1, encode/1, decode/1, safe_encode/1, safe_decode/1, find_value/2, find_value/3]).
-export([remove_common/2]).

%% @doc Encode an ID in any format to a normalized, b64u 43 character binary.
id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
	Bin;
id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
	b64fast:encode(Bin);
id(Data) when is_list(Data) ->
	id(list_to_binary(Data)).

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
        {ok, Value} -> Value;
        error -> Default
    end;
find_value(Key, List, Default) ->
	case lists:keyfind(Key, 1, List) of
		{Key, Val} -> Val;
		false -> Default
	end.

remove_common([X|Rest1], [X|Rest2]) ->
    remove_common(Rest1, Rest2);
remove_common([$/|Path], _) -> Path;
remove_common(Rest, _) -> Rest.
-module(ar_util).
-export([encode/1, decode/1, safe_encode/1, safe_decode/1]).

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
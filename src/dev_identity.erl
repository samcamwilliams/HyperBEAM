-module(dev_identity).
-export([info/0, keys/1]).
-include_lib("eunit/include/eunit.hrl").

%%% The identity device: Simply return a key from the message as it is found
%%% in the message's underlying Erlang map. Private keys (`priv[.*]`) are 
%%% not included.

%% @doc Return the info for the identity device.
info() -> #{ default => fun get_public/2 }.

%% @doc Get the public keys of a message.
keys(Msg) ->
	{
		ok,
		lists:filter(
			fun(Key) -> not is_private(Key) end,
			maps:keys(Msg)
		)
	}.

%% @doc Return the value associated with the key as it exists in the message's
%% underlying Erlang map.
get_public(Key, Msg) ->
	{ok, PublicKeys} = keys(Msg),
	case lists:member(Key, PublicKeys) of
		true -> {ok, maps:get(Key, Msg)};
		false -> {error, {badkey, Key}}
	end.

%% @doc Check if a key is private.
is_private(Key) ->
	Str = key_to_list(Key),
	lists:prefix("priv", Str).

%% @doc Convert a key to a list.
key_to_list(Key) when is_atom(Key) ->
	atom_to_list(Key);
key_to_list(Key) when is_binary(Key) ->
	binary_to_list(Key);
key_to_list(Key) when is_list(Key) ->
	binary_to_list(iolist_to_binary(Key)).

%%% Tests

%%% Internal module functionality tests:
get_keys_mod_test() ->
	?assertEqual([a], maps:keys(#{a => 1})).

is_private_mod_test() ->
	?assertEqual(true, is_private(private)),
	?assertEqual(true, is_private(<<"private">>)),
	?assertEqual(true, is_private(<<"private.foo">>)),
	?assertEqual(false, is_private(a)),
	% Generate a long list of characters and check it does not match.
	?assertEqual(false, is_private([ C || C <- lists:seq($a, $z) ])).

%%% Device functionality tests:

keys_from_device_test() ->
	?assertEqual({ok, [a]}, hb_device:call(#{a => 1}, keys)).

private_keys_are_filtered_test() ->
	?assertEqual(
		{ok, [a]},
		hb_device:call(#{a => 1, private => 2}, keys)
	),
	?assertEqual(
		{ok, [a]},
		hb_device:call(#{a => 1, "priv_foo" => 4}, keys)
	).

cannot_get_private_keys_test() ->
	?assertEqual(
		{error, {badkey, private_key}},
		hb_device:call(#{ a => 1, private_key => 2 }, private_key)
	).

key_from_device_test() ->
	?assertEqual({ok, 1}, hb_device:call(#{a => 1}, a)).
-module(dev_identity).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% The identity device: Simply return a key from the message as it is found
%%% in the message's underlying Erlang map. Private keys (`priv[.*]`) are 
%%% not included.
info(State) ->
	#{
		keys =>
			lists:filter(
				fun(Key) -> not is_private(Key) end,
				maps:keys(State)
			),
		handler =>
			fun(keys, Msg) -> {ok, maps:get(keys, info(Msg))};
				(Key, Msg) ->
					{ok, maps:get(Key, Msg)}
			end
	}.

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

key_from_device_test() ->
	?assertEqual({ok, 1}, hb_device:call(#{a => 1}, a)).
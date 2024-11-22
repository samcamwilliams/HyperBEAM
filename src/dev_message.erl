-module(dev_message).
-export([info/0, keys/1, id/1, unsigned_id/1, signers/1, set/3, remove/2]).
-export([no_serialize/0]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% The identity device: Simply return a key from the message as it is found
%%% in the message's underlying Erlang map. Private keys (`priv[.*]`) are 
%%% not included.

%% @doc Return the info for the identity device.
info() ->
	#{
		default => fun get_public_map_key/2
	}.

%% @doc Return the ID of a message. If the message already has an ID, return
%% that. Otherwise, return the signed ID.
id(M) ->
	{ok, raw_id(M, signed)}.

%% @doc Wrap a call to the `hb_util:id/2` function, which returns the
%% unsigned ID of a message.
unsigned_id(M) ->
	{ok, raw_id(M, unsigned)}.

%% @doc Encode an ID in any format to a normalized, b64u 43 character binary.
raw_id(Item) -> raw_id(Item, unsigned).
raw_id(TX, Type) when is_record(TX, tx) ->
	hb_util:encode(ar_bundles:id(TX, Type));
raw_id(Map, Type) when is_map(Map) ->
	hb_util:encode(ar_bundles:id(hb_message:message_to_tx(Map), Type));
raw_id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
	Bin;
raw_id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
	hb_util:encode(Bin);
raw_id(Data, _) when is_list(Data) ->
	raw_id(list_to_binary(Data)).

%% @doc Return the signers of a message.
signers(M) ->
	{ok, hb_util:list_to_numbered_map(hb_message:signers(M))}.

%% @doc Set keys in a message. Takes a map of key-value pairs and sets them in
%% the message, overwriting any existing values.
set(Message1, NewValuesMsg, Opts) ->
	{
		ok,
		maps:merge(
			Message1,
			maps:map(
				fun(Key, Value) ->
					?no_prod("Do not ship this without careful thought. "
						"Do we want to resolve values during set?"),
					{ok, ResolvedValue} = hb_pam:resolve(Key, Value, Opts),
					ResolvedValue
				end,
				hb_pam:keys(NewValuesMsg, Opts)
			)
		)
	}.

%% @doc Remove a key or keys from a message.
remove(Message1, #{ key := Key }) when is_atom(Key) ->
	{ok, maps:remove(Key, Message1)};
remove(Message1, #{ keys := Keys }) ->
	lists:foldl(
		fun(Key, {ok, Message1n}) ->
			% Note: We do not call `hb_pam:resolve` here because it may pollute
			% the HashPath with an extremely large number of calls.
			remove(Message1n, #{ key => Key }) end,
		{ok, Message1},
		Keys
	).

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
get_public_map_key(Key, Msg) ->
	{ok, PublicKeys} = keys(Msg),
	case lists:member(Key, PublicKeys) of
		true -> {ok, maps:get(Key, Msg)};
		false -> {error, {badkey, Key}}
	end.

%% @doc Return the keys that should not be serialized, but should be included
%% in the message's public list of keys.
no_serialize() -> {ok, [id, unsigned_id]}.

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
	?assertEqual({ok, [a]}, hb_pam:resolve(#{a => 1}, keys)).

private_keys_are_filtered_test() ->
	?assertEqual(
		{ok, [a]},
		hb_pam:resolve(#{a => 1, private => 2}, keys)
	),
	?assertEqual(
		{ok, [a]},
		hb_pam:resolve(#{a => 1, "priv_foo" => 4}, keys)
	).

cannot_get_private_keys_test() ->
	?assertEqual(
		{error, {badkey, private_key}},
		hb_pam:resolve(#{ a => 1, private_key => 2 }, private_key)
	).

key_from_device_test() ->
	?assertEqual({ok, 1}, hb_pam:resolve(#{a => 1}, a)).

remove_test() ->
	Msg = #{ <<"Key1">> => <<"Value1">>, <<"Key2">> => <<"Value2">> },
	?assertEqual({ok, #{ <<"Key2">> => <<"Value2">> }},
		hb_pam:resolve(Msg, #{ key => <<"Key1">> })),
	?assertEqual({ok, #{}},
		hb_pam:resolve(Msg, #{ keys => [<<"Key1">>, <<"Key2">>] })).
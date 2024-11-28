-module(hb_message).
-export([load/2]).
-export([serialize/1, serialize/2, deserialize/1, deserialize/2, signers/1]).
-export([message_to_tx/1, tx_to_message/1]).
-export([binary_to_term/1, term_to_binary/1]).
%%% Debugging tools:
-export([print/1, format/1, format/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @moduledoc This module acts an adapter between messages, as modeled in the
%%% Permaweb Abstract Machine (PAM), and their underlying binary representations.
%%% See `docs/permaweb-abstract-machine.md` for details on PAM. Unless you are
%%% implementing a new message serialization format, you should not need to 
%%% interact with this module directly. Instead, use the `hb_pam`
%%% interfaces to interact with all messages. The `dev_message` module
%%% implements a device interface for handling messages as the default PAM
%%% device.

%% @doc The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).
%% @doc The list of TX fields that users can set directly.
-define(USER_TX_KEYS,
	[id, unsigned_id, last_tx, owner, target, data, signature]).
-define(REGEN_KEYS, [id, unsigned_id]).

%% @doc Pretty-print a message.
print(Msg) -> print(Msg, 0).
print(Msg, Indent) ->
    io:format(standard_error, "~s", [lists:flatten(format(Msg, Indent))]).

%% @doc Format a message for printing, optionally taking an indentation level
%% to start from.
format(Item) -> format(Item, 0).
format(Bin, Indent) when is_binary(Bin) ->
	hb_util:format_indented(
		hb_util:format_binary(Bin),
		Indent
	);
format(Map, Indent) when is_map(Map) ->
    Header = hb_util:format_indented("Message {~n", Indent),
    Res = lists:map(
        fun({Key, Val}) ->
			NormKey = hb_pam:to_key(Key, #{ error_strategy => ignore }),
			KeyStr = 
				case NormKey of
					Key ->
						io_lib:format("~p", [NormKey]);
					undefined ->
						io_lib:format("~p [!!! INVALID KEY !!!]", [Key]);
					_ ->
						io_lib:format("~p [raw: ~p]", [NormKey, Key])
				end,
			hb_util:format_indented(
				"~s := ~s~n",
				[
					lists:flatten(KeyStr),
					case Val of
						NextMap when is_map(NextMap) ->
							hb_util:format_map(NextMap, Indent + 2);
						Bin when is_binary(Bin) ->
							hb_util:format_binary(Bin);
						Other ->
							io_lib:format("~p", [Other])
					end
				],
				Indent + 1
			)
		end,
        maps:to_list(Map)
    ),
	case Res of
		[] -> "[Empty map]";
		_ ->
			lists:flatten(
				Header ++ Res ++ hb_util:format_indented("}", Indent)
			)
	end;
format(Item, Indent) ->
    % Whatever we have is not a message map.
    hb_util:format_indented("[UNEXPECTED VALUE] ~p", [Item], Indent).

%% @doc Return the signers of a message. For now, this is just the signer
%% of the message itself. In the future, we will support multiple signers.
signers(Msg) ->
	[ar_bundles:signer(Msg)].

load(Store, ID) when is_binary(ID)
		andalso (byte_size(ID) == 43 orelse byte_size(ID) == 32) ->
	hb_cache:read_message(Store, ID);
load(Store, Path) ->
	hb_cache:read(Store, Path).

%% @doc Serialize a message to a binary representation, either as JSON or the
%% binary format native to the message/bundles spec in use.
serialize(M) -> serialize(M, binary).
serialize(M, json) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M));
serialize(M, binary) ->
	ar_bundles:serialize(message_to_tx(M)).

%% @doc Deserialize a message from a binary representation.
deserialize(B) -> deserialize(B, binary).
deserialize(J, json) ->
	{JSONStruct} = jiffy:decode(J),
	ar_bundles:json_struct_to_item(JSONStruct);
deserialize(B, binary) ->
	tx_to_message(ar_bundles:deserialize(B)).

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
message_to_tx(Binary) when is_binary(Binary) ->
	Binary;
message_to_tx(TX) when is_record(TX, tx) -> TX;
message_to_tx(M) when is_map(M) ->
	% Get the keys that will be serialized, excluding private keys. Note that
	% we do not call hb_pam:resolve here because we want to include all keys
	% in the underlying map, except the private ones.
	Keys = maps:keys(M),
	% Translate the keys into a binary map. If a key has a value that is a map,
	% we recursively turn its children into messages. Notably, we do not simply
	% call message_to_tx/1 on the inner map because that would lead to adding
	% an extra layer of nesting to the data.
	MsgKeyMap =
		maps:from_list(
			lists:map(
				fun(Key) ->
					case maps:find(Key, M) of
						{ok, Map} when is_map(Map) ->
							{
								hb_pam:key_to_binary(Key),
								maps:map(
									fun(_, InnerValue) ->
										message_to_tx(InnerValue)
									end,
									Map
								)
							};
						{ok, Value} when is_binary(Value) ->
							{hb_pam:key_to_binary(Key), Value};
						{ok, Value} ->
							{
								hb_pam:key_to_binary(Key),
								?MODULE:term_to_binary(Value)
							}
					end
				end,
				lists:filter(
					fun(Key) ->
						% Filter keys that the user could set directly, but
						% should be regenerated when moving msg -> TX, as well
						% as private keys.
						not lists:member(Key, ?REGEN_KEYS) andalso
							not hb_private:is_private(Key)
					end,
					Keys
				)
			)
		),
	% Get the fields and default values of the tx record.
	TXFields = record_info(fields, tx),
	DefaultValues = tl(tuple_to_list(#tx{})),
	DefaultTXList = lists:zip(TXFields, DefaultValues),
	% Iterate through the default fields, replacing them with the values from
	% the message map if they are present.
	{RemainingMap, BaseTXList} = lists:foldl(
		fun({Field, Default}, {RemMap, Acc}) ->
			case maps:get(NormKey = hb_pam:key_to_binary(Field), MsgKeyMap, not_found) of
				not_found -> {RemMap, [Default | Acc]};
				Value -> {maps:remove(NormKey, RemMap), [Value | Acc]}
			end
		end,
		{MsgKeyMap, []},
		DefaultTXList
	),
	% Rebuild the tx record from the new list of fields and values.
	TXWithoutTags = list_to_tuple([tx | lists:reverse(BaseTXList)]),
	% Calculate which set of the remaining keys will be used as tags.
	{Tags, DataItems} =
		lists:partition(
			fun({_Key, Value}) when byte_size(Value) =< ?MAX_TAG_VAL -> true;
				(_) -> false
			end,
			[ 
					{Key, maps:get(Key, RemainingMap)}
				||
					Key <- maps:keys(RemainingMap)
			]
		),
	% We don't let the user set the tags directly, but they can instead set any
	% number of keys to short binary values, which will be included as tags.
	TX = TXWithoutTags#tx { tags = Tags },
	% Set the data based on the remaining keys.
	TXWithData = case {TX#tx.data, DataItems} of
		{_, []} ->
			% There are no remaining data items so we use the default data value.
			TX;
		{?DEFAULT_DATA, _} ->
			% The user didn't set the data field and there are remaining data
			% items so we use those as the data.
			TX#tx { data = maps:from_list(DataItems) };
		{Data, _} when is_map(Data) ->
			% The user already set some data and there are remaining data items
			% so we merge the two.
			TX#tx { data = maps:merge(Data, maps:from_list(DataItems)) };
		{Data, _} ->
			% This should not happen.
			throw(
				{incompatible_data_and_keys,
					{data, Data},
					{keys, maps:keys(RemainingMap)}
				}
			)
	end,
	ar_bundles:reset_ids(TXWithData);
message_to_tx(Other) ->
	?event({unexpected_message_form, {explicit, Other}}),
	throw(invalid_tx).

%% @doc Convert non-binary values to binary for serialization.
%% CAUTION: This is not a standardized type conversion. We should formalize
%% this, or use an existing standard, before it becomes entrenched.
binary_to_term(<< "PAM1_INT::", Integer:64 >>) -> 
	?no_prod("Non-standardized type conversion invoked."),
	Integer;
binary_to_term(<< "PAM1_ATOM::", Atom:8/binary >>) ->
	?no_prod("Non-standardized type conversion invoked."),
	binary_to_existing_atom(Atom, latin1);
binary_to_term(Value) when is_binary(Value) ->
	Value.

%% @doc Convert a term to a binary representation.
term_to_binary(Value) when is_binary(Value) -> Value;
term_to_binary(Value) when is_integer(Value) ->
	?no_prod("Non-standardized type conversion invoked."),
	<< "PAM1_INT::", (integer_to_binary(Value, 64))/binary >>;
term_to_binary(Atom) when is_atom(Atom) ->
	?no_prod("Non-standardized type conversion invoked."),
	<< "PAM1_ATOM::", (atom_to_binary(Atom, latin1))/binary >>.


%% @doc Convert a #tx record into a message map recursively.
tx_to_message(Binary) when is_binary(Binary) -> Binary;
tx_to_message(TX) ->
	% We need to generate a map from each field of the tx record.
	% Get the raw fields and values of the tx record and pair them.
	Fields = record_info(fields, tx),
	Values = tl(tuple_to_list(TX)),
	TXKeyVals = lists:zip(Fields, Values),
	% Convert the list of key-value pairs into a map.
	UnfilteredTXMap = maps:from_list(TXKeyVals),
	TXMap = maps:with(?USER_TX_KEYS, UnfilteredTXMap),
	% Next, merge the tags into the map.
	MapWithoutData = maps:merge(TXMap, maps:from_list(TX#tx.tags)),
	% Finally, merge the data into the map.
	case TX#tx.data of
		Data when is_map(Data) ->
			% If the data is a map, we need to recursively turn its children
			% into messages from their tx representations.
			MapWithoutData#{
				data =>
					maps:map(
						fun(_, InnerValue) ->
							tx_to_message(InnerValue)
						end,
						Data
					)
			};
		Data when is_binary(Data) ->
			MapWithoutData#{ data => ?MODULE:binary_to_term(Data) };
		Data ->
			?event({unexpected_data_type, {explicit, Data}}),
			?event({was_processing, {explicit, TX}}),
			throw(invalid_tx)
	end.

%%% @doc Unit tests for the module.

basic_map_to_tx_test() ->
	Msg = #{ normal_key => <<"NORMAL_VALUE">> },
	TX = message_to_tx(Msg),
	?assertEqual([{<<"normal_key">>, <<"NORMAL_VALUE">>}], TX#tx.tags).

%% @doc Test that we can convert a message into a tx record and back.
single_layer_message_to_tx_test() ->
	Msg = #{
		last_tx => << 2:256 >>,
		owner => << 3:4096 >>,
		target => << 4:256 >>,
		data => <<"DATA">>,
		<<"Special-Key">> => <<"SPECIAL_VALUE">>
	},
	TX = message_to_tx(Msg),
	?assertEqual(maps:get(last_tx, Msg), TX#tx.last_tx),
	?assertEqual(maps:get(owner, Msg), TX#tx.owner),
	?assertEqual(maps:get(target, Msg), TX#tx.target),
	?assertEqual(maps:get(data, Msg), TX#tx.data),
	?assertEqual({<<"Special-Key">>, <<"SPECIAL_VALUE">>},
		lists:keyfind(<<"Special-Key">>, 1, TX#tx.tags)).

%% @doc Test that we can convert a #tx record into a message correctly.
single_layer_tx_to_message_test() ->
	TX = #tx {
		id = << 0:256 >>,
		unsigned_id = << 1:256 >>,
		last_tx = << 2:256 >>,
		owner = << 3:256 >>,
		target = << 4:256 >>,
		data = <<"DATA">>,
		tags = [{<<"special_key">>, <<"SPECIAL_KEY">>}]
	},
	Msg = tx_to_message(TX),
	?assertEqual(maps:get(<<"special_key">>, Msg), <<"SPECIAL_KEY">>),
	?assertEqual(maps:get(data, Msg), <<"DATA">>),
	?assertEqual(maps:get(id, Msg), << 0:256 >>),
	?assertEqual(maps:get(unsigned_id, Msg), << 1:256 >>),
	?assertEqual(maps:get(last_tx, Msg), << 2:256 >>),
	?assertEqual(maps:get(owner, Msg), << 3:256 >>),
	?assertEqual(maps:get(target, Msg), << 4:256 >>).

%% @doc Test that the keys that are present in both maps match. If a key has a
%% value that is a map, we recursively check that the keys in the nested map
%% match.
present_keys_match(Map1, Map2) ->
	Keys1 = maps:keys(Map1),
	Keys2 = maps:keys(Map2),
	lists:all(
		fun(Key) ->
			Val1 = maps:get(Key, Map1, not_found),
			Val2 = maps:get(Key, Map2, not_found),
			case is_map(Val1) andalso is_map(Val2) of
				true -> present_keys_match(Val1, Val2);
				false ->
					case Val1 == Val2 of
						true -> true;
						false ->
							?event({key_mismatch, {explicit, {Key, Val1, Val2}}}),
							false
					end
			end
		end,
		lists:filter(fun(Key) -> lists:member(Key, Keys2) end, Keys1)
	).

%% @doc Test that two txs match. Note: This function uses tx_to_message/1
%% underneath, which (depending on the test) could potentially lead to false
%% positives.
txs_match(TX1, TX2) ->
	present_keys_match(tx_to_message(TX1), tx_to_message(TX2)).

%% @doc Test that we can convert a nested message into a tx record and back.
nested_message_to_tx_and_back_test() ->
	Msg = #{
		<<"tx_depth">> => <<"outer">>,
		data => #{
			<<"tx_map_item">> =>
				#{
					<<"tx_depth">> => <<"inner">>,
					data => <<"DATA">>
				}
		}
	},
	?assert(present_keys_match(Msg, tx_to_message(message_to_tx(Msg)))).

%% @doc Test that we can convert a signed tx into a message and back.
signed_tx_to_message_and_back_test() ->
	TX = #tx {
		data = <<"TEST_DATA">>,
		tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
	},
	SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
	?assert(ar_bundles:verify_item(SignedTX)),
	SignedMsg = tx_to_message(SignedTX),
	SignedTX2 = message_to_tx(SignedMsg),
	?assert(ar_bundles:verify_item(SignedTX2)).

signed_deep_tx_to_message_and_back_test() ->
	TX = #tx {
		tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}],
		data = #{
			<<"NESTED_TX">> =>
				#tx {
					data = <<"NESTED_DATA">>,
					tags = [{<<"NESTED_KEY">>, <<"NESTED_VALUE">>}]
				}
		}
	},
	SignedTX =
		ar_bundles:deserialize(
			ar_bundles:sign_item(TX, hb:wallet())
		),
	?assert(ar_bundles:verify_item(SignedTX)),
	SignedMsg = tx_to_message(SignedTX),
	SignedTX2 = message_to_tx(SignedMsg),
	?assert(
		txs_match(SignedTX, SignedTX2)
	).

signed_deep_tx_serialize_and_deserialize_test() ->
	TX = #tx {
		tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}],
		data = #{
			<<"NESTED_TX">> =>
				#tx {
					data = <<"NESTED_DATA">>,
					tags = [{<<"NESTED_KEY">>, <<"NESTED_VALUE">>}]
				}
		}
	},
	SignedTX = ar_bundles:deserialize(
		ar_bundles:sign_item(TX, hb:wallet())
	),
	?assert(ar_bundles:verify_item(SignedTX)),
	SerializedTX = serialize(SignedTX),
	DeserializedTX = deserialize(SerializedTX),
	?assert(
		present_keys_match(
			tx_to_message(SignedTX),
			DeserializedTX
		)
	).

calculate_unsigned_message_id_test() ->
	Msg = #{
		data => <<"DATA">>,
		<<"special_key">> => <<"SPECIAL_KEY">>
	},
	UnsignedTX = message_to_tx(Msg),
	UnsignedMessage = tx_to_message(UnsignedTX),
	?assertEqual(
		hb_util:encode(ar_bundles:id(UnsignedTX, unsigned)),
		hb_util:id(UnsignedMessage, unsigned)
	).

sign_serialize_deserialize_verify_test() ->
	Msg = #{
		data => <<"DATA">>,
		<<"special_key">> => <<"SPECIAL_KEY">>
	},
	TX = message_to_tx(Msg),
	SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
	?assert(ar_bundles:verify_item(SignedTX)),
	SerializedMsg = serialize(SignedTX),
	DeserializedMsg = deserialize(SerializedMsg),
	DeserializedTX = message_to_tx(DeserializedMsg),
	?assert(ar_bundles:verify_item(DeserializedTX)).

unsigned_id_test() ->
	UnsignedTX = ar_bundles:normalize(#tx { data = <<"TEST_DATA">> }),
	UnsignedMessage = tx_to_message(UnsignedTX),
	?assertEqual(
		hb_util:encode(ar_bundles:id(UnsignedTX, unsigned)),
		hb_util:id(UnsignedMessage, unsigned)
	).

signed_id_test_disabled() ->
	TX = #tx {
		data = <<"TEST_DATA">>,
		tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
	},
	SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
	?assert(ar_bundles:verify_item(SignedTX)),
	SignedMsg = tx_to_message(SignedTX),
	?assertEqual(
		hb_util:encode(ar_bundles:id(SignedTX, signed)),
		hb_util:id(SignedMsg, signed)
	).
-module(hb_message).
-export([id/1, id/2, load/2, serialize/2, deserialize/2, signers/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The main module for engaing with messages. All messages are represented as
%%% maps in Erlang at the data layer, but should be accessed through this
%%% module such that any necessary executions can be made to retrieve their
%%% underlying data.

%% @doc The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).
%% @doc The list of TX fields that users can set directly.
-define(USER_TX_FIELDS,
	[id, unsigned_id, last_tx, owner, target, data, signature]).

get(Message, Key) ->
	hb_device:call(Message, Key).

%% @doc Encode an ID in any format to a normalized, b64u 43 character binary.
id(Item) -> id(Item, unsigned).
id(TX, Type) when is_record(TX, tx) ->
	case ar_bundles:id(TX, Type) of
		not_signed -> not_signed;
		ID -> id(ID, Type)
	end;
id(Map, unsigned) when is_map(Map) ->
	id(ar_bundles:normalize(#tx { data = Map }), unsigned);
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
	Bin;
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
	ar_util:encode(Bin);
id(Data, _) when is_list(Data) ->
	id(list_to_binary(Data)).

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
	message_to_tx(M).

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
message_to_tx(Binary) when is_binary(Binary) ->
	Binary;
message_to_tx(TX) when is_record(TX, tx) -> TX;
message_to_tx(M) when is_map(M) ->
	% Get the keys that will be serialized, excluding private keys.
	{ok, Keys} = hb_device:call(M, keys),
	% Translate the keys into a binary map. If a key has a value that is a map,
	% we recursively turn its children into messages. Notably, we do not simply
	% call message_to_tx/1 on the inner map because that would lead to adding
	% an extra layer of nesting to the data.
	MsgKeyMap =
		maps:from_list(
			lists:map(
				fun(Key) ->
					case hb_device:call(M, Key) of
						{ok, Map} when is_map(Map) ->
							{
								Key,
								maps:map(
									fun(_, InnerValue) ->
										message_to_tx(InnerValue)
									end,
									Map
								)
							};
						{ok, Value} -> {Key, Value};
						not_found -> {Key, not_found}
					end
				end,
				Keys
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
			case maps:get(Field, MsgKeyMap, not_found) of
				not_found -> {RemMap, [Default | Acc]};
				Value ->
					{maps:remove(Field, RemMap), [Value | Acc]}
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
	% Finally, we set the data based on the remaining keys.
	case {TX#tx.data, DataItems} of
		{_, []} ->
			% There are no remaining data items so we use the default data value.
			TX;
		{?DEFAULT_DATA, _} ->
			% The user didn't set the data field and there are remaining data items
			% so we use those as the data.
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
	end;
message_to_tx(Other) ->
	?event({unexpected_message_form, {explicit, Other}}),
	throw(invalid_tx).

%% @doc Deserialize a message from a binary representation.
deserialize(B) -> deserialize(B, binary).
deserialize(J, json) ->
    {JSONStruct} = jiffy:decode(J),
    ar_bundles:json_struct_to_item(JSONStruct);
deserialize(B, binary) ->
    tx_to_message(B).

%% @doc Convert a #tx record into a message map recursively.
tx_to_message(Binary) when is_binary(Binary) -> Binary;
tx_to_message(TX) ->
	% First, get the fields and values of the tx record and pair them.
	Fields = record_info(fields, tx),
	Values = tl(tuple_to_list(TX)),
	TXKeyVals = lists:zip(Fields, Values),
	% Then, convert the list of key-value pairs into a map.
	UnfilteredTXMap = maps:from_list(TXKeyVals),
	TXMap = maps:with(?USER_TX_FIELDS, UnfilteredTXMap),
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
			MapWithoutData#{ data => Data };
		Data ->
			?event({unexpected_data_type, {explicit, Data}}),
			?event({was_processing, {explicit, TX}}),
			throw(invalid_tx)
	end.

%%% @doc Unit tests for the module.

basic_map_to_tx_test() ->
	Msg = #{ <<"NORMAL_KEY">> => <<"NORMAL_VALUE">> },
	TX = message_to_tx(Msg),
	?assertEqual([{<<"NORMAL_KEY">>, <<"NORMAL_VALUE">>}], TX#tx.tags).

%% @doc Test that we can convert a message into a tx record and back.
single_layer_message_to_tx_test() ->
	Msg = #{
		id => << 0:256 >>,
		unsigned_id => << 1:256 >>,
		last_tx => << 2:256 >>,
		owner => << 3:256 >>,
		target => << 4:256 >>,
		data => <<"DATA">>,
		special_key => <<"SPECIAL_KEY">>
	},
	TX = message_to_tx(Msg),
	?assertEqual(maps:get(id, Msg), TX#tx.id),
	?assertEqual(maps:get(unsigned_id, Msg), TX#tx.unsigned_id),
	?assertEqual(maps:get(last_tx, Msg), TX#tx.last_tx),
	?assertEqual(maps:get(owner, Msg), TX#tx.owner),
	?assertEqual(maps:get(target, Msg), TX#tx.target),
	?assertEqual(maps:get(data, Msg), TX#tx.data),
	?assertEqual({special_key, <<"SPECIAL_KEY">>},
		lists:keyfind(special_key, 1, TX#tx.tags)).

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
					?assertEqual(Val1, Val2),
					true
			end
		end,
		lists:filter(fun(Key) -> lists:member(Key, Keys2) end, Keys1)
	).

%% @doc Test that we can convert a nested message into a tx record and back.
nested_message_to_tx_and_back_test() ->
	Msg = #{
		id => << 1:256 >>,
		<<"tx_depth">> => <<"outer">>,
		data => #{
			<<"tx_map_item">> =>
				#{
					id => << 2:256 >>,
					<<"tx_depth">> => <<"inner">>,
					data => <<"DATA">>
				}
		}
	},
	?assert(present_keys_match(Msg, tx_to_message(message_to_tx(Msg)))).
-module(hb_message).
-export([id/1, id/2, load/2, serialize/2, deserialize/2, signers/1]).
-include("include/hb.hrl").

%%% The main module for engaing with messages. All messages are represented as
%%% maps in Erlang at the data layer, but should be accessed through this
%%% module such that any necessary executions can be made to retrieve their
%%% underlying data.

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

serialize(M, json) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M));
serialize(M, binary) ->
	ar_bundles:serialize(M).

deserialize(J, json) ->
    {JSONStruct} = jiffy:decode(J),
    ar_bundles:json_struct_to_item(JSONStruct);
deserialize(B, binary) ->
    ar_bundles:deserialize(B).
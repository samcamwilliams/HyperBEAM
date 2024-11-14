-module(ao_message).
-export([id/1, id/2, load/2, serialize/2, deserialize/2]).
-include("include/ao.hrl").


%% @doc Encode an ID in any format to a normalized, b64u 43 character binary.
id(Item) -> id(Item, unsigned).
id(TX, Type) when is_record(TX, tx) ->
	case ar_bundles:id(TX, Type) of
		not_signed -> not_signed;
		ID -> id(ID, Type)
	end;
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
	Bin;
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
	ar_util:encode(Bin);
id(Data, _) when is_list(Data) ->
	id(list_to_binary(Data)).

load(Store, ID) when is_binary(ID)
		andalso (byte_size(ID) == 43 orelse byte_size(ID) == 32) ->
	ao_cache:read_message(Store, ID);
load(Store, Path) ->
	ao_store:read(Store, Path).

serialize(M, json) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M));
serialize(M, binary) ->
	ar_bundles:serialize(M).

deserialize(J, json) ->
    {JSONStruct} = jiffy:decode(J),
    ar_bundles:json_struct_to_item(JSONStruct);
deserialize(B, binary) ->
    ar_bundles:deserialize(B).
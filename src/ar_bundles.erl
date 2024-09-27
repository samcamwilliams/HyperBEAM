%%% @doc Module for creating, signing, and verifying Arweave data items and bundles.
-module(ar_bundles).

-export([new_item/4, sign_item/2, verify_item/1]).
-export([encode_tags/1, decode_tags/1]).
-export([serialize/1, serialize/2, deserialize/1, deserialize/2]).
-export([item_to_json_struct/1, json_struct_to_item/1]).
-export([data_item_signature_data/1]).

-include("include/ar.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(BUNDLE_TAGS, [
    {<<"Bundle-Format">>, <<"Binary">>},
    {<<"Bundle-Version">>, <<"2.0.0">>}
]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Create a new data item.
new_item(Target, Anchor, Tags, Data) ->
    #tx{
        format = ans104,
        target = Target,
        last_tx = Anchor,
        tags = Tags,
        data = Data,
        data_size = byte_size(Data)
    }.

%% @doc Sign a data item.
sign_item(DataItem, {PrivKey, {KeyType, Owner}}) ->
    SignedDataItem = DataItem#tx{ format = ans104, owner = Owner, signature_type = KeyType },
    Sig = ar_wallet:sign(PrivKey, data_item_signature_data(SignedDataItem)),
    ID = crypto:hash(sha256, <<Sig/binary>>),
    SignedDataItem#tx{ id = ID, signature = Sig }.

%% @doc Verify the validity of a data item.
verify_item(DataItem) ->
    ValidID = verify_data_item_id(DataItem),
    ValidSignature = verify_data_item_signature(DataItem),
    ValidTags = verify_data_item_tags(DataItem),
    ValidID andalso ValidSignature andalso ValidTags.

%%%===================================================================
%%% Private functions.
%%%===================================================================


%% @doc Generate the data segment to be signed for a data item.
data_item_signature_data(DataItem) ->
    List = [
        utf8_encoded("dataitem"),
        utf8_encoded("1"),
        utf8_encoded("1"), %% Only SignatureType 1 is supported for now (RSA 4096)
        <<(DataItem#tx.owner)/binary>>,
        <<(DataItem#tx.target)/binary>>,
        <<(DataItem#tx.last_tx)/binary>>,
        encode_tags(DataItem#tx.tags),
        <<(DataItem#tx.data)/binary>>
    ],
    ar_deep_hash:hash(List).

%% @doc Verify the data item's ID matches the signature.
verify_data_item_id(DataItem) ->
    ExpectedID = crypto:hash(sha256, DataItem#tx.signature),
    DataItem#tx.id == ExpectedID.

%% @doc Verify the data item's signature.
verify_data_item_signature(DataItem) ->
    SignatureData = data_item_signature_data(DataItem),
    ar_wallet:verify({DataItem#tx.signature_type, DataItem#tx.owner}, SignatureData, DataItem#tx.signature).

%% @doc Verify the validity of the data item's tags.
verify_data_item_tags(DataItem) ->
    ValidCount = length(DataItem#tx.tags) =< 128,
    ValidTags = lists:all(fun({Name, Value}) ->
        byte_size(Name) =< 1024 andalso byte_size(Value) =< 3072
    end, DataItem#tx.tags),
    ValidCount andalso ValidTags.

%% @doc Convert a #tx record to its binary representation.
serialize(TX) -> serialize(TX, binary).
serialize(BundleList, binary) when is_list(BundleList) ->
    serialize(#tx { tags = ?BUNDLE_TAGS, data = BundleList }, binary);
serialize(InitialTX = #tx { data = Data }, binary) when is_map(Data) ->
    {ManifestID, SerializedData} = serialize_bundle_map_data(Data),
    MapTX = InitialTX#tx {
        data = SerializedData,
        tags = add_manifest_tags(add_bundle_tags(InitialTX#tx.tags), ManifestID)
    },
    serialize(
        MapTX,
        binary
    );
serialize(InitialTX = #tx { data = Data }, binary) when is_list(Data) ->
    serialize(
        InitialTX#tx {
            data = serialize_bundle_data(Data),
            tags = add_bundle_tags(InitialTX#tx.tags)
        },
        binary
    );
serialize(TX, binary) when is_binary(TX) -> TX;
serialize(RawTX, binary) ->
    TX = update_id(RawTX),
    EncodedTags = encode_tags(TX#tx.tags),
    <<
        (encode_signature_type(TX#tx.signature_type))/binary,
        (TX#tx.signature)/binary,
        (TX#tx.owner)/binary,
        (encode_optional_field(TX#tx.target))/binary,
        (encode_optional_field(TX#tx.last_tx))/binary,
        (encode_tags_size(TX#tx.tags, EncodedTags))/binary,
        EncodedTags/binary,
        (TX#tx.data)/binary
    >>;
serialize(TX, json) ->
    jiffy:encode(item_to_json_struct(TX)).

update_id(TX = #tx{}) when TX#tx.id =:= ?DEFAULT_ID; TX#tx.signature =:= ?DEFAULT_SIG ->
    TX#tx{ id = data_item_signature_data(TX) };
update_id(TX) ->
    TX.

add_bundle_tags(Tags) -> ?BUNDLE_TAGS ++ (Tags -- ?BUNDLE_TAGS).

add_manifest_tags(Tags, ManifestID) ->
    lists:filter(fun({<<"Bundle-Map">>, _}) -> false;
                    (_) -> true
                end, Tags) ++ [{<<"Bundle-Map">>, ManifestID}].

serialize_bundle_data(List) ->
    finalize_bundle_data(lists:map(fun to_serialized_pair/1, List)).

finalize_bundle_data(Processed) ->
    Length = <<(length(Processed)):256/integer>>,
    Index = << << (byte_size(Data)):256/integer, ID/binary >> || {ID, Data} <- Processed >>,
    Items = << << Data/binary >> || {_, Data} <- Processed >>,
    << Length/binary, Index/binary, Items/binary >>.

to_serialized_pair(Item) ->
    Serialized = serialize(Item, binary),
    {Item#tx.id, Serialized}.

serialize_bundle_map_data(Map) ->
    % TODO: Make this compatible with the normal manifest spec.
    % For now we just serialize the map to a JSON string of Key=>TXID
    BinItems = maps:map(fun(_, Item) -> to_serialized_pair(Item) end, Map),
    Index = maps:map(fun(_, {TXID, _}) -> TXID end, BinItems),
    Manifest = new_manifest(Index),
    {Manifest#tx.id, finalize_bundle_data([to_serialized_pair(Manifest) | maps:values(BinItems)])}.

new_manifest(Index) ->
    new_item(
        <<>>,
        <<>>,
        [
            {<<"Data-Protocol">>, <<"Bundle-Map">>},
            {<<"Variant">>, <<"0.0.1">>}
        ],
        jiffy:encode(Index)
    ).

%% @doc Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
%% little-endian format which is why we encode to <<1, 0>>.
encode_signature_type({rsa, 65537}) ->
    <<1, 0>>;
encode_signature_type(_) ->
    unsupported_tx_format.

%% @doc Encode an optional field (target, anchor) with a presence byte.
encode_optional_field(<<>>) ->
    <<0>>;
encode_optional_field(Field) ->
    <<1:8/integer, Field/binary>>.

%% @doc Encode a UTF-8 string to binary.
utf8_encoded(String) ->
    unicode:characters_to_binary(String, utf8).

encode_tags_size([], <<>>) ->
    <<0:64/little-integer, 0:64/little-integer>>;
encode_tags_size(Tags, EncodedTags) ->
    <<(length(Tags)):64/little-integer, (byte_size(EncodedTags)):64/little-integer>>.

%% @doc Encode tags into a binary format using Apache Avro.
encode_tags([]) ->
    <<>>;
encode_tags(Tags) ->
    EncodedBlocks = lists:flatmap(fun({Name, Value}) ->
        EncName = encode_avro_string(Name),
        EncValue = encode_avro_string(Value),
        [EncName, EncValue]
    end, Tags),
    TagCount = length(Tags),
    ZigZagCount = encode_zigzag(TagCount),
    <<ZigZagCount/binary, (list_to_binary(EncodedBlocks))/binary, 0>>.

%% @doc Encode a string for Avro using ZigZag and VInt encoding.
encode_avro_string(String) ->
    StringBytes = unicode:characters_to_binary(String, utf8),
    Length = byte_size(StringBytes),
    <<(encode_zigzag(Length))/binary, StringBytes/binary>>.

%% @doc Encode an integer using ZigZag encoding.
encode_zigzag(Int) when Int >= 0 ->
    encode_vint(Int bsl 1);
encode_zigzag(Int) ->
    encode_vint(Int bsl 1, -1).

%% @doc Encode a ZigZag integer to VInt binary format.
encode_vint(ZigZag) ->
    encode_vint(ZigZag, []).

encode_vint(0, Acc) ->
    list_to_binary(lists:reverse(Acc));
encode_vint(ZigZag, Acc) ->
    VIntByte = ZigZag band 16#7F,
    ZigZagShifted = ZigZag bsr 7,
    case ZigZagShifted of
        0 -> encode_vint(0, [VIntByte | Acc]);
        _ -> encode_vint(ZigZagShifted, [VIntByte bor 16#80 | Acc])
    end.

%% @doc Convert binary data back to a #tx record.
deserialize(Binary) -> deserialize(Binary, binary).
deserialize(Binary, binary) ->
    %try
        {SignatureType, Signature, Owner, Rest} = decode_signature(Binary),
        {Target, Rest2} = decode_optional_field(Rest),
        {Anchor, Rest3} = decode_optional_field(Rest2),
        {Tags, Data} = decode_tags(Rest3),
        maybe_unbundle(
            #tx{
                format=ans104,
                signature_type = SignatureType, 
                signature = Signature,
                owner = Owner,
                target = Target,
                last_tx = Anchor,
                tags = Tags,
                data = Data,
                data_size = byte_size(Data),
                %% Since the id isn't included in the data-item spec, we'll fill it in ourselves.
                id = crypto:hash(sha256, Signature)
            }
        );
    %catch
    %    _:_:_Stack ->
    %        {error, invalid_item}
    %end;
deserialize(Bin, json) ->
    try
        maybe_unbundle(json_struct_to_item(element(1, jiffy:decode(Bin))))
    catch
        _:_:_Stack ->
            {error, invalid_item}
    end.

maybe_unbundle(Item) ->
    Format = lists:keyfind(<<"Bundle-Format">>, 1, Item#tx.tags),
    Version = lists:keyfind(<<"Bundle-Version">>, 1, Item#tx.tags),
    case {Format, Version} of
        {{<<"Bundle-Format">>, <<"Binary">>}, {<<"Bundle-Version">>, <<"2.0.0">>}} ->
            maybe_unbundle_map(Item);
        _ -> Item
    end.

maybe_unbundle_map(Bundle) ->
    case lists:keyfind(<<"Bundle-Map">>, 1, Bundle#tx.tags) of
        {<<"Bundle-Map">>, MapTXID} ->
            Items = unbundle(Bundle),
            MapItem = find_item(MapTXID, Items),
            Map = jiffy:decode(MapItem#tx.data, [use_null, return_maps]),
            maps:map(fun(_K, TXID) -> find_item(TXID, Items) end, Map);
        _ -> unbundle(Bundle)
    end.

find_item(TXID, Items) ->
    case lists:keyfind(TXID, #tx.id, Items) of
        false -> false;
        Item -> Item
    end.

unbundle(Item = #tx { data = << Count:256/integer, Content/binary >> }) ->
    {ItemsBin, Items} = decode_bundle_header(Count, Content),
    Item#tx { data = decode_bundle_items(Items, ItemsBin) }.

decode_bundle_items([], <<>>) -> [];
decode_bundle_items([{_ID, Size} | RestItems], ItemsBin) ->
    [
        deserialize(binary:part(ItemsBin, 0, Size))
    |
        decode_bundle_items(RestItems, binary:part(ItemsBin, Size, byte_size(ItemsBin) - Size))
    ].

decode_bundle_header(Count, Bin) -> decode_bundle_header(Count, Bin, []).
decode_bundle_header(0, ItemsBin, Header) -> {ItemsBin, lists:reverse(Header)};
decode_bundle_header(Count, << Size:256/integer, ID:32/binary, Rest/binary >>, Header) ->
    decode_bundle_header(Count - 1, Rest, [ {ID, Size} | Header ]).

item_to_json_struct(
	#tx{
		id = ID,
		last_tx = Last,
		owner = Owner,
		tags = Tags,
		target = Target,
		data = Data,
		signature = Sig
	}) ->
	Fields = [
		{<<"Id">>, ar_util:encode(ID)},
		{<<"Anchor">>, ar_util:encode(Last)}, % NOTE: In Arweave TXs, these are called "last_tx"
		{<<"Owner">>, ar_util:encode(Owner)},
		{<<"From">>, ar_util:encode(ar_wallet:to_address(Owner))},
		{<<"Tags">>,
			lists:map(
				fun({Name, Value}) ->
					{
						[
							{name, maybe_list_to_binary(Name)},
							{value, maybe_list_to_binary(Value)}
						]
					}
				end,
				Tags
			)
		},
		{<<"Target">>, ar_util:encode(Target)},
		{<<"Data">>, Data},
		{<<"Signature">>, ar_util:encode(Sig)}
	],
	{Fields}.

maybe_list_to_binary(List) when is_list(List) ->
    list_to_binary(List);
maybe_list_to_binary(Bin) ->
    Bin.

json_struct_to_item({TXStruct}) -> json_struct_to_item(TXStruct);
json_struct_to_item(RawTXStruct) ->
    TXStruct = [ { string:lowercase(FieldName), Value} || {FieldName, Value} <- RawTXStruct ],
	Tags =
		case ar_util:find_value(<<"tags">>, TXStruct) of
			undefined ->
                [];
			Xs ->
				Xs
		end,
	TXID = ar_util:decode(ar_util:find_value(<<"id">>, TXStruct, <<>>)),
	#tx{
		format = ans104,
		id = TXID,
		last_tx = ar_util:decode(ar_util:find_value(<<"anchor">>, TXStruct, <<>>)),
		owner = ar_util:decode(ar_util:find_value(<<"owner">>, TXStruct, <<>>)),
		tags = 
            lists:map(
                fun({KeyVals}) ->
                    {_, Name} = lists:keyfind(<<"name">>, 1, KeyVals),
                    {_, Value} = lists:keyfind(<<"value">>, 1, KeyVals),
                    {Name, Value}
                end,
                Tags
            ),
		target = ar_util:decode(ar_util:find_value(<<"target">>, TXStruct, <<>>)),
		data = ar_util:find_value(<<"data">>, TXStruct, <<>>),
		signature = ar_util:decode(ar_util:find_value(<<"signature">>, TXStruct, <<>>))
	}.

%% @doc Decode the signature from a binary format. Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
%% little-endian format which is why we match on <<1, 0>>.
decode_signature(<<1, 0, Signature:512/binary, Owner:512/binary, Rest/binary>>) ->
    {{rsa, 65537}, Signature, Owner, Rest};
decode_signature(_Other) ->
    unsupported_tx_format.

%% @doc Decode tags from a binary format using Apache Avro.
decode_tags(<<0:64/little-integer, 0:64/little-integer, Rest/binary>>) ->
    {[], Rest};
decode_tags(<<_TagCount:64/little-integer, _TagSize:64/little-integer, Binary/binary>>) ->
    {Count, BlocksBinary} = decode_zigzag(Binary),
    {Tags, Rest} = decode_avro_tags(BlocksBinary, Count),
    %% Pull out the terminating zero
    {0, Rest2} = decode_zigzag(Rest),
    {Tags, Rest2}.

decode_optional_field(<<0, Rest/binary>>) ->
    {<<>>, Rest};
decode_optional_field(<<1:8/integer, Field:32/binary, Rest/binary>>) ->
    {Field, Rest}.

%% @doc Decode Avro blocks (for tags) from binary.
decode_avro_tags(<<>>, _) ->
    {[], <<>>};
decode_avro_tags(Binary, Count) when Count =:= 0 ->
    {[], Binary};
decode_avro_tags(Binary, Count) ->
    {NameSize, Rest} = decode_zigzag(Binary),
    decode_avro_name(NameSize, Rest, Count).

decode_avro_name(0, Rest, _) ->
    {[], Rest};
decode_avro_name(NameSize, Rest, Count) ->
    <<Name:NameSize/binary, Rest2/binary>> = Rest,
    {ValueSize, Rest3} = decode_zigzag(Rest2),
    decode_avro_value(ValueSize, Name, Rest3, Count).

decode_avro_value(0, _, Rest, _) ->
    {[], Rest};
decode_avro_value(ValueSize, Name, Rest, Count) ->
    <<Value:ValueSize/binary, Rest2/binary>> = Rest,
    {DecodedTags, NonAvroRest} = decode_avro_tags(Rest2, Count - 1),
    {[{Name, Value} | DecodedTags], NonAvroRest}.

%% @doc Decode a VInt encoded ZigZag integer from binary.
decode_zigzag(Binary) ->
    {ZigZag, Rest} = decode_vint(Binary, 0, 0),
    case ZigZag band 1 of
        1 -> {-(ZigZag bsr 1) - 1, Rest};
        0 -> {ZigZag bsr 1, Rest}
    end.

decode_vint(<<>>, Result, _Shift) ->
    {Result, <<>>};
decode_vint(<<Byte, Rest/binary>>, Result, Shift) ->
    VIntPart = Byte band 16#7F,
    NewResult = Result bor (VIntPart bsl Shift),
    case Byte band 16#80 of
        0 -> {NewResult, Rest};
        _ -> decode_vint(Rest, NewResult, Shift + 7)
    end.



%%%===================================================================
%%% Unit tests.
%%% To run:
%%% erlc -o ebin src/*.erl; erl -pa ebin -eval "eunit:test(ar_bundles, [verbose])" -s init stop
%%%===================================================================

ar_bundles_test_() ->
	[
		{timeout, 30, fun test_no_tags/0},
        {timeout, 30, fun test_no_tags_from_disk/0},
        {timeout, 30, fun test_with_tags/0},
        {timeout, 30, fun test_with_tags_from_disk/0},
        {timeout, 30, fun test_empty_bundle/0},
        {timeout, 30, fun test_bundle_with_one_item/0},
        {timeout, 30, fun test_bundle_with_two_items/0},
        {timeout, 30, fun test_recursive_bundle/0},
        {timeout, 30, fun test_bundle_map/0}
	].

test_no_tags() ->
    {Priv, Pub} = ar_wallet:new(),
    {KeyType, Owner} = Pub,
    Target = crypto:strong_rand_bytes(32),
    Anchor = crypto:strong_rand_bytes(32),
    DataItem = new_item(Target, Anchor, [], <<"data">>),
    SignedDataItem = sign_item(DataItem, {Priv, Pub}),

    ?assertEqual(true, verify_item(SignedDataItem)),
    assert_data_item(KeyType, Owner, Target, Anchor, [], <<"data">>, SignedDataItem),

    SignedDataItem2 = deserialize(serialize(SignedDataItem)),

    ?assertEqual(SignedDataItem, SignedDataItem2),
    ?assertEqual(true, verify_item(SignedDataItem2)),
    assert_data_item(KeyType, Owner, Target, Anchor, [], <<"data">>, SignedDataItem2).

test_no_tags_from_disk() ->
    {ok, BinaryDataItem} = file:read_file("src/test/dataitem_notags"),
    DataItem = deserialize(BinaryDataItem),
    ?assertEqual(true, verify_item(DataItem)),
    ?assertEqual(<<"notags">>, DataItem#tx.data).

test_with_tags() ->
    {Priv, Pub} = ar_wallet:new(),
    {KeyType, Owner} = Pub,
    Target = crypto:strong_rand_bytes(32),
    Anchor = crypto:strong_rand_bytes(32),
    Tags = [{<<"tag1">>, <<"value1">>}, {<<"tag2">>, <<"value2">>}],
    DataItem = new_item(Target, Anchor, Tags, <<"taggeddata">>),
    SignedDataItem = sign_item(DataItem, {Priv, Pub}),

    ?assertEqual(true, verify_item(SignedDataItem)),
    assert_data_item(KeyType, Owner, Target, Anchor, Tags, <<"taggeddata">>, SignedDataItem),

    SignedDataItem2 = deserialize(serialize(SignedDataItem)),

    ?assertEqual(SignedDataItem, SignedDataItem2),
    ?assertEqual(true, verify_item(SignedDataItem2)),
    assert_data_item(KeyType, Owner, Target, Anchor, Tags, <<"taggeddata">>, SignedDataItem2).

test_with_tags_from_disk() ->
    {ok, BinaryDataItem} = file:read_file("src/test/dataitem_withtags"),
    DataItem = deserialize(BinaryDataItem),
    ?assertEqual(true, verify_item(DataItem)),
    ?assertEqual(<<"withtags">>, DataItem#tx.data).
    
assert_data_item(KeyType, Owner, Target, Anchor, Tags, Data, DataItem) ->
    ?assertEqual(KeyType, DataItem#tx.signature_type),
    ?assertEqual(Owner, DataItem#tx.owner),
    ?assertEqual(Target, DataItem#tx.target),
    ?assertEqual(Anchor, DataItem#tx.last_tx),
    ?assertEqual(Tags, DataItem#tx.tags),
    ?assertEqual(Data, DataItem#tx.data),
    ?assertEqual(byte_size(Data), DataItem#tx.data_size).

test_empty_bundle() ->
    Bundle = serialize([]),
    BundleItem = deserialize(Bundle),
    ?assertEqual([], BundleItem#tx.data).

test_bundle_with_one_item() ->
    Item = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [],
        ItemData = crypto:strong_rand_bytes(32)
    ),
    Bundle = serialize([Item]),
    BundleItem = deserialize(Bundle),
    ?assertEqual(ItemData, (hd(BundleItem#tx.data))#tx.data).

test_bundle_with_two_items() ->
    Item1 = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [],
        ItemData1 = crypto:strong_rand_bytes(32)
    ),
    Item2 = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [{<<"tag1">>, <<"value1">>}, {<<"tag2">>, <<"value2">>}],
        ItemData2 = crypto:strong_rand_bytes(32)
    ),
    Bundle = serialize([Item1, Item2]),
    BundleItem = deserialize(Bundle),
    ?assertEqual(ItemData1, (hd(BundleItem#tx.data))#tx.data),
    ?assertEqual(ItemData2, (hd(tl(BundleItem#tx.data)))#tx.data).

test_recursive_bundle() ->
    Item1 = #tx{
        id = crypto:strong_rand_bytes(32),
        last_tx = crypto:strong_rand_bytes(32),
        data = << 1:256/integer >>
    },
    Item2 = #tx{
        id = crypto:strong_rand_bytes(32),
        last_tx = crypto:strong_rand_bytes(32),
        data = [Item1]
    },
    Item3 = #tx{
        id = crypto:strong_rand_bytes(32),
        last_tx = crypto:strong_rand_bytes(32),
        data = [Item2]
    },
    Bundle = serialize([Item3]),
    BundleItem = deserialize(Bundle),
    [UnbundledItem3] = BundleItem#tx.data,
    [UnbundledItem2] = UnbundledItem3#tx.data,
    [UnbundledItem1] = UnbundledItem2#tx.data,
    ?assertEqual(Item1#tx.data, UnbundledItem1#tx.data).

test_bundle_map() ->
    Item1 = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [],
        ItemData1 = <<"item1_data">>
    ),
    Item2 = #tx{
        format = ans104,
        id = crypto:strong_rand_bytes(32),
        last_tx = crypto:strong_rand_bytes(32),
        data = #{<<"key1">> => Item1}
    },
    Bundle = serialize(Item2),
    ok.
    %BundleItem = deserialize(Bundle),
    %?assertEqual(ItemData1, (maps:get(<<"key1">>, BundleItem#tx.data))#tx.data).
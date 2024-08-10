%%% @doc Module for creating, signing, and verifying Arweave data items and bundles.
-module(ar_bundles).

-export([new_item/4, sign_item/2, verify_item/1]).
-export([encode_tags/1, decode_tags/1]).

-include("include/ar.hrl").

-include_lib("eunit/include/eunit.hrl").

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
    SignedDataItem = DataItem#tx{ owner = Owner, signature_type = KeyType },
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
item_to_binary(TX) ->
    EncodedTags = encode_tags(TX#tx.tags),
    <<(encode_signature_type(TX#tx.signature_type))/binary,
      (TX#tx.signature)/binary,
      (TX#tx.owner)/binary,
      (encode_optional_field(TX#tx.target))/binary,
      (encode_optional_field(TX#tx.last_tx))/binary,
      (encode_tags_size(TX#tx.tags, EncodedTags))/binary,
      EncodedTags/binary,
      (TX#tx.data)/binary>>.

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
    <<0:64/integer, 0:64/integer>>;
encode_tags_size(Tags, EncodedTags) ->
    <<(length(Tags)):64/integer, (byte_size(EncodedTags)):64/integer>>.

%% @doc Encode tags into a binary format using Apache Avro.
encode_tags([]) ->
    <<>>;
encode_tags(Tags) ->
    EncodedBlocks = lists:flatmap(fun({Name, Value}) ->
        EncName = encode_avro_string(Name),
        EncValue = encode_avro_string(Value),
        [EncName, EncValue]
    end, Tags),
    TotalSize = lists:sum(lists:map(fun(B) -> byte_size(B) end, EncodedBlocks)),
    BlockCount = length(EncodedBlocks),
    ZigZagCount = encode_zigzag(BlockCount),
    ZigZagSize = encode_zigzag(TotalSize),
    <<ZigZagCount/binary, ZigZagSize/binary, (list_to_binary(EncodedBlocks))/binary, 0>>.

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
binary_to_item(Binary) ->
    {SignatureType, Signature, Owner, Rest} = decode_signature(Binary),
    {Target, Rest2} = decode_optional_field(Rest),
    {Anchor, Rest3} = decode_optional_field(Rest2),
    {Tags, Data} = decode_tags(Rest3),
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
    }.

%% @doc Decode the signature from a binary format. Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
%% little-endian format which is why we match on <<1, 0>>.
decode_signature(<<1, 0, Signature:512/binary, Owner:512/binary, Rest/binary>>) ->
    {{rsa, 65537}, Signature, Owner, Rest};
decode_signature(_) ->
    unsupported_tx_format.

%% @doc Decode tags from a binary format using Apache Avro.
decode_tags(<<0:64/integer, 0:64/integer, Rest/binary>>) ->
    {[], Rest};
decode_tags(<<TagCount:64/integer, TagSize:64/integer, Binary/binary>>) ->
    {Count, Rest} = decode_zigzag(Binary),
    {Size, BlocksBinary} = decode_zigzag(Rest),
    decode_avro_tags(BlocksBinary, Count).

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
        {timeout, 30, fun test_with_tags/0}
        % {timeout, 30, fun test_with_tags_from_disk/0}
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

    SignedDataItem2 = binary_to_item(item_to_binary(SignedDataItem)),

    ?assertEqual(SignedDataItem, SignedDataItem2),
    ?assertEqual(true, verify_item(SignedDataItem2)),
    assert_data_item(KeyType, Owner, Target, Anchor, [], <<"data">>, SignedDataItem2).

test_no_tags_from_disk() ->
    {ok, BinaryDataItem} = file:read_file("src/test/dataitem_notags"),
    
    DataItem = binary_to_item(BinaryDataItem),

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

    SignedDataItem2 = binary_to_item(item_to_binary(SignedDataItem)),

    ?assertEqual(SignedDataItem, SignedDataItem2),
    ?assertEqual(true, verify_item(SignedDataItem2)),
    assert_data_item(KeyType, Owner, Target, Anchor, Tags, <<"taggeddata">>, SignedDataItem2).

test_with_tags_from_disk() ->
    {ok, BinaryDataItem} = file:read_file("src/test/dataitem_withtags"),
    
    DataItem = binary_to_item(BinaryDataItem),

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

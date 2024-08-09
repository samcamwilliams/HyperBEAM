%%% @doc Module for creating, signing, and verifying Arweave data items and bundles.
-module(ar_bundles).

-export([new_item/5, sign_item/2, verify_item/1, create_bundle/1, verify_bundle/1]).
-export([encode_tags/1, decode_tags/1]).

-include("include/ar.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Create a new data item.
new_item(Owner, Target, Anchor, Tags, Data) ->
    #tx{
        format = ans104,
        owner = Owner,
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

%% @doc Create a bundle of data items.
create_bundle(DataItems) ->
    NumItems = length(DataItems),
    SizesAndIDs = lists:map(fun(Item) ->
        {byte_size(Item#tx.data), Item#tx.id} % Byte size should be of the encoded binary
    end, DataItems),
    BinaryItems = lists:map(fun(Item) -> tx_to_binary(Item) end, DataItems),
    (ar_tx:new(<<>>, 0, 0, <<>>, <<>>))#tx {
        tags = [{"Bundle-Format", "1"}, {"Bundle-Version", "2.0.0"}],
        data = <<NumItems:32/binary, (list_to_binary(flatten_size_id_pairs(SizesAndIDs)))/binary, (list_to_binary(BinaryItems))/binary>>
    }.

%% @doc Verify the validity of a bundle.
verify_bundle({NumItems, SizesAndIDs, BinaryItems}) ->
    ValidSizesAndIDs = lists:all(fun({Size, _ID}) ->
        Size == byte_size(binary_part(BinaryItems, {0, Size})),
        verify_data_item_id(tx_from_binary(binary_part(BinaryItems, {0, Size})))
    end, SizesAndIDs),
    NumItems == length(SizesAndIDs) andalso ValidSizesAndIDs.

%% @doc Encode tags into a binary format using Apache Avro.
encode_tags(Tags) ->
    EncodedBlocks = lists:map(fun({Name, Value}) ->
        EncName = encode_avro_string(Name),
        EncValue = encode_avro_string(Value),
        <<EncName/binary, EncValue/binary>>
    end, Tags),
    TotalSize = lists:sum(lists:map(fun(B) -> byte_size(B) end, EncodedBlocks)),
    BlockCount = length(EncodedBlocks),
    ZigZagCount = encode_zigzag(BlockCount),
    ZigZagSize = encode_zigzag(TotalSize),
    <<ZigZagCount/binary, ZigZagSize/binary, (list_to_binary(EncodedBlocks))/binary, 0>>.

%% @doc Decode tags from a binary format using Apache Avro.
decode_tags(Binary) ->
    {Count, Rest} = decode_zigzag(Binary),
    {_Size, BlocksBinary} = decode_zigzag(Rest),
    decode_avro_blocks(BlocksBinary, Count).

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @doc Flatten size and ID pairs into binary format.
flatten_size_id_pairs([]) -> [];
flatten_size_id_pairs([{Size, ID} | Rest]) ->
    <<Size:32/binary, ID:256/binary>> ++ flatten_size_id_pairs(Rest).

%% @doc Generate the data segment to be signed for a data item.
data_item_signature_data(DataItem) ->
    List = [
        utf8_encoded("dataitem"),
        utf8_encoded("1"),
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
tx_to_binary(TX) ->
    <<(TX#tx.signature_type)/binary,
      (TX#tx.signature)/binary,
      (TX#tx.owner)/binary,
      (encode_optional_field(TX#tx.target))/binary,
      (encode_optional_field(TX#tx.last_tx))/binary,
      (encode_tags(TX#tx.tags))/binary,
      (TX#tx.data)/binary>>.

%% @doc Convert binary data back to a #tx record.
tx_from_binary(Binary) ->
    <<SignatureType:2/binary, Rest/binary>> = Binary,
    %% Decode the binary data back into a #tx record
    %% (Note: Simplified, real implementation would need to handle all fields)
    #tx{signature_type = SignatureType, data = Rest}.

%% @doc Encode an optional field (target, anchor) with a presence byte.
encode_optional_field(<<>>) ->
    <<0>>;
encode_optional_field(Field) ->
    <<1:8, Field/binary>>.

%% @doc Encode a UTF-8 string to binary.
utf8_encoded(String) ->
    unicode:characters_to_binary(String, utf8).

%% @doc Encode a string for Avro using ZigZag and VInt encoding.
encode_avro_string(String) ->
    StringBytes = unicode:characters_to_binary(String, utf8),
    Length = byte_size(StringBytes),
    <<(encode_zigzag(Length))/binary, StringBytes/binary>>.

%% @doc Decode Avro blocks (for tags) from binary.
decode_avro_blocks(<<>>, _) ->
    [];
decode_avro_blocks(Binary, Count) ->
    case Count of
        0 -> [];
        _ ->
            {NameSize, Rest1} = decode_zigzag(Binary),
            <<Name:NameSize/binary, Rest2/binary>> = Rest1,
            {ValueSize, Rest3} = decode_zigzag(Rest2),
            <<Value:ValueSize/binary, Rest4/binary>> = Rest3,
            [{Name, Value} | decode_avro_blocks(Rest4, Count - 1)]
    end.

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

%% @doc Decode a VInt encoded ZigZag integer from binary.
decode_zigzag(Binary) ->
    decode_vint(Binary, 0, 0).

decode_vint(<<>>, Result, _Shift) ->
    {Result, <<>>};
decode_vint(<<Byte, Rest/binary>>, Result, Shift) ->
    VIntPart = Byte band 16#7F,
    NewResult = Result bor (VIntPart bsl Shift),
    case Byte band 16#80 of
        0 -> {NewResult, Rest};
        _ -> decode_vint(Rest, NewResult, Shift + 7)
    end.

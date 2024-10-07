%%% @doc Module for creating, signing, and verifying Arweave data items and bundles.
-module(ar_bundles).
-export([id/1, id/2, new_item/4, sign_item/2, verify_item/1, type/1]).
-export([encode_tags/1, decode_tags/1]).
-export([serialize/1, serialize/2, deserialize/1, deserialize/2]).
-export([item_to_json_struct/1, json_struct_to_item/1]).
-export([data_item_signature_data/1]).
-export([normalize/1]).
-export([print/1]).

-include("include/ao.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(BUNDLE_TAGS, [
    {<<"Bundle-Format">>, <<"Binary">>},
    {<<"Bundle-Version">>, <<"2.0.0">>}
]).

% How many bytes of a binary to print with `print/1`.
-define(BIN_PRINT, 10).

%%%===================================================================
%%% Public interface.
%%%===================================================================

print(Item) -> print(Item, 0).
print(Item, Indent) when is_list(Item); is_map(Item) ->
    print(normalize(Item), Indent);
print(Item, Indent) ->
    Valid = verify_item(Item),
    print_line(
        "TX { ~s:~s } [",
        [
            if
                Item#tx.signature =/= ?DEFAULT_SIG -> "signed";
                true -> "unsigned"
            end,
            if
                Valid == true -> "valid";
                true -> "INVALID"
            end
        ],
        Indent
    ),
    print_line("Tags:", Indent + 1),
    lists:foreach(
        fun({Key, Val}) -> print_line("~s -> ~s", [Key, Val], Indent + 2) end,
        Item#tx.tags
    ),
    print_line("Data:", Indent + 1),
    print_data(Item, Indent + 2),
    print_line("]", Indent).

print_data(Item, Indent) when is_binary(Item#tx.data) ->
    case lists:keyfind(<<"Bundle-Format">>, 1, Item#tx.tags) of
        {_, _} ->
            print(deserialize(serialize(Item)), Indent);
        false ->
            print_line(
                "Binary: ~p... <~p bytes>",
                [format_binary(Item#tx.data), byte_size(Item#tx.data)],
                Indent
            )
    end;
print_data(Item, Indent) when is_map(Item#tx.data) ->
    print_line("Map:", Indent),
    maps:map(
        fun(Name, MapItem) ->
            print_line("~s ->", [Name], Indent + 1),
            print(MapItem, Indent + 2)
        end,
        Item#tx.data
    );
print_data(Item, Indent) when is_list(Item#tx.data) ->
    print_line("List:", Indent),
    lists:foreach(
        fun(ListItem) ->
            print(ListItem, Indent + 1)
        end,
        Item#tx.data
    ).

format_binary(Bin) ->
    lists:flatten(
        io_lib:format(
            "~p",
            [
                binary:part(
                    Bin,
                    0,
                    case byte_size(Bin) of
                        X when X < ?BIN_PRINT -> X;
                        _ -> ?BIN_PRINT
                    end
                )
            ]
        )
    ).

print_line(Str, Indent) -> print_line(Str, "", Indent).
print_line(RawStr, Fmt, Ind) ->
    Str = lists:flatten(RawStr),
    io:format(
        [$\s || _ <- lists:seq(1, Ind)] ++
            Str ++ "\n",
        Fmt
    ).

type(Item) when not is_record(Item, tx) ->
    type(normalize(Item));
type(#tx{data = Data}) when is_binary(Data) ->
    binary;
type(#tx{data = Data}) when is_map(Data) ->
    map;
type(#tx{data = Data}) when is_list(Data) ->
    list.

id(Item) when Item#tx.id == ?DEFAULT_ID ->
    id(normalize_data(Item), unsigned);
id(Item) when not is_record(Item, tx) ->
    id(normalize(Item), unsigned);
id(Item) -> id(Item, signed).
id(Item, unsigned) ->
    crypto:hash(sha256, data_item_signature_data(Item));
id(Item, signed) ->
    Item#tx.id.

%% @doc Create a new data item.
new_item(Target, Anchor, Tags, Data) ->
    update_id(
        #tx{
            format = ans104,
            target = Target,
            last_tx = Anchor,
            tags = Tags,
            data = Data,
            data_size = byte_size(Data)
        }
    ).

%% @doc Sign a data item.
sign_item(RawItem, {PrivKey, {KeyType, Owner}}) ->
    Item = (normalize_data(RawItem))#tx{format = ans104, owner = Owner, signature_type = KeyType},
    Sig = ar_wallet:sign(PrivKey, data_item_signature_data(Item)),
    ID = crypto:hash(sha256, <<Sig/binary>>),
    Item#tx{id = ID, signature = Sig}.

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
data_item_signature_data(RawItem) ->
    NormItem = normalize_data(RawItem),
    List = [
        utf8_encoded("dataitem"),
        utf8_encoded("1"),
        %% Only SignatureType 1 is supported for now (RSA 4096)
        utf8_encoded("1"),
        <<(NormItem#tx.owner)/binary>>,
        <<(NormItem#tx.target)/binary>>,
        <<(NormItem#tx.last_tx)/binary>>,
        encode_tags(NormItem#tx.tags),
        <<(NormItem#tx.data)/binary>>
    ],
    ar_deep_hash:hash(List).

%% @doc Verify the data item's ID matches the signature.
verify_data_item_id(DataItem) ->
    ExpectedID = crypto:hash(sha256, DataItem#tx.signature),
    DataItem#tx.id == ExpectedID.

%% @doc Verify the data item's signature.
verify_data_item_signature(DataItem) ->
    SignatureData = data_item_signature_data(DataItem),
    ar_wallet:verify(
        {DataItem#tx.signature_type, DataItem#tx.owner}, SignatureData, DataItem#tx.signature
    ).

%% @doc Verify the validity of the data item's tags.
verify_data_item_tags(DataItem) ->
    ValidCount = length(DataItem#tx.tags) =< 128,
    ValidTags = lists:all(
        fun({Name, Value}) ->
            byte_size(Name) =< 1024 andalso byte_size(Value) =< 3072
        end,
        DataItem#tx.tags
    ),
    ValidCount andalso ValidTags.

normalize(Item) -> update_id(normalize_data(Item)).

%% @doc Ensure that a data item (potentially containing a map or list) has a standard, serialized form.
normalize_data(Bundle) when is_list(Bundle); is_map(Bundle) ->
    normalize_data(#tx{data = Bundle});
normalize_data(Item = #tx { data = Data }) when is_list(Data) ->
    normalize_data(
        Item#tx{
            data =
                maps:from_list(
                    lists:zipwith(
                        fun(Index, MapItem) ->
                            {integer_to_binary(Index), normalize_data(MapItem)}
                        end,
                        lists:seq(1, length(Data)),
                        Data
                    )
                )
        }
    );
normalize_data(Item = #tx{data = Bin}) when is_binary(Bin) ->
    normalize_data_size(Item);
normalize_data(Item = #tx{data = Data}) ->
    normalize_data_size(
        case serialize_bundle_data(Data) of
            {ManifestID, Bin} ->
                Item#tx{
                    data = Bin,
                    tags = add_manifest_tags(add_bundle_tags(Item#tx.tags), ManifestID)
                };
            DirectBin ->
                Item#tx{
                    data = DirectBin,
                    tags = add_bundle_tags(Item#tx.tags)
                }
        end
    ).

%% @doc Reset the data size of a data item. Assumes that the data is already normalized.
normalize_data_size(Item = #tx{data = Bin}) when is_binary(Bin) ->
    Item#tx{data_size = byte_size(Bin)};
normalize_data_size(Item) -> Item.

%% @doc Convert a #tx record to its binary representation.
serialize(TX) -> serialize(TX, binary).
serialize(TX, binary) when is_binary(TX) -> TX;
serialize(RawTX, binary) ->
    TX = normalize(RawTX),
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

update_id(TX = #tx{id = ?DEFAULT_ID, signature = ?DEFAULT_SIG}) ->
    ID = crypto:hash(sha256, data_item_signature_data(TX)),
    TX#tx{format = ans104, id = ID};
update_id(TX = #tx{id = ?DEFAULT_ID, signature = Sig}) ->
    TX#tx{format = ans104, id = crypto:hash(sha256, Sig)};
update_id(TX) ->
    TX.

add_bundle_tags(Tags) -> ?BUNDLE_TAGS ++ (Tags -- ?BUNDLE_TAGS).

add_manifest_tags(Tags, ManifestID) ->
    lists:filter(
        fun
            ({<<"Bundle-Map">>, _}) -> false;
            (_) -> true
        end,
        Tags
    ) ++ [{<<"Bundle-Map">>, ar_util:encode(ManifestID)}].

finalize_bundle_data(Processed) ->
    Length = <<(length(Processed)):256/integer>>,
    Index = <<<<(byte_size(Data)):256/integer, ID/binary>> || {ID, Data} <- Processed>>,
    Items = <<<<Data/binary>> || {_, Data} <- Processed>>,
    <<Length/binary, Index/binary, Items/binary>>.

to_serialized_pair(Item) ->
    Serialized = serialize(Item, binary),
    % TODO: This is a hack to get the ID of the item. We need to do this because we may not
    % have the ID in 'item' if it is just a map/list. We need to make this more efficient.
    ID =
        if
            is_record(Item, tx) -> Item#tx.id;
            true -> (deserialize(Serialized, binary))#tx.id
        end,
    {ID, Serialized}.

serialize_bundle_data(Map) when is_map(Map) ->
    % TODO: Make this compatible with the normal manifest spec.
    % For now we just serialize the map to a JSON string of Key=>TXID
    BinItems = maps:map(fun(_, Item) -> to_serialized_pair(Item) end, Map),
    Index = maps:map(fun(_, {TXID, _}) -> ar_util:encode(TXID) end, BinItems),
    Manifest = new_manifest(Index),
    {Manifest#tx.id, finalize_bundle_data([to_serialized_pair(Manifest) | maps:values(BinItems)])};
serialize_bundle_data(List) ->
    finalize_bundle_data(lists:map(fun to_serialized_pair/1, List)).

new_manifest(Index) ->
    TX = normalize(#tx{
        format = ans104,
        tags = [
            {<<"Data-Protocol">>, <<"Bundle-Map">>},
            {<<"Variant">>, <<"0.0.1">>}
        ],
        data = jiffy:encode(Index)
    }),
    TX.

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
    EncodedBlocks = lists:flatmap(
        fun({Name, Value}) ->
            EncName = encode_avro_string(Name),
            EncValue = encode_avro_string(Value),
            [EncName, EncValue]
        end,
        Tags
    ),
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
        update_id(#tx{
            format = ans104,
            signature_type = SignatureType,
            signature = Signature,
            owner = Owner,
            target = Target,
            last_tx = Anchor,
            tags = Tags,
            data = Data,
            data_size = byte_size(Data)
        })
    );
%catch
%    _:_:_Stack ->
%        {error, invalid_item}
%end;
deserialize(Bin, json) ->
    try
        normalize(
            maybe_unbundle(
                json_struct_to_item(element(1, jiffy:decode(Bin)))
            )
        )
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
        _ ->
            Item
    end.

maybe_unbundle_map(Bundle) ->
    case lists:keyfind(<<"Bundle-Map">>, 1, Bundle#tx.tags) of
        {<<"Bundle-Map">>, MapTXID} ->
            Items = unbundle(Bundle),
            MapItem = find_item(ar_util:decode(MapTXID), Items),
            Map = jiffy:decode(MapItem#tx.data, [return_maps]),
            Bundle#tx{
                data =
                    maps:map(
                        fun(_K, TXID) ->
                            find_item(ar_util:decode(TXID), Items)
                        end,
                        Map
                    )
            };
        _ ->
            unbundle(Bundle)
    end.

find_item(TXID, TX) when is_record(TX, tx) ->
    find_item(TXID, TX#tx.data);
find_item(TXID, Items) ->
    case lists:keyfind(TXID, #tx.id, Items) of
        false -> false;
        Item -> Item
    end.

unbundle(Item = #tx{data = <<Count:256/integer, Content/binary>>}) ->
    {ItemsBin, Items} = decode_bundle_header(Count, Content),
    Item#tx{data = decode_bundle_items(Items, ItemsBin)}.

decode_bundle_items([], <<>>) ->
    [];
decode_bundle_items([{_ID, Size} | RestItems], ItemsBin) ->
    [
        deserialize(binary:part(ItemsBin, 0, Size))
        | decode_bundle_items(RestItems, binary:part(ItemsBin, Size, byte_size(ItemsBin) - Size))
    ].

decode_bundle_header(Count, Bin) -> decode_bundle_header(Count, Bin, []).
decode_bundle_header(0, ItemsBin, Header) ->
    {ItemsBin, lists:reverse(Header)};
decode_bundle_header(Count, <<Size:256/integer, ID:32/binary, Rest/binary>>, Header) ->
    decode_bundle_header(Count - 1, Rest, [{ID, Size} | Header]).

item_to_json_struct(
    #tx{
        id = ID,
        last_tx = Last,
        owner = Owner,
        tags = Tags,
        target = Target,
        data = Data,
        signature = Sig
    }
) ->
    % Set "From" if From-Process is Tag or set with "Owner" address
    From =
        case lists:filter(fun({Name, _}) -> Name =:= <<"From-Process">> end, Tags) of
            [{_, FromProcess}] -> FromProcess;
            [] -> ar_util:encode(ar_wallet:to_address(Owner))
        end,
    Fields = [
        {<<"Id">>, ar_util:encode(ID)},
        % NOTE: In Arweave TXs, these are called "last_tx"
        {<<"Anchor">>, ar_util:encode(Last)},
        % NOTE: When sent to ao "Owner" is the wallet address
        {<<"Owner">>, ar_util:encode(ar_wallet:to_address(Owner))},
        {<<"From">>, From},
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
            )},
        {<<"Target">>, ar_util:encode(Target)},
        {<<"Data">>, Data},
        {<<"Signature">>, ar_util:encode(Sig)}
    ],
    {Fields}.

maybe_list_to_binary(List) when is_list(List) ->
    list_to_binary(List);
maybe_list_to_binary(Bin) ->
    Bin.

json_struct_to_item(Map) when is_map(Map) ->
    deserialize(jiffy:encode(Map), json);
json_struct_to_item({TXStruct}) ->
    json_struct_to_item(TXStruct);
json_struct_to_item(RawTXStruct) ->
    TXStruct = [{string:lowercase(FieldName), Value} || {FieldName, Value} <- RawTXStruct],
    Tags =
        case ar_util:find_value(<<"tags">>, TXStruct) of
            undefined ->
                [];
            Xs ->
                Xs
        end,
    TXID = ar_util:decode(ar_util:find_value(<<"id">>, TXStruct, ar_util:encode(?DEFAULT_ID))),
    #tx{
        format = ans104,
        id = TXID,
        last_tx = ar_util:decode(ar_util:find_value(<<"anchor">>, TXStruct, <<>>)),
        owner = ar_util:decode(
            ar_util:find_value(<<"owner">>, TXStruct, ar_util:encode(?DEFAULT_OWNER))
        ),
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
        signature = ar_util:decode(
            ar_util:find_value(<<"signature">>, TXStruct, ar_util:encode(?DEFAULT_SIG))
        )
    }.

%% @doc Decode the signature from a binary format. Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
%% little-endian format which is why we match on <<1, 0>>.
decode_signature(<<1, 0, Signature:512/binary, Owner:512/binary, Rest/binary>>) ->
    {{rsa, 65537}, Signature, Owner, Rest};
decode_signature(Other) ->
    ao:c({error_decoding_signature, Other}),
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
        {timeout, 30, fun test_unsigned_data_item_id/0},
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

test_unsigned_data_item_id() ->
    Item1 = deserialize(serialize(update_id(#tx{format = ans104, data = <<"data1">>}))),
    Item2 = deserialize(serialize(update_id(#tx{format = ans104, data = <<"data2">>}))),
    ?assertNotEqual(Item1#tx.id, Item2#tx.id).

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
        data = <<1:256/integer>>
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
    Item1 = normalize(#tx{
        format = ans104,
        data = <<"item1_data">>
    }),
    Item2 = normalize(#tx{
        format = ans104,
        last_tx = crypto:strong_rand_bytes(32),
        data = #{<<"key1">> => Item1}
    }),
    Bundle = serialize(Item2),
    BundleItem = deserialize(Bundle),
    ?assertEqual(Item1#tx.data, (maps:get(<<"key1">>, BundleItem#tx.data))#tx.data).

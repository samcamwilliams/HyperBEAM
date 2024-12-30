
%%% @doc This module acts an adapter between messages, as modeled in the
%%% Converge Protocol, and their uderlying binary representations and formats.
%%% 
%%% Unless you are implementing a new message serialization codec, you should
%%% not need to interact with this module directly. Instead, use the
%%% `hb_converge' interfaces to interact with all messages. The `dev_message'
%%% module implements a device interface for abstracting over the different
%%% message formats.
%%% 
%%% `hb_message' and the HyperBEAM caches can interact with multiple different
%%% types of message formats:
%%% 
%%%     - Richly typed Converge messages.
%%%     - Arweave transations.
%%%     - ANS-104 data items.
%%%     - HTTP Signed Messages.
%%%     - Flat Maps.
%%% 
%%% This module is responsible for converting between these formats. It does so
%%% by normalizing messages to a common format: `Type Annotated Binary Messages`
%%% (TABM). TABMs are deep Erlang maps with keys than only contain either other
%%% TABMs or binary values. By marshalling all messages into this format, they
%%% can easily be coerced into other output formats. For example, generating a
%%% `HTTP Signed Message` format output from an Arweave transaction. TABM is
%%% also a simple format from a computational perspective (only binary literals
%%% and O(1) access maps), such that operations upon them are efficient.
%%% 
%%% The structure of the conversions is as follows:
%%% 
%%% ```
%%%     Arweave TX/ANS-104 ==> hb_codec_tx:from/1 ==> TABM
%%%     HTTP Signed Message ==> hb_codec_http:from/1 ==> TABM
%%%     Flat Maps ==> hb_codec_flat:from/1 ==> TABM
%%% 
%%%     TABM ==> hb_codec_converge:to/1 ==> Converge Message
%%%     Converge Message ==> hb_codec_converge:from/1 ==> TABM
%%% 
%%%     TABM ==> hb_codec_tx:to/1 ==> Arweave TX/ANS-104
%%%     TABM ==> hb_codec_http:to/1 ==> HTTP Signed Message
%%%     TABM ==> hb_codec_flat:to/1 ==> Flat Maps
%%% '''
%%% 
%%% Additionally, this module provides a number of utility functions for
%%% manipulating messages. For example, `hb_message:sign/2' to sign a message of
%%% arbitrary type, or `hb_message:format/1' to print a Converge/TABM message in
%%% a human-readable format.
%%% 
%%% The `hb_cache' module is responsible for storing and retrieving messages in
%%% the HyperBEAM stores configured on the node. Each store has its own storage
%%% backend, but each works with simple key-value pairs. Subsequently, the 
%%% `hb_cache' module uses TABMs as the internal format for storing and 
%%% retrieving messages.
-module(hb_message).
-export([convert/3, convert/4]).
-export([sign/2, verify/1, match/2, type/1, minimize/1, normalize_keys/1]). 
-export([signers/1, serialize/1, serialize/2, deserialize/1, deserialize/2]).
%%% Helpers:
-export([default_tx_list/0]).
%%% Debugging tools:
-export([print/1, format/1, format/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Convert a message from one format to another. Taking a message in the
%% source format, a target format, and a set of opts. If not given, the source
%% is assumed to be `converge`. Additional codecs can be added by ensuring they
%% are part of the `Opts` map -- either globally, or locally for a computation.
%% 
%% The encoding happens in two phases:
%% 1. Convert the message to a TABM.
%% 2. Convert the TABM to the target format.
%% 
%% The conversion to a TABM is done by the `converge' codec, which is always
%% available. The conversion from a TABM is done by the target codec.
convert(Msg, TargetFormat, Opts) ->
    convert(Msg, TargetFormat, converge, Opts).
convert(Msg, TargetFormat, converge, Opts) ->
    % No need to convert to TABM, just convert to the target format.
    convert(Msg, TargetFormat, Opts);
convert(Msg, TargetFormat, SourceFormat, Opts) ->
    SourceCodecMod = get_codec(SourceFormat, Opts),
    TABM = SourceCodecMod:from(Msg),
    TargetCodecMod = get_codec(TargetFormat, Opts),
    TargetCodecMod:to(TABM).

%% @doc Get a codec from the options.
get_codec(TargetFormat, Opts) ->
    case hb_opts:get(codecs, #{}, Opts) of
        #{ TargetFormat := CodecMod } -> CodecMod;
        _ -> throw({message_codec_not_found, TargetFormat})
    end.

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
    % Define helper functions for formatting elements of the map.
    ValOrUndef =
        fun(Key) ->
            case dev_message:get(Key, Map, #{}) of
                {ok, Val} ->
                    case hb_util:short_id(Val) of
                        undefined -> Val;
                        ShortID -> ShortID
                    end;
                {error, _} -> undefined
            end
        end,
    FilterUndef = 
        fun(List) ->
            lists:filter(fun({_, undefined}) -> false; (_) -> true end, List)
        end,
    % Prepare the metadata row for formatting.
    % Note: We try to get the IDs _if_ they are *already* in the map. We do not
    % force calculation of the IDs here because that may cause significant
    % overhead.
    IDMetadata =
        [
            {<<"#P">>, ValOrUndef(hashpath)},
            {<<"*U">>, ValOrUndef(unsigned_id)},
            {<<"*S">>, ValOrUndef(id)}
        ],
    SignerMetadata =
        case signers(Map) of
            [] -> [];
            [Signer] ->
                [{<<"Sig">>, hb_util:short_id(Signer)}];
            Signers ->
                [
                    {
                        <<"Sigs">>,
                        string:join(lists:map(fun hb_util:short_id/1, Signers), ", ")
                    }
                ]
        end,
    % Concatenate the present metadata rows.
    Metadata = FilterUndef(lists:flatten([IDMetadata, SignerMetadata])),
    % Format the metadata row.
    Header =
        hb_util:format_indented("Message [~s] {",
            [
                string:join(
                    [
                        io_lib:format("~s: ~s", [Lbl, Val])
                        || {Lbl, Val} <- Metadata
                    ],
                    ", "
                )
            ],
            Indent
        ),
    % Put the path and device rows into the output at the _top_ of the map.
    PriorityKeys = [{<<"Path">>, ValOrUndef(path)}, {<<"Device">>, ValOrUndef(device)}],
    FooterKeys =
        case hb_private:from_message(Map) of
            PrivMap when map_size(PrivMap) == 0 -> [];
            PrivMap -> [{<<"!Private!">>, PrivMap}]
        end,
    % Concatenate the path and device rows with the rest of the key values.
    KeyVals =
        FilterUndef(PriorityKeys) ++
        maps:to_list(
            minimize(Map,
                [owner, signature, id, unsigned_id, hashpath, path, device]
                ++ [<<"Device">>, <<"Path">>] % Hack: Until key capitalization is fixed.
            )
        ) ++ FooterKeys,
    % Format the remaining 'normal' keys and values.
    Res = lists:map(
        fun({Key, Val}) ->
            NormKey = hb_converge:to_key(Key, #{ error_strategy => ignore }),
            KeyStr = 
                case NormKey of
                    undefined ->
                        io_lib:format("~p [!!! INVALID KEY !!!]", [Key]);
                    _ ->
                        hb_converge:key_to_binary(Key)
                end,
            hb_util:format_indented(
                "~s => ~s~n",
                [
                    lists:flatten([KeyStr]),
                    case Val of
                        NextMap when is_map(NextMap) ->
                            hb_util:format_map(NextMap, Indent + 2);
                        _ when (byte_size(Val) == 32) or (byte_size(Val) == 43) ->
                            Short = hb_util:short_id(Val),
                            io_lib:format("~s [*]", [Short]);
                        _ when byte_size(Val) == 88 ->
                            io_lib:format("~s [#p]", [hb_util:short_id(Val)]);
                        Bin when is_binary(Bin) ->
                            hb_util:format_binary(Bin);
                        Other ->
                            io_lib:format("~p", [Other])
                    end
                ],
                Indent + 1
            )
        end,
        KeyVals
    ),
    case Res of
        [] -> lists:flatten(Header ++ " [Empty] }");
        _ ->
            lists:flatten(
                Header ++ ["\n"] ++ Res ++ hb_util:format_indented("}", Indent)
            )
    end;
format(Item, Indent) ->
    % Whatever we have is not a message map.
    hb_util:format_indented("[UNEXPECTED VALUE] ~p", [Item], Indent).

%% @doc Return the signers of a message. For now, this is just the signer
%% of the message itself. In the future, we will support multiple signers.
signers(Msg) when is_map(Msg) ->
    case {maps:find(owner, Msg), maps:find(signature, Msg)} of
        {_, error} -> [];
        {error, _} -> [];
        {{ok, Owner}, {ok, _}} -> [ar_wallet:to_address(Owner)]
    end;
signers(TX) when is_record(TX, tx) ->
    ar_bundles:signer(TX);
signers(_) -> [].

%% @doc Sign a message with the given wallet. Only supports the `tx' format
%% at the moment.
sign(Msg, Wallet) ->
    TX = convert(Msg, tx, #{}),
    ar_bundles:sign_item(TX, Wallet),
    convert(TX, converge, tx, #{}).

%% @doc Verify a message.
verify(Msg) ->
    TX = convert(Msg, tx, #{}),
    ar_bundles:verify_item(TX),
    convert(TX, converge, tx, #{}).

%% @doc Return the type of a message.
type(TX) when is_record(TX, tx) -> tx;
type(Binary) when is_binary(Binary) -> binary;
type(Msg) when is_map(Msg) ->
    IsDeep = lists:any(
        fun({_, Value}) -> is_map(Value) end,
        lists:filter(
            fun({Key, _}) -> not hb_private:is_private(Key) end,
            maps:to_list(Msg)
        )
    ),
    case IsDeep of
        true -> deep;
        false -> shallow
    end.

%% @doc Check if two maps match, including recursively checking nested maps.
match(Map1, Map2) ->
    Keys1 = maps:keys(NormMap1 = minimize(normalize(Map1))),
    Keys2 = maps:keys(NormMap2 = minimize(normalize(Map2))),
    case Keys1 == Keys2 of
        true ->
            lists:all(
                fun(Key) ->
                    Val1 = maps:get(Key, NormMap1, not_found),
                    Val2 = maps:get(Key, NormMap2, not_found),
                    case is_map(Val1) andalso is_map(Val2) of
                        true -> match(Val1, Val2);
                        false ->
                            case Val1 == Val2 of
                                true -> true;
                                false ->
                                    ?event(
                                        {value_mismatch,
                                            {explicit, {Key, Val1, Val2}}
                                        }
                                    ),
                                    false
                            end
                    end
                end,
                Keys1
            );
        false ->
            ?event({keys_mismatch, Keys1, Keys2}),
            false
    end.
	
matchable_keys(Map) ->
    lists:sort(lists:map(fun hb_converge:key_to_binary/1, maps:keys(Map))).

%% @doc Normalize the keys in a map. Also takes a list of keys and returns a
%% sorted list of normalized keys if the input is a list.
normalize_keys(Keys) when is_list(Keys) ->
    lists:sort(lists:map(fun hb_converge:key_to_binary/1, Keys));
normalize_keys(Map) ->
    maps:from_list(
        lists:map(
            fun({Key, Value}) ->
                {hb_converge:key_to_binary(Key), Value}
            end,
            maps:to_list(Map)
        )
    ).

%% @doc Remove keys from the map that can be regenerated. Optionally takes an
%% additional list of keys to include in the minimization.
minimize(Msg) -> minimize(Msg, []).
minimize(RawVal, _) when not is_map(RawVal) -> RawVal;
minimize(Map, ExtraKeys) ->
    NormKeys = normalize_keys(?REGEN_KEYS) ++ normalize_keys(ExtraKeys),
    maps:filter(
        fun(Key, _) ->
            (not lists:member(hb_converge:key_to_binary(Key), NormKeys))
                andalso (not hb_private:is_private(Key))
        end,
        maps:map(fun(_K, V) -> minimize(V) end, Map)
    ).

%% @doc Return a map with only the keys that necessary, without those that can
%% be regenerated.
normalize(Map) ->
    NormalizedMap = normalize_keys(Map),
    FilteredMap = filter_default_keys(NormalizedMap),
    maps:with(matchable_keys(FilteredMap), FilteredMap).

%% @doc Remove keys from a map that have the default values found in the tx
%% record.
filter_default_keys(Map) ->
    DefaultsMap = default_tx_message(),
    maps:filter(
        fun(Key, Value) ->
            case maps:find(hb_converge:key_to_binary(Key), DefaultsMap) of
                {ok, Value} -> false;
                _ -> true
            end
        end,
        Map
    ).

%% @doc Get the normalized fields and default values of the tx record.
default_tx_message() ->
    normalize_keys(maps:from_list(default_tx_list())).

%% @doc Get the ordered list of fields and default values of the tx record.
default_tx_list() ->
    lists:zip(record_info(fields, tx), tl(tuple_to_list(#tx{}))).

%% @doc Serialize a message to a binary representation, either as JSON or the
%% binary format native to the message/bundles spec in use.
serialize(M) -> serialize(M, binary).
serialize(M, json) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M));
serialize(M, binary) ->
    ar_bundles:serialize(convert(M, tx, #{})).

%% @doc Deserialize a message from a binary representation.
deserialize(B) -> deserialize(B, binary).
deserialize(J, json) ->
    {JSONStruct} = jiffy:decode(J),
    ar_bundles:json_struct_to_item(JSONStruct);
deserialize(B, binary) ->
    convert(ar_bundles:deserialize(B), converge, tx, #{}).

%%% Tests

%% @doc Test that the filter_default_keys/1 function removes TX fields
%% that have the default values found in the tx record, but not those that
%% have been set by the user.
default_keys_removed_test() ->
    TX = #tx { unsigned_id = << 1:256 >>, last_tx = << 2:256 >> },
    TXMap = #{
        unsigned_id => TX#tx.unsigned_id,
        last_tx => TX#tx.last_tx,
        <<"owner">> => TX#tx.owner,
        <<"target">> => TX#tx.target,
        data => TX#tx.data
    },
    FilteredMap = filter_default_keys(TXMap),
    ?assertEqual(<< 1:256 >>, maps:get(unsigned_id, FilteredMap)),
    ?assertEqual(<< 2:256 >>, maps:get(last_tx, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"owner">>, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"target">>, FilteredMap, not_found)).

minimization_test() ->
    Msg = #{
        unsigned_id => << 1:256 >>,
        <<"id">> => << 2:256 >>
    },
    MinimizedMsg = minimize(Msg),
    ?event({minimized, MinimizedMsg}),
    ?assertEqual(0, maps:size(MinimizedMsg)).


basic_map_codec_test(Codec) ->
    Msg = #{ normal_key => <<"NORMAL_VALUE">> },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assertEqual(Msg, Decoded).

%% @doc Test that we can convert a message into a tx record and back.
single_layer_message_to_encoding_test(Codec) ->
    Msg = #{
        last_tx => << 2:256 >>,
        owner => << 3:4096 >>,
        target => << 4:256 >>,
        data => <<"DATA">>,
        <<"Special-Key">> => <<"SPECIAL_VALUE">>
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assertEqual(maps:get(last_tx, Msg), maps:get(last_tx, Decoded)),
    ?assertEqual(maps:get(owner, Msg), maps:get(owner, Decoded)),
    ?assertEqual(maps:get(target, Msg), maps:get(target, Decoded)),
    ?assertEqual(maps:get(data, Msg), maps:get(data, Decoded)),
    ?assertEqual({<<"Special-Key">>, <<"SPECIAL_VALUE">>},
        lists:keyfind(<<"Special-Key">>, 1, maps:get(tags, Decoded))).

% %% @doc Test that different key encodings are converted to their corresponding
% %% TX fields.
% key_encodings_to_tx_test() ->
%     Msg = #{
%         <<"last_tx">> => << 2:256 >>,
%         <<"Owner">> => << 3:4096 >>,
%         <<"Target">> => << 4:256 >>
%     },
%     TX = message_to_tx(Msg),
%     ?event({key_encodings_to_tx, {msg, Msg}, {tx, TX}}),
%     ?assertEqual(maps:get(<<"last_tx">>, Msg), TX#tx.last_tx),
%     ?assertEqual(maps:get(<<"Owner">>, Msg), TX#tx.owner),
%     ?assertEqual(maps:get(<<"Target">>, Msg), TX#tx.target).

%% @doc Test that the message matching function works.
match_test(Codec) ->
    Msg = #{ a => 1, b => 2 },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

%% @doc Structured field parsing tests.
structured_field_atom_parsing_test(Codec) ->
    Msg = #{ highly_unusual_http_header => highly_unusual_value },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

structured_field_decimal_parsing_test(Codec) ->
    Msg = #{ integer_field => 1234567890 },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

binary_to_binary_test(Codec) ->
    % Serialization must be able to turn a raw binary into a TX, then turn
    % that TX back into a binary and have the result match the original.
    Bin = <<"THIS IS A BINARY, NOT A NORMAL MESSAGE">>,
    Encoded = convert(Bin, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assertEqual(Bin, Decoded).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
message_with_large_keys_test(Codec) ->
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:((1 + 1024) * 8) >>,
        <<"another_large_key">> => << 0:((1 + 1024) * 8) >>,
        <<"another_normal_key">> => <<"another_normal_value">>
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

%% @doc Check that large keys and data fields are correctly handled together.
nested_message_with_large_keys_and_data_test(Codec) ->
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:(1024 * 16) >>,
        <<"another_large_key">> => << 0:(1024 * 16) >>,
        <<"another_normal_key">> => <<"another_normal_value">>,
        data => <<"Hey from the data field!">>
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(match(Msg, Decoded)).

simple_nested_message_test(Codec) ->
    Msg = #{
        a => <<"1">>,
        nested => #{ <<"b">> => <<"1">> },
        c => <<"3">>
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(
        match(
            Msg,
            Decoded
        )
    ).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
nested_message_with_large_data_test(Codec) ->
    Msg = #{
        <<"tx_depth">> => <<"outer">>,
        data => #{
            <<"tx_map_item">> =>
                #{
                    <<"tx_depth">> => <<"inner">>,
                    <<"large_data_inner">> => << 0:((1 + 1024) * 8) >>
                },
            <<"large_data_outer">> => << 0:((1 + 1024) * 8) >>
        }
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

%% @doc Test that we can convert a 3 layer nested message into a tx record and back.
deeply_nested_message_with_data_test(Codec) ->
    Msg = #{
        <<"tx_depth">> => <<"outer">>,
        data => #{
            <<"tx_map_item">> =>
                #{
                    <<"tx_depth">> => <<"inner">>,
                    data => #{
                        <<"tx_depth">> => <<"innermost">>,
                        data => <<"DATA">>
                    }
                }
        }
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

nested_structured_fields_test(Codec) ->
    NestedMsg = #{ a => #{ b => 1 } },
    Encoded = convert(NestedMsg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(NestedMsg, Decoded)).

nested_message_with_large_keys_test(Codec) ->
    Msg = #{
        a => <<"1">>,
        long_data => << 0:((1 + 1024) * 8) >>,
        nested => #{ <<"b">> => <<"1">> },
        c => <<"3">>
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

%% @doc Test that we can convert a signed tx into a message and back.
signed_tx_to_message_and_back_test(Codec) ->
    TX = #tx {
        data = <<"TEST_DATA">>,
        tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    Encoded = convert(SignedTX, Codec, tx, #{}),
    ?assert(ar_bundles:verify_item(SignedTX)),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(ar_bundles:verify_item(Decoded)).

signed_deep_tx_serialize_and_deserialize_test(Codec) ->
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
    Encoded = convert(SignedTX, Codec, tx, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(
        match(
            convert(SignedTX, converge, tx, #{}),
            Decoded
        )
    ).

calculate_unsigned_message_id_test(Codec) ->
    Msg = #{
        data => <<"DATA">>,
        <<"special_key">> => <<"SPECIAL_KEY">>
    },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assertEqual(
        dev_message:unsigned_id(Decoded),
        dev_message:unsigned_id(Msg)
    ).

sign_serialize_deserialize_verify_test(Codec) ->
    Msg = #{
        data => <<"DATA">>,
        <<"special_key">> => <<"SPECIAL_KEY">>
    },
    SignedTX = ar_bundles:sign_item(Msg, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SerializedMsg = serialize(SignedTX),
    DeserializedMsg = deserialize(SerializedMsg),
    Decoded = convert(DeserializedMsg, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

unsigned_id_test(Codec) ->
    Msg = #{ data => <<"TEST_DATA">> },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assertEqual(
        dev_message:unsigned_id(Decoded),
        dev_message:unsigned_id(Msg)
    ).

% signed_id_test_disabled() ->
%     TX = #tx {
%         data = <<"TEST_DATA">>,
%         tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
%     },
%     SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
%     ?assert(ar_bundles:verify_item(SignedTX)),
%     SignedMsg = hb_codec_tx:from(SignedTX),
%     ?assertEqual(
%         hb_util:encode(ar_bundles:id(SignedTX, signed)),
%         hb_util:id(SignedMsg, signed)
%     ).

message_with_simple_list_test(Codec) ->
    Msg = #{ a => [<<"1">>, <<"2">>, <<"3">>] },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).

empty_string_in_tag_test(Codec) ->
    Msg =
        #{
            dev =>
                #{
                    <<"stderr">> => <<"">>,
                    <<"stdin">> => <<"b">>,
                    <<"stdout">> => <<"c">>
                }
        },
    Encoded = convert(Msg, Codec, converge, #{}),
    Decoded = convert(Encoded, converge, Codec, #{}),
    ?assert(match(Msg, Decoded)).


%%% Test helpers

test_codecs() ->
    [converge, flat, tx].

generate_test_suite(Suite) ->
    lists:map(
        fun(CodecName) ->
            {foreach,
                fun() -> ok end,
                fun(_) -> ok end,
                [
                    {
                        atom_to_list(CodecName) ++ ": " ++ Desc,
                        fun() -> Test(CodecName) end
                    }
                ||
                    {Desc, Test} <- Suite
                ]
            }
        end,
        test_codecs()
    ).

message_suite_test_() ->
    generate_test_suite([
        {"basic map codec test", fun basic_map_codec_test/1},
        {"match test", fun match_test/1},
        {"single layer message to encoding test", fun single_layer_message_to_encoding_test/1},
        {"message with large keys test", fun message_with_large_keys_test/1},
        {"nested message with large keys and data test", fun nested_message_with_large_keys_and_data_test/1},
        {"simple nested message test", fun simple_nested_message_test/1},
        {"nested message with large data test", fun nested_message_with_large_data_test/1},
        {"deeply nested message with data test", fun deeply_nested_message_with_data_test/1},
        {"structured field atom parsing test", fun structured_field_atom_parsing_test/1},
        {"structured field decimal parsing test", fun structured_field_decimal_parsing_test/1},
        {"binary to binary test", fun binary_to_binary_test/1},
        {"nested structured fields test", fun nested_structured_fields_test/1},
        {"nested message with large keys test", fun nested_message_with_large_keys_test/1},
        {"message with simple list test", fun message_with_simple_list_test/1},
        {"empty string in tag test", fun empty_string_in_tag_test/1},
        {"signed tx to message and back test", fun signed_tx_to_message_and_back_test/1},
        {"signed deep tx serialize and deserialize test", fun signed_deep_tx_serialize_and_deserialize_test/1},
        {"calculate unsigned message id test", fun calculate_unsigned_message_id_test/1},
        {"sign serialize deserialize verify test", fun sign_serialize_deserialize_verify_test/1},
        {"unsigned id test", fun unsigned_id_test/1}
    ]).
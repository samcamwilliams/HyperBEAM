%%% @doc A device implementing the codec interface (to/1, from/1) for 
%%% HyperBEAM's internal, richly typed message format.
%%% 
%%% This format mirrors HTTP Structured Fields, aside from its limitations of 
%%% compound type depths, as well as limited floating point representations.
%%% 
%%% As with all Converge codecs, its target format (the format it expects to 
%%% receive in the `to/1' function, and give in `from/1') is TABM.
%%% 
%%% For more details, see the HTTP Structured Fields (RFC-9651) specification.
-module(dev_codec_structured).
-export([to/1, from/1, attest/3, verify/3]).
-export([decode_value/2, encode_value/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Route signature functions to the `dev_codec_httpsig' module
attest(Msg, Req, Opts) -> dev_codec_httpsig:attest(Msg, Req, Opts).
verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).
attested(Msg, Req, Opts) -> dev_codec_httpsig:attested(Msg, Req, Opts).

%% @doc Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).
from(Bin) when is_binary(Bin) -> Bin;
from(Msg) when is_map(Msg) ->
    maps:from_list(lists:flatten(
        lists:map(
            fun(Key) ->
                case maps:find(Key, Msg) of
                    {ok, <<>>} ->
                        BinKey = hb_converge:normalize_key(Key),
                        {<<"converge-type-", BinKey/binary>>, <<"empty-binary">>};
                    {ok, Value} when is_binary(Value) ->
                        {Key, Value};
                    {ok, Map} when is_map(Map) ->
                        {Key, from(Map)};
                    {ok, Msgs = [Msg1|_]} when is_map(Msg1) ->
                        % We have a list of maps. Convert to a numbered map and
                        % recurse.
                        {Key, from(hb_converge:normalize_keys(Msgs))};
                    {ok, []} ->
                        BinKey = hb_converge:normalize_key(Key),
                        {<<"converge-type-", BinKey/binary>>, <<"empty-list">>};
                    {ok, Value} when
                            is_atom(Value) or is_integer(Value)
                            or is_list(Value) or is_float(Value) ->
                        ItemKey = hb_converge:normalize_key(Key),
                        {Type, BinaryValue} = encode_value(Value),
                        [
                            {<<"converge-type-", ItemKey/binary>>, Type},
                            {ItemKey, BinaryValue}
                        ];
                    {ok, {resolve, Operations}} when is_list(Operations) ->
                        {Key, {resolve, Operations}};
                    {ok, _UnsupportedValue} ->
                        []
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
                maps:keys(Msg)
            )
        )
    ));
from(Other) -> hb_path:to_binary(Other).

%% @doc Convert a TABM into a native HyperBEAM message.
to(Bin) when is_binary(Bin) -> Bin;
to(TABM0) ->
    % First, handle special cases of empty items, which `ar_bundles' cannot
    % handle. Needs to be transformed into a list (unfortunately) so that we
    % can also remove the "Converge-Type-" prefix from the key.
    TABM1 =
        maps:from_list(
            lists:map(
                fun({<<"converge-type-", Key/binary>>, <<"empty-binary">>}) ->
                    {Key, <<>>};
                ({<<"converge-type-", Key/binary>>, <<"empty-list">>}) ->
                    {Key, []};
                ({Key, Value}) ->
                    {Key, Value}
                end,
                maps:to_list(TABM0)
            )
        ),
    % 1. Remove any keys from output that have a "Converge-Type-" prefix;
    % 2. Decode any binary values that have a "Converge-Type-" prefix;
    % 3. Recursively decode any maps that we encounter;
    % 4. Return the remaining keys and values as a map.
    hb_message:filter_default_keys(maps:filtermap(
        fun(<<"converge-type-", _/binary>>, _) ->
            % Remove any keys from output that have a "Converge-Type-" prefix.
            false;
        (RawKey, BinaryValue) when is_binary(BinaryValue) ->
            Key = hb_converge:normalize_key(RawKey),
            case maps:find(<<"converge-type-", Key/binary>>, TABM1) of
                error -> {true, BinaryValue};
                {ok, Type} ->
                    {true, decode_value(Type, BinaryValue)}
            end;
        (_Key, ChildTABM) when is_map(ChildTABM) ->
            {true, to(ChildTABM)};
        (_Key, Value) ->
            % We encountered a key that already has a converted type.
            % We can just return it as is.
            {true, Value}
        end,
        TABM1
    )).

%% @doc Convert a term to a binary representation, emitting its type for
%% serialization as a separate tag.
encode_value(Value) when is_integer(Value) ->
    [Encoded, _] = dev_codec_structured_conv:item({item, Value, []}),
    {<<"integer">>, Encoded};
encode_value(Value) when is_float(Value) ->
    ?no_prod("Must use structured field representation for floats!"),
    {<<"float">>, float_to_binary(Value)};
encode_value(Value) when is_atom(Value) ->
    [EncodedIOList, _] =
        dev_codec_structured_conv:item(
            {item, {string, atom_to_binary(Value, latin1)}, []}),
    Encoded = list_to_binary(EncodedIOList),
    {<<"atom">>, Encoded};
encode_value(Values) when is_list(Values) ->
    EncodedValues =
        lists:map(
            fun(Bin) when is_binary(Bin) -> {item, {string, Bin}, []};
               (Item) ->
                {RawType, Encoded} = encode_value(Item),
                Type = hb_converge:normalize_key(RawType),
                {
                    item,
                    {
                        string,
                        <<
                            "(converge-type-", Type/binary, ") ",
                            Encoded/binary
                        >>
                    },
                    []
                }
            end,
            Values
        ),
    EncodedList = dev_codec_structured_conv:list(EncodedValues),
    {<<"list">>, iolist_to_binary(EncodedList)};
encode_value(Value) when is_binary(Value) ->
    {<<"binary">>, Value};
encode_value(Value) ->
    Value.

%% @doc Convert non-binary values to binary for serialization.
decode_value(Type, Value) when is_list(Type) ->
    decode_value(list_to_binary(Type), Value);
decode_value(Type, Value) when is_binary(Type) ->
    decode_value(
        binary_to_existing_atom(
            list_to_binary(string:to_lower(binary_to_list(Type))),
            latin1
        ),
        Value
    );
decode_value(integer, Value) ->
    {item, Number, _} = dev_codec_structured_conv:parse_item(Value),
    Number;
decode_value(float, Value) ->
    binary_to_float(Value);
decode_value(atom, Value) ->
    {item, {string, AtomString}, _} =
        dev_codec_structured_conv:parse_item(Value),
    binary_to_existing_atom(AtomString);
decode_value(list, Value) ->
    lists:map(
        fun({item, {string, <<"(converge-type-", Rest/binary>>}, _}) ->
            [Type, Item] = binary:split(Rest, <<") ">>),
            decode_value(Type, Item);
           ({item, Item, _}) -> dev_codec_structured_conv:from_bare_item(Item)
        end,
        dev_codec_structured_conv:parse_list(iolist_to_binary(Value))
    );
decode_value(map, Value) ->
    maps:from_list(
        lists:map(
            fun({Key, {item, Item, _}}) ->
                ?event({decoded_item, {explicit, Key}, Item}),
                {Key, dev_codec_structured_conv:from_bare_item(Item)}
            end,
            dev_codec_structured_conv:parse_dictionary(iolist_to_binary(Value))
        )
    );
decode_value(BinType, Value) when is_binary(BinType) ->
    decode_value(
        list_to_existing_atom(
            string:to_lower(
                binary_to_list(BinType)
            )
        ),
        Value
    );
decode_value(OtherType, Value) ->
    ?event({unexpected_type, OtherType, Value}),
    throw({unexpected_type, OtherType, Value}).

%%% Tests

list_encoding_test() ->
    % Test that we can encode and decode a list of integers.
    {<<"list">>, Encoded} = encode_value(List1 = [1, 2, 3]),
    Decoded = decode_value(list, Encoded),
    ?assertEqual(List1, Decoded),
    % Test that we can encode and decode a list of binaries.
    {<<"list">>, Encoded2} = encode_value(List2 = [<<"1">>, <<"2">>, <<"3">>]),
    ?assertEqual(List2, decode_value(list, Encoded2)),
    % Test that we can encode and decode a mixed list.
    {<<"list">>, Encoded3} = encode_value(List3 = [1, <<"2">>, 3]),
    ?assertEqual(List3, decode_value(list, Encoded3)).
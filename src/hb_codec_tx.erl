%%% @doc Codec for managing transformations from `ar_bundles`-style Arweave TX
%%% records to and from TABMs.
-module(hb_codec_tx).
-export([to/1, from/1]).
-include("include/hb.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).
%% The list of TX fields that users can set directly.
-define(TX_KEYS,
    [
        <<"id">>,
        <<"unsigned_id">>,
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"signature">>
    ]
).
-define(FILTERED_TAGS,
    [
        <<"bundle-format">>,
        <<"bundle-map">>,
        <<"bundle-version">>
    ]
).

%% @doc Convert a #tx record into a message map recursively.
from(Binary) when is_binary(Binary) -> Binary;
from(TX) when is_record(TX, tx) ->
    case lists:keyfind(<<"converge-type">>, 1, TX#tx.tags) of
        false ->
            do_from(TX);
        {<<"converge-type">>, <<"binary">>} ->
            TX#tx.data
    end.
do_from(RawTX) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)), % <- Is norm necessary?
    % Get the raw fields and values of the tx record and pair them. Then convert 
    % the list of key-value pairs into a map, removing irrelevant fields.
    TXKeysMap =
        maps:with(?TX_KEYS,
            hb_converge:normalize_keys(
                maps:from_list(
                    lists:zip(
                        record_info(fields, tx),
                        tl(tuple_to_list(TX))
                    )
                )
            )
        ),
    % Generate a TABM from the tags.
    MapWithoutData = maps:merge(TXKeysMap, maps:from_list(TX#tx.tags)),
    DataMap =
        case TX#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                maps:merge(
                    MapWithoutData,
                    maps:map(
                        fun(_, InnerValue) -> from(InnerValue) end,
                        Data
                    )
                );
            Data when Data == ?DEFAULT_DATA ->
                MapWithoutData;
            Data when is_binary(Data) ->
                MapWithoutData#{ <<"data">> => Data };
            Data ->
                ?event({unexpected_data_type, {explicit, Data}}),
                ?event({was_processing, {explicit, TX}}),
                throw(invalid_tx)
        end,
    % Merge the data map with the rest of the TX map and remove any keys that
    % are not part of the message.
    NormalizedDataMap =
        hb_converge:normalize_keys(maps:merge(DataMap, MapWithoutData)),
    maps:without(?FILTERED_TAGS, NormalizedDataMap).

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    #tx{
        tags= [{<<"converge-type">>, <<"binary">>}],
        data = Binary
    };
to(TX) when is_record(TX, tx) -> TX;
to(RawTABM) when is_map(RawTABM) ->
    % The path is a special case so we normalized it first. It may have been
    % modified by `hb_converge` in order to set it to the current key that is
    % being executed. We should check whether the path is in the
    % `priv/Converge/Original-Path` field, and if so, use that instead of the
    % stated path. This normalizes the path, such that the signed message will
    % continue to validate correctly.
    TABM = hb_converge:normalize_keys(RawTABM),
    M =
        case {maps:find(<<"path">>, TABM), hb_private:from_message(TABM)} of
            {{ok, _}, #{ <<"converge">> := #{ <<"original-path">> := Path } }} ->
                maps:put(<<"path">>, Path, TABM);
            _ -> TABM
        end,
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call message_to_tx/1 on the inner map because that would lead to adding
    % an extra layer of nesting to the data.
    %?event({message_to_tx, {keys, Keys}, {map, M}}),
    MsgKeyMap =
        maps:map(
            fun(_Key, Msg) when is_map(Msg) -> to(Msg);
            (_Key, Value) -> Value
            end,
            M
        ),
    NormalizedMsgKeyMap = hb_converge:normalize_keys(MsgKeyMap),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    {RemainingMap, BaseTXList} =
        lists:foldl(
            fun({Field, Default}, {RemMap, Acc}) ->
                NormKey = hb_converge:normalize_key(Field),
                case maps:find(NormKey, NormalizedMsgKeyMap) of
                    error -> {RemMap, [Default | Acc]};
                    {ok, Value} when is_binary(Default) andalso ?IS_ID(Value) ->
                        {
                            maps:remove(NormKey, RemMap),
                            [hb_util:native_id(Value)|Acc]
                        };
                    {ok, Value} ->
                        {
                            maps:remove(NormKey, RemMap),
                            [Value|Acc]
                        }
                end
            end,
            {NormalizedMsgKeyMap, []},
            hb_message:default_tx_list()
        ),
    % Rebuild the tx record from the new list of fields and values.
    TXWithoutTags = list_to_tuple([tx | lists:reverse(BaseTXList)]),
    % Calculate which set of the remaining keys will be used as tags.
    {Tags, RawDataItems} =
        lists:partition(
            fun({_Key, Value}) when is_binary(Value) ->
                    case unicode:characters_to_binary(Value) of
                        {error, _, _} -> false;
                        _ -> byte_size(Value) =< ?MAX_TAG_VAL
                    end;
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
    % Recursively turn the remaining data items into tx records.
    DataItems = maps:from_list(lists:map(
        fun({Key, Value}) ->
            {hb_converge:normalize_key(Key), to(Value)}
        end,
        RawDataItems
    )),
    % Set the data based on the remaining keys.
    TXWithData = 
        case {TX#tx.data, maps:size(DataItems)} of
            {Binary, 0} when is_binary(Binary) ->
                TX;
            {?DEFAULT_DATA, _} ->
                TX#tx { data = DataItems };
            {Data, _} when is_map(Data) ->
                TX#tx { data = maps:merge(Data, DataItems) };
            {Data, _} when is_record(Data, tx) ->
                TX#tx { data = DataItems#{ <<"data">> => Data } };
            {Data, _} when is_binary(Data) ->
                TX#tx { data = DataItems#{ <<"data">> => to(Data) } }
        end,
    % ar_bundles:reset_ids(ar_bundles:normalize(TXWithData));
    Res = try ar_bundles:reset_ids(ar_bundles:normalize(TXWithData))
    catch
        _:Error ->
            ?event({{reset_ids_error, Error}, {tx_without_data, TX}}),
            ?event({prepared_tx_before_ids,
                {tags, {explicit, TXWithData#tx.tags}},
                {data, TXWithData#tx.data}
            }),
            throw(Error)
    end,
    %?event({result, {explicit, Res}}),
    Res;
to(Other) ->
    throw(invalid_tx).
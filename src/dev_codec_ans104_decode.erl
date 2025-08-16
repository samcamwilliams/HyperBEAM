%%% @doc Library functions for decoding ANS-104-style data items to TABM form.
-module(dev_codec_ans104_decode).
-export([fields/2, tags/2, data/4, committed/5, base/5]).
-export([with_commitments/5]).
-include("include/hb.hrl").
-include("include/dev_codec_ans104.hrl").

%% @doc Return a TABM message containing the fields of the given decoded
%% ANS-104 data item that should be included in the base message.
fields(Item, _Opts) ->
    case Item#tx.target of
        ?DEFAULT_TARGET -> #{};
        Target -> #{ <<"target">> => Target }
    end.

%% @doc Return a TABM of the raw tags of the item, including all metadata
%% (e.g. `ao-type', `ao-data-key', etc.)
tags(Item, Opts) ->
    hb_ao:normalize_keys(
        dev_codec_ans104:deduplicating_from_list(Item#tx.tags, Opts),
        Opts
    ).

%% @doc Return a TABM of the keys and values found in the data field of the item.
data(Item, Req, Tags, Opts) ->
    RawData =
        case Item#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                hb_ao:normalize_keys(
                    hb_maps:map(
                        fun(_, InnerValue) ->
                            hb_util:ok(dev_codec_ans104:from(InnerValue, Req, Opts))
                        end,
                        Data,
                        Opts
                    ),
                    Opts
                );
            Data when is_binary(Data) -> Data;
            Data ->
                ?event({unexpected_data_type, {explicit, Data}}),
                ?event({was_processing, {explicit, Item}}),
                throw(invalid_tx)
        end,
    % Normalize the `data` field to the `ao-data-key' tag's value, if set.
    DataKey = maps:get(<<"ao-data-key">>, Tags, <<"data">>),
    case {DataKey, RawData} of
        {_, ?DEFAULT_DATA} -> #{};
        {<<"data">>, _Data} when is_map(RawData) -> RawData;
        {DataKey, _Data} -> #{ DataKey => RawData }
    end.

%% @doc Calculate the list of committed keys for an item, based on its 
%% components (fields, tags, and data).
committed(Item, Fields, Tags, Data, Opts) ->
    hb_util:unique(
        field_keys(Fields, Tags, Data, Opts) ++
        data_keys(Data, Opts) ++
        tag_keys(Item, Opts)
    ).

%% @doc Return the list of the keys from the fields TABM.
field_keys(BaseFields, Tags, Data, Opts) ->
    HasTarget =
        hb_maps:is_key(<<"target">>, BaseFields, Opts) orelse
        hb_maps:is_key(<<"target">>, Tags, Opts) orelse
        hb_maps:is_key(<<"target">>, Data, Opts),
    case HasTarget of
        true -> [<<"target">>];
        false -> []
    end.

%% @doc Return the list of the keys from the data TABM.
data_keys(Data, Opts) ->
    hb_util:to_sorted_keys(Data, Opts).

%% @doc Return the list of the keys from the tags TABM. Filter all metadata
%% tags: `ao-data-key', `ao-types', `bundle-format', `bundle-version'.
tag_keys(Item, _Opts) ->
    MetaTags = [
        <<"ao-data-key">>,
        <<"ao-types">>,
        <<"bundle-format">>,
        <<"bundle-version">>
    ],
    lists:filtermap(
        fun({Tag, _}) ->
            case lists:member(Tag, MetaTags) of
                true -> false;
                false -> {true, hb_util:to_lower(hb_ao:normalize_key(Tag))}
            end
        end,
        Item#tx.tags
    ).

%% @doc Return the complete message for an item, less its commitments. The
%% precidence order for choosing fields to place into the base message is:
%% 1. Data
%% 2. Tags
%% 3. Fields
%% 
base(CommittedKeys, Fields, Tags, Data, Opts) ->
    hb_maps:from_list(
        lists:map(
            fun(Key) ->
                case hb_maps:find(Key, Data, Opts) of
                    error ->
                        case hb_maps:find(Key, Fields, Opts) of
                            error ->
                                case hb_maps:find(Key, Tags, Opts) of
                                    error -> throw({missing_key, Key});
                                    {ok, Value} -> {Key, Value}
                                end;
                            {ok, Value} -> {Key, Value}
                        end;
                    {ok, Value} -> {Key, Value}
                end
            end,
            CommittedKeys
        )
    ).

%% @doc Return a message with the appropriate commitments added to it.
with_commitments(Item, Tags, Base, CommittedKeys, Opts) ->
    case Item#tx.signature of
        ?DEFAULT_SIG ->
            case dev_codec_ans104:normal_tags(Item#tx.tags) of
                true -> Base;
                false ->
                    with_unsigned_commitment(Item, Base, CommittedKeys, Opts)
            end;
        _ -> with_signed_commitment(Item, Tags, Base, CommittedKeys, Opts)
    end.

%% @doc Returns a commitments message for an item, containing an unsigned
%% commitment.
with_unsigned_commitment(Item, UncommittedMessage, CommittedKeys, Opts) ->
    ID = hb_util:human_id(Item#tx.unsigned_id),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID =>
                filter_unset(
                    #{
                        <<"commitment-device">> => <<"ans104@1.0">>,
                        <<"committed">> => CommittedKeys,
                        <<"type">> => <<"unsigned-sha256">>,
                        <<"original-tags">> => original_tags(Item, Opts)
                    },
                    Opts
                )
        }
    }.

%% @doc Returns a commitments message for an item, containing a signed
%% commitment.
with_signed_commitment(Item, Tags, UncommittedMessage, CommittedKeys, Opts) ->
    Address = hb_util:human_id(ar_wallet:to_address(Item#tx.owner)),
    ID = hb_util:human_id(Item#tx.id),
    Commitment =
        filter_unset(
            #{
                <<"commitment-device">> => <<"ans104@1.0">>,
                <<"committer">> => Address,
                <<"committed">> => CommittedKeys,
                <<"signature">> => hb_util:encode(Item#tx.signature),
                <<"type">> => <<"rsa-pss-sha256">>,
                <<"bundle">> => hb_maps:is_key(<<"bundle-format">>, Tags, Opts),
                <<"original-tags">> => original_tags(Item, Opts),
                <<"last_tx">> =>
                    case Item#tx.last_tx of
                        ?DEFAULT_LAST_TX -> unset;
                        LastTX -> LastTX
                    end
            },
            Opts
        ),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID => Commitment
        }
    }.

%% @doc Return the original tags of an item if it is applicable. Otherwise,
%% return `undefined'.
original_tags(Item, _Opts) ->
    case dev_codec_ans104:normal_tags(Item#tx.tags) of
        true -> unset;
        false -> dev_codec_ans104:encoded_tags_to_map(Item#tx.tags)
    end.

%% @doc Remove all undefined values from a map.
filter_unset(Map, Opts) ->
    hb_maps:filter(
        fun(_, Value) ->
            case Value of
                unset -> false;
                _ -> true
            end
        end,
        Map,
        Opts
    ).
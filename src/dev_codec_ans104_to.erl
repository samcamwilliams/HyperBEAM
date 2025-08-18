%%% @doc Library functions for encoding messages to the ANS-104 format.
-module(dev_codec_ans104_to).
-export([maybe_load/3, siginfo/2, data/3, tags/4]).
-include("include/hb.hrl").
-include("include/dev_codec_ans104.hrl").

%% @doc Determine if the message should be loaded from the cache and re-converted
%% to the TABM format. We do this if the `bundle' key is set to true.
maybe_load(RawTABM, Req, Opts) ->
    case hb_util:atom(hb_ao:get(<<"bundle">>, Req, false, Opts)) of
        false -> RawTABM;
        true ->
            % Convert back to the fully loaded structured@1.0 message, then
            % convert to TABM with bundling enabled.
            Structured = hb_message:convert(RawTABM, <<"structured@1.0">>, Opts),
            Loaded = hb_cache:ensure_all_loaded(Structured, Opts),
            % Convert to TABM with bundling enabled.
            LoadedTABM =
                hb_message:convert(
                    Loaded,
                    tabm,
                    #{
                        <<"device">> => <<"structured@1.0">>,
                        <<"bundle">> => true
                    },
                    Opts
                ),
            % Ensure the commitments from the original message are the only
            % ones in the fully loaded message.
            LoadedComms = maps:get(<<"commitments">>, RawTABM, #{}),
            LoadedTABM#{ <<"commitments">> => LoadedComms }
    end.

%% @doc Calculate the fields field for a message, returning an initial TX
%% record.
siginfo(Message, Opts) ->
    MaybeCommitment =
        hb_message:commitment(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            Message,
            Opts
        ),
    case MaybeCommitment of
        {ok, _, Commitment} ->
            commitment_to_tx(Commitment, Opts);
        not_found -> #tx{};
        multiple_matches ->
            throw({multiple_ans104_commitments_unsupported, Message})
    end.

%% @doc Convert a commitment to a base TX record. Extracts the owner, signature,
%% tags, and last TX from the commitment. If the value is not present, the
%% default value is used.
commitment_to_tx(Commitment, Opts) ->
    Signature =
        hb_util:decode(
            maps:get(<<"signature">>, Commitment, hb_util:encode(?DEFAULT_SIG))
        ),
    Owner =
        case hb_maps:find(<<"keyid">>, Commitment, Opts) of
            {ok, KeyID} ->
                hb_util:decode(
                    dev_codec_httpsig_keyid:remove_scheme_prefix(KeyID)
                );
            error -> ?DEFAULT_OWNER
        end,
    Tags =
        case hb_maps:find(<<"original-tags">>, Commitment, Opts) of
            {ok, OriginalTags} -> original_tags_to_tags(OriginalTags);
            error -> []
        end,
    LastTX =
        case hb_maps:find(<<"field-anchor">>, Commitment, Opts) of
            {ok, EncodedLastTX} -> hb_util:decode(EncodedLastTX);
            error -> ?DEFAULT_LAST_TX
        end,
    Target =
        case hb_maps:find(<<"field-target">>, Commitment, Opts) of
            {ok, EncodedTarget} -> hb_util:decode(EncodedTarget);
            error -> ?DEFAULT_TARGET
        end,
    ?event({commitment_owner, Owner}),
    ?event({commitment_signature, Signature}),
    ?event({commitment_tags, Tags}),
    ?event({commitment_last_tx, LastTX}),
    #tx{
        owner = Owner,
        signature = Signature,
        tags = Tags,
        anchor = LastTX,
        target = Target
    }.

%% @doc Calculate the data field for a message.
data(TABM, Req, Opts) ->
    DataKey = inline_key(TABM),
    UncommittedTABM =
        hb_maps:without(
            [<<"commitments">>],
            hb_private:reset(TABM),
            Opts
        ),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages.
    NestedMsgs =
        hb_maps:filtermap(
            fun(_Key, Msg) when is_map(Msg) ->
                {true, hb_util:ok(dev_codec_ans104:to(Msg, Req, Opts))};
                (_Key, _Value) -> false
            end,
            hb_ao:normalize_keys(UncommittedTABM, Opts),
            Opts
        ),
    DataVal = hb_maps:get(DataKey, TABM, ?DEFAULT_DATA),
    ?event(debug_data, {data_val, DataVal}),
    case {DataVal, hb_maps:size(NestedMsgs, Opts)} of
        {Binary, 0} when is_binary(Binary) ->
            % There are no nested messages, so we return the binary alone.
            Binary;
        {?DEFAULT_DATA, _} ->
            NestedMsgs;
        {Data, _} when is_map(Data) ->
            hb_maps:merge(Data, NestedMsgs, Opts);
        {Data, _} when is_record(Data, tx); is_binary(Data) ->
            NestedMsgs#{ <<"data">> => Data }
    end.

%% @doc Calculate the tags field for a data item. If the TX already has tags
%% from the commitment decoding step, we use them. Otherwise we determine the
%% keys to use from the `committed' field of the TABM.
tags(#tx{ tags = ExistingTags }, _, _, _) when ExistingTags =/= [] ->
    ExistingTags;
tags(TX, TABM, Data, Opts) ->
    DataKey = inline_key(TABM),
    MaybeCommitment =
        hb_message:commitment(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            TABM,
            Opts
        ),
    CommittedTagKeys =
        case MaybeCommitment of
            {ok, _, Commitment} ->
                % There is already a commitment, so the tags and ordered are 
                % pre-determined. If the target is set in the base TX from the
                % commitment, we check if the TABM equals that value. If it does,
                % we do not additionally add the target tag. If they differ, we
                % include it.
                hb_util:ok(hb_maps:find(<<"committed">>, Commitment, Opts)) --
                    case {TX#tx.target, hb_maps:get(<<"target">>, TABM, undefined, Opts)} of
                        {?DEFAULT_TARGET, _} -> [];
                        {FieldTarget, TagTarget} when FieldTarget =/= TagTarget ->
                            [<<"target">>];
                        _ -> []
                    end;
            not_found ->
                % There is no commitment, so we need to generate the tags. We add
                % the bundle-format and bundle-version tags, and the ao-data-key
                % tag if it is set to a non-default value, followed by the keys
                % from the TABM (less the target key if it is an ID).
                hb_util:list_without(
                    if is_map(Data) -> hb_maps:keys(Data, Opts);
                    true -> []
                    end,
                    hb_private:reset(hb_util:to_sorted_keys(TABM, Opts)) --
                        [<<"commitments">>]
                );
            multiple_matches ->
                throw({multiple_ans104_commitments_unsupported, TABM})
        end,
    ?event(
        {tags_before_data_key,
            {committed_tag_keys, CommittedTagKeys},
            {data_key, DataKey},
            {data, Data},
            {tabm, TABM}
        }),
    committed_tag_keys_to_tags(DataKey, CommittedTagKeys, TABM, TX, Opts).

%% @doc Apply the `ao-data-key' to the committed keys to generate the list of
%% tags to include in the message.
committed_tag_keys_to_tags(DataKey, Committed, TABM, TX, Opts) ->
    DataKeysToExclude = case TX#tx.data of
        Data when is_map(Data)-> maps:keys(Data);
        _ -> []
    end,
    case DataKey of
        <<"data">> -> [];
        _ -> [{<<"ao-data-key">>, DataKey}]
    end ++
    lists:map(
        fun(Key) ->
            case hb_maps:find(Key, TABM, Opts) of
                error -> throw({missing_committed_key, Key});
                {ok, Value} -> {Key, Value}
            end
        end,
        hb_util:message_to_ordered_list(Committed -- DataKeysToExclude) -- [DataKey]
    ).

%%% Utility functions
    
%% @doc Determine if an `ao-data-key` should be added to the message.
inline_key(Msg) ->
    InlineKey = maps:get(<<"ao-data-key">>, Msg, undefined),
    case {
        InlineKey,
        maps:get(<<"data">>, Msg, ?DEFAULT_DATA) == ?DEFAULT_DATA,
        maps:is_key(<<"body">>, Msg)
            andalso not ?IS_LINK(maps:get(<<"body">>, Msg, undefined))
    } of
        {Explicit, _, _} when Explicit =/= undefined ->
            % ao-data-key already exists, so we honor it.
            InlineKey;
        {_, true, true} -> 
            % There is no specific data field set, but there is a body, so we
            % use that as the `inline-key`.
            <<"body">>;
        _ ->
            % Default: `data' resolves to `data'.
            <<"data">>
    end.

%% @doc Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
%% recreating the original order of the tags.
original_tags_to_tags(TagMap) ->
    OrderedList = hb_util:message_to_ordered_list(hb_private:reset(TagMap)),
    ?event({ordered_tagmap, {explicit, OrderedList}, {input, {explicit, TagMap}}}),
    lists:map(
        fun(#{ <<"name">> := Key, <<"value">> := Value }) ->
            {Key, Value}
        end,
        OrderedList
    ).
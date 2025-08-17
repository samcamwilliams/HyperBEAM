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
            {ok, OriginalTags} ->
                lists:map(
                    fun(#{ <<"name">> := Name, <<"value">> := Value }) ->
                        {Name, Value}
                    end,
                    hb_util:message_to_ordered_list(OriginalTags)
                );
            error -> []
        end,
    LastTX =
        case hb_maps:find(<<"field-anchor">>, Commitment, Opts) of
            {ok, EncodedLastTX} -> hb_util:decode(EncodedLastTX);
            error -> ?DEFAULT_LAST_TX
        end,
    ?event({commitment_owner, Owner}),
    ?event({commitment_signature, Signature}),
    ?event({commitment_tags, Tags}),
    ?event({commitment_last_tx, LastTX}),
    #tx{
        owner = Owner,
        signature = Signature,
        tags = Tags,
        anchor = LastTX
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
tags(_TX, TABM, Data, Opts) ->
    DataKey = inline_key(TABM),
    MaybeCommitment =
        hb_message:commitment(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            TABM,
            Opts
        ),
    CommittedKeys =
        case MaybeCommitment of
            {ok, _, Commitment} ->
                % There is already a commitment, so the tags and ordered are 
                % pre-determined.
                {ok, Committed} = hb_maps:find(<<"committed">>, Commitment, Opts),
                hb_util:message_to_ordered_list(Committed) --
                    [DataKey, <<"target">>];
            not_found ->
                % There is no commitment, so we need to generate the tags. We add
                % the bundle-format and bundle-version tags, and the ao-data-key
                % tag if it is set to a non-default value, followed by the keys
                % from the TABM (less the target key if it is an ID).
                hb_util:list_without(
                    case ?IS_ID(hb_maps:get(<<"target">>, TABM, Opts)) of
                        true -> [<<"target">>];
                        false -> []
                    end ++
                    if is_map(Data) ->
                        hb_util:to_sorted_keys(Data, Opts);
                    true -> []
                    end,
                    hb_private:reset(hb_util:to_sorted_keys(TABM, Opts)) --
                        [<<"commitments">>]
                );
            multiple_matches ->
                throw({multiple_ans104_commitments_unsupported, TABM})
        end,
    ?event({committed_keys_before_data_key, CommittedKeys}),
    lists:map(
        fun(Key) ->
            case hb_maps:find(Key, TABM, Opts) of
                error -> throw({missing_committed_key, Key});
                {ok, Value} -> {Key, Value}
            end
        end,
        apply_data_key_to_committed(DataKey, CommittedKeys)
    ).

%% @doc Apply the `ao-data-key' to the committed keys to generate the list of
%% tags to include in the message.
apply_data_key_to_committed(DataKey, Committed) ->
    case DataKey of
        <<"data">> -> [];
        _ -> [<<"ao-data-key">>]
    end ++
    hb_util:message_to_ordered_list(Committed) --
        [DataKey, <<"target">>].
    
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

%% @doc Where should we place the `target' key of a message?
target_location(Msg) ->
    case maps:get(<<"target">>, Msg, undefined) of
        undefined -> undefined;
        ID when ?IS_ID(ID) -> field;
        _ -> tag
    end.
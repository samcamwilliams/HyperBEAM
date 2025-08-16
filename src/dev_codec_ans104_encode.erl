%%% @doc Library functions for encoding messages to the ANS-104 format.
-module(dev_codec_ans104_encode).
-export([siginfo/2, data/2]).
-include("include/hb.hrl").
-include("include/dev_codec_ans104.hrl").

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
            {ok, OriginalTags} -> OriginalTags;
            error -> []
        end,
    LastTX =
        case hb_maps:find(<<"last_tx">>, Commitment, Opts) of
            {ok, EncodedLastTX} -> hb_util:decode(EncodedLastTX);
            error -> ?DEFAULT_LAST_TX
        end,
    #tx{
        owner = Owner,
        signature = Signature,
        tags = Tags,
        last_tx = LastTX
    }.

%% @doc Calculate the data field for a message.
data(TX, TABM, Req, Opts) ->
    DataKey = inline_key(TABM),
    UncommittedTABM = hb_maps:without([<<"commitments">>], TABM, Opts),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call `dev_codec_ans104:to/1' on the inner map because that would lead to
    % adding an extra layer of nesting to the data.
    UserData =
        hb_maps:map(
            fun(_Key, Msg) when is_map(Msg) ->
                hb_util:ok(
                    dev_codec_ans104:to(Msg, Req, Opts)
                );
               (_Key, Value) -> Value
            end,
            UncommittedTABM,
            Opts
        ),
    % Calculate the keys that should be present in the data vs the tags.
    DataMap =
        hb_maps:filter(
            fun(_Key, Value) -> not is_binary(Value) end,
            UncommittedTABM
        ),
    case maps:size(DataMap) of
        0 -> hb_maps:get(DataKey, UserData, ?DEFAULT_DATA);
        _ -> DataMap
    end.

%% @doc Calculate the tags field for a data item. If the TX already has tags
%% from the commitment decoding step, we use them. Otherwise we determine the
%% keys to use from the `committed' field of the TABM.
tags(#tx{ tags = ExistingTags }, _, _, _, _) when ExistingTags =/= [] ->
    ExistingTags;
tags(TX, TABM, DataKey, Data, Opts) ->
    

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
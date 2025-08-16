%%% Field definitions for the ANS-104 codec.

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 4096).
%% The list of TX fields that users can set directly. Data is excluded because
%% it may be set by the codec in order to support nested messages.
-define(TX_KEYS,
    [
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"signature">>
    ]
).

%%% The list of keys that should be forced into the tag list, rather than being
%%% encoded as fields in the TX record.
-define(FORCED_TAG_FIELDS,
    [
        <<"quantity">>,
        <<"manifest">>,
        <<"data_size">>,
        <<"data_tree">>,
        <<"data_root">>,
        <<"reward">>,
        <<"denomination">>,
        <<"signature_type">>
    ]
).
%%% The list of tags that a user is explicitly committing to when they sign an
%%% ANS-104 message.
-define(BASE_COMMITTED_TAGS, ?TX_KEYS ++ [<<"data">>]).
%% List of tags that should be removed during `to'. These relate to the nested
%% ar_bundles format that is used by the `ans104@1.0' codec.
-define(FILTERED_TAGS,
    [
        <<"bundle-format">>,
        <<"bundle-map">>,
        <<"bundle-version">>
    ]
).
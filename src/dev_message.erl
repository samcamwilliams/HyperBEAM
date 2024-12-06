-module(dev_message).
-export([info/0, keys/1, id/1, unsigned_id/1, signers/1, set/3, remove/2, get/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% The identity device: Simply return a key from the message as it is found
%%% in the message's underlying Erlang map. Private keys (`priv[.*]`) are 
%%% not included.

%% The list of keys that are exported by this device.
-define(DEVICE_KEYS, [path, id, unsigned_id, signers, keys, get, set, remove]).

%% @doc Return the info for the identity device.
info() ->
    #{
        default => fun get/3,
        exports => ?DEVICE_KEYS
    }.

%% @doc Return the ID of a message. If the message already has an ID, return
%% that. Otherwise, return the signed ID.
id(M) ->
    {ok, raw_id(M, signed)}.

%% @doc Wrap a call to the `hb_util:id/2` function, which returns the
%% unsigned ID of a message.
unsigned_id(M) ->
    {ok, raw_id(M, unsigned)}.

%% @doc Encode an ID in any format to a normalized, b64u 43 character binary.
raw_id(Item) -> raw_id(Item, unsigned).
raw_id(TX, Type) when is_record(TX, tx) ->
    hb_util:encode(ar_bundles:id(TX, Type));
raw_id(Map, Type) when is_map(Map) ->
    Msg = hb_message:message_to_tx(Map),
    hb_util:encode(ar_bundles:id(Msg, Type));
raw_id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    Bin;
raw_id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    hb_util:encode(Bin);
raw_id(Data, _) when is_list(Data) ->
    raw_id(list_to_binary(Data)).

%% @doc Return the signers of a message.
signers(M) ->
    {ok, hb_util:list_to_numbered_map(hb_message:signers(M))}.

%% @doc Set keys in a message. Takes a map of key-value pairs and sets them in
%% the message, overwriting any existing values.
set(Message1, NewValuesMsg, Opts) ->
    ?event({setting_keys, {msg1, Message1}, {msg2, NewValuesMsg}, {opts, Opts}}),
    KeysToSet =
        lists:filter(
            fun(Key) -> not lists:member(Key, ?DEVICE_KEYS) end,
            hb_pam:keys(NewValuesMsg, Opts)
        ),
    ?event({keys_to_set, {keys, KeysToSet}}),
    {
        ok,
        maps:merge(
            Message1,
            maps:from_list(
                lists:map(
                    fun(Key) ->
                        ?no_prod("Are we sure that the default device should "
                            "resolve values?"),
                        {Key, hb_pam:get(Key, NewValuesMsg, Opts)}
                    end,
                    KeysToSet
                )
            )
        )
    }.

%% @doc Remove a key or keys from a message.
remove(Message1, #{ item := Key }) ->
    remove(Message1, #{ items => [hb_pam:to_key(Key)] });
remove(Message1, #{ items := Keys }) ->
    NormalizedKeysToRemove = lists:map(fun hb_pam:to_key/1, Keys),
    {
        ok,
        maps:filtermap(
            fun(KeyN, Val) ->
                NormalizedKeyN = hb_pam:to_key(KeyN),
                case lists:member(NormalizedKeyN, NormalizedKeysToRemove) of
                    true -> false;
                    false -> {true, Val}
                end
            end,
            Message1 
        )
    }.

%% @doc Get the public keys of a message.
keys(Msg) ->
    {
        ok,
        lists:filter(
            fun(Key) -> not hb_private:is_private(Key) end,
            maps:keys(Msg)
        )
    }.

%% @doc Return the value associated with the key as it exists in the message's
%% underlying Erlang map. First check the public keys, then check case-
%% insensitively if the key is a binary.
get(Key, Msg) -> get(Key, Msg, #{ path => get }).
get(Key, Msg, _Msg2) ->
    ?event({getting_key, {key, Key}, {msg, Msg}}),
    {ok, PublicKeys} = keys(Msg),
    case lists:member(Key, PublicKeys) of
        true -> {ok, maps:get(Key, Msg)};
        false when is_binary(Key) -> case_insensitive_get(Key, Msg);
        false -> {error, not_found}
    end.

%% @doc Key matching should be case insensitive, following RFC-9110, so we 
%% implement a case-insensitive key lookup rather than delegating to
%% `maps:get/2`. Encode the key to a binary if it is not already.
case_insensitive_get(Key, Msg) ->
    {ok, Keys} = keys(Msg),
    case_insensitive_get(Key, Msg, Keys).
case_insensitive_get(Key, Msg, Keys) when byte_size(Key) > 43 ->
    do_case_insensitive_get(Key, Msg, Keys);
case_insensitive_get(Key, Msg, Keys) ->
    do_case_insensitive_get(hb_pam:to_key(Key), Msg, Keys).
do_case_insensitive_get(_Key, _Msg, []) -> {error, not_found};
do_case_insensitive_get(Key, Msg, [CurrKey | Keys]) ->
    case hb_pam:to_key(CurrKey) of
        Key -> {ok, maps:get(Key, Msg)};
        _ -> do_case_insensitive_get(Key, Msg, Keys)
    end.

%%% Tests

%%% Internal module functionality tests:
get_keys_mod_test() ->
    ?assertEqual([a], maps:keys(#{a => 1})).

is_private_mod_test() ->
    ?assertEqual(true, hb_private:is_private(private)),
    ?assertEqual(true, hb_private:is_private(<<"private">>)),
    ?assertEqual(true, hb_private:is_private(<<"private.foo">>)),
    ?assertEqual(false, hb_private:is_private(a)),
    % Generate a long list of characters and check it does not match.
    ?assertEqual(false, hb_private:is_private([ C || C <- lists:seq($a, $z) ])).

%%% Device functionality tests:

keys_from_device_test() ->
    ?assertEqual({ok, [a]}, hb_pam:resolve(#{a => 1}, keys)).

case_insensitive_get_test() ->
    ?assertEqual({ok, 1}, case_insensitive_get(a, #{a => 1})),
    ?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{a => 1})),
    ?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{a => 1})).

private_keys_are_filtered_test() ->
    ?assertEqual(
        {ok, [a]},
        hb_pam:resolve(#{a => 1, private => 2}, keys)
    ),
    ?assertEqual(
        {ok, [a]},
        hb_pam:resolve(#{a => 1, "priv_foo" => 4}, keys)
    ).

cannot_get_private_keys_test() ->
    ?assertEqual(
        {error, not_found},
        hb_pam:resolve(#{ a => 1, private_key => 2 }, private_key)
    ).

key_from_device_test() ->
    ?assertEqual({ok, 1}, hb_pam:resolve(#{a => 1}, a)).

remove_test() ->
    Msg = #{ <<"Key1">> => <<"Value1">>, <<"Key2">> => <<"Value2">> },
    ?assertMatch({ok, #{ <<"Key2">> := <<"Value2">> }},
        hb_pam:resolve(Msg, #{ path => remove, item => <<"Key1">> })),
    ?assertMatch({ok, #{}},
        hb_pam:resolve(Msg, #{ path => remove, items => [<<"Key1">>, <<"Key2">>] })).
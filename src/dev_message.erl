%%% @doc The identity device: Simply return a key from the message as it is found
%%% in the message's underlying Erlang map. Private keys (`priv[.*]') are 
%%% not included.
-module(dev_message).
-export([info/0, keys/1, id/1, unsigned_id/1, signed_id/1, signers/1]).
-export([set/3, remove/2, get/2, get/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% The list of keys that are exported by this device.
-define(DEVICE_KEYS, [
    <<"path">>,
    <<"id">>,
    <<"unsigned_id">>,
    <<"signed_id">>,
    <<"signers">>,
    <<"keys">>,
    <<"get">>,
    <<"set">>,
    <<"remove">>
]).

%% @doc Return the info for the identity device.
info() ->
    #{
        default => fun get/3
        %exports => ?DEVICE_KEYS
    }.

%% @doc Return the ID of a message. If the message already has an ID, return
%% that. Otherwise, return the signed ID.
id(M) ->
    ID = 
        case get(<<"signature">>, M) of
            {error, not_found} -> raw_id(M, unsigned);
            {ok, ?DEFAULT_SIG} -> raw_id(M, unsigned);
            _ -> raw_id(M, signed)
        end,
    {ok, ID}.

%% @doc Wrap a call to the `hb_util:id/2' function, which returns the
%% unsigned ID of a message.
unsigned_id(M) ->
    {ok, raw_id(M, unsigned)}.

%% @doc Return the signed ID of a message.
signed_id(M) ->
    try
        {ok, raw_id(M, signed)}
    catch
        _:_ -> {error, not_signed}
    end.

%% @doc Encode an ID in any format to a normalized, b64u 43 character binary.
raw_id(Item) -> raw_id(Item, unsigned).
raw_id(TX, Type) when is_record(TX, tx) ->
    hb_util:encode(ar_bundles:id(TX, Type));
raw_id(Map, Type) when is_map(Map) ->
    case maps:get(<<"signature-device">>, Map, undefined) of
        <<"HTTP-Sig@1.0">> ->
            {ok, ID} = hb_http_signature:id(Map, Type, #{}),
            ID;
        _ ->
            TX = hb_message:convert(Map, tx, converge, #{}),
            hb_util:encode(ar_bundles:id(TX, Type))
    end;
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
set(Message1, NewValuesMsg, _Opts) ->
	% Filter keys that are in the default device (this one).
    {ok, NewValuesKeys} = keys(NewValuesMsg),
	KeysToSet =
		lists:filter(
			fun(Key) ->
				not lists:member(Key, ?DEVICE_KEYS) andalso
					(maps:get(Key, NewValuesMsg, undefined) =/= undefined)
			end,
			NewValuesKeys
		),
	% Find keys in the message that are already set (case-insensitive), and 
	% note them for removal.
	ConflictingKeys =
		lists:filter(
			fun(Key) ->
				lists:member(Key, KeysToSet)
			end,
			maps:keys(Message1)
		),
    UnsetKeys =
        lists:filter(
            fun(Key) ->
                case maps:get(Key, NewValuesMsg, not_found) of
                    unset -> true;
                    _ -> false
                end
            end,
            maps:keys(Message1)
        ),
	{
		ok,
		maps:merge(
			maps:without(ConflictingKeys ++ UnsetKeys, Message1),
			maps:from_list(
				lists:filtermap(
					fun(Key) ->
                        case maps:get(Key, NewValuesMsg, undefined) of
                            undefined -> false;
                            unset -> false;
                            Value -> {true, {Key, Value}}
                        end
					end,
					KeysToSet
				)
			)
		)
	}.

%% @doc Remove a key or keys from a message.
remove(Message1, #{ <<"item">> := Key }) ->
    remove(Message1, #{ <<"items">> => [Key] });
remove(Message1, #{ <<"items">> := Keys }) ->
    { ok, maps:without(Keys, Message1) }.

%% @doc Get the public keys of a message.
keys(Msg) when not is_map(Msg) ->
    case hb_converge:normalize_keys(Msg) of
        NormMsg when is_map(NormMsg) -> keys(NormMsg);
        _ -> throw(badarg)
    end;
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
get(Key, Msg) -> get(Key, Msg, #{ <<"path">> => <<"get">> }).
get(Key, Msg, _Msg2) ->
    case hb_private:is_private(Key) of
        true -> {error, not_found};
        false ->
            case maps:get(Key, Msg, not_found) of
                not_found -> case_insensitive_get(Key, Msg);
                Value -> {ok, Value}
            end
    end.

%% @doc Key matching should be case insensitive, following RFC-9110, so we 
%% implement a case-insensitive key lookup rather than delegating to
%% `maps:get/2'. Encode the key to a binary if it is not already.
case_insensitive_get(Key, Msg) ->
    NormKey = hb_converge:normalize_key(Key),
    NormMsg = hb_converge:normalize_keys(Msg),
    case maps:get(NormKey, NormMsg, not_found) of
        not_found -> {error, not_found};
        Value -> {ok, Value}
    end.

%%% Tests

%%% Internal module functionality tests:
get_keys_mod_test() ->
    ?assertEqual([a], maps:keys(#{a => 1})).

is_private_mod_test() ->
    ?assertEqual(true, hb_private:is_private(<<"private">>)),
    ?assertEqual(true, hb_private:is_private(<<"private.foo">>)),
    ?assertEqual(false, hb_private:is_private(<<"a">>)).

%%% Device functionality tests:

keys_from_device_test() ->
    ?assertEqual({ok, [<<"a">>]}, hb_converge:resolve(#{ <<"a">> => 1 }, keys, #{})).

case_insensitive_get_test() ->
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"a">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"A">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"a">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"A">> => 1 })).

private_keys_are_filtered_test() ->
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_converge:resolve(#{ <<"a">> => 1, <<"private">> => 2 }, keys, #{})
    ),
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_converge:resolve(#{ <<"a">> => 1, <<"priv_foo">> => 4 }, keys, #{})
    ).

cannot_get_private_keys_test() ->
    ?assertEqual(
        {error, not_found},
        hb_converge:resolve(#{ <<"a">> => 1, <<"private_key">> => 2 }, <<"private_key">>, #{})
    ).

key_from_device_test() ->
    ?assertEqual({ok, 1}, hb_converge:resolve(#{ <<"a">> => 1 }, <<"a">>, #{})).

remove_test() ->
	Msg = #{ <<"key1">> => <<"Value1">>, <<"key2">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"key2">> := <<"Value2">> }},
		hb_converge:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => <<"key1">> },
            #{ hashpath => ignore }
        )
    ),
	?assertMatch({ok, #{}},
		hb_converge:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"items">> => [<<"key1">>, <<"key2">>] },
            #{ hashpath => ignore }
        )
    ).

set_conflicting_keys_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"dangerous">> := <<"Value2">> }},
		hb_converge:resolve(Msg1, Msg2, #{})).

unset_with_set_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => unset },
	?assertMatch({ok, Msg3} when map_size(Msg3) == 0,
		hb_converge:resolve(Msg1, Msg2, #{ hashpath => ignore })).

set_ignore_undefined_test() ->
	Msg1 = #{ <<"test-key">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"test-key">> => undefined },
	?assertEqual({ok, #{ <<"test-key">> => <<"Value1">> }},
		set(Msg1, Msg2, #{ hashpath => ignore })).

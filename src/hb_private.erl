-module(hb_private).
-export([from_message/1, get/2, get/3, set/3, reset/1, is_private/1]).
-include_lib("eunit/include/eunit.hrl").

%%% @moduledoc This module provides basic helper utilities for managing the
%%% private element of a message, which can be used to store state that is
%%% not included in serialized messages, or those granted to users via the
%%% APIs. Private elements of a message can be useful for storing state that
%%% is only relevant temporarily. For example, a device might use the private
%%% element to store a cache of values that are expensive to recompute. They
%%% should _not_ be used for encoding state that makes the execution of a
%%% device non-deterministic (unless you are sure you know what you are doing).
%%% 
%%% See `hb_pam` for more information about the Permaweb Abstract Machine (PAM)
%%% and private elements of messages.

%% @doc Return the `private` key from a message. If the key does not exist, an
%% empty map is returned.
from_message(Msg) -> maps:get(private, Msg, #{}).

%% @doc Helper for getting a value from the private element of a message.
get(Msg, Key) -> 
    get(Msg, Key, undefined).
get(Msg, Key, Default) -> 
    maps:get(Key, from_message(Msg), Default).

%% @doc Helper function for setting a key in the private element of a message.
set(Msg, Key, Value) ->
    maps:put(
        private,
        maps:put(Key, Value, from_message(Msg)),
        Msg
    ).

%% @doc Check if a key is private.
is_private(Key) ->
    Str = key_to_list(Key),
    lists:prefix("priv", Str).

%% @doc Convert a key to a list.
key_to_list(Key) when is_atom(Key) ->
    atom_to_list(Key);
key_to_list(Key) when is_binary(Key) ->
    binary_to_list(Key);
key_to_list(Key) when is_list(Key) ->
    binary_to_list(iolist_to_binary(Key)).

%% @doc Unset all of the private keys in a message.
reset(Msg) ->
    maps:without(
        lists:filter(fun is_private/1, maps:keys(Msg)),
        Msg
    ).

%%% Tests

set_private_test() ->
    ?assertEqual(#{a => 1, private => #{b => 2}}, ?MODULE:set(#{a => 1}, b, 2)),
    Res = ?MODULE:set(#{a => 1}, a, 1),
    ?assertEqual(#{a => 1, private => #{a => 1}}, Res),
    ?assertEqual(#{a => 1, private => #{a => 1}}, ?MODULE:set(Res, a, 1)).

get_private_key_test() ->
    M1 = #{a => 1, private => #{b => 2}},
    ?assertEqual(undefined, ?MODULE:get(M1, a)),
    {ok, [a]} = hb_pam:resolve(M1, <<"Keys">>),
    ?assertEqual(2, ?MODULE:get(M1, b)),
    {Res, _} = hb_pam:resolve(M1, <<"Private">>),
    ?assertNotEqual(ok, Res),
    {Res, _} = hb_pam:resolve(M1, private).
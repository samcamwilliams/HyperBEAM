%%% @doc This module provides basic helper utilities for managing the
%%% private element of a message, which can be used to store state that is
%%% not included in serialized messages, or those granted to users via the
%%% APIs. Private elements of a message can be useful for storing state that
%%% is only relevant temporarily. For example, a device might use the private
%%% element to store a cache of values that are expensive to recompute. They
%%% should _not_ be used for encoding state that makes the execution of a
%%% device non-deterministic (unless you are sure you know what you are doing).
%%%
%%% The `set` and `get` functions of this module allow you to run those keys
%%% as converge paths if you would like to have private `devices` in the
%%% messages non-public zone.
%%% 
%%% See `hb_converge' for more information about the Converge Protocol
%%% and private elements of messages.

-module(hb_private).
-export([from_message/1, reset/1, is_private/1]).
-export([get/3, get/4, set/4, set/3, set_priv/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Return the `private' key from a message. If the key does not exist, an
%% empty map is returned.
from_message(Msg) when is_map(Msg) ->
    case maps:is_key(<<"priv">>, Msg) of
        true -> maps:get(<<"priv">>, Msg, #{});
        false -> maps:get(priv, Msg, #{})
    end;
from_message(_NonMapMessage) -> #{}.

%% @doc Helper for getting a value from the private element of a message. Uses
%% Converge resolve under-the-hood, removing the private specifier from the
%% path if it exists.
get(Key, Msg, Opts) ->
    get(Key, Msg, not_found, Opts).
get(InputPath, Msg, Default, Opts) ->
    Path = hb_path:term_to_path_parts(remove_private_specifier(InputPath)),
    ?event({get_private, {in, InputPath}, {out, Path}}),
    % Resolve the path against the private element of the message.
    Resolve =
        hb_converge:resolve(
            from_message(Msg),
            #{ <<"path">> => Path },
            priv_converge_opts(Opts)
        ),
    case Resolve of
        {error, _} -> Default;
        {ok, Value} -> Value
    end.

%% @doc Helper function for setting a key in the private element of a message.
set(Msg, InputPath, Value, Opts) ->
    Path = remove_private_specifier(InputPath),
    Priv = from_message(Msg),
    NewPriv = hb_converge:set(Priv, Path, Value, priv_converge_opts(Opts)),
    set(Msg, NewPriv, Opts).
set(Msg, PrivMap, Opts) ->
    CurrentPriv = from_message(Msg),
    NewPriv = hb_converge:set(CurrentPriv, PrivMap, priv_converge_opts(Opts)),
    set_priv(Msg, NewPriv).

%% @doc Helper function for setting the complete private element of a message.
set_priv(Msg, PrivMap) ->
    Msg#{ <<"priv">> => PrivMap }.

%% @doc Check if a key is private.
is_private(Key) ->
	case hb_converge:normalize_key(Key) of
		<<"priv", _/binary>> -> true;
		_ -> false
	end.

%% @doc Remove the first key from the path if it is a private specifier.
remove_private_specifier(InputPath) ->
    case is_private(hd(Path = hb_path:term_to_path_parts(InputPath))) of
        true -> tl(Path);
        false -> Path
    end.

%% @doc The opts map that should be used when resolving paths against the
%% private element of a message.
priv_converge_opts(Opts) ->
    Opts#{ hashpath => ignore, cache_control => [<<"no-cache">>, <<"no-store">>] }.

%% @doc Unset all of the private keys in a message.
reset(Msg) ->
    maps:without(
        lists:filter(fun is_private/1, maps:keys(Msg)),
        Msg
    ).

%%% Tests

set_private_test() ->
    ?assertEqual(#{<<"a">> => 1, <<"priv">> => #{<<"b">> => 2}}, set(#{<<"a">> => 1}, <<"b">>, 2, #{})),
    Res = set(#{<<"a">> => 1}, <<"a">>, 1, #{}),
    ?assertEqual(#{<<"a">> => 1, <<"priv">> => #{<<"a">> => 1}}, Res),
    ?assertEqual(#{<<"a">> => 1, <<"priv">> => #{<<"a">> => 1}}, set(Res, a, 1, #{})).

get_private_key_test() ->
    M1 = #{<<"a">> => 1, <<"priv">> => #{<<"b">> => 2}},
    ?assertEqual(not_found, get(<<"a">>, M1, #{})),
    {ok, [<<"a">>]} = hb_converge:resolve(M1, <<"keys">>, #{}),
    ?assertEqual(2, get(<<"b">>, M1, #{})),
    {error, _} = hb_converge:resolve(M1, <<"priv/a">>, #{}),
    {error, _} = hb_converge:resolve(M1, <<"priv">>, #{}).

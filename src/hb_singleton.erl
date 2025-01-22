%%% @doc A parser that translates Converge HTTP API requests in TABM format
%%% into an ordered list of messages to evaluate. The details of this format
%%% are described in `docs/converge-http-api.md`.
%%%
%%% Syntax overview:
%%% ```
%%%     Singleton: Message containing keys and a `path` field,
%%%                which may also contain a query string of key-value pairs.
%%%
%%%     Path:
%%%         - /Part1/Part2/.../PartN/ => [Part1, Part2, ..., PartN]
%%%         - /ID/Part2/.../PartN => [ID, Part2, ..., PartN]
%%%
%%%     Part: (Key | Resolution), Device?, #{ K => V}?
%%%         - Part => #{ path => Part }
%%%         - Part+Key=Value => #{ path => Part, Key => Value }
%%%         - Part+Key => #{ path => Part, Key => true }
%%%         - Part+k1=v1&k2=v2 => #{ path => Part, k1 => <<"v1">>, k2 => <<"v2">> }
%%%         - Part!Device => {as, Device, #{ path => Part }}
%%%         - Part!D+K1=V1 => {as, D, #{ path => Part, K1 => <<"v1">> }}
%%%         - pt+k1|int=1 => #{ path => pt, k1 => 1 }
%%%         - pt!d+k1|int=1 => {as, d, #{ path => pt, k1 => 1 }}
%%%         - (/nested/path) => Resolution of the path /nested/path
%%%         - (/nested/path+k1=v1) => (resolve /nested/path)#{k1 => v1}
%%%         - (/nested/path!D+K1=V1) => (resolve /nested/path)#{K1 => V1}
%%%         - pt+k1|res=(/a/b/c) => #{ path => pt, k1 => (resolve /a/b/c) }
%%%     Key:
%%%         - key: <<"value">> => #{ key => <<"value">>, ... } for all messages
%%%         - n.key: <<"value">> => #{ key => <<"value">>, ... } for Nth message
%%%         - key|Int: 1 => #{ key => 1, ... }
%%%         - key|Res: /nested/path => #{ key => (resolve /nested/path), ... }
%%%         - N.Key|Res=(/a/b/c) => #{ Key => (resolve /a/b/c), ... }
%%% '''
-module(hb_singleton).
-export([from/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(MAX_SEGMENT_LENGTH, 512).

%% @doc Normalize a singleton TABM message into a list of executable Converge
%% messages.
from(RawMsg) ->
    {ok, Path, Query} =
        parse_full_path(hb_path:to_binary(hb_path:from_message(request, RawMsg))),
    MsgWithoutBasePath = maps:merge(
        maps:remove(<<"path">>, RawMsg),
        Query
    ),
    % 2. Decode, split, and sanitize path segments. Each yields one step message.
    RawMsgs = lists:flatten(lists:map(fun path_messages/1, Path)),
    Msgs = normalize_base(RawMsgs),
    % 3. Type keys and values
    Typed = apply_types(MsgWithoutBasePath),
    % 4. Group keys by N-scope and global scope
    ScopedModifications = group_scoped(Typed, Msgs),
    % 5. Generate the list of messages (plus-notation, device, typed keys).
    build_messages(Msgs, ScopedModifications).

%% @doc Parse the relative reference into path, query, and fragment.
parse_full_path(RelativeRef) ->
    {Path, QKVList} =
        case binary:split(RelativeRef, <<"?">>) of
            [P, QStr] -> {P, cowboy_req:parse_qs(#{ qs => QStr })};
            [P] -> {P, []}
        end,
    {
        ok,
        lists:map(fun(Part) -> decode_string(Part) end, path_parts($/, Path)),
        maps:from_list(QKVList)
    }.

%% @doc Step 2: Decode + Split + Sanitize the path. Split by `/` but avoid
%% subpath components, such that their own path parts are not dissociated from
%% their parent path.
path_messages(RawBin) when is_binary(RawBin) ->
    lists:map(fun parse_part/1, path_parts([$/], decode_string(RawBin))).

%% @doc Normalize the base path.
normalize_base([]) -> [];
normalize_base([First|Rest]) when ?IS_ID(First) -> [First|Rest];
normalize_base(Rest) -> [#{}|Rest].

%% @doc Split the path into segments, filtering out empty segments and
%% segments that are too long.
path_parts(Sep, PathBin) when is_binary(PathBin) ->
    Res = lists:filtermap(
        fun(Part) ->
            case byte_size(Part) of
                0 -> false;
                TooLong when TooLong > ?MAX_SEGMENT_LENGTH ->
                    throw({error, segment_too_long, Part});
                _ -> {true, Part}
            end
        end,
        all_path_parts(Sep, PathBin)
    ),
    Res.

%% @doc Extract all of the parts from the binary, given (a list of) separators.
all_path_parts(_Sep, <<>>) -> [];
all_path_parts(Sep, Bin) ->
    {_MatchedSep, Part, Rest} = part(Sep, Bin),
    [Part | all_path_parts(Sep, Rest)].

%% @doc Extract the characters from the binary until a separator is found.
%% The first argument of the function is an explicit separator character, or
%% a list of separator characters. Returns a tuple with the separator, the
%% accumulated characters, and the rest of the binary.
part(Sep, Bin) when not is_list(Sep) ->
    part([Sep], Bin);
part(Seps, Bin) ->
    part(Seps, Bin, 0, <<>>).
part(_Seps, <<>>, _Depth, CurrAcc) -> {no_match, CurrAcc, <<>>};
part(Seps, << $\(, Rest/binary>>, Depth, CurrAcc) ->
    %% Increase depth
    part(Seps, Rest, Depth + 1, << CurrAcc/binary, "(" >>);
part(Seps, << $\), Rest/binary>>, Depth, CurrAcc) when Depth > 0 ->
    %% Decrease depth
    part(Seps, Rest, Depth - 1, << CurrAcc/binary, ")">>);
part(Seps, <<C:8/integer, Rest/binary>>, Depth, CurrAcc) ->
    case Depth == 0 andalso lists:member(C, Seps) of
        true -> {C, CurrAcc, Rest};
        false ->
            part(Seps, Rest, Depth, << CurrAcc/binary, C:8/integer >>)
    end.

%% @doc Step 3: Apply types to values and remove specifiers.
apply_types(Msg) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            {_, K, V} = maybe_typed(Key, Val),
            maps:put(K, V, Acc)
        end,
        #{},
        Msg
    ).

%% @doc Step 4: Group headers/query by N-scope.
%% `N.Key` => applies to Nth step. Otherwise => global
group_scoped(Map, Msgs) ->
    {NScope, Global} =
        maps:fold(
            fun(KeyBin, Val, {Ns, Gs}) ->
                case parse_scope(KeyBin) of
                    {OkN, RealKey} when OkN > 0 ->
                        Curr = maps:get(OkN, Ns, #{}),
                        Ns2 = maps:put(OkN, maps:put(RealKey, Val, Curr), Ns),
                        {Ns2, Gs};
                    global -> {Ns, maps:put(KeyBin, Val, Gs)}
                end
          end,
          {#{}, #{}},
          Map
        ),
    [
        maps:merge(Global, maps:get(N, NScope, #{}))
    ||
        N <- lists:seq(1, length(Msgs))
    ].

%% @doc Get the scope of a key. Adds 1 to account for the base message.
parse_scope(KeyBin) ->
    case binary:split(KeyBin, <<".">>, [global]) of
        [Front, Remainder] ->
            case catch erlang:binary_to_integer(Front) of
                NInt when is_integer(NInt) -> {NInt + 1, Remainder};
                _ -> throw({error, invalid_scope, KeyBin})
            end;
        _ -> global
    end.

%% @doc Step 5: Merge the base message with the scoped messages.
build_messages(Msgs, ScopedModifications) ->
    do_build(1, Msgs, ScopedModifications).

do_build(_, [], _ScopedKeys) -> [];
do_build(I, [Msg|Rest], ScopedKeys) when not is_map(Msg) ->
    [Msg | do_build(I+1, Rest, ScopedKeys)];
do_build(I, [Msg | Rest], ScopedKeys) ->
    ScopedKey = lists:nth(I, ScopedKeys),
    StepMsg = hb_message:convert(
        maps:merge(Msg, ScopedKey),
        converge,
        #{ topic => converge_internal }
    ),
    [StepMsg | do_build(I+1, Rest, ScopedKeys)].

%% @doc Parse a path part into a message or an ID.
%% Applies the syntax rules outlined in the module doc, in the following order:
%% 1. ID
%% 2. Part subpath resolutions
%% 3. Inlined key-value pairs
%% 4. Device specifier
parse_part(ID) when ?IS_ID(ID) -> ID;
parse_part(Part) ->
    case maybe_subpath(Part) of
        {resolve, Subpath} -> {resolve, Subpath};
        Part ->
            case part([$+, $&, $!, $|], Part) of
                {no_match, PartKey, <<>>} ->
                    #{ <<"path">> => PartKey };
                {Sep, PartKey, PartModBin} ->
                    parse_part_mods(
                        << Sep:8/integer, PartModBin/binary >>,
                        #{ <<"path">> => PartKey }
                    )
            end
    end.

%% @doc Parse part modifiers:
%% 1. `!Device` => {as, Device, Msg}
%% 2. `+K=V` => Msg#{ K => V }
parse_part_mods([], Msg) -> Msg;
parse_part_mods(<<>>, Msg) -> Msg;
parse_part_mods(<<"!", PartMods/binary>>, Msg) ->
    % Get the string until the end of the device specifier or end of string.
    [DeviceBin|Rest] = path_parts($+, PartMods),
    InlinedMsgBin = maybe_join(Rest, <<"+">>),
    % Calculate the inlined keys
    MsgWithInlines = parse_part_mods(InlinedMsgBin, Msg),
    % Apply the device specifier
    {as, maybe_subpath(DeviceBin), MsgWithInlines};
parse_part_mods(<< "+", InlinedMsgBin/binary >>, Msg) ->
    InlinedKeys = path_parts($&, InlinedMsgBin),
    MsgWithInlined =
        lists:foldl(
            fun(InlinedKey, Acc) ->
                {Key, Val} = parse_inlined_key_val(InlinedKey),
                maps:put(Key, Val, Acc)
            end,
            Msg,
            InlinedKeys
        ),
    MsgWithInlined.

%% @doc Extrapolate the inlined key-value pair from a path segment. If the
%% key has a value, it may provide a type (as with typical keys), but if a
%% value is not provided, it is assumed to be a boolean `true`.
parse_inlined_key_val(Bin) ->
    case part([$=, $&], Bin) of
        {no_match, K, <<>>} -> {K, true};
        {$=, K, V} ->
            {_, Key, Val} = maybe_typed(K, maybe_subpath(V)),
            {Key, Val}
    end.

%% @doc Attempt Cowboy URL decode, then sanitize the result.
decode_string(B) ->
    case catch http_uri:decode(B) of
        DecodedBin when is_binary(DecodedBin) -> DecodedBin;
        _ -> throw({error, cannot_decode, B})
    end.

%% @doc Check if the string is a subpath, returning it in parsed form,
%% or the original string with a specifier.
maybe_subpath(Str) when byte_size(Str) >= 2 ->
    case {binary:first(Str), binary:last(Str)} of
        {$(, $)} ->
            Inside = binary:part(Str, 1, byte_size(Str) - 2),
            {resolve, from(#{ <<"path">> => Inside })};
        _ -> Str
    end;
maybe_subpath(Other) -> Other.

%% @doc Parse a key's type (applying it to the value) and device name if present.
maybe_typed(Key, Value) ->
    case part($|, Key) of
        {no_match, OnlyKey, <<>>} -> {untyped, OnlyKey, Value};
        {$|, OnlyKey, Type} ->
            case {Type, Value} of
                {<<"resolve">>, Subpath} ->
                    % If the value needs to be resolved before it is converted,
                    % use the `Codec/1.0` device to resolve it.
                    % For example:
                    % /a/b+k|Int=(/x/y/z)` => /a/b+k=(/x/y/z/body+Type=Int|Codec)
                    {typed,
                        OnlyKey,
                        {resolve, from(#{ <<"path">> => Subpath })}
                    };
                {_T, Bin} when is_binary(Bin) ->
                    {typed, OnlyKey, hb_codec_converge:decode_value(Type, Bin)}
            end
    end.

%% @doc Join a list of items with a separator, or return the first item if there
%% is only one item. If there are no items, return an empty binary.
maybe_join(Items, Sep) ->
    case length(Items) of
        0 -> <<>>;
        1 -> hd(Items);
        _ -> iolist_to_binary(lists:join(Sep, Items))
    end.

%%% Tests

%%% Simple tests

single_message_test() ->
    Req = #{
        <<"path">> => <<"/a">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(2, length(Msgs)),
    ?assert(is_map(hd(Msgs))),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, hd(Msgs))).

basic_hashpath_test() ->
    Hashpath = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Path = <<"/", Hashpath/binary, "/some-other">>,
    Req = #{
        <<"path">> => Path,
        <<"method">> => <<"GET">>
    },
    Msgs = from(Req),
    ?assertEqual(2, length(Msgs)),
    [Base, Msg2] = Msgs,
    ?assertEqual(Base, Hashpath),
    ?assertEqual(<<"GET">>, maps:get(<<"method">>, Msg2)),
    ?assertEqual(<<"some-other">>, maps:get(<<"path">>, Msg2)).

multiple_messages_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_Base, Msg1, Msg2, Msg3] = Msgs,
    ?assert(lists:all(fun is_map/1, Msgs)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg1)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg2)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg3)).

%%% Advanced key syntax tests

scoped_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

typed_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key|integer">> => <<"123">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(123, maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

subpath_in_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key|resolve">> => <<"/x/y/z">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        maps:get(<<"test-key">>, Msg2, not_found)
    ),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

%%% Advanced path syntax tests

subpath_in_path_test() ->
    Req = #{
        <<"path">> => <<"/a/(x/y/z)/z">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"a">>, maps:get(<<"path">>, Msg1)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        Msg2
    ),
    ?assertEqual(<<"z">>, maps:get(<<"path">>, Msg3)).

inlined_keys_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b+k1=v1/c+k2=v2">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"v1">>, maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, maps:get(<<"k2">>, Msg3)),
    ?assertEqual(not_found, maps:get(<<"k1">>, Msg1, not_found)),
    ?assertEqual(not_found, maps:get(<<"k2">>, Msg2, not_found)).

multiple_inlined_keys_test() ->
    Path = <<"/a/b+k1=v1&k2=v2">>,
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => Path
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [_, Msg1, Msg2] = Msgs,
    ?assertEqual(not_found, maps:get(<<"k1">>, Msg1, not_found)),
    ?assertEqual(not_found, maps:get(<<"k2">>, Msg1, not_found)),
    ?assertEqual(<<"v1">>, maps:get(<<"k1">>, Msg2, not_found)),
    ?assertEqual(<<"v2">>, maps:get(<<"k2">>, Msg2, not_found)).

subpath_in_inlined_test() ->
    Path = <<"/part1/part2+test=1&b=(/x/y)/part3">>,
    Req = #{
        <<"path">> => Path
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, First, Second, Third] = Msgs,
    ?assertEqual(<<"part1">>, maps:get(<<"path">>, First)),
    ?assertEqual(<<"part3">>, maps:get(<<"path">>, Third)),
    ?assertEqual(
        {resolve, [#{}, #{ <<"path">> => <<"x">> }, #{ <<"path">> => <<"y">> }] },
        maps:get(<<"b">>, Second)
    ).

path_parts_test() ->
    ?assertEqual(
        [<<"a">>, <<"b+c=(/d/e)">>, <<"f">>],
        path_parts($/, <<"/a/b+c=(/d/e)/f">>)
    ),
    ?assertEqual([<<"a">>], path_parts($/, <<"/a">>)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], path_parts($/, <<"/a/b/c">>)),
    ?assertEqual(
        [
            <<"IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q">>,
            <<"msg2">>
        ],
        path_parts($/, <<"/IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q/msg2">>)
    ),
    ?assertEqual(
        [<<"a">>, <<"b+K1=V1">>, <<"c+K2=V2">>],
        path_parts($/, <<"/a/b+K1=V1/c+K2=V2">>)
    ),
    ?assertEqual(
        [<<"a">>, <<"(x/y/z)">>, <<"c">>],
        path_parts($/, <<"/a/(x/y/z)/c">>)
    ),
    ok.
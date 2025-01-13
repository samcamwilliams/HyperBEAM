%%% @doc A parser that translates Converge HTTP API requests in TABM format
%%% into an ordered list of messages to evaluate. The details of this format
%%% are described in `docs/converge-http-api.md`.
%%% 
%%% Syntax overview:
%%% ```
%%%     Singleton: Message containing keys and a `relative-reference` field,
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
%%%         - Part+K1=V1&K2=V2 => #{ path => Part, K1 => <<"V1">>, K2 => <<"V2">> }
%%%         - Part!Device => {as, Device, #{ path => Part }}
%%%         - Part!D+K1=V1 => {as, D, #{ path => Part, K1 => <<"V1">> }}
%%%         - Part+K1|Int=1 => #{ path => Part, K1 => 1 }
%%%         - Part!D+K1|Int=1 => {as, D, #{ path => Part, K1 => 1 }}
%%%         - (/nested/path) => Resolution of the path /nested/path
%%%         - (/nested/path+K1=V1) => (resolve /nested/path)#{`K1 => V1}
%%%         - (/nested/path!D+K1=V1) => (resolve /nested/path)#{K1 => V1}
%%%         - Pt+K1|Res=(/a/b/c) => #{ path => Pt, K1 => (resolve /a/b/c) }
%%%     Key:
%%%         - Key: <<"Value">> => #{ Key => <<"Value">>, ... } for all messages
%%%         - N.Key: <<"Value">> => #{ Key => <<"Value">>, ... } for Nth message
%%%         - Key|Int: 1 => #{ Key => 1, ... }
%%%         - Key|Res: /nested/path => #{ Key => (resolve /nested/path), ... }
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
    % 1. Extract relevant request pieces from the incoming TABM.
    {ok, Path, Query} = 
        parse_rel_ref(
            maps:get(<<"relative-reference">>, RawMsg, <<"/">>)
        ),
    MsgWithoutRef = maps:merge(
        maps:remove(<<"relative-reference">>, RawMsg),
        Query
    ),
    % 2. Decode, split, and sanitize path segments. Each yields one step message.
    Msgs = path_messages(Path),
    % 3. Determine if the first segment is a 43-char Hashpath.
    {BaseMsg, ReqMsgs} = extract_base_message(Msgs),
    % 4. Type keys and values
    Typed = apply_types(MsgWithoutRef),
    % 5. Group keys by N-scope and global scope
    NumSteps = max(1, length(Msgs)),
    Scoped = group_scoped(Typed, NumSteps),
    BaseMsgMods = maps:get(1, Scoped, #{}),
    if is_binary(BaseMsg) andalso (map_size(BaseMsgMods) > 0) ->
        throw(
            {error, cannot_modify_base_message_before_execution, BaseMsg}
        );
    true ->
        % 6. Generate the list of step messages (plus-notation, device, typed keys).
        build_messages(BaseMsg, ReqMsgs, Scoped)
    end.

%% @doc Parse the relative reference into path, query, and fragment.
parse_rel_ref(RelativeRef) ->
    {Path, QMap} =
        case binary:split(RelativeRef, <<"?">>) of
            [P, QStr] -> {P, cowboy_req:parse_qs(#{ qs => QStr })};
            [P] -> {P, #{}}
        end,
    {
        lists:map(fun(Part) -> decode_string(Part) end, path_parts($/, Path)),
        QMap
    }.

%% @doc Step 2: Decode + Split + Sanitize the path. Split by `/` but avoid
%% subpath components, such that their own path parts are not dissociated from 
%% their parent path.
path_messages(RawBin) when is_binary(RawBin) ->
    Parts =
        path_parts(
            $/,
            case catch cow_uri:urldecode(RawBin) of
                DecodedBin when is_binary(DecodedBin) -> DecodedBin;
                _ -> throw({error, cannot_decode_path, RawBin})
            end
        ),
    lists:map(fun parse_part/1, Parts).

%% @doc Parse the path into segments.
path_parts(Sep, PathBin) when is_binary(PathBin) ->
    path_parts(Sep, PathBin, 0, <<>>, []).
%%   - Chars: remaining characters to parse
%%   - Depth: current parentheses depth
%%   - CurrAcc: reverse-accumulation of the current segment
%%   - SegsAcc: reversed list of segments we’ve formed so far
path_parts(_Sep, <<>>, _Depth, FinalPart, SegsAcc) ->
    %% End of input. Add final segment (if any) and reverse the list
    lists:filtermap(
        fun(Part) ->
            case byte_size(Part) of
                0 -> false;
                TooLong when TooLong > ?MAX_SEGMENT_LENGTH ->
                    throw({error, segment_too_long, Part});
                _ -> {true, Part}
            end
        end,
        lists:reverse([FinalPart | SegsAcc])
    );
path_parts(Sep, << Sep, Rest/binary>>, 0, CurrAcc, SegsAcc) ->
    %% We hit the separator at top level => new segment boundary
    path_parts(Sep, Rest, 0, [], [CurrAcc | SegsAcc]);
path_parts(Sep, << $\(, Rest/binary>>, Depth, CurrAcc, SegsAcc) ->
    %% Increase depth
    path_parts(Sep, Rest, Depth + 1, << SegsAcc/binary, $\( >>, CurrAcc);
path_parts(Sep, << $\), Rest/binary>>, Depth, CurrAcc, SegsAcc) when Depth > 0 ->
    %% Decrease depth
    path_parts(Sep, Rest, Depth - 1, << SegsAcc/binary, $\) >>, CurrAcc);
path_parts(Sep, <<C:1/binary, Rest/binary>>, Depth, CurrAcc, SegsAcc) ->
    %% Normal character: keep accumulating
    path_parts(Sep, Rest, Depth, << SegsAcc/binary, C:1/binary>>, CurrAcc).

%% @doc Step 3: Determine if the first segment is a 43-char base64URL 
%% and either sets up a base message with hashpath or treats the first 
%% segment as "base_segment".
extract_base_message([]) ->
    {#{}, []};
extract_base_message([S|Rest]) ->
    case ?IS_ID(S) of
        true -> {S, Rest};
        false -> {#{}, Rest}
    end.

%% @doc Step 4: Apply types to values and remove specifiers.
apply_types(Msg) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            {_, Key, Val} = maybe_typed(Key, Val),
            maps:put(Key, Val, Acc)
        end,
        #{},
        Msg
    ).

%% @doc Step 5: Group headers/query by N-scope. 
%% `N.Key` => applies to Nth step. Otherwise => global
group_scoped(Map0, StepsCount) ->
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
          Map0
        ),
    [
        maps:merge(Global, maps:get(N, NScope, #{})) 
    ||
        N <- lists:seq(1, StepsCount)
    ].

%% @doc Get the scope of a key.
parse_scope(KeyBin) ->
    case binary:split(KeyBin, <<".">>, [global]) of
        [Front, Remainder] ->
            case catch erlang:binary_to_integer(Front) of
                NInt when is_integer(NInt) -> {NInt, Remainder};
                _ -> throw({error, invalid_scope, KeyBin})
            end;
        _ -> global
    end.

%% @doc Step 6: Merge the base message with the scoped messages.
build_messages(_Base, [], _ScopedKeys) -> [];
build_messages(Base, Segs, ScopedKeys) ->
    do_build(1, Base, Segs, ScopedKeys, []).

do_build(_I, _Global, [], _ScopedKeys, Acc) ->
    lists:reverse(Acc);
do_build(I, Global, [Msg|Rest], ScopedKeys, Acc0) ->
    StepMsg0 = maps:merge(Global, Msg),
    HdrMap = lists:nth(I, ScopedKeys),
    StepMsg = maps:merge(StepMsg0, HdrMap),
    do_build(I+1, StepMsg, Rest, ScopedKeys, [StepMsg | Acc0]).

%% @doc Parse a path part into a message or an ID.
%% Applies the syntax rules outlined in the module doc, in the following order:
%% 1. ID
%% 2. Part subpath resolutions
%% 3. Inlined key-value pairs
%% 4. Device specifier
parse_part(ID) when ?IS_ID(ID) -> ID;
parse_part(Part) ->
    case subpath(Part) of
        {subpath, Subpath} ->
            Subpath;
        {ok, Part} ->
            [PartKey|PartModBin] =
                binary:split(Part, [<<"+">>, <<"&">>, <<"!">>, <<"|">>]),
            parse_part_mods(PartModBin, #{ path => PartKey })
    end.

%% @doc Parse part modifiers: 
%% 1. `!Device` => {as, Device, Msg}
%% 2. `+K=V` => Msg#{ K => V }
parse_part_mods(<<>>, Msg) -> Msg;
parse_part_mods(<<"!", PartMods/binary>>, Msg) ->
    % Get the string until the end of the device specifier or end of string.
    [DeviceBin, Rest] = path_parts($+, PartMods),
    InlinedMsgBin = iolist_to_binary(lists:join(Rest, <<"+">>)),
    % Calculate the inlined keys
    MsgWithInlines = parse_part_mods(InlinedMsgBin, Msg),
    % Apply the device specifier
    case subpath(DeviceBin) of
        {subpath, Path} -> {as, {resolve, Path}, MsgWithInlines};
        {ok, Device} -> {as, Device, MsgWithInlines}
    end;
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
    case path_parts($=, Bin) of
        [K, V] ->
            {_, V2} = subpath(V),
            {_, Key, Val} = maybe_typed(K, V2),
            {Key, Val};
        [K] -> {K, true}
    end.

%% @doc Attempt Cowboy URL decode, then sanitize the result.
decode_string(B) ->
    case catch cow_uri:urldecode(B) of
        DecodedBin when is_binary(DecodedBin) -> DecodedBin;
        _ -> throw({error, cannot_decode, B})
    end.

%% @doc Check if the string is a subpath, returning it in parsed form,
%% or the original string with a specifier.
subpath(Str) when length(Str) >= 3 ->
    First = binary:first(Str),
    Last  = binary:last(Str),
    if
        First =:= $( andalso Last =:= $) ->
            Inside = binary:part(Str, 2, length(Str) - 2),
            {subpath, from(#{ <<"relative-reference">> => Inside })};
        true ->
            {ok, Str}
    end;
subpath(Str) ->
    {ok, Str}.

%% @doc Parse a key's type (applying it to the value) and device name if present.
maybe_typed(Key, Value) ->
    case binary:split(Key, <<"|">>, [global]) of
        [OnlyKey, T] ->
            case Value of
                Bin when is_binary(Bin) ->
                    {typed, OnlyKey, hb_codec_converge:decode_value(T, Bin)};
                {resolve, #{ path := Submessage }} ->
                    % If the value needs to be resolved before it is converted,
                    % use the `Codec/1.0` device to resolve it.
                    % For example:
                    %   `/a/b+k|Int=(/x/y/z)` => /a/b+k=(/x/y/z/body+Type=Int|Codec)
                    {typed,
                        OnlyKey,
                        {resolve,
                            #{
                                path =>
                                    Submessage ++
                                        [
                                            #{
                                                path => <<"body">>,
                                                type => T,
                                                device => <<"Codec/1.0">>
                                            }
                                        ]
                            }
                        }
                    }
            end;
        _ ->
            {untyped, Key, Value}
    end.

%%% Tests

%%% Simple tests
single_message_test() ->
    Req = #{
        <<"relative-reference">> => <<"/a">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(1, length(Msgs)),
    ?assert(is_map(hd(Msgs))),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, hd(Msgs))).

basic_hashpath_test() ->
    Hashpath = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Path = <<"/", Hashpath/binary, "/someOther">>,
    Req = #{
        <<"relative-reference">> => Path,
        <<"method">> => <<"GET">>
    },
    Msgs = from(Req),
    ?assertEqual(2, length(Msgs)),
    [Base, Msg2] = Msgs,
    ?assertEqual(Base, Hashpath),
    ?assertEqual(<<"GET">>, maps:get(<<"method">>, Msg2)),
    ?assertEqual(<<"someOther">>, maps:get(<<"path">>, Msg2)).

multiple_messages_test() ->
    Req = #{
        <<"relative-reference">> => <<"/a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [Base, Msg2, Msg3] = Msgs,
    ?assert(lists:all(fun is_map/1, Msgs)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Base)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg2)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg3)).

%%% Advanced key syntax tests
scoped_key_test() ->
    Req = #{
        <<"relative-reference">> => <<"/a/b/c">>,
        <<"2.test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

typed_key_test() ->
    Req = #{
        <<"relative-reference">> => <<"/a/b/c">>,
        <<"2.test-key|Integer">> => <<"123">>
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(123, maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

subpath_in_key_test() ->
    Req = #{
        <<"relative-reference">> => <<"/a/b/c">>,
        <<"2.test-key|Resolve">> => <<"/x/y/z">>
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(
        #{
            path =>
                {resolve,
                    [
                        #{ path => <<"x">> },
                        #{ path => <<"y">> },
                        #{ path => <<"z">> }
                    ]
                }
        },
        maps:get(<<"test-key">>, Msg2, not_found)
    ),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

%%% Advanced path syntax
subpath_in_path_test() ->
    Req = #{
        <<"path">> => <<"/a/(x/y/z)/z">>
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"a">>, maps:get(path, Msg1)),
    Msg2Path = maps:get(path, Msg2),
    ?assertEqual(
        [
            #{ path => <<"x">> },
            #{ path => <<"y">> },
            #{ path => <<"z">> }
        ],
        Msg2Path
    ),
    ?assertEqual(<<"z">>, maps:get(path, Msg3)).

inlined_keys_test() ->
    Path = <<"/a/b+K1=V1/c+K2=V2">>,
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => Path
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"V1">>, maps:get(<<"K1">>, Msg2)),
    ?assertEqual(<<"V2">>, maps:get(<<"K2">>, Msg3)),
    ?assertEqual(not_found, maps:get(<<"K1">>, Msg1, not_found)),
    ?assertEqual(not_found, maps:get(<<"K2">>, Msg2, not_found)).


multiple_inlined_keys_test() ->
    Path = <<"/a/b+K1=V1&K2=V2">>,
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => Path
    },
    Msgs = from(Req),
    ?assertEqual(2, length(Msgs)),
    [Msg1, Msg2] = Msgs,
    ?assertEqual(not_found, maps:get(<<"K1">>, Msg1, not_found)),
    ?assertEqual(not_found, maps:get(<<"K2">>, Msg1, not_found)),
    ?assertEqual(<<"V1">>, maps:get(<<"K1">>, Msg2, not_found)),
    ?assertEqual(<<"V2">>, maps:get(<<"K2">>, Msg2, not_found)).

%% 4) Inlined dictionary with subpath in parentheses
subpath_in_inlined_test() ->
    Path = <<"/A+B=(/x/y)/C">>,
    Req = #{
        <<"path">> => Path
    },
    Msgs = from(Req),
    ?assertEqual(2, length(Msgs)),
    [First, Second] = Msgs,
    ?assertEqual(<<"A">>, maps:get(path, First)),
    ?assertEqual(<<"C">>, maps:get(path, Second)),
    ?assertEqual(
        #{ path => [#{ path => <<"x">> }, #{ path => <<"y">> }] },
        maps:get(<<"B">>, First)
    ).
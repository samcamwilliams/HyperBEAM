-module(hb_util).
-export([id/1, id/2, native_id/1, human_id/1]).
-export([encode/1, decode/1, safe_encode/1, safe_decode/1]).
-export([find_value/2, find_value/3]).
-export([number/1, list_to_numbered_map/1, message_to_numbered_list/1]).
-export([hd/1, hd/2, hd/3]).
-export([remove_common/2, to_lower/1]).
-export([maybe_throw/2]).
-export([format_indented/2, format_indented/3, format_binary/1]).
-export([format_map/1, format_map/2]).
-export([debug_print/4, debug_fmt/1]).
-export([print_trace/4, trace_macro_helper/4]).
-include("include/hb.hrl").

%%% @moduledoc A collection of utility functions for building with HyperBEAM.

%% @doc Return the human-readable form of an ID of a message when given either
%% a message explicitly, raw encoded ID, or an Erlang Arweave `tx` record.
id(Item) -> id(Item, unsigned).
id(TX, Type) when is_record(TX, tx) ->
    encode(ar_bundles:id(TX, Type));
id(Map, Type) when is_map(Map) ->
    case Type of
        unsigned -> hb_pam:get(unsigned_id, Map);
        signed -> encode(hb_pam:get(id, Map))
    end;
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    Bin;
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    encode(Bin);
id(Data, Type) when is_list(Data) ->
    id(list_to_binary(Data), Type).

%% @doc Convert a string to a lowercase.
to_lower(Str) when is_list(Str) ->
    string:to_lower(Str);
to_lower(Bin) when is_binary(Bin) ->
    list_to_binary(to_lower(binary_to_list(Bin))).

%% @doc Convert a human readable ID to a native binary ID. If the ID is already
%% a native binary ID, it is returned as is.
native_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    decode(Bin);
native_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    Bin.

%% @doc Convert a native binary ID to a human readable ID. If the ID is already
%% a human readable ID, it is returned as is.
human_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    encode(Bin);
human_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    Bin.

%% @doc Encode a binary to URL safe base64 binary string.
encode(Bin) ->
  b64fast:encode(Bin).

%% @doc Try to decode a URL safe base64 into a binary or throw an error when
%% invalid.
decode(Input) ->
  b64fast:decode(Input).

%% @doc Safely encode a binary to URL safe base64.
safe_encode(Bin) when is_binary(Bin) ->
  encode(Bin);
safe_encode(Bin) ->
  Bin.

%% @doc Safely decode a URL safe base64 into a binary returning an ok or error
%% tuple.
safe_decode(E) ->
  try
    D = decode(E),
    {ok, D}
  catch
    _:_ ->
    {error, invalid}
  end.

%% @doc Label a list of elements with a number.
number(List) ->
    lists:map(
        fun({N, Item}) -> {integer_to_binary(N), Item} end,
        lists:zip(lists:seq(1, length(List)), List)
    ).

%% @doc Convert a list of elements to a map with numbered keys.
list_to_numbered_map(List) ->
  maps:from_list(number(List)).

%% @doc Take a message with numbered keys and convert it to a list of tuples
%% with the associated key as an integer and a value. Optionally, it takes a
%% standard map of HyperBEAM runtime options.
message_to_numbered_list(Message) ->
    message_to_numbered_list(Message, #{}).
message_to_numbered_list(Message, Opts) ->
    {ok, Keys} = hb_pam:keys(Message, Opts),
    KeyValList =
        lists:filtermap(
            fun(Key) ->
                case string:to_integer(Key) of
                    {Int, ""} ->
                        {
                            true,
                            {Int, hb_pam:get(Key, Message, Opts)}
                        };
                    _ -> false
                end
            end,
            Keys
        ),
    lists:sort(KeyValList).

%% @doc Convert a map of numbered elements to a list. We stop at the first
%% integer key that is not associated with a value.

%% @doc Get the first element (the lowest integer key >= 1) of a numbered map.
%% Optionally, it takes a specifier of whether to return the key or the value,
%% as well as a standard map of HyperBEAM runtime options.
%% 
%% If `error_strategy` is `throw`, raise an exception if no integer keys are
%% found. If `error_strategy` is `any`, return `undefined` if no integer keys
%% are found. By default, the function does not pass a `throw` execution
%% strategy to `hb_pam:to_key/2`, such that non-integer keys present in the
%% message will not lead to an exception.
hd(Message) -> hd(Message, value).
hd(Message, ReturnType) ->
    hd(Message, ReturnType, #{ error_strategy => throw }).
hd(Message, ReturnType, Opts) -> 
    {ok, Keys} = hb_pam:resolve(Message, keys),
    hd(Message, Keys, 1, ReturnType, Opts).
hd(_Map, [], _Index, _ReturnType, #{ error_strategy := throw }) ->
    throw(no_integer_keys);
hd(_Map, [], _Index, _ReturnType, _Opts) -> undefined;
hd(Message, [Key|Rest], Index, ReturnType, Opts) ->
    case hb_pam:to_key(Key, Opts#{ error_strategy => return }) of
        undefined ->
            hd(Message, Rest, Index + 1, ReturnType, Opts);
        Key ->
            case ReturnType of
                key -> Key;
                value -> hb_pam:resolve(Message, Key)
            end
    end.

%% @doc Find the value associated with a key in parsed a JSON structure list.
find_value(Key, List) ->
  hb_util:find_value(Key, List, undefined).

find_value(Key, Map, Default) when is_map(Map) ->
  case maps:find(Key, Map) of
    {ok, Value} ->
    Value;
    error ->
    Default
  end;
find_value(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Val} ->
    Val;
    false ->
    Default
  end.

%% @doc Remove the common prefix from two strings, returning the remainder of the
%% first string. This function also coerces lists to binaries where appropriate,
%% returning the type of the first argument.
remove_common(MainStr, SubStr) when is_binary(MainStr) and is_list(SubStr) ->
    remove_common(MainStr, list_to_binary(SubStr));
remove_common(MainStr, SubStr) when is_list(MainStr) and is_binary(SubStr) ->
    binary_to_list(remove_common(list_to_binary(MainStr), SubStr));
remove_common(<< X:8, Rest1/binary>>, << X:8, Rest2/binary>>) ->
    remove_common(Rest1, Rest2);
remove_common([X|Rest1], [X|Rest2]) ->
    remove_common(Rest1, Rest2);
remove_common([$/|Path], _) -> Path;
remove_common(Rest, _) -> Rest.

%% @doc Throw an exception if the Opts map has an `error_strategy` key with the
%% value `throw`. Otherwise, return the value.
maybe_throw(Val, Opts) ->
    case hb_pam:get(error_strategy, Opts) of
        throw -> throw(Val);
        _ -> Val
    end.

%% @doc Print a message to the standard error stream, prefixed by the amount
%% of time that has elapsed since the last call to this function.
debug_print(X, Mod, Func, LineNum) ->
    Now = erlang:system_time(millisecond),
    Last = erlang:put(last_debug_print, Now),
    TSDiff = case Last of undefined -> 0; _ -> Now - Last end,
    io:format(standard_error, "=== HB DEBUG ===[~pms in ~p @ ~s:~w ~p]==> ~s~n",
        [
            TSDiff, self(), Mod, LineNum, Func,
            lists:flatten(debug_fmt(X, 0))
        ]),
    X.

%% @doc Convert a term to a string for debugging print purposes.
debug_fmt(X) -> debug_fmt(X, 0).
debug_fmt({explicit, X}, Indent) ->
    format_indented("~p", [X], Indent);
debug_fmt({X, Y}, Indent) when is_atom(X) and is_atom(Y) ->
    format_indented("~p: ~p", [X, Y], Indent);
debug_fmt({X, Y}, Indent) when is_record(Y, tx) ->
    format_indented("~p: [TX item]~n~s",
        [X, ar_bundles:format(Y, Indent + 1)],
        Indent
    );
debug_fmt({X, Y}, Indent) when is_map(Y) ->
    Formatted = hb_util:format_map(Y, Indent + 1),
    HasNewline = lists:member($\n, Formatted),
    format_indented("~p~s",
        [
            X,
            case HasNewline of
                true -> " ==>" ++ Formatted;
                false -> ": " ++ Formatted
            end
        ],
        Indent
    );
debug_fmt({X, Y}, Indent) ->
    format_indented("~s: ~s", [debug_fmt(X, Indent), debug_fmt(Y, Indent)], Indent);
debug_fmt(Map, Indent) when is_map(Map) ->
    hb_util:format_map(Map, Indent);
debug_fmt(Tuple, Indent) when is_tuple(Tuple) ->
    format_tuple(Tuple, Indent);
debug_fmt(Str = [X | _], Indent) when is_integer(X) andalso X >= 32 andalso X < 127 ->
    format_indented("~s", [Str], Indent);
debug_fmt(X, Indent) ->
    format_indented("~120p", [X], Indent).

%% @doc Helper function to format tuples with arity greater than 2.
format_tuple(Tuple, Indent) ->
    to_lines(lists:map(
        fun(Elem) ->
            debug_fmt(Elem, Indent)
        end,
        tuple_to_list(Tuple)
    )).

to_lines([]) -> [];
to_lines(In =[RawElem | Rest]) ->
    Elem = lists:flatten(RawElem),
    case lists:member($\n, Elem) of
        true -> lists:flatten(lists:join("\n", In));
        false -> Elem ++ ", " ++ to_lines(Rest)
    end.

%% @doc Format a string with an indentation level.
format_indented(Str, Indent) -> format_indented(Str, "", Indent).
format_indented(RawStr, Fmt, Ind) ->
    IndentSpaces = hb_opts:get(debug_print_indent),
    lists:droplast(
        lists:flatten(
            io_lib:format(
                [$\s || _ <- lists:seq(1, Ind * IndentSpaces)] ++
                    lists:flatten(RawStr) ++ "\n",
                Fmt
            )
        )
    ).

%% @doc Format a binary as a short string suitable for printing.
format_binary(Bin) ->
    MaxBinPrint = hb_opts:get(debug_print_binary_max),
    Printable =
        binary:part(
            Bin,
            0,
            case byte_size(Bin) of
                X when X < MaxBinPrint -> X;
                _ -> MaxBinPrint
            end
        ),
    PrintSegment = lists:flatten(io_lib:format("~p", [Printable])),
    lists:flatten(
        io_lib:format(
            "~s~s <~p bytes>",
            [
                PrintSegment,
                case Bin == Printable of
                    true -> "";
                    false -> "..."
                end,
                byte_size(Bin)
            ]
        )
    ).

%% @doc Format a map as either a single line or a multi-line string depending
%% on the value of the `debug_print_map_line_threshold` runtime option.
format_map(Map) -> format_map(Map, 0).
format_map(Map, Indent) ->
    MaxLen = hb_opts:get(debug_print_map_line_threshold),
    SimpleFmt = io_lib:format("~p", [Map]),
    case lists:flatlength(SimpleFmt) of
        Len when Len > MaxLen ->
            "\n" ++ lists:flatten(hb_message:format(Map, Indent));
        _ -> SimpleFmt
    end.

%% @doc Print the trace of the current stack, up to the first non-hyperbeam
%% module. Prints each stack frame on a new line, until it finds a frame that
%% does not start with a prefix in the `stack_print_prefixes` hb_opts.
%% Optionally, you may call this function with a custom label and caller info,
%% which will be used instead of the default.
print_trace(Stack, CallMod, CallFunc, CallLine) ->
    print_trace(Stack, "HB TRACE",
        lists:flatten(io_lib:format("[~s:~w ~p]",
            [CallMod, CallLine, CallFunc])
    )).

print_trace(Stack, Label, CallerInfo) ->
    io:format(standard_error, "=== ~s ===~s==>~n~s",
        [
            Label, CallerInfo,
            lists:flatten(
                format_trace(
                    Stack,
                    hb_opts:get(stack_print_prefixes, [], #{})
                )
            )
        ]).

%% @doc Format a stack trace as a list of strings, one for each stack frame.
%% Each stack frame is formatted if it matches the `stack_print_prefixes`
%% option. At the first frame that does not match a prefix in the
%% `stack_print_prefixes` option, the rest of the stack is not formatted.
format_trace([], _) -> [];
format_trace([Item|Rest], Prefixes) ->
    case element(1, Item) of
        Atom when is_atom(Atom) ->
            case string:tokens(atom_to_list(Atom), "_") of
                [Prefix, _] ->
                    case lists:member(
                        Prefix,
                        Prefixes
                    ) of
                        true ->
                            [
                                format_trace(Item, Prefixes) |
                                format_trace(Rest, Prefixes)
                            ];
                        false -> []
                    end;
                _ -> []
            end;
        _ -> []
    end;
format_trace({Func, ArityOrTerm, Extras}, Prefixes) ->
    format_trace({no_module, Func, ArityOrTerm, Extras}, Prefixes);
format_trace({Mod, Func, ArityOrTerm, Extras}, _Prefixes) ->
    ExtraMap = maps:from_list(Extras),
    format_indented(
        "~p:~p/~p [~s]~n",
        [
            Mod, Func, ArityOrTerm,
            case maps:get(line, ExtraMap, undefined) of
                undefined -> "No details";
                Line ->
                    maps:get(file, ExtraMap)
                        ++ ":" ++ integer_to_list(Line)
            end
        ],
        1
    ).

%% @doc Utility function to help macro `?trace/0` remove the first frame of the
%% stack trace.
trace_macro_helper({_, {_, [_IgnoredFrame|Stack]}}, Mod, Func, Line) ->
    print_trace(Stack, Mod, Func, Line).
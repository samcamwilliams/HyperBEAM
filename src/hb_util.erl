%% @doc A collection of utility functions for building with HyperBEAM.
-module(hb_util).
-export([id/1, id/2, native_id/1, human_id/1, short_id/1, human_int/1, to_hex/1]).
-export([encode/1, decode/1, safe_encode/1, safe_decode/1]).
-export([find_value/2, find_value/3]).
-export([number/1, list_to_numbered_map/1, message_to_numbered_list/1]).
-export([is_string_list/1]).
-export([hd/1, hd/2, hd/3]).
-export([remove_common/2, to_lower/1]).
-export([maybe_throw/2]).
-export([format_indented/2, format_indented/3, format_binary/1]).
-export([format_map/1, format_map/2, remove_trailing_noise/2]).
-export([debug_print/4, debug_fmt/1, eunit_print/2]).
-export([print_trace/4, trace_macro_helper/5, print_trace_short/4]).
-export([ok/1, ok/2]).
-export([format_trace_short/1]).
-export([count/2, mean/1, stddev/1, variance/1]).
-include("include/hb.hrl").

%% @doc Unwrap a tuple of the form `{ok, Value}', or throw/return, depending on
%% the value of the `error_strategy' option.
ok(Value) -> ok(Value, #{}).
ok({ok, Value}, _Opts) -> Value;
ok(Other, Opts) ->
	case hb_opts:get(error_strategy, throw, Opts) of
		throw -> throw({unexpected, Other});
		_ -> {unexpected, Other}
	end.

%% @doc Return the human-readable form of an ID of a message when given either
%% a message explicitly, raw encoded ID, or an Erlang Arweave `tx' record.
id(Item) -> id(Item, unsigned).
id(TX, Type) when is_record(TX, tx) ->
    encode(ar_bundles:id(TX, Type));
id(Map, Type) when is_map(Map) ->
    hb_message:id(Map, Type);
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    Bin;
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    encode(Bin);
id(Data, Type) when is_list(Data) ->
    id(list_to_binary(Data), Type).

%% @doc Convert a binary to a lowercase.
to_lower(Str) ->
    string:lowercase(Str).

%% @doc Is the given term a string list?
is_string_list(MaybeString) ->
    lists:all(fun is_integer/1, MaybeString).

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

%% @doc Return a short ID for the different types of IDs used in Converge.
short_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    short_id(human_id(Bin));
short_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    << FirstTag:5/binary, _:33/binary, LastTag:5/binary >> = Bin,
    << FirstTag/binary, "..", LastTag/binary >>;
short_id(Bin) when byte_size(Bin) > 43 andalso byte_size(Bin) < 100 ->
    case binary:split(Bin, <<"/">>, [trim_all, global]) of
        [First, Second] when byte_size(Second) == 43 ->
            FirstEnc = short_id(First),
            SecondEnc = short_id(Second),
            << FirstEnc/binary, "/", SecondEnc/binary >>;
        [First, Key] ->
            FirstEnc = short_id(First),
            << FirstEnc/binary, "/", Key/binary >>;
        _ ->
            Bin
    end;
short_id(<< "/", SingleElemHashpath/binary >>) ->
    Enc = short_id(SingleElemHashpath),
    if is_binary(Enc) -> << "/", Enc/binary >>;
    true -> undefined
    end;
short_id(Key) when byte_size(Key) < 43 -> Key;
short_id(_) -> undefined.

%% @doc Determine whether a binary is human-readable.
is_human_binary(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin) of
        {error, _, _} -> false;
        _ -> true
    end;
is_human_binary(_) -> false.

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

%% @doc Convert a binary to a hex string. Do not use this for anything other than
%% generating a lower-case, non-special character id. It should not become part of
%% the core protocol. We use b64u for efficient encoding.
to_hex(Bin) when is_binary(Bin) ->
    to_lower(
        iolist_to_binary(
            [io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]
        )
    ).

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
    {ok, Keys} = hb_converge:keys(Message, Opts),
    KeyValList =
        lists:filtermap(
            fun(Key) ->
                case string:to_integer(Key) of
                    {Int, ""} ->
                        {
                            true,
                            {Int, hb_converge:get(Key, Message, Opts)}
                        };
                    _ -> false
                end
            end,
            Keys
        ),
    lists:sort(KeyValList).

%% @doc Get the first element (the lowest integer key >= 1) of a numbered map.
%% Optionally, it takes a specifier of whether to return the key or the value,
%% as well as a standard map of HyperBEAM runtime options.
hd(Message) -> hd(Message, value).
hd(Message, ReturnType) ->
    hd(Message, ReturnType, #{ error_strategy => throw }).
hd(Message, ReturnType, Opts) -> 
    hd(Message, hb_converge:keys(Message, Opts), 1, ReturnType, Opts).
hd(_Map, [], _Index, _ReturnType, #{ error_strategy := throw }) ->
    throw(no_integer_keys);
hd(_Map, [], _Index, _ReturnType, _Opts) -> undefined;
hd(Message, [Key|Rest], Index, ReturnType, Opts) ->
    case hb_converge:normalize_key(Key, Opts#{ error_strategy => return }) of
        undefined ->
            hd(Message, Rest, Index + 1, ReturnType, Opts);
        Key ->
            case ReturnType of
                key -> Key;
                value -> hb_converge:resolve(Message, Key, #{})
            end
    end.

%% @doc Find the value associated with a key in parsed a JSON structure list.
find_value(Key, List) ->
    find_value(Key, List, undefined).

find_value(Key, Map, Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end;
find_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Val} -> Val;
        false -> Default
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

%% @doc Throw an exception if the Opts map has an `error_strategy' key with the
%% value `throw'. Otherwise, return the value.
maybe_throw(Val, Opts) ->
    case hb_converge:get(error_strategy, Opts) of
        throw -> throw(Val);
        _ -> Val
    end.

%% @doc Print a message to the standard error stream, prefixed by the amount
%% of time that has elapsed since the last call to this function.
debug_print(X, Mod, Func, LineNum) ->
    Now = erlang:system_time(millisecond),
    Last = erlang:put(last_debug_print, Now),
    TSDiff = case Last of undefined -> 0; _ -> Now - Last end,
    io:format(standard_error, "=== HB DEBUG ===[~pms in ~p @ ~s]==>~n~s~n",
        [
            TSDiff, self(),
            format_debug_trace(Mod, Func, LineNum),
            debug_fmt(X, 0)
        ]),
    X.

%% @doc Generate the appropriate level of trace for a given call.
format_debug_trace(Mod, Func, Line) ->
    case hb_opts:get(debug_print_trace, false, #{}) of
        short ->
            format_trace_short(get_trace());
        false ->
            io_lib:format("~p:~w ~p", [Mod, Line, Func])
    end.

%% @doc Convert a term to a string for debugging print purposes.
debug_fmt(X) -> debug_fmt(X, 0).
debug_fmt(X, Indent) ->
    try do_debug_fmt(X, Indent)
    catch A:B:C ->
        eunit_print(
            "~p:~p:~p",
            [A, B, C]
        ),
        case hb_opts:get(mode, prod) of
            prod ->
                format_indented("[!PRINT FAIL!]", Indent);
            _ ->
                format_indented("[PRINT FAIL:] ~80p", [X], Indent)
        end
    end.

do_debug_fmt(Wallet = {{rsa, _PublicExpnt}, _Priv, _Pub}, Indent) ->
    format_address(Wallet, Indent);
do_debug_fmt({_, Wallet = {{rsa, _PublicExpnt}, _Priv, _Pub}}, Indent) ->
    format_address(Wallet, Indent);
do_debug_fmt({explicit, X}, Indent) ->
    format_indented("~p", [X], Indent);
do_debug_fmt({X, Y}, Indent) when is_atom(X) and is_atom(Y) ->
    format_indented("~p: ~p", [X, Y], Indent);
do_debug_fmt({X, Y}, Indent) when is_record(Y, tx) ->
    format_indented("~p: [TX item]~n~s",
        [X, ar_bundles:format(Y, Indent + 1)],
        Indent
    );
do_debug_fmt({X, Y}, Indent) when is_map(Y) ->
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
do_debug_fmt({X, Y}, Indent) ->
    format_indented("~s: ~s", [debug_fmt(X, Indent), debug_fmt(Y, Indent)], Indent);
do_debug_fmt(Map, Indent) when is_map(Map) ->
    hb_util:format_map(Map, Indent);
do_debug_fmt(Tuple, Indent) when is_tuple(Tuple) ->
    format_tuple(Tuple, Indent);
do_debug_fmt(X, Indent) when is_binary(X) ->
    format_indented("~s", [format_binary(X)], Indent);
do_debug_fmt(Str = [X | _], Indent) when is_integer(X) andalso X >= 32 andalso X < 127 ->
    format_indented("~s", [Str], Indent);
do_debug_fmt(X, Indent) ->
    format_indented("~80p", [X], Indent).

%% @doc If the user attempts to print a wallet, format it as an address.
format_address(Wallet, Indent) ->
    format_indented(human_id(ar_wallet:to_address(Wallet)), Indent).

%% @doc Helper function to format tuples with arity greater than 2.
format_tuple(Tuple, Indent) ->
    to_lines(lists:map(
        fun(Elem) ->
            debug_fmt(Elem, Indent)
        end,
        tuple_to_list(Tuple)
    )).

to_lines(Elems) ->
    remove_trailing_noise(do_to_lines(Elems)).
do_to_lines([]) -> [];
do_to_lines(In =[RawElem | Rest]) ->
    Elem = lists:flatten(RawElem),
    case lists:member($\n, Elem) of
        true -> lists:flatten(lists:join("\n", In));
        false -> Elem ++ ", " ++ do_to_lines(Rest)
    end.

remove_trailing_noise(Str) ->
    remove_trailing_noise(Str, " \n,").
remove_trailing_noise(Str, Noise) ->
    case lists:member(lists:last(Str), Noise) of
        true ->
            remove_trailing_noise(lists:droplast(Str), Noise);
        false -> Str
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
    case short_id(Bin) of
        undefined ->
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
            PrintSegment =
                case is_human_binary(Printable) of
                    true -> Printable;
                    false -> encode(Printable)
                end,
            lists:flatten(
                [
                    "\"",
                    [PrintSegment],
                    case Printable == Bin of
                        true -> "\"";
                        false ->
                            io_lib:format("...\" <~s bytes>", [human_int(byte_size(Bin))])
                    end
                ]
            );
        ShortID ->
            lists:flatten(io_lib:format("~s", [ShortID]))
    end.

%% @doc Add `,' characters to a number every 3 digits to make it human readable.
human_int(Int) ->
    lists:reverse(add_commas(lists:reverse(integer_to_list(Int)))).

add_commas([A,B,C,Z|Rest]) -> [A,B,C,$,|add_commas([Z|Rest])];
add_commas(List) -> List.

%% @doc Format a map as either a single line or a multi-line string depending
%% on the value of the `debug_print_map_line_threshold' runtime option.
format_map(Map) -> format_map(Map, 0).
format_map(Map, Indent) ->
    MaxLen = hb_opts:get(debug_print_map_line_threshold),
    SimpleFmt = io_lib:format("~p", [Map]),
    case lists:flatlength(SimpleFmt) of
        Len when Len > MaxLen ->
            "\n" ++ lists:flatten(hb_message:format(Map, Indent));
        _ -> SimpleFmt
    end.

%% @doc Format and print an indented string to standard error.
eunit_print(FmtStr, FmtArgs) ->
    io:format(
        standard_error,
        "~n~s ",
        [hb_util:format_indented(FmtStr ++ "...", FmtArgs, 4)]
    ).

%% @doc Print the trace of the current stack, up to the first non-hyperbeam
%% module. Prints each stack frame on a new line, until it finds a frame that
%% does not start with a prefix in the `stack_print_prefixes' hb_opts.
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
%% Each stack frame is formatted if it matches the `stack_print_prefixes'
%% option. At the first frame that does not match a prefix in the
%% `stack_print_prefixes' option, the rest of the stack is not formatted.
format_trace([], _) -> [];
format_trace([Item|Rest], Prefixes) ->
    case element(1, Item) of
        Atom when is_atom(Atom) ->
            case trace_is_relevant(Atom, Prefixes) of
                true ->
                    [
                        format_trace(Item, Prefixes) |
                        format_trace(Rest, Prefixes)
                    ];
                false -> []
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

%% @doc Is the trace formatted string relevant to HyperBEAM?
trace_is_relevant(Atom, Prefixes) when is_atom(Atom) ->
    trace_is_relevant(atom_to_list(Atom), Prefixes);
trace_is_relevant(Str, Prefixes) ->
    case string:tokens(Str, "_") of
        [Pre|_] ->
            lists:member(Pre, Prefixes);
        _ ->
            false
    end.

%% @doc Print a trace to the standard error stream.
print_trace_short(Trace, Mod, Func, Line) ->
    io:format(standard_error, "=== [ HB SHORT TRACE ~p:~w ~p ] ==> ~s~n",
        [
            Mod, Line, Func,
            format_trace_short(Trace)
        ]
    ).

%% @doc Format a trace to a short string.
format_trace_short(Trace) -> 
    lists:join(
        " / ",
        lists:reverse(format_trace_short(
            hb_opts:get(short_trace_len, 3, #{}),
            false,
            Trace,
            hb_opts:get(stack_print_prefixes, [], #{})
        ))
    ).
format_trace_short(_Max, _Latch, [], _Prefixes) -> [];
format_trace_short(0, _Latch, _Trace, _Prefixes) -> [];
format_trace_short(Max, Latch, [Item|Rest], Prefixes) ->
    Formatted = format_trace_short(Max, Latch, Item, Prefixes),
    case {Latch, trace_is_relevant(Formatted, Prefixes)} of
        {false, true} ->
            [Formatted | format_trace_short(Max - 1, true, Rest, Prefixes)];
        {false, false} ->
            format_trace_short(Max, false, Rest, Prefixes);
        {true, true} ->
            [Formatted | format_trace_short(Max - 1, true, Rest, Prefixes)];
        {true, false} -> []
    end;
format_trace_short(Max, Latch, {Func, ArityOrTerm, Extras}, Prefixes) ->
    format_trace_short(
        Max, Latch, {no_module, Func, ArityOrTerm, Extras}, Prefixes
    );
format_trace_short(_, _Latch, {Mod, _, _, [{file, _}, {line, Line}|_]}, _) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Line]));
format_trace_short(_, _Latch, {Mod, Func, _ArityOrTerm, _Extras}, _Prefixes) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Func])).

%% @doc Utility function to help macro `?trace/0' remove the first frame of the
%% stack trace.
trace_macro_helper(Fun, {_, {_, Stack}}, Mod, Func, Line) ->
    Fun(Stack, Mod, Func, Line).

%% @doc Get the trace of the current process.
get_trace() ->
    case catch error(debugging_print) of
        {_, {_, Stack}} ->
            normalize_trace(Stack);
        _ -> []
    end.

%% @doc Remove all calls from this module from the top of a trace.
normalize_trace([]) -> [];
normalize_trace([{Mod, _, _, _}|Rest]) when Mod == ?MODULE ->
    normalize_trace(Rest);
normalize_trace(Trace) -> Trace.

%%% Statistics

count(Item, List) ->
    length(lists:filter(fun(X) -> X == Item end, List)).

mean(List) ->
    lists:sum(List) / length(List).

stddev(List) ->
    math:sqrt(variance(List)).

variance(List) ->
    Mean = mean(List),
    lists:sum([ math:pow(X - Mean, 2) || X <- List ]) / length(List).
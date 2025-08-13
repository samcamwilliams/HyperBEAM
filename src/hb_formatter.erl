%%% @doc A module that provides formatting utilities for HyperBEAM.
%%% This module contains functions for debugging output, trace formatting,
%%% string manipulation, and other text formatting operations.
-module(hb_formatter).
%%% Public API.
-export([indent/1, indent/2, escape_format/1, short_id/1]).
-export([format_error/2]).
-export([format_indented/2, format_indented/3, format_indented/4, format_binary/1]).
-export([format_maybe_multiline/3, remove_trailing_noise/2]).
-export([debug_print/1, debug_print/3, debug_print/4, debug_print/5, eunit_print/2]).
-export([debug_format/1, debug_format/2, debug_format/3]).
-export([get_trace/0, print_trace/4, trace_macro_helper/5, print_trace_short/4]).
-export([format_trace/1, trace_to_list/1, format_trace_short/0, format_trace_short/1]).
-include("include/hb.hrl").

%%% Characters that are considered noise and should be removed from strings
%%% with the `remove_noise_[leading|trailing]' functions.
-define(NOISE_CHARS, " \t\n,").

%% @doc Return a short ID for the different types of IDs used in AO-Core.
short_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    short_id(hb_util:human_id(Bin));
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

%% @doc Print a message to the standard error stream, prefixed by the amount
%% of time that has elapsed since the last call to this function.
debug_print(X) ->
    debug_print(X, <<>>, #{}).
debug_print(X, Info, Opts) ->
    io:format(
        standard_error,
        "=== HB DEBUG ===~s==>~n~s~n",
        [Info, debug_format(X, Opts, 0)]
    ),
    X.
debug_print(X, Mod, Func, LineNum) ->
    debug_print(X, format_debug_trace(Mod, Func, LineNum), #{}).
debug_print(X, Mod, Func, LineNum, Opts) ->
    Now = erlang:system_time(millisecond),
    Last = erlang:put(last_debug_print, Now),
    TSDiff = case Last of undefined -> 0; _ -> Now - Last end,
    Info =
        hb_util:bin(
            io_lib:format(
                "[~pms in ~s @ ~s]",
                [
                    TSDiff,
                    case server_id() of
                        undefined -> hb_util:bin(io_lib:format("~p", [self()]));
                        ServerID ->
                            hb_util:bin(
                                io_lib:format(
                                    "~s (~p)",
                                    [short_id(ServerID), self()]
                                )
                            )
                    end,
                    format_debug_trace(Mod, Func, LineNum)
                ]
            )
        ),
    debug_print(X, Info, Opts).

%% @doc Retreive the server ID of the calling process, if known.
server_id() ->
    server_id(#{ server_id => undefined }).
server_id(Opts) ->
    case hb_opts:get(server_id, undefined, Opts) of
        undefined -> get(server_id);
        ServerID -> ServerID
    end.

%% @doc Generate the appropriate level of trace for a given call.
format_debug_trace(Mod, Func, Line) ->
    case hb_opts:get(debug_print_trace, false, #{}) of
        short ->
            format_trace_short(get_trace());
        false ->
            io_lib:format("~p:~w ~p", [Mod, Line, Func])
    end.

%% @doc Convert a term to a string for debugging print purposes.
debug_format(X) -> debug_format(X, #{}).
debug_format(X, Opts) -> debug_format(X, Opts, 0).
debug_format(X, Opts, Indent) ->
    try do_debug_fmt(X, Opts, Indent)
    catch A:B:C ->
        Mode = hb_opts:get(mode, prod, Opts),
        PrintFailPreference = hb_opts:get(debug_print_fail_mode, quiet, Opts),
        case {Mode, PrintFailPreference} of
            {debug, quiet} ->
                format_indented("[!Format failed!] ~p", [X], Opts, Indent);
            {debug, _} ->
                format_indented(
                    "[PRINT FAIL:] ~80p~n===== PRINT ERROR WAS ~p:~p =====~n~s",
                    [
                        X,
                        A,
                        B,
                        hb_util:bin(
                            format_trace(
                                C,
                                hb_opts:get(stack_print_prefixes, [], #{})
                            )
                        )
                    ],
                    Opts,
                    Indent
                );
            _ ->
                format_indented("[!Format failed!]", [], Opts, Indent)
        end
    end.

do_debug_fmt(
    { { {rsa, _PublicExpnt1}, _Priv1, _Priv2 },
      { {rsa, _PublicExpnt2}, Pub }
    },
    Opts, Indent
) ->
    format_address(Pub, Opts, Indent);
do_debug_fmt(
    { AtomValue,
      {
        { {rsa, _PublicExpnt1}, _Priv1, _Priv2 },
        { {rsa, _PublicExpnt2}, Pub }
      }
    },
    Opts, Indent
) ->
    AddressString = format_address(Pub, Opts, Indent),
    format_indented("~p: ~s", [AtomValue, AddressString], Opts, Indent);
do_debug_fmt({explicit, X}, Opts, Indent) ->
    format_indented("[Explicit:] ~p", [X], Opts, Indent);
do_debug_fmt({string, X}, Opts, Indent) ->
    format_indented("~s", [X], Opts, Indent);
do_debug_fmt({trace, Trace}, Opts, Indent) ->
    format_indented("~n~s", [format_trace(Trace)], Opts, Indent);
do_debug_fmt({as, undefined, Msg}, Opts, Indent) ->
    "\n" ++ format_indented("Subresolve => ", [], Opts, Indent) ++
        format_maybe_multiline(Msg, Opts, Indent + 1);
do_debug_fmt({as, DevID, Msg}, Opts, Indent) ->
    "\n" ++ format_indented("Subresolve as ~s => ", [DevID], Opts, Indent) ++
        format_maybe_multiline(Msg, Opts, Indent + 1);
do_debug_fmt({X, Y}, Opts, Indent) when is_atom(X) and is_atom(Y) ->
    format_indented("~p: ~p", [X, Y], Opts, Indent);
do_debug_fmt({X, Y}, Opts, Indent) when is_record(Y, tx) ->
    format_indented("~p: [TX item]~n~s",
        [X, hb_tx:format(Y, Indent + 1)],
        Opts,
        Indent
    );
do_debug_fmt({X, Y}, Opts, Indent) when is_map(Y); is_list(Y) ->
    Formatted = format_maybe_multiline(Y, Opts, Indent + 1),
    format_indented(
        case is_binary(X) of
            true -> "~s";
            false -> "~p"
        end ++ "~s",
        [
            X,
            case is_multiline(Formatted) of
                true -> " ==>" ++ Formatted;
                false -> ": " ++ Formatted
            end
        ],
        Opts,
        Indent
    );
do_debug_fmt({X, Y}, Opts, Indent) ->
    format_indented(
        "~s: ~s",
        [
            debug_format(X, Opts, Indent),
            debug_format(Y, Opts, Indent)
        ],
        Opts,
        Indent
    );
do_debug_fmt(MaybePrivMap, Opts, Indent) when is_map(MaybePrivMap) ->
    Map = hb_private:reset(MaybePrivMap),
    case maybe_format_short(Map, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++ lists:flatten(hb_message:format(Map, Opts, Indent))
    end;
do_debug_fmt(Tuple, Opts, Indent) when is_tuple(Tuple) ->
    format_tuple(Tuple, Opts, Indent);
do_debug_fmt(X, Opts, Indent) when is_binary(X) ->
    format_indented("~s", [format_binary(X)], Opts, Indent);
do_debug_fmt(Str = [X | _], Opts, Indent) when is_integer(X) andalso X >= 32 andalso X < 127 ->
    format_indented("~s", [Str], Opts, Indent);
do_debug_fmt(MsgList, Opts, Indent) when is_list(MsgList) ->
    format_list(MsgList, Opts, Indent);
do_debug_fmt(X, Opts, Indent) ->
    format_indented("~80p", [X], Opts, Indent).

%% @doc If the user attempts to print a wallet, format it as an address.
format_address(Wallet, Opts, Indent) ->
    format_indented("Wallet [Addr: ~s]",
        [short_id(hb_util:human_id(ar_wallet:to_address(Wallet)))], 
        Opts, 
        Indent
    ).

%% @doc Helper function to format tuples with arity greater than 2.
format_tuple(Tuple, Opts, Indent) ->
    to_lines(lists:map(
        fun(Elem) ->
            debug_format(Elem, Opts, Indent)
        end,
        tuple_to_list(Tuple)
    )).

%% @doc Format a list. Comes in three forms: all on one line, individual items
%% on their own line, or each item a multi-line string.
format_list(MsgList, Opts, Indent) ->
    case maybe_format_short(MsgList, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++
                format_indented("List [~w] {", [length(MsgList)], Opts, Indent) ++
                format_list_lines(MsgList, Opts, Indent)
    end.

%% @doc Format a list as a multi-line string.
format_list_lines(MsgList, Opts, Indent) ->
    Numbered = hb_util:number(MsgList),
    Lines =
        lists:map(
            fun({N, Msg}) ->
                format_list_item(N, Msg, Opts, Indent)
            end,
            Numbered
        ),
    AnyLong =
        lists:any(
            fun({Mode, _}) -> Mode == multiline end,
            Lines
        ),
    case AnyLong of
        false ->
            "\n" ++
                remove_trailing_noise(
                    lists:flatten(
                        lists:map(
                            fun({_, Line}) ->
                                Line
                            end,
                            Lines
                        )
                    )
                ) ++
                "\n" ++
                format_indented("}", [], Opts, Indent);
        true ->
            "\n" ++
            lists:flatten(lists:map(
                fun({N, Msg}) ->
                    {_, Line} = format_list_item(multiline, N, Msg, Opts, Indent),
                    Line
                end,
                Numbered
            )) ++ format_indented("}", [], Opts, Indent)
    end.

%% @doc Format a single element of a list.
format_list_item(N, Msg, Opts, Indent) ->
    case format_list_item(short, N, Msg, Opts, Indent) of
        {short, String} -> {short, String};
        error -> format_list_item(multiline, N, Msg, Opts, Indent)
    end.
format_list_item(short, N, Msg, Opts, Indent) ->
    case maybe_format_short(Msg, Opts, Indent) of
        {ok, SimpleFmt} ->
            {short, format_indented("~s => ~s~n", [N, SimpleFmt], Opts, Indent + 1)};
        error -> error
    end;
format_list_item(multiline, N, Msg, Opts, Indent) ->
    Formatted =
        case is_multiline(Base = debug_format(Msg, Opts, Indent + 2)) of
            true -> Base;
            false -> remove_leading_noise(Base)
        end,
    {
        multiline,
        format_indented(
            "~s => ~s~n",
            [N, Formatted], 
            Opts,
            Indent + 1
        )
    }.

%% @doc Join a list of strings and remove trailing noise.
to_lines(Elems) ->
    remove_trailing_noise(do_to_lines(Elems)).
do_to_lines([]) -> [];
do_to_lines(In =[RawElem | Rest]) ->
    Elem = lists:flatten(RawElem),
    case lists:member($\n, Elem) of
        true -> lists:flatten(lists:join("\n", In));
        false -> Elem ++ ", " ++ do_to_lines(Rest)
    end.

%% @doc Remove any leading whitespace from a string.
remove_leading_noise(Str) ->
    remove_leading_noise(Str, ?NOISE_CHARS).
remove_leading_noise([Char|Str], Noise) ->
    case lists:member(Char, Noise) of
        true ->
            remove_leading_noise(Str, Noise);
        false -> [Char|Str]
    end.

%% @doc Remove trailing noise characters from a string. By default, this is
%% whitespace, newlines, and `,'.
remove_trailing_noise(Str) ->
    remove_trailing_noise(Str, ?NOISE_CHARS).
remove_trailing_noise(Str, Noise) ->
    case lists:member(lists:last(Str), Noise) of
        true ->
            remove_trailing_noise(lists:droplast(Str), Noise);
        false -> Str
    end.

%% @doc Format a string with an indentation level.
format_indented(Str, Indent) -> format_indented(Str, #{}, Indent).
format_indented(Str, Opts, Indent) -> format_indented(Str, [], Opts, Indent).
format_indented(FmtStr, Terms, Opts, Ind) ->
    IndentSpaces = hb_opts:get(debug_print_indent, Opts),
    EscapedFmt = escape_format(FmtStr),
    lists:droplast(
        lists:flatten(
            io_lib:format(
                [$\s || _ <- lists:seq(1, Ind * IndentSpaces)] ++
                    lists:flatten(EscapedFmt) ++ "\n",
                Terms
            )
        )
    ).

%% @doc Escape a string for use as an io_lib:format specifier.
escape_format(Str) when is_list(Str) ->
    re:replace(
        Str,
        "~([a-z\\-_]+@[0-9]+\\.[0-9]+)", "~~\\1",
        [global, {return, list}]
    );
escape_format(Else) -> Else.

%% @doc Format an error message as a string.
format_error(ErrorMsg, Opts) ->
    Type = hb_ao:get(<<"type">>, ErrorMsg, <<"">>, Opts),
    Details = hb_ao:get(<<"details">>, ErrorMsg, <<"">>, Opts),
    Stacktrace = hb_ao:get(<<"stacktrace">>, ErrorMsg, <<"">>, Opts),
    hb_util:bin(
        [
            <<"Termination type: '">>, Type,
            <<"'\n\nStacktrace:\n\n">>, Stacktrace,
            <<"\n\nError details:\n\n">>, Details
        ]
    ).

%% @doc Take a series of strings or a combined string and format as a
%% single string with newlines and indentation to the given level. Note: This
%% function returns a binary.
indent(Strings) ->
    indent(Strings, 0).
indent(Strings, Indent) when is_binary(Strings) ->
    indent(binary:split(Strings, <<"\n">>, [global]), Indent);
indent(Strings, Indent) when is_list(Strings) ->
    hb_util:bin(lists:join(
        "\n",
        [
            format_indented(hb_util:list(String), #{}, Indent)
        ||
            String <- Strings
        ]
    )).

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
                case hb_util:is_human_binary(Printable) of
                    true -> Printable;
                    false -> hb_util:encode(Printable)
                end,
            lists:flatten(
                [
                    "\"",
                    [PrintSegment],
                    case Printable == Bin of
                        true -> "\"";
                        false ->
                            io_lib:format(
                                "...\" <~s bytes>",
                                [hb_util:human_int(byte_size(Bin))]
                            )
                    end
                ]
            );
        ShortID ->
            lists:flatten(io_lib:format("~s", [ShortID]))
    end.


%% @doc Format a map as either a single line or a multi-line string depending
%% on the value of the `debug_print_map_line_threshold' runtime option.
format_maybe_multiline(X, Opts, Indent) ->
    case maybe_format_short(X, Opts, Indent) of
        {ok, SimpleFmt} -> SimpleFmt;
        error ->
            "\n" ++ lists:flatten(hb_message:format(X, Opts, Indent))
    end.

%% @doc Attempt to generate a short formatting of a message, using the given
%% node options.
maybe_format_short(X, Opts, _Indent) ->
    MaxLen = hb_opts:get(debug_print_map_line_threshold, 100, Opts),
    SimpleFmt =
        case is_binary(X) of
            true -> format_binary(X);
            false -> io_lib:format("~p", [X])
        end,
    case is_multiline(SimpleFmt) orelse (lists:flatlength(SimpleFmt) > MaxLen) of
        true -> error;
        false -> {ok, SimpleFmt}
    end.

%% @doc Is the given string a multi-line string?
is_multiline(Str) ->
    lists:member($\n, Str).

%% @doc Format and print an indented string to standard error.
eunit_print(FmtStr, FmtArgs) ->
    io:format(
        standard_error,
        "~n~s ",
        [format_indented(FmtStr ++ "...", FmtArgs, #{}, 4)]
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
            lists:flatten(format_trace(Stack))
        ]).

%% @doc Format a stack trace as a list of strings, one for each stack frame.
%% Each stack frame is formatted if it matches the `stack_print_prefixes'
%% option. At the first frame that does not match a prefix in the
%% `stack_print_prefixes' option, the rest of the stack is not formatted.
format_trace(Stack) ->
    format_trace(Stack, hb_opts:get(stack_print_prefixes, [], #{})).
format_trace([], _) -> [];
format_trace([Item|Rest], Prefixes) ->
    case element(1, Item) of
        Atom when is_atom(Atom) ->
            case true of %is_hb_module(Atom, Prefixes) of
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
    ExtraMap = hb_maps:from_list(Extras),
    format_indented(
        "~p:~p/~p [~s]~n",
        [
            Mod, Func, ArityOrTerm,
            case hb_maps:get(line, ExtraMap, undefined) of
                undefined -> "No details";
                Line ->
                    hb_maps:get(file, ExtraMap)
                        ++ ":" ++ integer_to_list(Line)
            end
        ],
        #{},
        1
    ).

%% @doc Print a trace to the standard error stream.
print_trace_short(Trace, Mod, Func, Line) ->
    io:format(standard_error, "=== [ HB SHORT TRACE ~p:~w ~p ] ==> ~s~n",
        [
            Mod, Line, Func,
            format_trace_short(Trace)
        ]
    ).

%% @doc Return a list of calling modules and lines from a trace, removing all
%% frames that do not match the `stack_print_prefixes' option.
trace_to_list(Trace) ->
    Prefixes = hb_opts:get(stack_print_prefixes, [], #{}),
    lists:filtermap(
        fun(TraceItem) ->
            Formatted = format_trace_element(TraceItem),
            case hb_util:is_hb_module(Formatted, Prefixes) of
                true -> {true, Formatted};
                false -> false
            end
        end,
        Trace
    ).

%% @doc Format a trace to a short string.
format_trace_short() -> format_trace_short(get_trace()).
format_trace_short(Trace) when is_list(Trace) ->
    lists:join(" / ", lists:reverse(trace_to_list(Trace))).

%% @doc Format a trace element in form `mod:line' or `mod:func'.
format_trace_element({Mod, _, _, [{file, _}, {line, Line}|_]}) ->
    lists:flatten(io_lib:format("~p:~p", [Mod, Line]));
format_trace_element({Mod, Func, _ArityOrTerm, _Extras}) ->
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

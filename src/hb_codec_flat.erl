%%% A codec for turning TABMs into/from flat Erlang maps that have (potentially
%%% multi-layer) paths as their keys, and a normal TABM binary as their value.
-module(hb_codec_flat).
-export([from/1, to/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Convert a flat map to a TABM.
from(Bin) when is_binary(Bin) -> Bin;
from(Map) when is_map(Map) ->
    maps:fold(
        fun(Path, Value, Acc) ->
            inject_at_path(hb_path:term_to_path(Path), from(Value), Acc)
        end,
        #{},
        Map
    ).

%% Helper function to inject a value at a specific path in a nested map
inject_at_path([Key], Value, Map) ->
    maps:put(Key, Value, Map);
inject_at_path([Key|Rest], Value, Map) ->
    SubMap = maps:get(Key, Map, #{}),
    maps:put(Key, inject_at_path(Rest, Value, SubMap), Map).

%% @doc Convert a TABM to a flat map.
to(Bin) when is_binary(Bin) -> Bin;
to(Map) when is_map(Map) ->
    Opts = #{ atom_keys => false },
    maps:fold(
        fun(Key, Value, Acc) ->
            case to(Value) of
                SubMap when is_map(SubMap) ->
                    maps:fold(
                        fun(SubKey, SubValue, InnerAcc) ->
                            maps:put(
                                hb_path:term_to_path(Key, Opts)
                                    ++ hb_path:term_to_path(SubKey, Opts),
                                SubValue,
                                InnerAcc
                            )
                        end,
                        Acc,
                        SubMap
                    );
                SimpleValue ->
                    maps:put(hb_path:term_to_path(Key, Opts), SimpleValue, Acc)
            end
        end,
        #{},
        Map
    ).

%%% Tests

simple_conversion_test() ->
    Flat = #{[<<"a">>] => <<"value">>},
    Nested = #{<<"a">> => <<"value">>},
    ?assert(hb_message:match(Nested, hb_codec_flat:from(Flat))),
    ?assert(hb_message:match(Flat, hb_codec_flat:to(Nested))).

nested_conversion_test() ->
    Flat = #{[<<"a">>, <<"b">>] => <<"value">>},
    Nested = #{<<"a">> => #{<<"b">> => <<"value">>}},
    ?assert(hb_message:match(Nested, hb_codec_flat:from(Flat))),
    ?assert(hb_message:match(Flat, hb_codec_flat:to(Nested))).

multiple_paths_test() ->
    Flat = #{
        [<<"x">>, <<"y">>] => <<"1">>,
        [<<"x">>, <<"z">>] => <<"2">>,
        [<<"a">>] => <<"3">>
    },
    Nested = #{
        <<"x">> => #{
            <<"y">> => <<"1">>,
            <<"z">> => <<"2">>
        },
        <<"a">> => <<"3">>
    },
    ?assert(hb_message:match(Nested, hb_codec_flat:from(Flat))),
    ?assert(hb_message:match(Flat, hb_codec_flat:to(Nested))).

binary_passthrough_test() ->
    Bin = <<"raw binary">>,
    ?assertEqual(Bin, hb_codec_flat:from(Bin)),
    ?assertEqual(Bin, hb_codec_flat:to(Bin)).

deep_nesting_test() ->
    Flat = #{[<<"a">>, <<"b">>, <<"c">>, <<"d">>] => <<"deep">>},
    Nested = #{<<"a">> => #{<<"b">> => #{<<"c">> => #{<<"d">> => <<"deep">>}}}},
    ?assert(hb_message:match(Nested, hb_codec_flat:from(Flat))),
    ?assert(hb_message:match(Flat, hb_codec_flat:to(Nested))).

empty_map_test() ->
    ?assertEqual(#{}, hb_codec_flat:from(#{})),
    ?assertEqual(#{}, hb_codec_flat:to(#{})).
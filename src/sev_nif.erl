-module(sev_nif).

-export([ add/2
        , my_map/0
        , my_maps/0
        , my_tuple/0
        , unit_enum_echo/1
        , tagged_enum_echo/1
        , untagged_enum_echo/1
        , request_attestation_report/1
        , verify_root_of_trust/3
        ]).

-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

%%%===================================================================
%%% API
%%%===================================================================

add(_A, _B) ->
    ?NOT_LOADED.

my_map() ->
    ?NOT_LOADED.

my_maps() ->
    ?NOT_LOADED.

my_tuple() ->
    ?NOT_LOADED.

unit_enum_echo(_Atom) ->
    ?NOT_LOADED.

tagged_enum_echo(_Tagged) ->
    ?NOT_LOADED.

untagged_enum_echo(_Untagged) ->
    ?NOT_LOADED.

request_attestation_report(_UniqueData) ->
    ?NOT_LOADED.

verify_root_of_trust(_ArkPem, _AskPem, _VcekPem) ->
    ?NOT_LOADED.

%%%===================================================================
%%% NIF
%%%===================================================================

init() ->
    ?load_nif_from_crate(sev_nif, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual(4, add(2, 2)).

my_map_test() ->
    ?assertEqual(#{lhs => 33, rhs => 21}, my_map()).

my_maps_test() ->
    ?assertEqual([#{lhs => 33, rhs => 21}, #{lhs => 33, rhs => 21}], my_maps()).

my_tuple_test() ->
    ?assertEqual({33, 21}, my_tuple()).

unit_enum_echo_test() ->
    ?assertEqual(foo_bar, unit_enum_echo(foo_bar)),
    ?assertEqual(baz, unit_enum_echo(baz)).

tagged_enum_echo_test() ->
    ?assertEqual(foo, tagged_enum_echo(foo)),
    ?assertEqual({bar, <<"string">>}, tagged_enum_echo({bar, <<"string">>})),
    ?assertEqual({baz,#{a => 1, b => 2}}, tagged_enum_echo({baz,#{a => 1, b => 2}})).

untagged_enum_echo_test() ->
    ?assertEqual(123, untagged_enum_echo(123)),
    ?assertEqual(<<"string">>, untagged_enum_echo(<<"string">>)).

request_attestation_report_test() ->
	UniqueData = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
					17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
					33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
					49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64>>,
	?assertMatch({ok, _Report}, request_attestation_report(UniqueData)).

verify_root_of_trust_test() ->
	ArkPem = <<"-----BEGIN CERTIFICATE-----\nMIIBIjANBgkqhkiG9w0BAQE...\n-----END CERTIFICATE-----">>,
    AskPem = <<"-----BEGIN CERTIFICATE-----\nMIIBIjANBgkqhkiG9w0BAQE...\n-----END CERTIFICATE-----">>,
    VcekPem = <<"-----BEGIN CERTIFICATE-----\nMIIBIjANBgkqhkiG9w0BAQE...\n-----END CERTIFICATE-----">>,
    ?assertEqual(true, verify_root_of_trust(ArkPem, AskPem, VcekPem)).

-endif.

-module(dev_snp_nif).
-export([request_attestation_report/1]).
-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

request_attestation_report(_UniqueData) ->
    ?NOT_LOADED.

%%%===================================================================
%%% NIF
%%%===================================================================

init() ->
    ?load_nif_from_crate(dev_snp_nif, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

request_attestation_report_test() ->
	UniqueData = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
					17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
					33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
					49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64>>,
	?assertMatch({ok, _Report}, request_attestation_report(UniqueData)).

-endif.

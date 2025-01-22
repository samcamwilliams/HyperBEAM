-module(dev_snp_nif).

-export([request_attestation_report/2, calculate_launch_digest/1]).
-include("cargo.hrl").
-include("include/hb.hrl").

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

%%%===================================================================
%%% API
%%%===================================================================


request_attestation_report(_UniqueData, _VMPL) ->
    ?NOT_LOADED.

calculate_launch_digest(_Args) ->
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
	VMPL = 1,
	?assertEqual({ok, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
	17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
	33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
	49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64>>}, dev_snp_nif:request_attestation_report(UniqueData, VMPL)).

calculate_launch_digest_test() ->
	%% Define the data structure
	ArgsMap = #{ 
		vcpus => 1,
		vcpu_type => 5, 
		vmm_type => 1,
		guest_features => 16#1,

		ovmf_hash_str => "b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510",
		kernel_hash => "69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576",
		initrd_hash => "ca3bf9f3c453471fb98beadb9ca735dd77e8f9879bb33177696b349114ff0737",
		append_hash => "f8532adfd035eefb418ff317c0095b0cc4bfd9d622aeadee98e04a134fa0edd9"
	},

	?event(ArgsMap),

		%% Call the NIF
	Result = dev_snp_nif:calculate_launch_digest(ArgsMap),

	%% Expected result
	?event(Result),
	?assertMatch({ok, [220,244,169,131,67,254,72,77,141,164,219,30,151,179,18,49,34,26,185,253,106,
	33,90,13,37,218,244,77,123,154,120,9,252,127,244,96,169,15,155,82,90,93,235,
	107,183,91,137,0]}, Result).

-endif.

-module(dev_snp_nif).
-export([request_attestation_report/2, calculate_launch_digest/1]).
-export([verify_measurement/2, verify_signature/1]).
-include("cargo.hrl").
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

request_attestation_report(_UniqueData, _VMPL) ->
    ?NOT_LOADED.

calculate_launch_digest(_Args) ->
	?NOT_LOADED.

verify_measurement(_Report, _Expected) ->
	?NOT_LOADED.

verify_signature(_Report) ->
	?NOT_LOADED.

init() ->
    ?load_nif_from_crate(dev_snp_nif, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

request_attestation_report_test() ->
	UniqueData = crypto:strong_rand_bytes(64),
	VMPL = 1,
	?assertEqual(
        {ok, UniqueData},
        dev_snp_nif:request_attestation_report(UniqueData, VMPL)
    ).

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
	?assertMatch({ok, [220,244,169,131,67,254,72,77,141,164,219,30,151,179,18,
    49,34,26,185,253,106,33,90,13,37,218,244,77,123,154,120,9,252,127,244,96,
    169,15,155,82,90,93,235,107,183,91,137,0]}, Result).

verify_measurement_test() ->
	%% Define a mock report (JSON string) as binary
    {ok, MockReport} = file:read_file("test/snp-measurement.json"),
	%% Define the expected measurement (binary)
	ExpectedMeasurement = <<94,87,4,197,20,11,255,129,179,197,146,104,8,212,152,248,110,11,60,246,82,254,24,55,201,47,157,229,163,82,108,66,191,138,241,229,40,144,133,170,116,109,17,62,20,241,144,119>>,
	%% Call the NIF
	Result = dev_snp_nif:verify_measurement(MockReport, ExpectedMeasurement),
	?assertMatch({ok, <<"Measurements match">>}, Result).

verify_signature_test() ->
	%% Define a mock report (JSON string) as binary
    {ok, MockAttestation} = file:read_file("test/snp-attestation.json"),
	Result = dev_snp_nif:verify_signature(MockAttestation),
	?assertMatch({ok, <<"Verification successful">>}, Result).

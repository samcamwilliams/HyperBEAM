-module(dev_snp_nif).

-export([request_attestation_report/2, calculate_launch_digest/1, verify_measurement/2]).
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

verify_measurement(_Report, _Expected) ->
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

verify_measurement_test() ->
	%% Define a mock report (JSON string) as binary
	MockReport = <<"{\"version\":2,\"guest_svn\":0,\"policy\":196608,\"family_id\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],\"image_id\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],\"vmpl\":1,\"sig_algo\":1,\"current_tcb\":{\"bootloader\":4,\"tee\":0,\"_reserved\":[0,0,0,0],\"snp\":22,\"microcode\":213},\"plat_info\":3,\"_author_key_en\":0,\"_reserved_0\":0,\"report_data\":[107,177,15,108,76,181,154,193,86,58,37,11,6,178,145,224,233,19,70,184,151,201,166,72,200,158,145,236,87,146,8,219,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],\"measurement\":[94,87,4,197,20,11,255,129,179,197,146,104,8,212,152,248,110,11,60,246,82,254,24,55,201,47,157,229,163,82,108,66,191,138,241,229,40,144,133,170,116,109,17,62,20,241,144,119],\"host_data\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],\"id_key_digest\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],\"author_key_digest\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],\"report_id\":[246,37,173,24,24,99,21,145,60,28,73,1,217,65,121,45,114,58,91,219,210,122,81,63,152,72,238,19,167,185,155,173],\"report_id_ma\":[255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255],\"reported_tcb\":{\"bootloader\":4,\"tee\":0,\"_reserved\":[0,0,0,0],\"snp\":22,\"microcode\":213},\"_reserved_1\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],\"chip_id\":[140,186,24,26,13,93,198,89,109,169,156,117,74,171,119,218,227,248,161,29,156,249,196,253,0,133,213,176,104,236,220,229,27,64,240,249,213,207,232,136,152,246,240,221,96,1,178,159,177,108,253,113,102,214,196,175,132,105,188,140,137,98,86,52],\"committed_tcb\":{\"bootloader\":4,\"tee\":0,\"_reserved\":[0,0,0,0],\"snp\":22,\"microcode\":213},\"current_build\":20,\"current_minor\":55,\"current_major\":1,\"_reserved_2\":0,\"committed_build\":20,\"committed_minor\":55,\"committed_major\":1,\"_reserved_3\":0,\"launch_tcb\":{\"bootloader\":4,\"tee\":0,\"_reserved\":[0,0,0,0],\"snp\":22,\"microcode\":213}}">>,

	%% Define the expected measurement (binary)
	ExpectedMeasurement = <<94,87,4,197,20,11,255,129,179,197,146,104,8,212,152,248,110,11,60,246,82,254,24,55,201,47,157,229,163,82,108,66,191,138,241,229,40,144,133,170,116,109,17,62,20,241,144,119>>,

	%% Call the NIF
	Result = dev_snp_nif:verify_measurement(MockReport, ExpectedMeasurement),

	%% Assert the result
	?event(Result),
	?assertMatch({ok, <<"Measurements match">>}, Result).
	

-endif.

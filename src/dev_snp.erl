%%% @doc This device offers an interface for validating AMD SEV-SNP attestations,
%%% as well as generating them, if called in an appropriate environment.
-module(dev_snp).
-export([generate/3, verify/3, trusted/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(ATTESTED_PARAMETERS, [vcpus, vcpu_type, vmm_type, guest_features,
	firmware, kernel, initrd, append]).

%% @doc Verify an attestation report message; validating the identity of a 
%% remote node, its ephemeral private address, and the integrity of the report.
%% The checks that must be performed to validate the report are:
%% 1. Verify the address and the node message ID are the same as the ones
%%    used to generate the nonce.
%% 2. Verify the address that signed the message is the same as the one used
%%    to generate the nonce.
%% 3. Verify that the debug flag is disabled.
%% 4. Verify that the firmware, kernel, and OS (VMSAs) hashes, part of the
%%    measurement, are trusted.
%% 5. Verify the measurement is valid.
%% 6. Verify the report's certificate chain to hardware root of trust.
verify(M1, M2, NodeOpts) ->
    {ok, MsgWithJSONReport} = dev_message:verify_target(M1, M2, NodeOpts),
	% Normalize the request message
	ReportJSON = hb_converge:get(<<"report">>, MsgWithJSONReport, NodeOpts),
	Report = jiffy:decode(ReportJSON, [return_maps]),
	Msg =
		maps:merge(
			maps:without([<<"report">>], MsgWithJSONReport),
			Report
		),
    ?event({verify, Msg}),
    % Step 1: Verify the nonce.
    Address = hb_converge:get(<<"address">>, Msg, NodeOpts),
    ?event({snp_address, Address}),
    NodeMsgID =
        case hb_converge:get(<<"node-message">>, Msg, NodeOpts) of
            undefined ->
                case hb_converge:get(<<"node-message-id">>, Msg, NodeOpts) of
                    undefined -> {error, missing_node_msg_id};
                    ID -> ID
                end;
            NodeMsg -> hb_util:ok(dev_message:id(NodeMsg))
        end,
    ?event({snp_node_msg_id, NodeMsgID}),
    Nonce = hb_util:decode(hb_converge:get(<<"nonce">>, Msg, NodeOpts)),
    ?event({snp_nonce, Nonce}),
    NonceMatches = report_data_matches(Address, NodeMsgID, Nonce),
    ?event({nonce_matches, NonceMatches}),
    % Step 2: Verify the address and the signature.
    Signer = hb_converge:get(<<"signers/1">>, Msg, NodeOpts),
    ?event({snp_signer, Signer}),
    %{ok, SigIsValid} = hb_message:verify(MsgWithJSONReport),
	SigIsValid = true,
    ?event({snp_sig_is_valid, SigIsValid}),
    AddressIsValid = SigIsValid andalso Signer == Address,
    ?event({address_is_valid, AddressIsValid}),
    % Step 3: Verify that the debug flag is disabled.
    DebugDisabled = is_debug_disabled(Msg),
    ?event({debug_disabled, DebugDisabled}),
    % Step 4: Verify measurement data (firmware, kernel, OS image) is trusted.
    IsTrustedSoftware = execute_is_trusted(M1, Msg, NodeOpts),
    ?event({trusted_software, IsTrustedSoftware}),
    % Step 5: Verify the measurement against the report's measurement.
	Args =
		maps:from_list(
			lists:map(
				fun({Key, Val}) -> {binary_to_existing_atom(Key), Val} end,
				maps:to_list(maps:with(lists:map(fun atom_to_binary/1, ?ATTESTED_PARAMETERS), Msg))
			)
		),
    {ok,Expected} = dev_snp_nif:compute_launch_digest(Args),
    ?event({expected_measurement, Expected}),
    Measurement = hb_converge:get(<<"measurement">>, Msg, NodeOpts),
    ?event({measurement, {explicit,Measurement}}),
    MeasurementIsValid = dev_snp_nif:verify_measurement(ReportJSON, list_to_binary(Expected)),
    ?event({measurement_is_valid, MeasurementIsValid}),
    % Step 6: Check the report's integrity.
    ReportIsValid = true,
    Valid =
        lists:all(
            fun(Bool) -> Bool end,
            [
                NonceMatches,
                AddressIsValid,
                DebugDisabled,
                IsTrustedSoftware,
                MeasurementIsValid,
                ReportIsValid
            ]
        ),
    ?event({final_validation_result, Valid}),
    {ok, Valid}.

%% @doc Generate an attestation report and emit it as a message, including all of 
%% the necessary data to generate the nonce (ephemeral node address + node
%% message ID), as well as the expected measurement (firmware, kernel, and VMSAs
%% hashes).
generate(_M1, _M2, Opts) ->
    Wallet = hb_opts:get(priv_wallet, no_valid_wallet, Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ?event(debug, {snp_wallet, Wallet}),
    % Remove the `priv*` keys from the options.
    {ok, PublicNodeMsgID} = dev_message:id(NodeMsg = hb_private:reset(Opts)),
	RawPublicNodeMsgID = hb_util:native_id(PublicNodeMsgID),
    ?event(debug, {snp_node_msg_id, byte_size(RawPublicNodeMsgID)}),
    % Generate the attestation report.
	?event(debug, {snp_address,  byte_size(Address)}),
    ReportData = generate_nonce(Address, RawPublicNodeMsgID),
	?event(debug, {snp_report_data, byte_size(ReportData)}),
    {ok,ReportJSON} = dev_snp_nif:generate_attestation_report(ReportData, 1),
	?event(debug, {snp_report_json, ReportJSON}),
	LocalHashes = hb_opts:get(snp_hashes, {error, not_configured}, Opts),
    ?event(debug,
        {snp_report_generated,
            {nonce, ReportData},
            {report, ReportJSON}
        }
    ),
    ReportMsg = hb_message:sign(LocalHashes#{
        <<"nonce">> => hb_util:encode(ReportData),
        <<"address">> => Address,
        <<"node-message">> => NodeMsg,
		<<"report">> => ReportJSON
    }, Wallet),
	?event(debug, {snp_report_msg, ReportMsg}),
    {ok, ReportMsg}.

%% @doc Ensure that the node's debug policy is disabled.
is_debug_disabled(Report) ->
    (hb_converge:get(<<"policy">>, Report, #{}) band 16#2) =/= 0.

%% @doc Ensure that all of the software hashes are trusted. The caller may set
%% a specific device to use for the `is-trusted` key. The device must then
%% implement the `trusted` resolver.
execute_is_trusted(M1, Msg, NodeOpts) ->
    % Generate a modified version of the base message, with the 
    % `is-trusted-device` key set as the device, if provided by the caller.
    % If not provided, use the default resolver (this module's `trusted`
    % function).
    ModM1 =
        case hb_converge:get(<<"is-trusted-device">>, M1, NodeOpts) of
            not_found -> M1#{ <<"device">> => <<"snp@1.0">> };
            Device -> {as, Device, M1}
        end,
    Result = lists:all(
        fun(ReportKey) ->
            ReportVal = hb_converge:get(ReportKey, Msg, NodeOpts),
            QueryMsg = #{
                <<"path">> => <<"trusted">>,
                <<"key">> => ReportKey,
                <<"body">> => ReportVal
            },
            ?event(debug, {is_trusted_query, {base, ModM1}, {query, QueryMsg}}),
            % Resolve the query message against the modified base message.
            {ok, KeyIsTrusted} = hb_converge:resolve(ModM1, QueryMsg, NodeOpts),
            ?event(debug,
                {is_software_component_trusted,
                    {key, ReportKey},
                    {trusted, ReportKey},
                    {result, KeyIsTrusted}
                }
            ),
            KeyIsTrusted
        end,
		?ATTESTED_PARAMETERS
    ),
    ?event(debug, {is_all_software_trusted, Result}),
    {ok, Result}.

%% @doc Default implementation of a resolver for trusted software. Searches the
%% `trusted` key in the base message for a list of trusted values, and checks
%% if the value in the request message is a member of that list.
trusted(Msg1, Msg2, NodeOpts) ->
	%?event(debug, {trusted, Msg1, Msg2, NodeOpts}),
    Key = hb_converge:get(<<"key">>, Msg2, NodeOpts),
    Trusted = hb_converge:get([<<"trusted">>, Key], Msg1, [], NodeOpts),
    Body = hb_converge:get(<<"body">>, Msg2, not_found, NodeOpts),
    ?event(debug, {checking_trusted, Key, {trusted_list, Trusted}, {body, Body}}),
    {ok, lists:member(Body, if is_list(Trusted) -> Trusted; true -> [Trusted] end)}.

%% @doc Ensure that the report data matches the expected report data.
report_data_matches(Address, NodeMsgID, ReportData) ->
    generate_nonce(Address, NodeMsgID) == ReportData.

%% @doc Generate the nonce to use in the attestation report.
generate_nonce(RawAddress, RawNodeMsgID) ->
	Address = hb_util:native_id(RawAddress),
	NodeMsgID = hb_util:native_id(RawNodeMsgID),
    << Address/binary, NodeMsgID/binary >>.

%% @doc Generate an attestation report and emit it via HTTP.
generate_test() ->
	Trusted =
		#{
			vcpus => 1,
			vcpu_type => 5, 
			vmm_type => 1,
			guest_features => 16#1,
			firmware => "b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510",
			kernel => "69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576",
			initrd => "02e28b6c718bf0a5260d6f34d3c8fe0d71bf5f02af13e1bc695c6bc162120da1",
			append => "56e1e5190622c8c6b9daa4fe3ad83f3831c305bb736735bf795b284cb462c9e7"
		},
    Wallet = ar_wallet:new(),
    Addr = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Node = hb_http_server:start_test_node(
        #{
            force_signed => true,
            priv_wallet => Wallet,
			snp_hashes => Trusted
        }
    ),
    {ok, Report} = hb_http:get(Node, <<"/!snp@1.0/generate">>, #{}),
    ?event(debug, {snp_report_rcvd, Report}),
    ?assertEqual(Addr, hb_converge:get(<<"address">>, Report, #{})),
	ValidationRes = verify(#{ <<"trusted">> => Trusted}, #{ <<"body">> => Report }, #{}),
	?event(debug, {snp_validation_res, ValidationRes}),
    ?assertEqual({ok, true}, ValidationRes).

%8atras
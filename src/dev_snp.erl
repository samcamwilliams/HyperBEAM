%%% @doc This device offers an interface for validating AMD SEV-SNP attestations,
%%% as well as generating them, if called in an appropriate environment.
-module(dev_snp).
-export([generate/3, verify/3, trusted/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    {ok, Msg} = dev_message:verify_target(M1, M2, NodeOpts),
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
    Nonce = hb_converge:get(<<"nonce">>, Msg, NodeOpts),
    ?event({snp_nonce, Nonce}),
    NonceMatches = report_data_matches(Address, NodeMsgID, Nonce),
    ?event({nonce_matches, NonceMatches}),
    % Step 2: Verify the address and the signature.
    Signer = hb_converge:get(<<"signers/1">>, Msg, NodeOpts),
    ?event({snp_signer, Signer}),
    {ok, SigIsValid} = hb_message:verify(Msg),
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
    Expected = dev_snp_nif:calculate_launch_digest(JSONReport = jiffy:encode(Msg)),
    ?event({expected_measurement, Expected}),
    Measurement = hb_converge:get(<<"measurement">>, Msg, NodeOpts),
    ?event({measurement, Measurement}),
    MeasurementIsValid = dev_snp_nif:verify_measurement(JSONReport, Expected),
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
    Address = hb_util:native_id(ar_wallet:to_address(Wallet)),
    ?event(debug, {snp_wallet, Wallet}),
    % Remove the `priv*` keys from the options.
    {ok, PublicNodeMsgID} = dev_message:id(NodeMsg = hb_private:reset(Opts)),
    ?event(debug, {snp_node_msg_id, PublicNodeMsgID}),
    % Generate the attestation report.
    ReportData = generate_nonce(Address, PublicNodeMsgID),
    ReportJSON = dev_snp_nif:request_attestation_report(ReportData, 1),
    Report = jiffy:decode(ReportJSON, [return_maps]),
    ?event(debug,
        {snp_report_generated,
            {nonce, ReportData},
            {report, Report}
        }
    ),
    ReportMsg = hb_message:sign(Report#{
        <<"nonce">> => ReportData,
        <<"address">> => Address,
        <<"node-message">> => NodeMsg
    }, Wallet),
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
            undefined -> {as, ?MODULE, M1};
            Device -> {as, Device, M1}
        end,
    Result = lists:all(
        fun({ReportKey, TrustKey}) ->
            ReportVal = hb_converge:get(ReportKey, Msg, NodeOpts),
            QueryMsg = #{
                <<"path">> => <<"trusted">>,
                <<"key">> => TrustKey,
                <<"body">> => ReportVal
            },
            ?event(debug, {is_trusted_query, {base, ModM1}, {query, QueryMsg}}),
            % Resolve the query message against the modified base message.
            {ok, KeyIsTrusted} = hb_converge:resolve(ModM1, QueryMsg, NodeOpts),
            ?event(debug,
                {is_software_component_trusted,
                    {key, ReportKey},
                    {trusted, TrustKey},
                    {result, KeyIsTrusted}
                }
            ),
            KeyIsTrusted
        end,
        [
            % The list of keys, as found in the report, and the corresponding
            % keys in the trust settings.
            {<<"ovmf_hash_str">>, <<"firmware">>},
            {<<"kernel_hash">>, <<"kernel">>},
            {<<"initrd_hash">>, <<"initrd">>},
            {<<"append_hash">>, <<"append">>}
        ]
    ),
    ?event(debug, {is_all_software_trusted, Result}),
    Result.

%% @doc Default implementation of a resolver for trusted software. Searches the
%% `trusted` key in the base message for a list of trusted values, and checks
%% if the value in the request message is a member of that list.
trusted(Msg1, Msg2, NodeOpts) ->
    Key = hb_converge:get(<<"key">>, Msg2, NodeOpts),
    TrustedList = hb_converge:get([<<"trusted">>, Key], Msg1, [], NodeOpts),
    Body = hb_converge:get(<<"body">>, Msg2, not_found, NodeOpts),
    ?event(debug, {checking_trusted, Key, {trusted_list, TrustedList}, {body, Body}}),
    lists:member(Body, TrustedList).

%% @doc Ensure that the report data matches the expected report data.
report_data_matches(Address, NodeMsgID, ReportData) ->
    generate_nonce(Address, NodeMsgID) == ReportData.

%% @doc Generate the nonce to use in the attestation report.
generate_nonce(Address, NodeMsgID) ->
    << Address/binary, NodeMsgID/binary >>.

%% @doc Generate an attestation report and emit it via HTTP.
generate_test() ->
    Wallet = ar_wallet:new(),
    Addr = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Node = hb_http_server:start_test_node(
        #{
            force_signed => true,
            wallet => Wallet
        }
    ),
    {ok, Report} = hb_http:get(Node, <<"/!snp@1.0/generate">>, #{}),
    ?event(debug, {snp_report_rcvd, Report}),
    ?assertEqual({ok, Addr}, hb_converge:get(<<"address">>, Report, #{})),
    {ok, ValidationRes} = verify(#{}, Report, #{}),
    ?event(debug, {snp_validation_res, ValidationRes}),
    ?assertEqual({ok, true}, ValidationRes).
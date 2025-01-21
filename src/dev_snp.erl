%%% @doc This device offers an interface for validating AMD SEV-SNP attestations,
%%% as well as generating them, if called in an appropriate environment.
-module(dev_snp).
-export([generate/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Verify an attestation report message; validating the identity of a 
%% remote node, its ephemeral private address, and the integrity of the report.
verify(M1, _M2, _NodeOpts) ->
    % Extract data that is used in the attestation nonce.
    Address = hb_converge:get(<<"address">>, M1, #{}),
    NodeMsgID =
        case hb_converge:get(<<"node-message">>, M1, #{}) of
            undefined ->
                case hb_converge:get(<<"node-message-id">>, M1, #{}) of
                    undefined ->
                        {error, missing_node_msg_id};
                    ID -> ID
                end;
            NodeMsg -> hb_util:ok(dev_message:id(NodeMsg))
        end,
    % Extract hashes used in the expected measurement.
    FirmwareHash = hb_converge:get(<<"firmware">>, M1, #{}),
    KernelHash = hb_converge:get(<<"kernel">>, M1, #{}),
    VMSAsHash = hb_converge:get(<<"vmsas">>, M1, #{}),
    % Extract the attestation report and components.
    Report = hb_converge:get(<<"report">>, M1, #{}),
    ReportData = extract(report_data, Report),
    Measurement = extract(measurement, Report),
    ExpectedMeasurement =
        generate_measurement(FirmwareHash, KernelHash, VMSAsHash),
    case Measurement == ExpectedMeasurement of
        false -> {error, measurement_mismatch};
        true ->
            case << Address/binary, NodeMsgID/binary >> == ReportData of
                false -> {error, report_data_mismatch};
                true ->
                    case dev_snp_attestation:verify(Report) of
                        {error, Reason} -> {error, Reason};
                        ok -> ok
                    end
            end
    end.

%% @doc Generate an attestation report and emit it as a message, including all of 
%% the necessary data to generate the nonce (ephemeral node address + node
%% message ID), as well as the expected measurement (firmware, kernel, and VMSAs
%% hashes).
generate(_M1, _M2, Opts) ->
    Wallet = hb_opts:get(wallet, no_valid_wallet, Opts),
    Address = hb_util:native_id(ar_wallet:to_address(Wallet)),
    ?event(debug, {snp_wallet, Wallet}),
    % Remove the `priv*` keys from the options.
    {ok, NodeMsgID} = dev_message:id(hb_private:reset(Opts)),
    ?event(debug, {snp_node_msg_id, NodeMsgID}),
    % Generate the attestation report.
    ReportData = << Address/binary, NodeMsgID/binary >>,
    Report = dev_snp_attestation:generate(ReportData),
    ?event(debug,
        {snp_report_generated,
            {report_data, ReportData},
            {report, Report}
        }
    ),
    ReportMsg = #{
        <<"report">> => Report,
        <<"report-data">> => ReportData,
        <<"node-message">> => NodeMsgID,
        <<"address">> => Address,
        <<"firmware">> => <<0:384>>, % Grab the firmware hash from the node
        <<"kernel">> => <<0:384>>, % Grab the kernel hash from the node
        <<"vmsas">> => <<0:384>> % Grab the VMSAs hash from the node
    },
    {ok, ReportMsg}.

%% @doc Given the firmware, kernel, and VMSAs hashes, generate the expected
%% measurement for the attestation report.
generate_measurement(FirmwareHash, KernelHash, VMSAsHash) ->
    S0 = crypto:hash_init(sha384),
    S1 = crypto:hash_update(S0, FirmwareHash),
    S2 = crypto:hash_update(S1, KernelHash),
    S3 = crypto:hash_update(S2, VMSAsHash),
    crypto:hash_final(S3).

extract(measurement, _Report) -> <<0:384>>;
extract(report_data, _Report) -> <<0:256>>.

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
    {ok, Report} = hb_http:get(Node, <<"/!snp/generate">>, #{}),
    ?event(debug, {snp_report_rcvd, Report}),
    ?assertEqual({ok, Addr}, hb_converge:get(<<"address">>, Report, #{})),
    {ok, ValidationRes} = verify(#{}, Report, #{}),
    ?event(debug, {snp_validation_res, ValidationRes}),
    ?assertEqual({ok, true}, ValidationRes).

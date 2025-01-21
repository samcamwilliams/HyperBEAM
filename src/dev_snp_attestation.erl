%%%---------------------------------------------------------------------
%%% Module: sec_tee
%%%---------------------------------------------------------------------
%%% Purpose:
%%% This module handles SEV-SNP attestation and verification processes.
%%% It generates attestation reports, retrieves necessary certificates,
%%% and verifies the attestation against AMD's root of trust using the
%%% snpguest and OpenSSL commands.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% generate_attestation(Nonce)
%%%   Generates an attestation report and retrieves certificates.
%%%   Returns a binary with the attestation report and public key.
%%%
%%% verify_attestation(AttestationBinary)
%%%   Verifies the attestation report against the VCEK certificate
%%%   and AMD root of trust.
%%%---------------------------------------------------------------------
-module(dev_snp_attestation).
-export([
    generate_attestation/1,
    verify_attestation/1
]).
-include("include/hb.hrl").

%% Define the file paths
-define(ROOT_DIR, "/tmp/tee").
-define(REQUEST_FILE, ?ROOT_DIR ++ "/request-file.txt").
-define(REPORT_FILE, ?ROOT_DIR ++ "/report.bin").
-define(CERT_CHAIN_FILE, ?ROOT_DIR ++ "/cert_chain.pem").
-define(VCEK_FILE, ?ROOT_DIR ++ "/vcek.pem").

%% Define the commands
-define(SNP_GUEST_REPORT_CMD, "snpguest report " ++ ?REPORT_FILE ++ " " ++ ?REQUEST_FILE).
-define(SNP_GUEST_CERTIFICATES_CMD, "snpguest certificates PEM " ++ ?ROOT_DIR).
-define(VERIFY_VCEK_CMD, "openssl verify --CAfile  " ++ ?CERT_CHAIN_FILE ++ " " ++ ?VCEK_FILE).
-define(VERIFY_REPORT_CMD, "snpguest verify attestation " ++ ?ROOT_DIR ++ " " ++ ?REPORT_FILE).

%% Temporarily hard-code the VCEK download command
-define(DOWNLOAD_VCEK_CMD,
    "curl --proto \'=https\' --tlsv1.2 -sSf https://kdsintf.amd.com/vcek/v1/Milan/cert_chain -o " ++ ?CERT_CHAIN_FILE
).

%% Generate attestation, request certificates, download VCEK, and upload a transaction
generate_attestation(Nonce) ->
    % Check if the root directory exists, and create it if not
    case filelib:is_dir(?ROOT_DIR) of
        true -> ok;
        false -> file:make_dir(?ROOT_DIR)
    end,
    % Debug: Print starting attestation generation
    ?event("Starting attestation generation..."),
    % Generate request file and attestation report
    ?event("Generating request file with nonce..."),
    generate_request_file(Nonce),
    ?event("Generating attestation report..."),
    generate_attestation_report(),
    % Request certificates, download VCEK, and upload the attestation
    ?event("Fetching certificates from host memory..."),
    fetch_certificates(),
    ?event("Downloading VCEK root of trust certificate..."),
    download_vcek_cert(),
    % Debug: Print reading the attestation report and public key
    ?event("Reading the attestation report and public key..."),
    % Ensure that read_file returns the binary data as expected
    {_, ReportBin} = sec_helpers:read_file(?REPORT_FILE),
    {_, PublicKeyBin} = sec_helpers:read_file(?VCEK_FILE),
    % Get sizes of the individual files (in binary)
    ReportSize = byte_size(ReportBin),
    PublicKeySize = byte_size(PublicKeyBin),
    % Debug: Print the sizes of the files
    ?event({"Report size", ReportSize}),
    ?event({"Public key size", PublicKeySize}),
    % Create a binary header with the sizes and offsets
    Header = <<ReportSize:32/unit:8, PublicKeySize:32/unit:8>>,
    % Create a binary with both the report and public key data concatenated 
    % after the header
    AttestationBinary = <<Header/binary, ReportBin/binary, PublicKeyBin/binary>>,
    % Debug: Print the final binary data size
    ?event({"Generated attestation binary size", byte_size(AttestationBinary)}),
    % Return the binary containing the attestation data
    {ok, AttestationBinary}.

%% @doc Helper to generate the request file with the padded address and nonce
generate_request_file(Nonce) ->
    RequestFile = ?REQUEST_FILE,
    NonceHex = binary_to_list(binary:encode_hex(Nonce)),
    % Debug: Print the nonce
    ?event({"Nonce in hex", NonceHex}),
    case file:write_file(RequestFile, NonceHex) of
        ok ->
            ?event({"Request file written successfully", RequestFile}),
            ok;
        {error, Reason} ->
            ?event({"Failed to write request file", RequestFile, Reason}),
            {error, failed_to_write_request_file}
    end.

%% @doc Helper to generate the attestation report
generate_attestation_report() ->
    ?event({generating_snp_report, ?SNP_GUEST_REPORT_CMD}),
    case run_command(?SNP_GUEST_REPORT_CMD) of
        {ok, _} ->
            ?event("SEV-SNP report generated successfully"),
            ok;
        {error, Reason} ->
            ?event({"Failed to generate SEV-SNP report", Reason}),
            {error, failed_to_generate_report}
    end.

%% Verify the attestation report using snpguest and VCEK certificate
verify_attestation(AttestationBinary) ->
    % Extract the header (size info)
    <<ReportSize:32/unit:8, PublicKeySize:32/unit:8, Rest/binary>>
        = AttestationBinary,
    % Extract the individual components using the sizes from the header
    <<ReportData:ReportSize/binary, Rest1/binary>> = Rest,
    <<PublicKeyData:PublicKeySize/binary>> = Rest1,
    % Debug: Print the extracted components
    ?event({extracted_report_data, ReportData}),
    ?event({extracted_public_key_data, PublicKeyData}),
    % Write the components to temporary files (if needed for verification)
    file:write_file(?REPORT_FILE, ReportData),
    file:write_file(?VCEK_FILE, PublicKeyData),
    % Verify the VCEK certificate
    ?event({verifying_vcek_certificate, ?VERIFY_VCEK_CMD}),
    case run_command(?VERIFY_VCEK_CMD) of
        {ok, CertOutput} ->
            TrimmedOutput = string:trim(CertOutput),
            ?event({vcek_certificate_verification_output, TrimmedOutput}),
            % Compute outside the guard
            ExpectedOutput = ?VCEK_FILE ++ ": OK",
            if
                TrimmedOutput =:= ExpectedOutput ->
                    ?event({vcek_certificate_signature_verified_successfully}),
                    verify_attestation_report();
                true ->
                    ?event({vcek_signature_verification_failed, CertOutput}),
                    {error, invalid_signature}
            end;
        {error, Reason} ->
            ?event({failed_to_verify_vcek_signature, Reason}),
            {error, verification_failed}
    end.

%% Verify the attestation report
verify_attestation_report() ->
    ?event({verifying_attestation_report, ?VERIFY_REPORT_CMD}),
    case run_command(?VERIFY_REPORT_CMD) of
        {ok, Output} ->
            ?event({attestation_verification_result, Output}),
            case string:find(Output, "VEK signed the Attestation Report!", leading) of
                nomatch ->
                    ?event({attestation_verification_failed}),
                    {error, verification_failed};
                _ ->
                    ?event({attestation_verified_successfully}),
                    {ok, Output}
            end;
        {error, Reason} ->
            ?event({failed_to_verify_attestation, Reason}),
            {error, verification_failed}
    end.

%% Fetch certificates from host memory and store as PEM files
fetch_certificates() ->
    ?event({fetching_sev_snp_certificates, ?SNP_GUEST_CERTIFICATES_CMD}),
    case run_command(?SNP_GUEST_CERTIFICATES_CMD) of
        {ok, _} ->
            ?event({certificates_fetched_successfully}),
            ok;
        {error, Reason} ->
            ?event({failed_to_fetch_certificates, Reason}),
            {error, failed_to_fetch_certificates}
    end.

%% Download VCEK root of trust certificate !!TEMPORARY!!
download_vcek_cert() ->
    ?event({downloading_vcek_root_of_trust_certificate, ?DOWNLOAD_VCEK_CMD}),
    case run_command(?DOWNLOAD_VCEK_CMD) of
        {ok, _} ->
            ?event({vcek_root_of_trust_certificate_downloaded_successfully}),
            ok;
        {error, Reason} ->
            ?event({failed_to_download_vcek_certificate, Reason}),
            {error, failed_to_download_cert}
    end.

%%% Helpers

%% @doc Generalized function to run a shell command, hiding the stdout.
run_command(Command) ->
    ?event({"Executing command", Command}),
    Output = os:cmd(Command ++ " 2>&1"),
    {ok, Output}.
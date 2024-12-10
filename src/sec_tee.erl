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

-module(sec_tee).
-export([
    generate_attestation/1,
    verify_attestation/1
]).
-include("include/hb.hrl").
-hb_debug(print).

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

    % Create a binary with both the report and public key data concatenated after the header
    AttestationBinary = <<Header/binary, ReportBin/binary, PublicKeyBin/binary>>,

    % Debug: Print the final binary data size
    ?event({"Generated attestation binary size", byte_size(AttestationBinary)}),

    % Return the binary containing the attestation data
    {ok, AttestationBinary}.

%% Helper to generate the request file with the padded address and nonce
generate_request_file(Nonce) ->
    RequestFile = ?REQUEST_FILE,
    NonceHex = binary_to_list(binary:encode_hex(Nonce)),
    % Debug: Print the nonce
    ?event({"Nonce in hex", NonceHex}),
    case sec_helpers:write_to_file(RequestFile, NonceHex) of
        {"Written data to file", FilePath} when FilePath == RequestFile ->
            ?event({"Request file written successfully", RequestFile}),
            ok;
        {error, Reason} ->
            ?event({"Failed to write request file", RequestFile, Reason}),
            {error, failed_to_write_request_file}
    end.

% Helper to generate the attestation report
generate_attestation_report() ->
    Command = ?SNP_GUEST_REPORT_CMD,
    ?event({"Running command to generate attestation report", Command}),
    case sec_helpers:run_command(Command) of
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
    <<ReportSize:32/unit:8, PublicKeySize:32/unit:8, Rest/binary>> = AttestationBinary,

    % Extract the individual components using the sizes from the header
    <<ReportData:ReportSize/binary, Rest1/binary>> = Rest,
    <<PublicKeyData:PublicKeySize/binary>> = Rest1,

    % Debug: Print the extracted components
    ?event({"Extracted report data", ReportData}),
    ?event({"Extracted public key data", PublicKeyData}),

    % Write the components to temporary files (if needed for verification)
    sec_helpers:write_to_file(?REPORT_FILE, ReportData),
    sec_helpers:write_to_file(?VCEK_FILE, PublicKeyData),

    % Verify the VCEK certificate
    ?event("Verifying VCEK certificate..."),
    case sec_helpers:run_command(?VERIFY_VCEK_CMD) of
        {ok, CertOutput} ->
            TrimmedOutput = string:trim(CertOutput),
            ?event({"VCEK certificate verification output", TrimmedOutput}),
            % Compute outside the guard
            ExpectedOutput = ?VCEK_FILE ++ ": OK",
            if
                TrimmedOutput =:= ExpectedOutput ->
                    ?event("VCEK certificate signature verified successfully"),
                    verify_attestation_report();
                true ->
                    ?event({"VCEK signature verification failed", CertOutput}),
                    {error, invalid_signature}
            end;
        {error, Reason} ->
            ?event({"Failed to verify VCEK signature", Reason}),
            {error, verification_failed}
    end.

%% Verify the attestation report
verify_attestation_report() ->
    Command = ?VERIFY_REPORT_CMD,
    ?event({"Running command to verify attestation report", Command}),
    case sec_helpers:run_command(Command) of
        {ok, Output} ->
            ?event({"Attestation verification result", Output}),
            case string:find(Output, "VEK signed the Attestation Report!", leading) of
                nomatch ->
                    ?event("Attestation verification failed"),
                    {error, verification_failed};
                _ ->
                    ?event("Attestation verified successfully"),
                    {ok, Output}
            end;
        {error, Reason} ->
            ?event({"Failed to verify attestation", Reason}),
            {error, verification_failed}
    end.

%% Fetch certificates from host memory and store as PEM files
fetch_certificates() ->
    Command = ?SNP_GUEST_CERTIFICATES_CMD,
    ?event({"Fetching SEV-SNP certificates from host memory", Command}),
    case sec_helpers:run_command(Command) of
        {ok, _} ->
            ?event("Certificates fetched successfully"),
            ok;
        {error, Reason} ->
            ?event({"Failed to fetch certificates", Reason}),
            {error, failed_to_fetch_certificates}
    end.

%% Download VCEK root of trust certificate !!TEMPORARY!!
download_vcek_cert() ->
    Command = ?DOWNLOAD_VCEK_CMD,
    ?event({"Downloading VCEK root of trust certificate", Command}),
    case sec_helpers:run_command(Command) of
        {ok, _} ->
            ?event("VCEK root of trust certificate downloaded successfully"),
            ok;
        {error, Reason} ->
            ?event({"Failed to download VCEK certificate", Reason}),
            {error, failed_to_download_cert}
    end.
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
-include("include/ao.hrl").
-ao_debug(print).

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
-define(VERIFY_REPORT_CMD, "snpguest verify attestation " ++ ?ROOT_DIR ++  " " ++ ?REPORT_FILE).

%% Temporarily hard-code the VCEK download command
-define(DOWNLOAD_VCEK_CMD, "curl --proto \'=https\' --tlsv1.2 -sSf https://kdsintf.amd.com/vcek/v1/Milan/cert_chain -o " ++ ?CERT_CHAIN_FILE).

%% Generate attestation, request certificates, download VCEK, and upload a transaction
generate_attestation(Nonce) ->
    % Check if the root directory exists, and create it if not
    case filelib:is_dir(?ROOT_DIR) of
        true -> ok;
        false -> file:make_dir(?ROOT_DIR)
    end,

    % Debug: Print starting attestation generation
    ?c("Starting attestation generation..."),

    % Generate request file and attestation report
    ?c("Generating request file with nonce..."),
    generate_request_file(Nonce),
    ?c("Generating attestation report..."),
    generate_attestation_report(),

    % Request certificates, download VCEK, and upload the attestation
    ?c("Fetching certificates from host memory..."),
    fetch_certificates(),
    ?c("Downloading VCEK root of trust certificate..."),
    download_vcek_cert(),

    % Debug: Print reading the attestation report and public key
    ?c("Reading the attestation report and public key..."),
    
    % Ensure that read_file returns the binary data as expected
    {_, ReportData} = sec_helpers:read_file(?REPORT_FILE),
    {_, PublicKeyData} = sec_helpers:read_file(?VCEK_FILE),

    % Ensure the read data is in binary format (already handled by read_file)
    ReportBin = list_to_binary(ReportData),
    PublicKeyBin = list_to_binary(PublicKeyData),

    % Get sizes of the individual files (in binary)
    ReportSize = byte_size(ReportBin),
    PublicKeySize = byte_size(PublicKeyBin),

    % Debug: Print the sizes of the files
    ?c({"Report size", ReportSize}),
    ?c({"Public key size", PublicKeySize}),

    % Create a binary header with the sizes and offsets
    Header = <<ReportSize:32/unit:8, PublicKeySize:32/unit:8>>,

    % Create a binary with both the report and public key data concatenated after the header
    AttestationBinary = <<Header/binary, ReportBin/binary, PublicKeyBin/binary>>,

    % Debug: Print the final binary data size
    ?c({"Generated attestation binary size", byte_size(AttestationBinary)}),

    % Return the binary containing the attestation data
    {ok, AttestationBinary}.


%% Helper to generate the request file with the padded address and nonce
generate_request_file(Nonce) ->
    RequestFile = ?REQUEST_FILE,
    NonceHex = binary_to_list(binary:encode_hex(Nonce)),
    % Debug: Print the nonce
    ?c({"Nonce in hex", NonceHex}),
    case sec_helpers:write_to_file(RequestFile, NonceHex) of
        {"Written data to file", FilePath} when FilePath == RequestFile -> 
            ?c({"Request file written successfully", RequestFile}),
            ok;
        {error, Reason} -> 
            ?c({"Failed to write request file", RequestFile, Reason}),
            {error, failed_to_write_request_file}
    end.


% Helper to generate the attestation report
generate_attestation_report() -> 
    Command = ?SNP_GUEST_REPORT_CMD,
    ?c({"Running command to generate attestation report", Command}),
    case sec_helpers:run_command(Command) of
        {ok, _} -> 
            ?c("SEV-SNP report generated successfully"),
            ok;
        {error, Reason} -> 
            ?c({"Failed to generate SEV-SNP report", Reason}),
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
    ?c({"Extracted report data", ReportData}),
    ?c({"Extracted public key data", PublicKeyData}),

    % Write the components to temporary files (if needed for verification)
    sec_helpers:write_to_file(?REPORT_FILE, ReportData),
    sec_helpers:write_to_file(?VCEK_FILE, PublicKeyData),

    % Verify the VCEK certificate
    ?c("Verifying VCEK certificate..."),
    case sec_helpers:run_command(?VERIFY_VCEK_CMD) of
        {ok, CertOutput} -> 
            TrimmedOutput = string:trim(CertOutput),
            ?c({"VCEK certificate verification output", TrimmedOutput}),
            ExpectedOutput = ?VCEK_FILE ++ ": OK", % Compute outside the guard
            if
                TrimmedOutput =:= ExpectedOutput -> 
                    ?c("VCEK certificate signature verified successfully"),
                    verify_attestation_report();
                true -> 
                    ?c({"VCEK signature verification failed", CertOutput}),
                    {error, invalid_signature}
            end;
        {error, Reason} -> 
            ?c({"Failed to verify VCEK signature", Reason}),
            {error, verification_failed}
    end.

%% Verify the attestation report
verify_attestation_report() -> 
    Command = ?VERIFY_REPORT_CMD,
    ?c({"Running command to verify attestation report", Command}),
    case sec_helpers:run_command(Command) of
        {ok, Output} -> 
            ?c({"Attestation verification result", Output}),
            case string:find(Output, "VEK signed the Attestation Report!", leading) of
                nomatch -> 
                    ?c("Attestation verification failed"),
                    {error, verification_failed};
                _ -> 
                    ?c("Attestation verified successfully"),
                    {ok, Output}
            end;
        {error, Reason} -> 
            ?c({"Failed to verify attestation", Reason}),
            {error, verification_failed}
    end.

%% Fetch certificates from host memory and store as PEM files
fetch_certificates() -> 
    Command = ?SNP_GUEST_CERTIFICATES_CMD,
    ?c({"Fetching SEV-SNP certificates from host memory", Command}),
    case sec_helpers:run_command(Command) of
        {ok, _} -> 
            ?c("Certificates fetched successfully"),
            ok;
        {error, Reason} -> 
            ?c({"Failed to fetch certificates", Reason}),
            {error, failed_to_fetch_certificates}
    end.

%% Download VCEK root of trust certificate !!TEMPORARY!!
download_vcek_cert() -> 
    Command = ?DOWNLOAD_VCEK_CMD,
    ?c({"Downloading VCEK root of trust certificate", Command}),
    case sec_helpers:run_command(Command) of
        {ok, _} -> 
            ?c("VCEK root of trust certificate downloaded successfully"),
            ok;
        {error, Reason} -> 
            ?c({"Failed to download VCEK certificate", Reason}),
            {error, failed_to_download_cert}
    end.

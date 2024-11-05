%%%---------------------------------------------------------------------
%%% Module: sec
%%%---------------------------------------------------------------------
%%% Purpose: 
%%% This module handles the generation and verification of attestation 
%%% reports using both TPM and SEV-SNP. It combines attestation reports 
%%% from both technologies into a single binary and provides functionality 
%%% to verify the combined reports. 
%%%
%%% It uses the `sec_tpm` and `sec_tee` modules to interact with the TPM 
%%% hardware and SEV-SNP for generating and verifying attestation reports.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% generate_attestation(Nonce)
%%%   Generates a combined attestation report using the provided nonce.
%%%   It generates attestation reports from both TPM and SEV-SNP, calculates
%%%   their sizes, creates a header containing the sizes, and combines them 
%%%   into a single binary.
%%%
%%% verify_attestation(AttestationBinary)
%%%   Verifies the provided attestation binary by extracting the TPM and 
%%%   SEV-SNP reports, verifying them using their respective verification 
%%%   methods, and then combining the results into a single binary.
%%%---------------------------------------------------------------------

-module(sec).
-export([generate_attestation/1, verify_attestation/1]).

-include("include/ao.hrl").

-ao_debug(print).

%% Generate attestation based on the provided nonce (both TPM and SEV-SNP)
generate_attestation(Nonce) ->
    ?c({"Generating TPM attestation..."}),

    case sec_tpm:generate_attestation(Nonce) of
        {ok, TPMAttestation} ->
            ?c({"TPM attestation generated, size:", byte_size(TPMAttestation)}),

            ?c({"Generating SEV-SNP attestation..."}),

            case sec_tee:generate_attestation(Nonce) of
                {ok, TEEAttestation} ->
                    ?c({"SEV-SNP attestation generated, size:", byte_size(TEEAttestation)}),

                    %% Calculate sizes of the two attestation binaries
                    TPMSize = byte_size(TPMAttestation),
                    TEESize = byte_size(TEEAttestation),

                    %% Create the header containing the sizes
                    Header = <<TPMSize:32/unit:8, TEESize:32/unit:8>>,
                    ?c({"Header created, TPMSize:", TPMSize, "TEESize:", TEESize}),

                    %% Combine the header with the two attestation binaries
                    CombinedAttestation = <<Header/binary, TPMAttestation/binary, TEEAttestation/binary>>,
                    ?c({"Combined attestation binary created, total size:", byte_size(CombinedAttestation)}),

                    {ok, CombinedAttestation};

                {error, Reason} ->
                    ?c({"Error generating SEV-SNP attestation:", Reason}),
                    {error, Reason}
            end;

        {error, Reason} ->
            ?c({"Error generating TPM attestation:", Reason}),
            {error, Reason}
    end.

%% Verify attestation report based on the provided binary (both TPM and SEV-SNP)
verify_attestation(AttestationBinary) ->
    ?c("Verifying attestation..."),

    %% Extract the header (size info) and the attestation binaries
    <<TPMSize:32/unit:8, TEESize:32/unit:8, Rest/binary>> = AttestationBinary,
    ?c({"Header extracted, TPMSize:", TPMSize, "TEESize:", TEESize}),

    %% Extract the TPM and SEV-SNP attestation binaries based on their sizes
    <<TPMAttestation:TPMSize/binary, TEEAttestation:TEESize/binary>> = Rest,
    ?c({"Extracted TPM and SEV-SNP attestation binaries"}),

    %% Verify TPM attestation
    case sec_tpm:verify_attestation(TPMAttestation) of
        {ok, _TPMVerification} ->
            ?c({"TPM attestation verification completed"}),

            %% Verify SEV-SNP attestation
            case sec_tee:verify_attestation(TEEAttestation) of
                {ok, _TEEVerification} ->
                    ?c({"SEV-SNP attestation verification completed"}),

                    %% Return success if both verifications succeeded
                    {ok, "Verified"};

                {error, Reason} ->
                    ?c({"Error verifying SEV-SNP attestation:", Reason}),
                    {error, Reason}
            end;

        {error, Reason} ->
            ?c({"Error verifying TPM attestation:", Reason}),
            {error, Reason}
    end.

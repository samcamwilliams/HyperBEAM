%%%---------------------------------------------------------------------
%%% Module: sec_tpm
%%%---------------------------------------------------------------------
%%% Purpose:
%%% This module handles TPM-based attestation and key management processes.
%%% It generates attestation reports, creates and loads TPM keys, and
%%% verifies attestation reports using the TPM hardware.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% setup_keys()
%%%   Sets up the primary and attestation keys on the TPM.
%%%
%%% generate_attestation(Nonce)
%%%   Generates an attestation report using a provided nonce and returns
%%%   the attestation binary containing the quote, signature, and public key.
%%%
%%% verify_attestation(AttestationBinary)
%%%   Verifies the attestation report by comparing the quote with the
%%%   signature using TPM2's verification commands.
%%%---------------------------------------------------------------------

-module(sec_tpm).
-export([setup_keys/0, generate_attestation/1, verify_attestation/1]).

-include("include/hb.hrl").
-hb_debug(print).

%% Define the file paths
-define(ROOT_DIR, "/tmp/tpm").
-define(QUOTE_MSG_FILE, ?ROOT_DIR ++ "/quote.msg").
-define(QUOTE_SIG_FILE, ?ROOT_DIR ++ "/quote.sig").
-define(AK_PUB_FILE, ?ROOT_DIR ++ "/ak.pub").
-define(AK_PRIV_FILE, ?ROOT_DIR ++ "/ak.priv").
-define(AK_CTX_FILE, ?ROOT_DIR ++ "/ak.ctx").
-define(PRIMARY_CTX_FILE, ?ROOT_DIR ++ "/primary.ctx").

%% Define the TPM2 commands using the defined file paths
-define(TPM2_CREATEPRIMARY_CMD, "tpm2_createprimary -C e -g sha256 -G rsa -c " ++ ?PRIMARY_CTX_FILE).
-define(TPM2_CREATE_CMD,
    "tpm2_create -C " ++ ?PRIMARY_CTX_FILE ++ " -G rsa -u " ++ ?AK_PUB_FILE ++ " -r " ++ ?AK_PRIV_FILE
).
-define(TPM2_LOAD_CMD,
    "tpm2_load -C " ++ ?PRIMARY_CTX_FILE ++ " -u " ++ ?AK_PUB_FILE ++ " -r " ++ ?AK_PRIV_FILE ++ " -c " ++ ?AK_CTX_FILE
).
-define(TPM2_READPUBLIC_CMD, "tpm2_readpublic -c " ++ ?AK_CTX_FILE ++ " -o " ++ ?AK_PUB_FILE ++ " -f pem").
-define(TPM2_QUOTE_CMD,
    "tpm2_quote -Q --key-context " ++ ?AK_CTX_FILE ++ " -l sha256:0,1 --message " ++ ?QUOTE_MSG_FILE ++ " --signature " ++
        ?QUOTE_SIG_FILE ++ " --qualification "
).
-define(TPM2_VERIFY_CMD,
    "tpm2_verifysignature -c " ++ ?AK_CTX_FILE ++ " -g sha256 -m " ++ ?QUOTE_MSG_FILE ++ " -s " ++ ?QUOTE_SIG_FILE
).

%% Generate an attestation using the provided nonce (as a string)
generate_attestation(Nonce) ->
    % Check if the root directory exists, and create it if not
    case filelib:is_dir(?ROOT_DIR) of
        true -> ok;
        false -> file:make_dir(?ROOT_DIR)
    end,

    % Setup the keys
    ok = setup_keys(),

    % Use the address in hex format as the nonce
    NonceHex = binary_to_list(binary:encode_hex(Nonce)),
    Command = ?TPM2_QUOTE_CMD ++ NonceHex,
    ?event({"Running command", Command}),
    case sec_helpers:run_command(Command) of
        {ok, _} ->
            % Read the quote.msg, quote.sig, and ak.pub files
            {_, QuoteBin} = sec_helpers:read_file(?QUOTE_MSG_FILE),
            {_, SignatureBin} = sec_helpers:read_file(?QUOTE_SIG_FILE),
            {_, AkPubBin} = sec_helpers:read_file(?AK_PUB_FILE),

            % Get sizes of the individual files (in binary)
            QuoteSize = byte_size(QuoteBin),
            SignatureSize = byte_size(SignatureBin),
            AkPubSize = byte_size(AkPubBin),

            % Create a binary header with the sizes and offsets
            Header = <<QuoteSize:32/unit:8, SignatureSize:32/unit:8, AkPubSize:32/unit:8>>,

            % Create a binary with all three files' data concatenated after the header
            AttestationBinary = <<Header/binary, QuoteBin/binary, SignatureBin/binary, AkPubBin/binary>>,

            % Log the data (if you need to debug)
            ?event({"Attestation generated, binary data:", AttestationBinary}),

            % Return the binary containing the header and files
            {ok, AttestationBinary};
        {error, Reason} ->
            ?event({"Error generating attestation", Reason}),
            {error, Reason}
    end.

%% Verify the attestation using the AttestationBinary
verify_attestation(AttestationBinary) ->
    % Extract the header (size info)
    <<QuoteSize:32/unit:8, SignatureSize:32/unit:8, AkPubSize:32/unit:8, Rest/binary>> = AttestationBinary,

    % Extract the individual components using the sizes from the header
    <<QuoteData:QuoteSize/binary, Rest1/binary>> = Rest,
    <<SignatureData:SignatureSize/binary, Rest2/binary>> = Rest1,
    <<AkPubData:AkPubSize/binary>> = Rest2,

    % Write the components to temporary files (if needed for verification)
    sec_helpers:write_to_file(?QUOTE_MSG_FILE, QuoteData),
    sec_helpers:write_to_file(?QUOTE_SIG_FILE, SignatureData),
    sec_helpers:write_to_file(?AK_PUB_FILE, AkPubData),

    % Run the TPM verification command using the files
    CommandVerify = ?TPM2_VERIFY_CMD,
    ?event({"Running command", CommandVerify}),
    case sec_helpers:run_command(CommandVerify) of
        {ok, VerificationMessage} ->
            {ok, VerificationMessage};
        {error, {failed, _}} = Error ->
            ?event({"Verification failed", Error}),
            Error
    end.

%% Set up primary and attestation keys
setup_keys() ->
    ?event("Starting key setup..."),
    create_primary_key(),
    create_attestation_key().

create_primary_key() ->
    CommandPrimary = ?TPM2_CREATEPRIMARY_CMD,
    ?event({"Running command", CommandPrimary}),
    case sec_helpers:run_command(CommandPrimary) of
        {ok, _} ->
            ?event("Primary key created successfully"),
            create_attestation_key();
        {error, Reason} ->
            ?event({"Error creating primary key", Reason}),
            {error, Reason}
    end.

create_attestation_key() ->
    CommandCreateAK = ?TPM2_CREATE_CMD,
    ?event({"Running command", CommandCreateAK}),
    case sec_helpers:run_command(CommandCreateAK) of
        {ok, _} ->
            ?event("Attestation key created successfully"),
            load_attestation_key();
        {error, Reason} ->
            ?event({"Error creating attestation key", Reason}),
            {error, Reason}
    end.

load_attestation_key() ->
    CommandLoadAK = ?TPM2_LOAD_CMD,
    ?event({"Running command", CommandLoadAK}),
    case sec_helpers:run_command(CommandLoadAK) of
        {ok, _} ->
            ?event("Attestation key loaded successfully"),
            export_ak_public_key();
        {error, Reason} ->
            ?event({"Error loading attestation key", Reason}),
            {error, Reason}
    end.

%% Helper to export the AK public key
export_ak_public_key() ->
    Command = ?TPM2_READPUBLIC_CMD,
    ?event({"Running command", Command}),
    case sec_helpers:run_command(Command) of
        {ok, _} ->
            ?event("AK public key exported successfully"),
            ok;
        {error, Reason} ->
            ?event({"Error exporting AK public key", Reason}),
            {error, Reason}
    end.
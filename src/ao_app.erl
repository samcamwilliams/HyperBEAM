%%%-------------------------------------------------------------------
%% @doc supersu public API
%% @end
%%%-------------------------------------------------------------------

-module(ao_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([attest_key/0]).

-include("include/ao.hrl").

start(_StartType, _StartArgs) ->
    attest_key(),
    ao_sup:start_link(),
    ok = dev_scheduler_registry:start(),
    _TimestampServer = ar_timestamp:start(),
    {ok, _} = ao_http_router:start().

stop(_State) ->
    ok.

attest_key() ->
    W = ao:wallet(),
    Addr = ar_wallet:to_address(W),

    % Pad the address to 32 bytes (64 hex characters) for the TPM nonce
    Nonce = pad_to_size(Addr, 32),

    % Pad the address to 64 bytes (128 hex characters) for the TEE nonce
    TeeNonce = pad_to_size(Addr, 64),

    % Determine tee-technology based on the existence of TEE devices
    TeeTech = case os:cmd("test -e /dev/tdx_guest && echo tdx || (test -e /dev/sev_guest && echo sev-snp)") of
        "tdx\n" -> "tdx";
        "sev-snp\n" -> "sev-snp";
        _ -> {error, "No TEE device found"}
    end,

    % Proceed if a valid TEE technology is found
    case TeeTech of
        {error, _} -> {error, "Required TEE device not found"};
        _ ->
            Cmd = lists:flatten(io_lib:format("sudo gotpm attest --key AK --nonce ~s --tee-nonce ~s --tee-technology ~s", [Nonce, TeeNonce, TeeTech])),
            CommandResult = os:cmd(Cmd),
            case is_list(CommandResult) of
                true ->
                    % If CommandResult is a list of integers, convert it to binary
                    BinaryResult = list_to_binary(CommandResult),
                    ?c(BinaryResult),
                    Signed = ar_bundles:sign_item(
                        #tx{
                            tags = [
                                {<<"Type">>, <<"TEE-Attestation">>},
                                {<<"Address">>, ao_message:id(Addr)}
                            ],
                            data = BinaryResult
                        },
                        W
                    ),
                    ?c(Signed),
                    ao_client:upload(Signed),
                    ok;
                false ->
                    {error, "Unexpected output format from gotpm attest command"}
            end
    end.

% Pads an address to the specified byte size (in hex characters)
pad_to_size(Addr, SizeInBytes) ->
    HexAddr = binary:encode_hex(Addr),
    RequiredLength = SizeInBytes * 2,  % Convert bytes to hex characters
    Padding = RequiredLength - byte_size(HexAddr),
    lists:duplicate(Padding, $0) ++ HexAddr.

%% internal functions

%%% @doc A device that allows a node to manage wallets on behalf of users. Users
%%% are able to create wallets, then have the node sign their messages and
%%% potentially continue execution upon the result. This device is intended for
%%% use in situations in which the node is trusted by the user, for example if
%%% it is running on their own machine or in a TEE-protected environment that 
%%% they deem to be secure.
%%% 
%%% # Authentication Flow
%%% 
%%% Each wallet is associated with an authentication message that controls access.
%%% The authentication system is pluggable -- users can employ any authentication
%%% message as they desire. The default authentication message is empty aside 
%%% the device being set to `cookie@1.0`.
%%% 
%%% During wallet generation:
%%% 1. The wallet device creates the wallet and determines its name (provided by
%%%    the user or derived from the wallet address).
%%% 2. The wallet device calls the authentication message with path `generate'
%%%    and the wallet name in the request.
%%% 3. The authentication device sets up authentication (e.g., creates cookies,
%%%    secrets) and returns a response.
%%% 4. The wallet device stores both the wallet and the authentication result, 
%%%    as well as its other metadata.
%%% 5. The wallet device returns the authentication response with the wallet name
%%%    added to the `body' field.
%%% 
%%% During wallet operations (commit, export, etc.):
%%% 1. The wallet device retrieves the stored authentication message for the
%%%    wallet either from persistent storage or from the node's message.
%%% 2. The wallet device calls the authentication message with path `verify' and
%%%    the user's request.
%%% 3. The authentication device verifies the request (e.g., checks cookies).
%%% 4. If verification passes, the wallet device performs the requested operation
%%% 5. If verification fails, a 400 error is returned.
%%% 
%%% # Authentication Message Requirements
%%% 
%%% Authentication devices must support two paths:
%%% 
%%% `/generate': Called during wallet creation.
%%%  - Input: Request message containing `name' field with wallet's name.
%%%  - Output: Response message with authentication setup (cookies, tokens, etc.).
%%%            This message will be used as the `Base' message for the `verify'
%%%            path.
%%% 
%%% `/verify': Called during wallet operations.
%%%   - Base: The output of the `generate' path.
%%%   - Request: User's request message with authentication credentials.
%%%   - Output: `false' if an error has occurred. If the request is valid, the
%%%            authentication message should return either `true' or a modification
%%%            of the request message which will be used for any subsequent
%%%            operations.
%%% 
%%% The default authentication device `~cookie@1.0' uses HTTP cookies with
%%% secrets to authenticate users.
%%% 
%%% # Wallet Device Keys
%%% 
%%% As well as by direct invocation, this device may be used as a `preprocessor'
%%% hook on the node, allowing it to sign requests for users before they are
%%% executed.
%%% 
%%% The keys provided by the device are as follows:
%%% ```
%%% /generate
%%%     Parameters:
%%%     - `auth' (optional): The authentication message to use. Defaults to
%%%       `#{<<"device">> => <<"cookie@1.0">>}' if not provided.
%%%     - `wallet' (optional): The name of the wallet to generate. If not provided,
%%%       the wallet's address will be used as the name.
%%%     - `persist' (optional): How the node should persist the wallet. Options:
%%%       - `client': The wallet is generated on the server, but not persisted.
%%%                  The full wallet key is returned for the user to store.
%%%       - `in-memory': The wallet is generated on the server and persisted only
%%%                  in local memory, never written to disk.
%%%       - `non-volatile': The wallet is persisted to non-volatile storage on 
%%%                  the node.
%%%     - `exportable' (optional): Whether the wallet should be exportable to
%%%                  remote peers via the `export' key (and its security
%%%                  mechanisms). If a wallet address or list of addresses is
%%%                  provided, the wallet is exportable only via those addresses.
%%%                  Defaults to the node's `wallet_admin' option if set, or its
%%%                  operator address if not.
%%% 
%%%     Generates a new wallet and sets up authentication for it. The steps are:
%%%     1. Creates a new wallet.
%%%     2. Determines the wallet name (provided name or wallet address).
%%%     3. Calls the authentication device with path `generate' and the wallet name.
%%%     4. Stores the wallet and authentication result.
%%%     5. Returns the authentication device's response with wallet name in `body'.
%%%     
%%%     The response will contain authentication setup (such as cookies) from the
%%%     authentication device, plus the wallet name in the `body' field. The
%%%     wallet's key is not returned to the user unless the `persist' option is
%%%     set to `client'. If it is, the `~cookie@1.0' device will be employed to
%%%     set the user's cookie with the wallet's key (and name, if provided).
%%% 
%%% /import
%%%     Parameters:
%%%     - `wallet' (optional): The name of the wallet to import.
%%%     - `key' (optional): The JSON-encoded wallet to import.
%%%     - `cookie' (optional): A structured-fields cookie containing a map with
%%%       a `key' field which is a JSON-encoded wallet.
%%%     - `auth' (optional): The authentication message to use.
%%%     - `persist' (optional): How the node should persist the wallet. The
%%%       supported options are as with the `generate' key.
%%% 
%%%     Imports a wallet for hosting from the user. Executes as `generate' does,
%%%     except that it expects the key to store to be provided either directly
%%%     via the `key' parameter as a `key' field in the cookie Structured-Fields
%%%     map. Support for loading the key from the cookie is provided such that
%%%     a previously-generated wallet by the user can have its persistence mode
%%%     changed.
%%% 
%%% /list
%%% 
%%%     Lists all hosted wallets on the node.
%%% 
%%% /commit
%%%     Parameters:
%%%     - `wallet' (optional): The name of the wallet to sign with.
%%%     - Authentication credentials as required by the wallet's authentication
%%%       message (e.g., cookies for `~cookie@1.0' device).
%%% 
%%%     Signs the given message using the specified wallet after authentication.
%%%     The process:
%%%     1. Identifies the wallet (from `wallet' parameter or authentication data).
%%%     2. Retrieves the stored authentication message for that wallet.
%%%     3. Calls the authentication device with path `verify' to validate the
%%%        request.
%%%     4. If authentication succeeds, signs the entire request message using
%%%        the wallet.
%%%     5. Returns the signed message with signature attached.
%%%     
%%%     If no `wallet' parameter is provided, the request's authentication data
%%%     (such as cookies) must contain wallet identification.
%%% 
%%% /export
%%%     Parameters:
%%%     - `wallet' (optional): The name of the wallet to export.
%%%     - `cookie' (optional): The cookie to use for the wallet.
%%%     - `batch' (optional): A list of wallet names to export, or `all' to
%%%       export all wallets for which the request passes authentication.
%%% 
%%%     Exports a given wallet or set of wallets (in JSON-encoded form). If
%%%     multiple wallets are requested, the result is a message with form
%%%     `name => #{ `key` => JSON-encoded wallet, `auth' => authentication
%%%     message, `exportable' => [address, ...], `persist' => `client' |
%%%     `in-memory' | `non-volatile' }'.
%%% 
%%%     A wallet will be exported if:
%%%     - The given request passes the wallet's authentication message; or
%%%     - The `exportable' field is set to `true` and the request is signed by
%%%       the `wallet_admin' key; or
%%%     - The `exportable' field is set to a list of addresses and the request
%%%       is signed by one of those addresses.
%%% 
%%% /sync
%%%     Parameters:
%%%     - `node': The node to pull wallets from.
%%%     - `id' (optional): The identity it should use when signing its request
%%%       to the remote peer.
%%%     - `batch' (optional): A list of wallet names to export, or `all' to load
%%%       every available wallet. Defaults to `all'.
%%% 
%%%     Attempts to download all wallets from the given node and import them.
%%% '''
-module(dev_wallet).
-export([generate/3, import_wallet/3, list/3, commit/3, export_wallet/3, sync/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Generate a new wallet
generate(_Base, Request, Opts) ->
    {error, not_implemented}.

%% @doc Import a wallet
import_wallet(_Base, Request, Opts) ->
    {error, not_implemented}.

%% @doc List all hosted wallets
list(_Base, Request, Opts) ->
    {error, not_implemented}.

%% @doc Sign a message with a wallet
commit(_Base, Request, Opts) ->
    {error, not_implemented}.

%% @doc Export a wallet or set of wallets
export_wallet(_Base, Request, Opts) ->
    {error, not_implemented}.

%% @doc Sync wallets from a remote node
sync(_Base, Request, Opts) ->
    {error, not_implemented}.

%%% Helper functions

%% @doc Parse the exportable setting for a wallet and return a list of addresses
%% which are allowed to export the wallet.
parse_exportable(default, Opts) ->
    case hb_ao:get(<<"wallet_admin">>, Opts, undefined, Opts) of
        undefined -> 
            case hb_opts:get(operator, undefined, Opts) of
                undefined -> false;
                Op -> [hb_util:human_id(Op)]
            end;
        Admin -> [Admin]
    end;
parse_exportable(true, _Opts) -> true;
parse_exportable(false, _Opts) -> false;
parse_exportable(Addresses, _Opts) when is_list(Addresses) -> Addresses;
parse_exportable(Address, _Opts) when is_binary(Address) -> [Address].

%% @doc Store wallet in memory (in node message).
store_wallet_in_memory(Name, Details, Opts) ->
    % Get existing wallets
    CurrentWallets = hb_opts:get(priv_wallet_hosted, #{}, Opts),
    % Add new wallet
    UpdatedWallets = CurrentWallets#{ Name => Details },
    % Update the node's options with the new wallets.
    hb_http_server:set_opts(Opts#{ priv_wallet_hosted => UpdatedWallets }),
    ok.

%% @doc Store wallet in non-volatile storage.
store_wallet_non_volatile(Name, Details, Opts) ->
    % Find the private store of the node.
    {ok, Msg} = hb_cache:write(#{ Name => Details }, PrivOpts = generate_opts(Opts)),
    PrivStore = hb_opts:get(priv_store, undefined, PrivOpts),
    % Link the wallet to the store.
    ok = hb_store:make_link(PrivStore, <<"wallet@1.0/", Name/binary>>, Msg).

%% @doc Generate a new `Opts' message with the `priv_store' as the only `store'
%% option.
generate_opts(Opts) ->
    Opts#{ store => hb_opts:get(priv_store, undefined, Opts) }.

%%% Tests

%% @doc Helper function to test wallet generation and verification flow.
test_wallet_generate_and_verify(GeneratePath, ExpectedName, CommitParams) ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate wallet with specified parameters
    {ok, GenResponse} = hb_http:get(Node, GeneratePath, #{}),
    % Should get wallet name in body and auth cookie
    ?assertMatch(#{<<"body">> := _}, GenResponse),
    WalletName = maps:get(<<"body">>, GenResponse),
    case ExpectedName of
        undefined -> 
            % For unnamed wallets, just check it's a non-empty binary
            ?assert(is_binary(WalletName) andalso byte_size(WalletName) > 0);
        _ -> 
            % For named wallets, check exact match
            ?assertEqual(ExpectedName, WalletName)
    end,
    ?assert(
        maps:is_key(<<"set-cookie">>, GenResponse)
            orelse maps:is_key(<<"cookie">>, GenResponse)
    ),
    AuthCookie =
        maps:get(
            <<"set-cookie">>,
            GenResponse,
            maps:get(<<"cookie">>, GenResponse, <<>>)
        ),
    % Now verify by signing a message
    TestMessage = maps:merge(#{
        <<"device">> => <<"wallet@1.0">>,
        <<"path">> => <<"commit">>,
        <<"cookie">> => AuthCookie,
        <<"body">> => <<"Test message signing">>
    }, CommitParams),
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
    % Should return signed message with correct signer
    ?assertMatch(#{<<"body">> := <<"Test message signing">>}, SignedMessage),
    ?assert(hb_message:signers(SignedMessage, #{}) =:= [WalletName]).

client_persist_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=client">>,
        undefined,
        #{}
    ).

named_wallet_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=in-memory&wallet=my-named-wallet">>,
        <<"my-named-wallet">>,
        #{<<"wallet">> => <<"my-named-wallet">>}
    ).

unnamed_wallet_generate_and_verify_test() ->
    test_wallet_generate_and_verify(
        <<"/~wallet@1.0/generate?persist=in-memory">>,
        undefined,
        #{}
    ).

import_wallet_with_key_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Create a test wallet key to import (in real scenario from user).
    TestWallet = ar_wallet:new(),
    WalletKey = ar_wallet:to_json(TestWallet),
    % Import the wallet with a specific name.
    ImportUrl =
        <<"/~wallet@1.0/import?wallet=imported-wallet&persist=in-memory&key=",
            WalletKey/binary>>,
    {ok, ImportResponse} = hb_http:get(Node, ImportUrl, #{}),
    % Response should come from auth device with wallet name in body.
    ?assertMatch(#{<<"body">> := <<"imported-wallet">>}, ImportResponse),
    % Should include cookie setup from auth device.
    ?assert(
        maps:is_key(<<"set-cookie">>, ImportResponse)
            orelse maps:is_key(<<"cookie">>, ImportResponse)
    ).

import_wallet_from_cookie_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Create a structured-fields cookie with wallet key (simulating client).
    TestWallet = ar_wallet:new(),
    WalletKey = ar_wallet:to_json(TestWallet),
    % Create structured-fields format cookie.
    Cookie = <<"(wallet=test-wallet key=", WalletKey/binary, ")">>,
    % Import using the cookie to change persistence mode.
    ImportUrl =
        <<"/~wallet@1.0/import?persist=in-memory&cookie=", Cookie/binary>>,
    {ok, ImportResponse} = hb_http:get(Node, ImportUrl, #{}),
    % Response should come from auth device with wallet name in body.
    ?assertMatch(#{<<"body">> := <<"test-wallet">>}, ImportResponse),
    % Should include cookie setup from auth device.
    ?assert(
        maps:is_key(<<"set-cookie">>, ImportResponse)
            orelse maps:is_key(<<"cookie">>, ImportResponse)
    ).

list_wallets_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate some wallets first.
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=wallet-1">>,
            #{}
        ),
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=wallet-2">>,
            #{}
        ),
    % List all wallets (no authentication required for listing).
    {ok, WalletList} = hb_http:get(Node, <<"/~wallet@1.0/list">>, #{}),
    ?assert(is_list(WalletList)),
    ?assert(length(WalletList) >= 2),
    % Each wallet entry should contain basic info.
    ?assert(
        lists:all(
            fun(Wallet) ->
                lists:member(Wallet, [<<"wallet-1">>, <<"wallet-2">>])
            end,
            WalletList
        )
    ).

commit_with_named_wallet_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate a wallet for signing.
    {ok, GenResponse} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory">>,
            #{}
        ),
    WalletName = maps:get(<<"body">>, GenResponse),
    % Extract auth cookie from generation response for subsequent requests.
    AuthCookie =
        maps:get(
            <<"set-cookie">>,
            GenResponse,
            maps:get(<<"cookie">>, GenResponse, <<>>)
        ),
    % Create a message to sign using the wallet device.
    TestMessage = #{
        <<"device">> => <<"wallet@1.0">>,
        <<"path">> => <<"commit">>,
        <<"body">> => <<"Hello, world!">>,
        <<"cookie">> => AuthCookie
    },
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
    % Should return the signed message with signature attached.
    ?assertMatch(#{<<"body">> := <<"Hello, world!">>}, SignedMessage),
    ?assert(hb_message:signers(SignedMessage, #{}) =:= [WalletName]).

commit_with_cookie_wallet_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate a client wallet to get a cookie with full wallet key.
    {ok, GenResponse} =
        hb_http:get(Node, <<"/~wallet@1.0/generate?persist=client">>, #{}),
    WalletName = maps:get(<<"body">>, GenResponse),
    AuthCookie =
        maps:get(
            <<"set-cookie">>,
            GenResponse,
            maps:get(<<"cookie">>, GenResponse, <<>>)
        ),
    % Use the cookie to sign a message (no wallet parameter needed).
    TestMessage = #{
        <<"device">> => <<"wallet@1.0">>,
        <<"path">> => <<"commit">>,
        <<"cookie">> => AuthCookie,
        <<"body">> => <<"Test data">>
    },
    {ok, SignedMessage} = hb_http:post(Node, TestMessage, #{}),
    % Should return the signed message with signature attached.
    ?assert(hb_message:signers(SignedMessage, #{}) =:= [WalletName]).

export_wallet_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate a wallet to export.
    {ok, GenResponse} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory">>,
            #{}
        ),
    AuthCookie =
        maps:get(
            <<"set-cookie">>,
            GenResponse,
            maps:get(<<"cookie">>, GenResponse, <<>>)
        ),
    % Export the wallet with authentication.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/export?&cookie=", AuthCookie/binary>>,
            #{}
        ),
    % Should return wallet details including key, auth, exportable, persist.
    ?assertMatch(#{<<"key">> := _, <<"persist">> := <<"in-memory">>}, ExportResponse),
    ?assert(maps:is_key(<<"auth">>, ExportResponse)),
    ?assert(maps:is_key(<<"exportable">>, ExportResponse)).

export_batch_wallets_test() ->
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new()
    }),
    % Generate multiple wallets and collect auth cookies.
    {ok, Gen1} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=batch-1">>,
            #{}
        ),
    {ok, _Gen2} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/generate?persist=in-memory&wallet=batch-2">>,
            #{}
        ),
    % For batch export, use any valid auth cookie (admin permissions needed).
    AuthCookie =
        maps:get(<<"set-cookie">>, Gen1, maps:get(<<"cookie">>, Gen1, <<>>)),
    % Export all wallets.
    {ok, ExportResponse} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/export?batch=all&cookie=", AuthCookie/binary>>,
            #{}
        ),
    ?assert(is_map(ExportResponse)),
    ?assert(maps:size(ExportResponse) >= 2),
    % Each exported wallet should have the required structure.
    lists:foreach(
        fun({_Name, WalletData}) ->
            ?assertMatch(
                #{
                    <<"key">> := _,
                    <<"auth">> := _,
                    <<"exportable">> := _,
                    <<"persist">> := _
                },
                WalletData
            )
        end,
        maps:to_list(ExportResponse)
    ).

sync_wallets_test() ->
    Node =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new()
        }),
    % Start a second node to sync from.
    Node2 =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new()
        }),
    % Generate a wallet on the second node.
    {ok, GenResponse} =
        hb_http:get(
            Node2,
            <<"/~wallet@1.0/generate?persist=in-memory">>,
            #{}
        ),
    WalletName = maps:get(<<"body">>, GenResponse),
    % Test sync to the first node from the second.
    {ok, _} =
        hb_http:get(
            Node,
            <<"/~wallet@1.0/sync?node=", Node2/binary, "&batch=all">>,
            #{}
        ),
    % Get the wallet list from the first node.
    {ok, WalletList} = hb_http:get(Node, <<"/~wallet@1.0/list">>, #{}),
    % Should return a map of successfully imported wallets or list of names.
    ?assert(lists:member(WalletName, WalletList)).
%%% @doc A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an attestation that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.
-module(hb_store_remote_node).
-export([scope/1, type/2, read/2, write/3, resolve/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Return the scope of this store.
%%
%% For the remote store, the scope is always `remote'.
%%
%% @param Arg An arbitrary parameter (ignored).
%% @returns remote.
scope(Arg) ->
    ?event(hb_store_remote_node, {scope, {arg, Arg}}),
    remote.

%% @doc Resolve a key path in the remote store.
%%
%% For the remote node store, the key is returned as-is.
%%
%% @param Data A map containing node configuration.
%% @param Key The key to resolve.
%% @returns The resolved key.
resolve(#{ <<"node">> := Node }, Key) ->
    ?event(hb_store_remote_node, {resolve, {node, Node}, {key, Key}}),
    Key.

%% @doc Determine the type of value at a given key.
%%
%% Remote nodes support only the `simple' type or `not_found'.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key whose value type is determined.
%% @returns simple if found, or not_found otherwise.
type(Opts = #{ <<"node">> := Node }, Key) ->
    ?event(hb_store_remote_node, {type, {node, Node}, {key, Key}}),
    case read(Opts, Key) of
        not_found -> not_found;
        _ -> simple
    end.

%% @doc Read a key from the remote node.
%%
%% Makes an HTTP GET request to the remote node and returns the
%% attested message.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key to read.
%% @returns {ok, Msg} on success or not_found if the key is missing.
read(Opts = #{ <<"node">> := Node }, Key) ->
    ?event(hb_store_remote_node, {read, {node, Node}, {key, Key}}),
    case hb_http:get(Node, Key, Opts) of
        {ok, Res} ->
            {ok, Msg} = hb_message:with_only_attested(Res),
            ?event(hb_store_remote_node, {read, {result, Msg}}),
            {ok, Msg};
        {error, _Err} ->
            ?event(hb_store_remote_node, {read, {result, not_found}}),
            not_found
    end.

%% @doc Write a key to the remote node.
%%
%% Constructs an HTTP POST write request. If a wallet is provided,
%% the message is signed. Returns {ok, Path} on HTTP 200, or
%% {error, Reason} on failure.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key to write.
%% @param Value The value to store.
%% @returns {ok, Path} on success or {error, Reason} on failure.
write(Opts = #{ <<"node">> := Node }, Key, Value) ->
    ?event(hb_store_remote_node, {write, {node, Node},
                                  {key, Key}, {value, Value}}),
    WriteMsg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Value
    },
    Wallet = hb:wallet(),
    SignedMsg = case Wallet of
        undefined -> WriteMsg;
        _ -> hb_message:attest(WriteMsg, Wallet)
    end,
    ?event(hb_store_remote_node, {write, {signed, SignedMsg}}),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            Status = hb_converge:get(<<"status">>, Response, 0, #{}),
            Path = hb_converge:get(<<"path">>, Response, <<>>, #{}),
            ?event(hb_store_remote_node, {write, {response, Response}}),
            case Status of
                200 -> {ok, Path};
                _ -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            ?event(hb_store_remote_node, {write, {error, Err}}),
            {error, Err}
    end.

%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

%% @doc Test that we can create a store, write a random message to it, then
%% start a remote node with that store, and read the message from it.
read_test() ->
    rand:seed(default),
    LocalStore = #{ 
		<<"store-module">> => hb_store_fs,
		<<"prefix">> => <<"cache-mainnet">> 
	},
    hb_store:reset(LocalStore),
    M = #{ <<"test-key">> => Rand = rand:uniform(1337) },
    ID = hb_message:id(M),
    {ok, ID} =
        hb_cache:write(
			M, 
			#{ store => LocalStore }
		),
    ?event({wrote, ID}),
    Node =
        hb_http_server:start_node(
            #{
                store => LocalStore
            }
        ),
    RemoteStore = [
		#{ <<"store-module">> => hb_store_remote_node, <<"node">> => Node }
	],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{ store => RemoteStore }),
    ?assertMatch(#{ <<"test-key">> := Rand }, RetrievedMsg).

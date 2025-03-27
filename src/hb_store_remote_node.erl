%%% @doc A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an attestation that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.
-module(hb_store_remote_node).
-export([scope/1, type/2, read/2, write/3, resolve/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Returns the scope of this store, which is always 'remote'.
scope(_) -> remote.

%% @doc Resolves a key path in the remote store context.
%% For the remote node store, the key is returned as-is.
resolve(#{ <<"node">> := Node }, Key) ->
    ?event({resolving_to_self, Node, Key}),
    Key.

%% @doc Determines the type of value at the given key.
%% Remote nodes only support 'simple' type or 'not_found'.
type(Opts = #{ <<"node">> := Node }, Key) ->
    ?event({remote_type, Node, Key}),
    case read(Opts, Key) of
        not_found -> not_found;
        _ -> simple
    end.

%% @doc Reads a key from the remote node.
%% Makes an HTTP request to the remote node and returns the attested message.
read(Opts = #{ <<"node">> := Node }, Key) ->
    case hb_http:get(Node, Key, Opts) of
        {ok, Res} ->
            {ok, Msg} = hb_message:with_only_attested(Res),
            {ok, Msg};
        {error, _Err} ->
            not_found
    end.

%% @doc Writes a key to the remote node.
write(Opts = #{ <<"node">> := Node }, Key, Value) ->
    ?event({writing, Key, Value, Opts}),
    % Create a write request message
    WriteMsg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Value
    },
    
    % Sign the message if wallet is provided
    Wallet = hb:wallet(),
    SignedMsg = case Wallet of
        undefined -> WriteMsg;
        _ -> hb_message:attest(WriteMsg, Wallet)
    end,
    
    % Send the write request
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} -> 
            Status = hb_converge:get(<<"status">>, Response, 0, #{}),
			Path = hb_converge:get(<<"path">>, Response, <<>>, #{}),
            case Status of
                200 -> {ok, Path};
                _ -> {error, {unexpected_status, Status}}
            end;
        {error, Err} -> {error, Err}
    end.

%%% Tests

%% @doc Test that we can create a store, write a random message to it, then start
%% a remote node with that store, and read the message from it.
read_test() ->
    rand:seed(default),
    LocalStore = #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> },
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
    RemoteStore = [#{ <<"store-module">> => hb_store_remote_node, <<"node">> => Node }],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{ store => RemoteStore }),
    ?assertMatch(#{ <<"test-key">> := Rand }, RetrievedMsg).

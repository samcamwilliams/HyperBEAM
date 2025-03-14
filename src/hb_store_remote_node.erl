%%% @doc A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an attestation that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.
-module(hb_store_remote_node).
-export([scope/1, type/2, read/2, resolve/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

scope(_) -> remote.

resolve(#{ <<"node">> := Node }, Key) ->
    ?event({resolving_to_self, Node, Key}),
    Key.

type(Opts = #{ <<"node">> := Node }, Key) ->
    ?event({remote_type, Node, Key}),
    case read(Opts, Key) of
        not_found -> not_found;
        _ -> simple
    end.

read(Opts = #{ <<"node">> := Node }, Key) ->
    ?event({reading, Key, Opts}),
    case hb_http:get(Node, Key, Opts) of
        {ok, Res} ->
            {ok, Msg} = hb_message:with_only_attested(Res),
            {ok, Msg};
        {error, Err} ->
            ?event({read_error, Key, Err}),
            not_found
    end.

%%% Tests

%% @doc Test that we can create a store, write a random message to it, then start
%% a remote node with that store, and read the message from it.
read_test() ->
    rand:seed(default),
    LocalStore = {hb_store_fs, #{ prefix => "mainnet-cache" }},
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
    RemoteStore = [{hb_store_remote_node, #{ <<"node">> => Node }}],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{ store => RemoteStore }),
    ?assertMatch(#{ <<"test-key">> := Rand }, RetrievedMsg).

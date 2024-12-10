-module(hb_store_remote_node).
-export([scope/1, type/2, read/2, resolve/2]).
-include("include/hb.hrl").

%%% A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an attestation that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.

scope(_) -> remote.

resolve(#{ node := Node }, Key) ->
    ?event({resolving_to_self, Node, Key}),
    Key.

type(Opts = #{ node := Node }, Key) ->
    ?no_prod("No need to get the whole message in order to get its type..."),
    ?event({remote_type, Node, Key}),
    case read(Opts, Key) of
        not_found -> not_found;
        #tx { data = Map } when is_map(Map) -> composite;
        _ -> simple
    end.

read(Opts, Key) when is_binary(Key) ->
    read(Opts, binary_to_list(Key));
read(Opts = #{ node := Node }, Key) ->
    Path = Node ++ "/data?Subpath=" ++ uri_string:quote(hb_store:join(Key)),
    ?event({reading, Key, Path, Opts}),
    case hb_http:get_binary(Path) of
        {ok, Bundle} ->
            case lists:keyfind(<<"Status">>, 1, Bundle#tx.tags) of
                {<<"Status">>, <<"404">>} ->
                    not_found;
                _ ->
                    {ok, Bundle}
            end;
        Error -> Error
    end.
-module(ao_remote_store).
-export([type/2, read/2, resolve/2, path/2]).
-include("include/ao.hrl").

%%% A store module that reads data from (an)other AO node(s).

resolve(#{ node := Node }, Key) ->
    ?c({resolving_to_self, Node, Key}),
    Key.

path(#{ node := Node }, Key) ->
    ?c({pathing_to_self, Node, Key}),
    Key.

type(Opts = #{ node := Node }, Key) ->
    ?no_prod("No need to get the whole message in order to get its type..."),
    ?c({remote_type, Node, Key}),
    case read(Opts, Key) of
        not_found -> not_found;
        #tx { data = Map } when is_map(Map) -> composite;
        _ -> simple
    end.

read(Opts, Key) when is_binary(Key) ->
    read(Opts, binary_to_list(Key));
read(Opts = #{ node := Node }, Key) ->
    Path = Node ++ "/data?Subpath=" ++ uri_string:quote(ao_store_common:join(Key)),
    ?c({reading, Key, Path, Opts}),
    case ao_http:get(Path) of
        {ok, Bundle} ->
            case lists:keyfind(<<"Status">>, 1, Bundle#tx.tags) of
                {<<"Status">>, <<"404">>} ->
                    not_found;
                _ ->
                    ?no_prod("Unnecessarily wasteful to serialize to deserialize later."),
                    %ar_bundles:print(Bundle#tx.data),
                    {ok, ar_bundles:serialize(Bundle)}
            end;
        Error ->
            ?no_prod("Validate this response path."),
            {ok, ar_bundles:serialize(Error)}
    end.

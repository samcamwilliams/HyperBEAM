-module(ao_remote_store).
-export([type/2, read/2]).
-include("include/ao.hrl").
-ao_debug(print).

%%% A store module that reads data from (an)other AO node(s).

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
            ?c(got_ok_reply),
            ar_bundles:print(Bundle),
            case lists:keyfind(<<"Status">>, 1, Bundle#tx.tags) of
                {<<"Status">>, <<"404">>} -> not_found;
                _ -> Bundle
            end;
        Error -> Error
    end.

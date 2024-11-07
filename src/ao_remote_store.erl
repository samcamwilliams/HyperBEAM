-module(ao_remote_store).
-export([type/2, read/2]).
-include("include/ao.hrl").
-ao_debug(print).

%%% A store module that reads data from (an)other AO node(s).

type(Opts = #{ node := Node }, Key) ->
    ?no_prod("No need to get the whole message in order to get its type..."),
    ?c({remote_type, Node, Key}),
    case ?c(read(Opts, Key)) of
        {ok, #tx { data = Map }} when is_map(Map) -> composite;
        {ok, _} -> simple;
        _Error -> not_found
    end.

read(Opts, Key) when is_binary(Key) ->
    read(Opts, binary_to_list(Key));
read(Opts = #{ node := Node }, Key) ->
    Path = Node ++ "/data?Subpath=" ++ uri_string:quote(ao_store_common:join(Key)),
    ?c({reading, Key, Path, Opts}),
    case ao_http:get(Path) of
        {ok, RespBin} -> ar_bundles:deserialize(RespBin);
        Error -> Error
    end.
-module(ao_remote_store).
-export([read/2]).
-include("include/ao.hrl").
-ao_debug(print).

%%% A store module that reads data from (an)other AO node(s).

read(Opts, Key) when is_binary(Key) ->
    read(Opts, binary_to_list(Key));
read(#{ node := Node }, Key) ->
    ?c({reading, Node, Key}),
    Path = Node ++ "/data/" ++ Key,
    case ao_http:get(Path) of
        {ok, RespBin} -> ar_bundles:deserialize(RespBin);
        Error -> Error
    end.
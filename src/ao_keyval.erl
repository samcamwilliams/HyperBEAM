-module(ao_keyval).
-export([read/1, write/2, type/1]).

%%% A key-value store abstraction, such that the underlying implementation
%%% can be swapped out easily. The default implementation is a file-based
%%% store.

read(Key) ->
    case file:read_file(Key) of
        {ok, Data} ->
            {ok, Data};
        {error, enoent} ->
            {error, enoent};
        {error, Reason} ->
            {error, Reason}
    end.

write(Key, Value) ->
    case file:write_file(Key, Value) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

type(Key) ->
    case file:is_dir(Key) of
        true -> composite;
        false ->
            case file:is_file(Key) of
                true -> simple;
                false -> not_found
            end
    end.
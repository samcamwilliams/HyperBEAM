-module(ao_store_common).
-export([join/1, add_prefix/2, remove_prefix/2]).

join([]) -> [];
join(Path) when is_binary(Path) -> Path;
join([""|Xs]) -> join(Xs);
join(FN = [X|_Xs]) when is_integer(X) -> FN;
join([X|Xs]) -> 
    filename:join(join(X), join(Xs)).

add_prefix(#{ prefix := Prefix }, Path) ->
    join([Prefix, Path]).

remove_prefix(#{ prefix := Prefix }, Path) ->
    ar_util:remove_common(Path, Prefix).

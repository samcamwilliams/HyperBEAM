%%% @doc Utility functions for working with links.
-module(hb_link).
-export([format/1, format/2]).
-export([read/1, read/2]).

%% @doc Read a link into memory. Uses `hb_cache:ensure_loaded/2' under-the-hood.
read(Link) -> read(Link, #{}).
read(Link, Opts) ->
    try hb_cache:ensure_loaded(Link, Opts)
    catch
        throw: {necessary_message_not_found, ID} ->
            {error, {link_not_found, ID}}
    end.

%% @doc Format a link as a short string suitable for printing. Checks the node
%% options (optionally) given, to see if it should resolve the link to a value
%% before printing.
format(Link) -> format(Link, #{}).
format(Link, Opts) ->
    case hb_opts:get(debug_resolve_links, false, Opts) of
        true ->
            case read(Link, Opts) of
                {ok, Val} when is_map(Val) ->
                    hb_message:format(Val);
                {error, _} ->
                    << "!UNRESOLVABLE! ", (do_format(Link))/binary >>
            end;
        false -> do_format(Link)
    end.

do_format({link, ID, #{ <<"type">> := Type }}) ->
    iolist_to_binary(io_lib:format("Link (~s): ~s", [hb_util:bin(Type), ID]));
do_format({link, ID, _}) ->
    iolist_to_binary(io_lib:format("Link: ~s", [ID])).
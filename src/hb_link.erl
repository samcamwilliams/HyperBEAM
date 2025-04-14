%%% @doc Utility functions for working with links.
-module(hb_link).
-export([read/1, read/2, linkify/1, linkify/2]).
-export([encode_link/1, decode_link/1, encode_all_links/1, decode_all_links/1]).
-export([format/1, format/2]).
-include("include/hb.hrl").

%% @doc Read a link into memory. Uses `hb_cache:ensure_loaded/2' under-the-hood.
read(Link) -> read(Link, #{}).
read(Link, Opts) ->
    try hb_cache:ensure_loaded(Link, Opts)
    catch
        throw: {necessary_message_not_found, ID} ->
            {error, {link_not_found, ID}}
    end.

%% @doc Turn a deep message into a flat representation with links to embedded
%% data as necessary.
linkify(Msg) -> linkify(Msg, #{}).
linkify(Msg, Opts) when is_map(Msg) ->
    maps:merge(
        Msg,
        maps:from_list(
            lists:map(
                fun({Key, InnerMsg}) when is_map(InnerMsg) ->
                    ID = hb_message:id(InnerMsg, all, Opts),
                    {<< Key/binary, "+link" >>, {link, ID, #{}}};
                ({Key, Val}) -> {Key, Val}
                end,
                maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg))
            )
        )
    ).

%% @doc Search a TABM for all links and replace them with a `+link' key.
encode_all_links(Map) ->
    maps:from_list(
        lists:map(
            fun({Key, Value}) when ?IS_LINK(Value) ->
                {<<Key/binary, "+link">>, encode_link(Value)};
            ({Key, Value}) ->
                {Key, Value}
            end,
            maps:to_list(Map)
        )
    ).

%% @doc Encode a link into a binary.
encode_link({link, ID, #{ <<"type">> := Type }}) ->
    <<ID/binary, "+", (hb_util:bin(Type))/binary>>;
encode_link({link, ID, #{}}) ->
    <<ID/binary>>.

%% @doc Decode links embedded in the headers of a message.
decode_all_links(Msg) ->
    maps:from_list(
        lists:map(
            fun({Key, Value}) when byte_size(Key) >= 5 ->
                case binary:part(Key, byte_size(Key) - 5, 5) of
                    <<"+link">> ->
                        NewKey = binary:part(Key, 0, byte_size(Key) - 5),
                        {NewKey, decode_link(Value)};
                    _ -> {Key, Value}
                end;
                ({Key, Value}) -> {Key, Value}
            end,
            maps:to_list(Msg)
        )
    ).

%% @doc Decode a link embedded in a header.
decode_link(LinkStr) ->
    case binary:split(LinkStr, <<"+">>) of
        [ID, Type] ->
            {link, ID, #{ <<"type">> => Type }};
        [ID] ->
            {link, ID, #{}}
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
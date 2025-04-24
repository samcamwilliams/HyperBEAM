%%% @doc Utility functions for working with links.
-module(hb_link).
-export([read/1, read/2, linkify/1, linkify/2, linkify/3, is_link_key/1]).
-export([encode_link/1, decode_link/1, encode_all_links/1, decode_all_links/1]).
-export([format/1, format/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Read a link into memory. Uses `hb_cache:ensure_loaded/2' under-the-hood.
read(Link) -> read(Link, #{}).
read(Link, Opts) ->
    try hb_cache:ensure_loaded(Link, Opts)
    catch
        throw:{necessary_message_not_found, ID} ->
            {error, {link_not_found, ID}}
    end.

%% @doc Turn a deep message into a flat representation with links to embedded
%% data as necessary.
linkify(Msg) -> linkify(Msg, discard).
linkify(Msg, Mode) when is_atom(Mode) -> linkify(Msg, Mode, #{});
linkify(Msg, Opts) when is_map(Opts) ->
    linkify(Msg, hb_opts:get(linkify_mode, discard, Opts), Opts).
linkify(Msg, Mode, Opts) when is_map(Msg) ->
    ?event(linkify, {linkify, {keys, maps:keys(Msg)}}),
    maps:merge(
        maps:with([<<"commitments">>, <<"priv">>], Msg),
        maps:from_list(
            lists:map(
                fun({Key, InnerMsg}) when is_map(InnerMsg) or is_list(InnerMsg) ->
                    LinkifiedInnerMsg = linkify(InnerMsg, Mode, Opts),
                    ID =
                        hb_message:id(
                            LinkifiedInnerMsg,
                            all,
                            Opts
                        ),
                    % If we are in `offload' mode, we write the message to the
                    % cache. If we are in `discard' mode, we simply drop the 
                    % nested message.
                    case Mode of
                        discard -> do_nothing;
                        offload -> hb_cache:write(LinkifiedInnerMsg, Opts)
                    end,
                    ?event(linkify, {generated_id, {key, Key}, {id, ID}}),
                    {Key, {link, ID, #{}}};
                ({Key, Val}) -> {Key, Val}
                end,
                maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg))
            )
        )
    );
linkify(Msg, Mode, Opts) when is_list(Msg) ->
    lists:map(fun(X) -> linkify(X, Mode, Opts) end, Msg);
linkify(OtherVal, _Mode, _Opts) ->
    OtherVal.

%% @doc Search a TABM for all links and replace them with a `+link' key.
encode_all_links(Map) ->
    ?event(linkify, {encode_all_links, {map, Map}}),
    maps:from_list(
        lists:map(
            fun({Key, Value}) when ?IS_LINK(Value) ->
                ?event(linkify, {encoding_link, {key, Key}, {link, Value}}),
                NormKey = hb_ao:normalize_key(Key),
                {<<NormKey/binary, "+link">>, encode_link(Value)};
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
            fun({Key, Value}) ->
                case is_link_key(Key) of
                    true ->
                        NewKey = binary:part(Key, 0, byte_size(Key) - 5),
                        {NewKey, decode_link(Value)};
                    _ -> {Key, Value}
                end
            end,
            maps:to_list(Msg)
        )
    ).

%% @doc Decode a link value embedded in a header.
decode_link(LinkStr) ->
    case binary:split(LinkStr, <<"+">>) of
        [ID, Type] ->
            {link, ID, #{ <<"type">> => Type }};
        [ID] ->
            {link, ID, #{}}
    end.

%% @doc Determine if a key is an encoded link.
is_link_key(Key) when byte_size(Key) >= 5 ->
    binary:part(Key, byte_size(Key) - 5, 5) =:= <<"+link">>;
is_link_key(_) -> false.

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

%%% Tests

offload_linked_message_test() ->
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"link-key">> => #{
            <<"immediate-key-2">> => <<"link-value">>,
            <<"link-key-2">> => #{
                <<"immediate-key-3">> => <<"link-value-2">>
            }
        }
    },
    Offloaded = linkify(Msg, offload),
    ?event(linkify, {test_recvd_linkified, {msg, Offloaded}}),
    Loaded = hb_cache:ensure_all_loaded(Offloaded),
    ?event(linkify, {test_recvd_loaded, {msg, Loaded}}),
    ?assertEqual(Msg, Loaded).

-module(ao_http).
-export([get/1, get/2, post/3, reply/2, reply/3]).

-include("include/ao.hrl").

get(Host, Path) -> ?MODULE:get(Host ++ Path).
get(URL) ->
    ao:c({http_get, URL}),
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, ar_bundles:deserialize(Body)};
        Response ->
            {error, Response}
    end.

post(Host, Path, Item) ->
    ao:c({http_post, ar_util:encode(Item#tx.id), Host ++ Path}),
    case httpc:request(
        post,
        {Host ++ Path, [], "application/octet-stream", ar_bundles:serialize(Item)},
        [],
        [{body_format, binary}]
    ) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, ar_bundles:deserialize(Body)};
        Response ->
            {error, Response}
    end.

reply(Req, Item) -> reply(Req, 200, Item).
reply(Req, Status, Item) ->
    ao:c(
        {replying,
            Status,
            maps:get(method, Req, undef_method),
            maps:get(path, Req, undef_path),
            case is_record(Item, tx) of true -> ar_util:encode(Item#tx.id); false -> Item end}
    ),
    cowboy_req:reply(
        Status,
        #{<<"Content-Type">> => <<"application/octet-stream">>},
        ar_bundles:serialize(Item),
        Req
    ),
    {ok, Req}.

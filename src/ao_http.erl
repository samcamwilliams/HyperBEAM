-module(ao_http).
-export([get/2, post/3, reply/2, reply/3]).

-include("include/ar.hrl").

get(Host, Path) ->
    ao:c({http_get, Host ++ Path}),
    case httpc:request(get, {Host ++ Path, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, ar_bundles:deserialize(Body)};
        Response ->
            {error, Response}
    end.

post(Host, Path, Item) ->
    ao:c({http_post, Item#tx.id, Host ++ Path}),
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
    ao:c({replying, Status, case is_record(Item, tx) of true -> Item#tx.id; false -> Item end}),
    cowboy_req:reply(
        Status,
        #{<<"Content-Type">> => <<"application/octet-stream">>},
        ar_bundles:serialize(Item),
        Req
    ),
    {ok, Req}.
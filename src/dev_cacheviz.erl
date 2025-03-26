-module(dev_cacheviz).
-export([dot/3, svg/3]).

dot(_Base, _Req, Opts) ->
    hb_cache_render:render(Opts).

svg(Base, Req, Opts) ->
    hb_cache_render:render(Opts).

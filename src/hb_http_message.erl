-module(hb_http_message).

-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([from_hb_message/1, to_hb_message/1]).

-define(SIGNATURES_FIELD, <<"Signatures">>).
-define(LINKS_FIELD, <<"Links">>).

from_hb_message(Msg) ->
    ok.

to_hb_message(Http) ->
    ok.

from_hb_signatures (Signatures) ->
    ok.

to_hb_signatures (HttpSignatures) ->
    ok.

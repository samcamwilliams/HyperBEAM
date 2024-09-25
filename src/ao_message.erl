-module(ao_message).
-export([id/1, get/1, to_json/1]).

-include("include/ao.hrl").

id(M) ->
    ar_bundles:data_item_signature_data(M).

get(ID) ->
    {ok, Bin} = file:read_file("test/" ++ binary_to_list(ID)),
    #tx { data = Bin }.

to_json(M) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M)).
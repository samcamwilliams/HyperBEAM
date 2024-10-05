-module(ao_message).
-export([id/1, get/1, to_json/1, from_json/1, to_binary/1, from_binary/1]).

-include("include/ao.hrl").

id(M) ->
    ar_bundles:data_item_signature_data(M).

get(ID) ->
    {ok, Bin} = file:read_file("test/" ++ binary_to_list(ID)),
    #tx { data = Bin }.

to_json(M) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M)).

from_json(J) ->
    {JSONStruct} = jiffy:decode(J),
    ar_bundles:json_struct_to_item(JSONStruct).

to_binary(M) ->
    ar_bundles:serialize(M).

from_binary(B) ->
    ar_bundles:deserialize(B).
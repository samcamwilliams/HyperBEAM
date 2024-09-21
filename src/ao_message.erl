-module(ao_message).
-export([id/1]).

id(M) ->
    ar_bundles:data_item_signature_data(M).
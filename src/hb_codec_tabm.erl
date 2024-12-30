%%% @doc A codec for the TABM format, which is used internally by HyperBEAM as
%%% the common format between encodings of messages.
-module(hb_codec_tabm).
-export([to/1, from/1]).
-include("include/hb.hrl").

-define(REGEN_KEYS, [id, unsigned_id]).

%% @doc Convert a message into a 'Type-Annotated-Binary-Message' (TABM): A message where
%% each key is a simple binary or another TABM.
to(Msg) ->
    maps:from_list(lists:flatten(
        lists:map(
            fun(Key) ->
                case maps:find(Key, Msg) of
                    {ok, <<>>} ->
                        BinKey = hb_converge:key_to_binary(Key),
                        {<<"Converge-Type:", BinKey/binary>>, <<"Empty-Binary">>};
                    {ok, Value} when is_binary(Value) ->
                        {Key, Value};
                    {ok, Map} when is_map(Map) ->
                        {Key, to(Map)};
                    {ok, []} ->
                        BinKey = hb_converge:key_to_binary(Key),
                        {<<"Converge-Type:", BinKey/binary>>, <<"Empty-List">>};
                    {ok, Value} when
                            is_atom(Value) or is_integer(Value)
                            or is_list(Value) ->
                        ItemKey = hb_converge:key_to_binary(Key),
                        {Type, BinaryValue} = encode_value(Value),
                        [
                            {<<"Converge-Type:", ItemKey/binary>>, Type},
                            {ItemKey, BinaryValue}
                        ];
                    {ok, _} -> []
                end
            end,
            lists:filter(
                fun(Key) ->
                    % Filter keys that the user could set directly, but
                    % should be regenerated when moving msg -> TX, as well
                    % as private keys.
                    not lists:member(Key, ?REGEN_KEYS) andalso
                        not hb_private:is_private(Key)
                end,
                maps:keys(Msg)
            )
        )
    )).

%% @doc Turns a TABM into a native HyperBEAM message.
from(TABM0) ->
    % First, handle special cases of empty items, which `ar_bundles` cannot
    % handle. Needs to be transformed into a list (unfortunately) so that we
    % can also remove the "Converge-Type:" prefix from the key.
    TABM1 =
        maps:from_list(
            lists:map(
                fun({<<"Converge-Type:", Key/binary>>, <<"Empty-Binary">>}) ->
                    {Key, <<>>};
                ({<<"Converge-Type:", Key/binary>>, <<"Empty-List">>}) ->
                    {Key, []};
                ({Key, Value}) ->
                    {Key, Value}
                end,
                maps:to_list(TABM0)
            )
        ),
    % 1. Remove any `ar_bundles` meta tags, which are not part of the message;
    % 2. Remove any keys from output that have a "Converge-Type:" prefix;
    % 3. Decode any binary values that have a "Converge-Type:" prefix;
    % 4. Recursively decode any maps that we encounter;
    % 5. Return the remaining keys and values as a map.
    maps:filtermap(
        fun(<<"Converge-Type:", _/binary>>, _) ->
            % Remove any keys from output that have a "Converge-Type:" prefix.
            false;
        (RawKey, BinaryValue) when is_binary(BinaryValue) ->
            Key = hb_converge:key_to_binary(RawKey),
            case maps:find(<<"Converge-Type:", Key/binary>>, TABM1) of
                error -> {true, BinaryValue};
                {ok, RawType} ->
                    NormType = list_to_existing_atom(
                        string:to_lower(binary_to_list(RawType))
                    ),
                    {true, decode_value(NormType, BinaryValue)}
            end;
        (_Key, ChildTABM) when is_map(ChildTABM) ->
            {true, from(ChildTABM)};
        (_Key, Value) ->
            % We encountered a key that already has a converted type.
            % We can just return it as is.
            {true, Value}
        end,
        TABM1
    ).
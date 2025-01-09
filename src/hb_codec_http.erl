
%%% @doc A codec for the that marshals TABM encoded messages to and from the
%%% "HTTP" message structure.
%%% 
%%% The HTTP Message is an Erlang Map with the following shape:
%%% #{ 
%%%     headers => [
%%%         {<<"Example-Header">>, <<"Value">>}
%%%     ],
%%%     body: <<"Some body">>
%%% }
%%% 
%%% Every HTTP message is an HTTP multipart message.
%%% See https://datatracker.ietf.org/doc/html/rfc7578
%%%
%%% For each TABM Key:
%%% 
%%% The TABM Key will be ignored if:
%%% - The field is private (according to hb_private:is_private/1)
%%% - The field is one of ?REGEN_KEYS
%%%
%%% The Key/Value Pair will be encoded according to the following rules:
%%%     "signatures" -> {SignatureInput, Signature} header Tuples, each encoded
%%% 					as a Structured Field Dictionary
%%%     "body" ->
%%%         - if a map, then recursively encode as its own HyperBEAM message
%%%         - otherwise encode as a normal field
%%%     _ -> encode as a normal field
%%% 
%%% Each field will be mapped to the HTTP Message according to the following 
%%% rules:
%%%     "body" -> always encoded part of the body as with Content-Disposition
%%% 			  type of "inline"
%%%     _ ->
%%%         - If the byte size of the value is less than the ?MAX_TAG_VALUE,
%%% 		  then encode as a header, also attempting to encode as a
%%% 		  structured field.
%%%         - Otherwise encode the value as a part in the multipart response
%%% 
-module(hb_codec_http).
-export([to/1, from/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% The max header length is 4KB
-define(MAX_HEADER_LENGTH, 4096).

%%% @doc Convert an HTTP Message into a TABM.
%%% Any HTTP Structured Field is encoded into it's equivalent TABM encoding.
from(#{ headers := Headers, body := Body }) when is_map(Headers) ->
    from(#{ headers => maps:to_list(Headers), body => Body });
from(#{ headers := Headers, body := Body }) ->
    % First, parse all headers and add as key-value pairs to the TABM
    TABM0 = from_headers(#{}, Headers),
    % Next, we need to potentially parse the body and add to the TABM
    % potentially as additional key-binary value pairs, or as sub-TABMs
    ContentType = case lists:keyfind(<<"Content-Type">>, 1, Headers) of
        false -> <<"application/octet-stream">>;
        {_, CT} -> CT
    end,
    TABM1 = from_body(TABM0, ContentType, Body),
    TABM1.

from_headers(TABM, Headers) -> from_headers(TABM, Headers, Headers).
from_headers(TABM, [], _) -> TABM;
from_headers(TABM, [{Name, Value} | Rest], Headers) ->
    TABM1 = case Name of
        % Handled as part of "Signature" so simply skip it
        <<"Signature-Input">> -> TABM;
        <<"signature-input">> -> TABM;

        <<"Signature">> ->
            {ok, SigInput} = get_signature_input(Headers),
            from_signature(TABM, Value, SigInput);
        <<"signature">> ->
            {ok, SigInput} = get_signature_input(Headers),
            from_signature(TABM, Value, SigInput);

        % Decode the header as normal
        N -> from_pair(TABM, {N, Value})
    end,
    from_headers(TABM1, Rest, Headers).

%%% @doc attempt to parse the value as a structured field
%%% First as an SF Item,
%%% then an SF Dictionary,
%%% then an SF List.
%%%
%%% If parsing is successful, convert the SF structure into
%%% a TAB structure
from_pair(TABM, {Name, Value}) when is_binary(Value) ->
    case catch hb_http_structured_fields:parse_item(Value) of
        {'EXIT', _} ->
            case catch hb_http_structured_fields:parse_dictionary(Value) of
                {'EXIT', _} ->
                    case catch hb_http_structured_fields:parse_list(Value) of
                        {'EXIT', _} -> {not_ok, undefined};
                        % [item()]
                        List ->
                            ?no_prod("Is this proper way to TABM encode a list?"),
                            BareValues = lists:map(
                                fun
                                    ({item, BareItem, []}) -> decode_sf_bare_item(BareItem);
                                    % Re-encode to preserve original binary
                                    (Item) -> hb_http_structured_fields:item(Item)
                                end,
                                List
                            ),
                            TopLevelFields = maps:from_list(hb_codec_converge:to_tab(Name, BareValues)),
                            maps:merge(TABM, TopLevelFields)
                    end;
                % [{binary(), item()}]
                DictList ->
                    TABM_Pairs = lists:foldl(
                        fun ({MapKey, Item}, Acc) ->
                            Pairs = sf_item_to_tab(MapKey, Item),
                            Pairs ++ Acc
                        end,
                        [],
                        DictList
                    ),
                    SubTABM = maps:from_list(TABM_Pairs),
                    maps:put(Name, SubTABM, TABM)
            end;
        Item ->
            TopLevelFields = maps:from_list(sf_item_to_tab(Name, Item)),
            maps:merge(TABM, TopLevelFields)
    end.

sf_item_to_tab(Key, Item = {item, _BareItem, _Params}) -> 
    case Item of
        {item, BareItem, []} ->
            hb_codec_converge:to_tab(Key, decode_sf_bare_item(BareItem));
        {item, _, _} ->
            ?no_prod("How do we handle SF Params?"),
            % Re-encode to preserve original binary
            hb_codec_converge:to_tab(Key, hb_http_structured_fields:item(Item))
    end.

decode_sf_bare_item (BareItem) ->
    case BareItem of
        I when is_integer(I) -> I;
        B when is_boolean(B) -> B;
        D = {decimal, _} -> list_to_float(hb_http_structured_fields:bare_item(D));
        {string, S} -> S;
        {token, T} -> binary_to_existing_atom(T);
        {binary, B} -> B
    end.

from_signature(TABM, RawSig, RawSigInput) ->
    SfSigs = hb_http_structured_fields:dictionary(RawSig),
    SfInputs = hb_http_structured_fields:dictionary(RawSigInput),
    % Build a TABM for Signatures by gathering each Signature
    % with its corresponding Inputs.
    % 
    % Inputs are merged as fields on the Signature Map
    Signatures = maps:fold(
        fun (SigName, {item, {_, Sig}, _}, TABM_Sigs) ->
            {list, SfInputItems, SfInputParams} = lists:keyfind(SigName, 1, SfInputs),
            % [<<"foo">>, <<"bar">>]
            Inputs = lists:map(fun({item, {_, Input}, _}) -> Input end, SfInputItems),
            InputFields = maps:from_list(hb_codec_converge:to_tab(<<"inputs">>, Inputs)),

            % Signature parameters are converted into top-level keys on the signature TABM
            Params = lists:foldl(
                fun({PName, PValue}, PAcc) ->
                    ParamFields = maps:from_list(hb_codec_converge:to_tab(PName, PValue)),
                    maps:merge(PAcc, ParamFields)
                end,
                #{ <<"signature">> => Sig },
                SfInputParams    
            ),

            TABM_Sig = maps:merge(Params, InputFields),
            % #{ [SigName/binary] => #{ <<"signature">> => <<>>, <<"inputs">> => , ... } }
            maps:put(SigName, TABM_Sig, TABM_Sigs)
        end,
        #{},
        SfSigs
    ),
    % Finally place the Signatures as a top-level TABM on the parent TABM
    maps:put(<<"Signatures">>, Signatures, TABM).

get_signature_input(Headers) ->
    case lists:keyfind(<<"Signature-Input">>, 1, Headers) of
        false -> case lists:keyfind(<<"signature-input">>, 1, Headers) of
            false -> signature_input_not_found;
            {_, V} -> {ok, V}
        end;
        {_, V} -> {ok, V}
    end.

from_body(TABM, _ContentType, <<>>) -> TABM;
from_body(TABM, ContentType, Body) ->
    {item, {_, _BodyType}, Params} = hb_http_structured_fields:item(ContentType),
    case lists:keyfind(<<"boundary">>, 1, Params) of
        % The body is not a multipart, so just set as is to the Body key on the TABM
        false -> maps:put(<<"Body">>, Body, TABM);
        % We need to manually parse the multipart body into key/values on the TABM
        {_, Boundary} ->
            % The first part will always be empty (since the boundary is always placed first
            % in the body
            [_ | Parts] = binary:split(Body, <<"--", Boundary/binary>>),
            % The last part MIGHT be "--" for the terminating boundary.
            %
            % So we need to check and potentially trim off the last
            % element
            TParts = case lists:last(Parts) of
                <<"--">> -> lists:sublist(Parts, length(Parts) - 1);
                _ -> Parts
            end,
            % Finally, for each body part, we need to parse it into its
            % own HTTP Message, then recursively convert into a TABM
            TABM1 = lists:foldl(
                fun (Part, Acc) -> append_body_part(Acc, Part) end,
                TParts,
                TABM
            ),
            TABM1
    end.

append_body_part(TABM, Part) ->
    % TODO
    % - extract headers block by splitting on "\n\n" then grabbing HEAD
    % - extract individual headers by splitting on "\n"
    % - parse each header, splitting on FIRST ": " -> {Name, Value}
    % - take body block and use as the body
    % - recursively call from(Http)
    % - Set as key on TABM as Content-Disposition: form-data; name="..." OR as <<"body">> key if "inline"
    % FIN
    not_implemented.

%%% @doc Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
%%% that can translated to a given web server Response API
to(Bin) when is_binary(Bin) -> Bin;
to(TABM) when is_map(TABM) ->
    % PublicMsg = hb_private:reset(TABM),
    % MinimizedMsg = hb_message:minimize(PublicMsg),
    % NormalizedMsg = hb_message:normalize_keys(MinimizedMsg),
    Http = maps:fold(
        fun
            % Signatures (note abbr. & case-insensitivity) are mapped according to RFC-9421
            (<<"signatures">>, Signatures, Http) -> signatures_to_http(Http, Signatures);
            (<<"sigs">>, Signatures, Http) -> signatures_to_http(Http, Signatures);
            (<<"sig">>, Signature, Http) -> signatures_to_http(Http, [Signature]);
            (<<"Signatures">>, Signatures, Http) -> signatures_to_http(Http, Signatures);
            (<<"Sigs">>, Signatures, Http) -> signatures_to_http(Http, Signatures);
            (<<"Sig">>, Signature, Http) -> signatures_to_http(Http, [Signature]);

            % Body (note case-insensitivity) is mapped into a multipart according to RFC-7578
            (<<"body">>, Body, Http) -> body_to_http(Http, Body);
            (<<"Body">>, Body, Http) -> body_to_http(Http, Body);

            % All other values are encoded as an HTTP Structured Fields.
            % non-map/list is encoded as an HTTP Structured Field Item
            (Name, Value, Http) when not (is_map(Value) orelse is_list(Value)) ->
                {ok, Item} = hb_http_structured_fields:to_item(Value),
                field_to_http(Http, {Name, iolist_to_binary(hb_http_structured_fields:item(Item))}, #{});
            % Further mapping of lists and maps delegated to field_to_http
            (Name, Value, Http) -> field_to_http(Http, {Name, Value}, #{})
        end,
        #{ headers => [], body => #{} },
        TABM
    ),
    Body = maps:get(body, Http),
    NewHttp = case maps:size(Body) of
        0 -> maps:put(body, <<>>, Http);
        _ ->
            ?no_prod("What should the Boundary be?"),
            Boundary = base64:encode(crypto:strong_rand_bytes(8)),
            % Transform body into a binary, delimiting each part with the Boundary
            BodyBin = maps:fold(
                fun (_, BodyPart, Acc) ->
                    <<Acc/binary, "--", Boundary/binary, "\n", BodyPart/binary, "\n">>
                end,
                <<>>,
                Body
            ),
            #{ 
                headers => [
                    {
						<<"Content-Type">>,
						<<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>
					}
                    | maps:get(headers, Http)
                ],
                % TODO: I _think_ this is needed, according to the spec
                % End the body with a final terminating Boundary
                body => <<BodyBin/binary, "--", Boundary/binary, "--">>
            }
    end,
    NewHttp.

encode_http_msg (#{ headers := SubHeaders, body := SubBody }) ->
    % Serialize the headers, to be included in the part of the multipart response
    EncodedHeaders = lists:foldl(
        fun ({HeaderName, HeaderValue}, Acc) ->
            <<Acc/binary, "\n", HeaderName/binary, ": ", HeaderValue/binary>>
        end,
        <<>>,
        SubHeaders
    ),
    % Some-Headers: some-value
    % Content-Type: image/png
    % 
    % <body>
    <<EncodedHeaders/binary, "\n\n", SubBody/binary>>.

signatures_to_http(Http, Signatures) when is_map(Signatures) ->
    signatures_to_http(Http, maps:to_list(Signatures));
signatures_to_http(Http, Signatures) when is_list(Signatures) ->
    {SfSigInputs, SfSigs} = lists:foldl(
        fun ({SigName, SignatureMap = #{ <<"inputs">> := Inputs, <<"signature">> := Signature }}, {SfSigInputs, SfSigs}) ->
            NextSigInput = hb_http_signature:sf_signature_params(Inputs, SignatureMap),
            NextSig = hb_http_signature:sf_signature(Signature),
            NextName = hb_converge:key_to_binary(SigName),
            {
                [{NextName, NextSigInput} | SfSigInputs],
                [{NextName, NextSig} | SfSigs]
            }
        end,
        % Start with empty Structured Field Dictionaries
        {[], []},
        Signatures
    ),
    % Signature and Signature-Input are always encoded as Structured Field dictionaries, and then
    % each transmitted either as a header, or as a part in the multi-part body
    WithSig = field_to_http(Http, {<<"Signature">>, hb_http_structured_fields:dictionary(SfSigs)}, #{}),
    WithSigAndInput = field_to_http(WithSig, {<<"Signature-Input">>, hb_http_structured_fields:dictionary(SfSigInputs)}, #{}),
    WithSigAndInput.

body_to_http(Http, Body) when is_map(Body) ->
    Disposition = <<"Content-Disposition: inline">>,
    SubHttp = to(Body),
    EncodedBody = encode_http_msg(SubHttp),
    field_to_http(Http, {<<"body">>, EncodedBody}, #{ disposition => Disposition, where => body });
body_to_http(Http, Body) when is_binary(Body) ->
    Disposition = <<"Content-Disposition: inline">>,
    field_to_http(Http, {<<"body">>, Body}, #{ disposition => Disposition, where => body }).

field_to_http(Http, {Name, {<<"List">>, Value}}, Opts) ->
    field_to_http(Http, {Name, Value}, Opts);
field_to_http(Http, {Name, MapOrList}, Opts) when is_map(MapOrList) orelse is_list(MapOrList) ->
    {Mapper, Parser} = case MapOrList of
        Map when is_map(Map) -> {fun hb_http_structured_fields:to_dictionary/1, fun hb_http_structured_fields:dictionary/1};
        List when is_list(List) -> {fun hb_http_structured_fields:to_list/1, fun hb_http_structured_fields:list/1}
    end,
    MaybeEncoded = case Mapper(MapOrList) of
        {ok, Sf} ->
            % Check the size of the encoded value, and signal to store
            % as a Structured Field in the header
            %
            % Otherwise, in the case of a Map, we will need to convert the Map into
            % its own HTTP message. For a list, we can still use the Structured field encoding.
            % In both cases, the value is appended as a part of the parent's multi-part body
            EncodedSf = iolist_to_binary(Parser(Sf)),
            Fits = byte_size(EncodedSf) =< ?MAX_HEADER_LENGTH,
            {Fits, EncodedSf};
        _ -> undefined
    end,
    ?no_prod("What should the name be?"),
    NormalizedName = hb_converge:key_to_binary(Name),
    case MaybeEncoded of
        {true, EncodedSfDict} ->
            field_to_http(Http, {NormalizedName, EncodedSfDict}, Opts);
        % Encode the map as a sub part, to be appended to the body
        {false, _} when is_map(MapOrList) ->
            SubHttp = to(MapOrList),
            EncodedHttpMap = encode_http_msg(SubHttp),
            field_to_http(Http, {Name, EncodedHttpMap}, maps:put(where, body, Opts));
        % Encode the SF list as a sub part, to be appended to the body
        {false, EncodedSfList} when is_list(MapOrList) ->
            field_to_http(Http, {NormalizedName, EncodedSfList}, maps:put(where, body, Opts));
        undefined when is_list(MapOrList) ->
            ?no_prod("how do we encode a list in HTTP message if it cannot be encoded as a structured field?"),
            not_implemented
    end;

field_to_http(Http, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_converge:key_to_binary(Name),

    % The default location where the value is encoded within the HTTP
    % message depends on its size.
    % 
    % So we check whether the size of the value is within the threshold
    % to encode as a header, and other default to encoding in the body
    DefaultWhere = case byte_size(Value) of
        Fits when Fits =< ?MAX_HEADER_LENGTH -> headers;
        _ -> maps:get(where, Opts, headers)
    end,

    case maps:get(where, Opts, DefaultWhere) of
        headers ->
            Headers = maps:get(headers, Http),
            NewHeaders = lists:append(Headers, [{NormalizedName, Value}]),
            maps:put(headers, NewHeaders, Http);
        % Append the value as a part of the multipart body
        %
        % We'll need to prepend a Content-Disposition header to the part, using
        % the field name as the form part name. (see https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
        % We allow the caller to provide a Content-Disposition in Opts, but default
        % to appending as a field on the form-data
        body ->
            Body = maps:get(body, Http),
            Disposition = maps:get(disposition, Opts, <<"Content-Disposition: form-data; name=", NormalizedName/binary>>),
            BodyPart = <<Disposition/binary, "\n\n", Value/binary>>,
            NewBody = maps:put(NormalizedName, BodyPart, Body),
            maps:put(body, NewBody, Http)
    end.

%%% Tests

simple_message_to_test() ->
    Http = hb_codec_http:to(Msg = #{ a => 1, b => <<"foo">> }),
    erlang:display({foooo, Http}),
    ?assertEqual(
        #{ headers => [{<<"a">>, <<"1">>}, {<<"b">>, <<"\"foo\"">>}], body => <<>> },
        Http
    ),
    ok.

simple_body_message_to_test() ->
    Html = <<"<html><body>Hello</body></html>">>,
    _Msg = #{ "Content-Type" => <<"text/html">>, body => Html },
    %Http = hb_codec_http:to(Msg),
    ok.
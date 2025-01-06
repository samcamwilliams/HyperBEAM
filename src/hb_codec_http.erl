
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

-define(MAX_HEADER_LENGTH, 256).

%%% @doc Convert an HTTP Message into a TABM.
%%% Any HTTP Structured Field is encoded into it's equivalent TABM encoding.
%%% TODO: special rules for Signature and Signature-Input headers
%% Recursively encode the multipart body into TABM
from(#{ headers := Headers, body := Body }) ->
    ContentType = lists:keyfind(<<"Content-Type">>, 1, Headers),
    {item, _, Params} = hb_http_structured_fields:item(ContentType),
    _Parts = case lists:keyfind(<<"boundary">>, 1, Params) of
        false -> [Body];
        {_, Boundary} ->
            % The first part will always be empty (since the boundary is always placed first
            % in the body
            [_, P] = binary:split(Body, <<"--", Boundary/binary>>),
            % The last part MIGHT be "--" for the terminating boundary.
            %
            % So we need to check and potentially trim off the last
            % element
            _TrimmedParts = case lists:last(P) of
                <<"--">> ->
                    lists:sublist(P, length(P) - 1);
                _ -> P
            end
    end,

    % TODO: WIP NOT DONE
    % Take each part and convert into a HB message
    %   - headers become fields
    %       - maybe parse as structured fields?
    %   - parts become fields (recursively parsed)
    %       - "inline" part becomes top level "body" field
    %   - "Signature" & "Signature-Input" are parsed as SF dictionaries and become "Signatures" on HB message

    not_implemented.

%%% @doc Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
%%% that can translated to a given web server Response API
to(Bin) when is_binary(Bin) -> Bin;
to(TABM) ->
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
        fun ({SigName, SignatureMap = #{ inputs := Inputs, signature := Signature }}, {SfSigInputs, SfSigs}) ->
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
            % the as a Structured Field encoded dictionary in the header
            %
            % Otherwise, we will need to convert the Map into its own HTTP message
            % and append as a part of the body in the parent multi-part msg
            EncodedSf = iolist_to_binary(Parser(Sf)),
            Fits = byte_size(EncodedSf) =< ?MAX_HEADER_LENGTH,
            {Fits, EncodedSf};
        _ -> undefined
    end,
    ?no_prod("What should the name be?"),
    NormalizedName = hb_converge:key_to_binary(Name),
    case MaybeEncoded of
        {true, Bin} ->
            field_to_http(Http, {NormalizedName, Bin}, Opts);
        % Encode the map as a sub part, to be appended to the body
        {false, _} when is_map(MapOrList) ->
            SubHttp = to(MapOrList),
            EncodedHttpMap = encode_http_msg(SubHttp),
            field_to_http(Http, {Name, EncodedHttpMap}, maps:put(where, body, Opts));
        % Encode the SF list as a sub part, to be appended to the body
        {false, Bin} when is_list(MapOrList) ->
            field_to_http(Http, {NormalizedName, Bin}, maps:put(where, body, Opts));
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

%%% @doc Maps the native HyperBEAM Message to an "HTTP" message.
%%% Every HyperBEAM Message is mapped to an HTTP multipart message.
%%% The HTTP Message data structure has the following shape:
%%% #{ 
%%%     headers => [
%%%         {<<"Example-Header">>, <<"Value">>}
%%%     ],
%%%     body: <<"Some body">>
%%% }
%%% 
%%% 
%%% For each HyperBEAM Message Key:
%%% 
%%% The Key will be ignored if:
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

-define(MAX_HEADER_LENGTH, 256).

to(Msg) ->
    PublicMsg = hb_private:reset(Msg),
    MinimizedMsg = hb_message:minimize(PublicMsg),
    NormalizedMsg = hb_message:normalize_keys(MinimizedMsg),
    Http = lists:foldl(
        fun
            ({<<"signatures">>, Signatures}, Http) -> signatures_to_http(Http, Signatures);
            ({<<"body">>, Body}, Http) -> body_to_http(Http, Body);
            ({Name, Value}, Http) -> field_to_http(Http, {Name, Value}, #{})
        end,
        #{
            headers => [],
            body => #{}
        },
        maps:to_list(NormalizedMsg)    
    ),
    Body = maps:get(body, Http),
    NewHttp = case maps:size(Body) of
        0 -> maps:put(body, <<>>, Http);
        _ ->
            ?no_prod("What should the Boundary be?"),
            Boundary = base64:encode(crypto:strong_rand_bytes(8)),
            % Transform body into a binary, delimiting each part,
            % with the Boundary
            Bin = maps:fold(
                fun (_, BodyPart, Acc) ->
                    <<Acc/binary, "--", Boundary/binary, "\n", BodyPart/binary, "\n">>
                end,
                <<>>,
                Body
            ),
            % TODO: I _think_ this is needed, according to spec
            % End the body with a final terminating Boundary
            EncodedBody = <<Bin/binary, "--", Boundary/binary, "--">>,
            #{ 
                headers => [
                    {
						<<"Content-Type">>,
						<<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>
					}
                    | maps:get(headers, Http)
                ],
                body => EncodedBody
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
    <<EncodedHeaders/binary, <<"\n\n">>, SubBody/binary>>.

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

field_to_http(Http, {Name, MapOrList}, Opts) when is_map(MapOrList) orelse is_list(MapOrList) ->
    {Mapper, Parser} = case MapOrList of
        Map when is_map(Map) -> {fun hb_http_structured_fields:to_dictionary/1, fun hb_http_structured_fields:dictionary/1};
        List when is_list(List) -> {fun hb_http_structured_fields:to_list/1, fun hb_http_structured_fields:list/1}
    end,
    MaybeBin = case Mapper(MapOrList) of
        {ok, Sf} ->
            % Check the size of the encoded dictionary, and signal to store
            % the map as an Structured Field encoded dictionary in the header
            %
            % Otherwise, we will need to convert the Map into its own HTTP message
            % and append as a part of the body in the parent multi-part msg
            EncodedSf = Parser(Sf),
            case byte_size(EncodedSf) of
                Fits when Fits =< ?MAX_HEADER_LENGTH -> EncodedSf;
                _ -> undefined
            end;
        _ -> undefined
    end,
    ?no_prod("What should the name be?"),
    NormalizedName = hb_converge:key_to_binary(Name),
    case MaybeBin of
        Bin when is_binary(Bin) ->
            field_to_http(Http, {NormalizedName, Bin}, Opts);
        undefined when is_map(MapOrList) ->
            SubHttp = to(MapOrList),
            EncodedHttpMap = encode_http_msg(SubHttp),
            % Append to the serialized field to the parent body, as a part
            field_to_http(Http, {Name, EncodedHttpMap}, Opts);
        undefined when is_list(MapOrList) ->
            ?no_prod("how do we further encode a list?"),
            not_implemented
    end;
% field_to_http(Http, {Name, List}, Opts) when is_list(List) ->
%     {not_implemented, List};
field_to_http(Http, {Name, Value}, Opts) ->
    NormalizedName = hb_converge:key_to_binary(Name),
    NormalizedValue = hb_converge:key_to_binary(Value),

    % Depending on the size of the value, we may need to force
    % the value to be encoded into the body.
    %
    % Otherwise, we place the value according to Opts,
    % defaulting to header
    DefaultWhere = case byte_size(NormalizedValue) of
        Fits when Fits =< ?MAX_HEADER_LENGTH -> headers;
        _ -> maps:get(where, Opts, headers)
    end,

    case maps:get(where, Opts, DefaultWhere) of
        headers ->
            Headers = maps:get(headers, Http),
            NewHeaders = lists:append(Headers, [{NormalizedName, NormalizedValue}]),
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
            BodyPart = <<Disposition/binary, "\n\n", NormalizedValue/binary>>,
            NewBody = maps:put(NormalizedName, BodyPart, Body),
            maps:put(body, NewBody, Http)
    end.

from(#{ headers := Headers, body := Body }) ->
    ContentType = lists:keyfind(<<"Content-Type">>, 1, Headers),
    {item, _, Params} = hb_http_structured_fields:item(ContentType),
    Parts = case lists:keyfind(<<"boundary">>, 1, Params) of
        false -> [Body];
        {_, Boundary} ->
            % The first part will always be empty (since the boundary is always placed first
            % in the body
            [_, P] = binary:split(Body, <<"--", Boundary/binary>>),
            % The last part MIGHT be "--" for the terminating boundary.
            %
            % So we need to check and potentially trim off the last
            % element
            TrimmedParts = case lists:last(P) of
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



%%% @doc A codec for the that marshals TABM encoded messages to and from the
%%% "HTTP" message structure.
%%% 
%%% The HTTP Message is an Erlang Map with the following shape:
%%% #{ 
%%%     headers => [
%%%         {<<"example-header">>, <<"value">>}
%%%     ],
%%%     body: <<"some body">>
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
%%% Helper utilities
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% The max header length is 4KB
-define(MAX_HEADER_LENGTH, 4096).
% https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.4
-define(CRLF, <<"\r\n">>).
-define(DOUBLE_CRLF, <<?CRLF/binary, ?CRLF/binary>>).

%% @doc Convert an HTTP Message into a TABM.
%% HTTP Structured Field is encoded into it's equivalent TABM encoding.
from(Bin) when is_binary(Bin) -> Bin;
from(#{ <<"headers">> := Headers, <<"body">> := Body }) when is_map(Headers) ->
    from(#{ <<"headers">> => maps:to_list(Headers), <<"body">> => Body });
from(#{ <<"headers">> := Headers, <<"body">> := Body }) ->
    % First, parse all headers and add as key-value pairs to the TABM
    Map = from_headers(#{}, Headers),
    % Next, we need to potentially parse the body and add to the TABM
    % potentially as additional key-binary value pairs, or as sub-TABMs
    {_, ContentType} = find_header(Headers, <<"content-type">>),
    maps:remove(<<"content-type">>, from_body(Map, ContentType, Body)).

from_headers(Map, Headers) -> from_headers(Map, Headers, Headers).
from_headers(Map, [], _) -> Map;
from_headers(Map, [{Name, Value} | Rest], Headers) ->
    NewMap = case Name of
        % Handled as part of "Signature" so simply skip it
        % <<"signature-input">> -> Map;
        % <<"signature">> ->
        %     {_, SigInput} = find_header(Headers, <<"signature-input">>),
        %     from_signature(Map, Value, SigInput);
        % Decode the header as normal
        N -> maps:put(N, Value, Map)
    end,
    from_headers(NewMap, Rest, Headers).

from_signature(KVList, RawSig, RawSigInput) when is_list(KVList) ->
    from_signature(maps:from_list(KVList), RawSig, RawSigInput);
from_signature(Map, RawSig, RawSigInput) ->
    SfSigs = hb_http_structured_fields:parse_dictionary(RawSig),
    SfInputs = hb_http_structured_fields:parse_dictionary(RawSigInput),
    % Build a Map for Signatures by gathering each Signature
    % with its corresponding Inputs.
    % 
    % Inputs are merged as fields on the Signature Map
    Signatures = lists:foldl(
        fun ({SigName, {item, {_, Sig}, _}}, Sigs) ->
            {<<"signature">>, {list, SfInputItems}, SfInputParams} = lists:keyfind(SigName, 1, SfInputs),
            % [<<"foo">>, <<"bar">>]
            Inputs = lists:map(fun({item, {_, Input}, _}) -> Input end, SfInputItems),

            SigMap = lists:foldl(
                fun({PName, PBareItem}, PAcc) ->
                    maps:put(
                        PName,
                        hb_http_structured_fields:from_bare_item(PBareItem),
                        PAcc
                    )
                end,
                #{ <<"signature">> => Sig, <<"inputs">> => Inputs },
                 % Signature parameters are converted into top-level keys on the signature Map
                SfInputParams    
            ),
            % #{ [SigName/binary] => #{ <<"signature">> => <<>>, <<"inputs">> => , ... } }
            maps:put(SigName, SigMap, Sigs)
        end,
        #{},
        SfSigs
    ),
    % Finally place the Signatures as a top-level Map on the parent Map
    maps:put(<<"signatures">>, Signatures, Map).


find_header(Headers, Name) ->
    find_header(Headers, Name, []).
find_header(Headers, Name, Opts) when is_list(Headers) ->
    Matcher =
        fun ({N, _Value}) ->
            hb_converge:normalize_key(N) =:= hb_converge:normalize_key(Name)
        end,
    case lists:filter(Matcher, Headers) of
        [] -> {undefined, undefined};
        Found -> case lists:member(global, Opts) of
            true -> Found;
            _ ->
                [First | _] = Found,
                First
        end
    end.

from_body(TABM, _ContentType, <<>>) -> TABM;
from_body(TABM, ContentType, Body) ->
    ?event({from_body, {from_headers, TABM}, {content_type, ContentType}, {body, Body}}),
    {_BodyType, Params} =
        case ContentType of
            undefined -> {undefined, []};
            _ ->
                {item, {_, XT}, XParams} = hb_http_structured_fields:parse_item(ContentType),
                {XT, XParams}
        end,
    case lists:keyfind(<<"boundary">>, 1, Params) of
        % The body is not a multipart, so just set as is to the Body key on the TABM
        false ->
            maps:put(<<"body">>, Body, TABM);
        % We need to manually parse the multipart body into key/values on the TABM
        {_, {_Type, Boundary}} ->
            % Find the sub-part of the body within the boundary
            % We also make sure to account for the CRLF at end and beginning
            % of the starting and terminating part boundary, respectively
            % 
            % ie.
            % --foo-boundary\r\n
            % My-Awesome: Part
            %
            % an awesome body\r\n
            % --foo-boundary--
            BegPat = <<"--", Boundary/binary, ?CRLF/binary>>,
            EndPat = <<?CRLF/binary, "--", Boundary/binary, "--">>,
            {Start, SL} = binary:match(Body, BegPat),
            {End, _} = binary:match(Body, EndPat),
            BodyPart = binary:part(Body, Start + SL, End - (Start + SL)),
            % By taking into account all parts of the surrounding boundary above,
            % we get precisely the sub-part that we're interested without any
            % additional parsing
            Parts = binary:split(BodyPart, [<<"--", Boundary/binary>>], [global]),
            % Finally, for each part within the sub-part, we need to parse it,
            % potentially recursively as a sub-TABM, and then add it to the
            % current TABM
            {ok, NewTABM} = from_body_parts(TABM, Parts),
            NewTABM
    end.

from_body_parts (TABM, []) -> {ok, TABM};
from_body_parts(TABM, [Part | Rest]) ->
    % Extract the Headers block and Body. Only split on the FIRST double CRLF
    [RawHeadersBlock, RawBody] = case binary:split(Part, [?DOUBLE_CRLF], []) of
        % no body
        [RHB] -> [RHB, <<>>];
        [RHB, RB] -> [RHB, RB]
    end,
    % Extract individual headers
    RawHeaders = binary:split(RawHeadersBlock, ?CRLF, [global]),
    % Now we parse each header, splitting into {Key, Value}
    Headers = lists:filtermap(
        fun
            % Skip empty headers that are missing in splitting
            (<<>>) -> false;
            (RawHeader) -> 
                case binary:split(RawHeader, [<<": ">>]) of
                    [Name, Value] -> {true, {Name, Value}};
                    % skip lines that aren't properly formatted headers
                    _ -> false
                end
        end,
        RawHeaders
    ),
    % The Content-Disposition is from the parent message,
    % so we separate off from the rest of the headers
    {AllContentDisposition, RestHeaders} = lists:partition(
        fun
            ({HeaderName, _}) -> hb_util:to_lower(HeaderName) =:= <<"content-disposition">>;
            (_) -> false
        end,
        Headers    
    ),
    RawDisposition = case AllContentDisposition of
        [] -> undefined;
        % Just grab the first Content-Disposition header value
        [{_, CD} | _Rest] -> CD
    end,
    case RawDisposition of
        % A Content-Disposition header is required for each part
        % in the multipart body
        undefined -> no_content_disposition_header_found;
        % Extract the name 
        RawDisposition when is_binary(RawDisposition) ->
            {item, {_, Disposition}, DispositionParams} =
                hb_http_structured_fields:parse_item(RawDisposition),
            {ok, PartName} = case Disposition of
                % The inline part is the body
                <<"inline">> ->
                    {ok, <<"body">>};
                % Otherwise, we need to extract the name of the part
                % from the Content-Disposition parameters
                _ ->
                    case lists:keyfind(<<"name">>, 1, DispositionParams) of
                        {_, {_type, PN}} -> {ok, PN};
                        false -> no_part_name_found
                    end
            end,
            ParsedPart = case RestHeaders of
                % There are no headers besides the content disposition header
                % So simply use the the raw body binary as the part
                [] -> RawBody;
                % We need recursively parse the sub part into its own TABM
                _ -> from(#{ <<"headers">> => RestHeaders, <<"body">> => RawBody })
            end,
            from_body_parts(maps:put(PartName, ParsedPart, TABM), Rest)
    end.

%%% @doc Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
%%% that can translated to a given web server Response API
to(Bin) when is_binary(Bin) -> Bin;
to(TABM) -> to(TABM, []).
to(TABM, Opts) when is_map(TABM) ->
    % PublicMsg = hb_private:reset(TABM),
    % MinimizedMsg = hb_message:minimize(PublicMsg),
    Http = maps:fold(
        fun(RawKey, Value, Http) ->
            Key = hb_converge:normalize_key(RawKey),
            case hb_util:to_lower(Key) of
                <<"body">> -> body_to_http(Http, Value);
                %<<"signature">> -> signatures_to_http(Http, [Value]);
                %<<"signatures">> -> signatures_to_http(Http, Value);
                _ -> field_to_http(Http, {Key, Value}, #{})
            end
        end,
        #{ <<"headers">> => [], <<"body">> => #{} },
        TABM
    ),
    BodyMap = maps:get(<<"body">>, Http),
    NewHttp =
        case {BodyMap, lists:member(sub_part, Opts)} of
            % If the body map is empty, then simply set the body to be a corresponding empty binary.
            {X, _} when map_size(X) =:= 0 ->
                maps:put(<<"body">>, <<>>, Http);
            % Simply set the sole body binary as the body of the
            % HTTP message, no further encoding required
            % 
            % NOTE: this may only be done for the top most message as sub-messages MUST be
            % encoded as sub-parts, in order to preserve the nested hierarchy of messages,
            % even in the case of a sole body binary.
            % 
            % In all other cases, the mapping fallsthrough to the case below that properly
            % encodes a nested body within a sub-part
            {#{ <<"body">> := UserBody }, false} when map_size(BodyMap) =:= 1 andalso is_binary(UserBody) ->
                ?event({encoding_single_body, {body, UserBody}, {http, Http}}),
                maps:put(<<"body">>, UserBody, Http);
            % Otherwise, we need to encode the body map as the
            % multipart body of the HTTP message
            _ ->
                ?event({encoding_multipart, {body, BodyMap}, {http, Http}}),
                % The id of the Message will be used as the Boundary
                % in the multipart body
                {ok, RawBoundary} = dev_message:id(TABM),
                Boundary = hb_util:encode(RawBoundary),
                % Transform body into a binary, delimiting each part with the Boundary
                BodyList = maps:fold(
                    fun (PartName, BodyPart, Acc) ->
                            ?event({encoding_multipart_part, {part, PartName, BodyPart}, {http, Http}}),
                            % We'll need to prepend a Content-Disposition header to the part, using
                            % the field name as the form part name.
                            % (See https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
                            Disposition = case PartName of
                                % The body is always made the inline part of the
                                % multipart body
                                <<"body">> -> <<"inline">>;
                                _ -> <<"form-data;name=", "\"", PartName/binary, "\"">>
                            end,
                            % Sub-parts MUST have at least one header, according to the multipart spec.
                            % Adding the Content-Disposition not only satisfies that requirement,
                            % but also encodes the HB message field that resolves to the sub-message
                            EncodedBodyPart = case BodyPart of
                                BPMap when is_map(BPMap) ->
                                    WithDisposition = maps:put(
                                        <<"Content-Disposition">>,
                                        Disposition,
                                        BPMap
                                    ),
                                    SubHttp = to(WithDisposition, [sub_part]),
                                    EncodedHttp = encode_http_msg(SubHttp),
                                    EncodedHttp;
                                BPBin when is_binary(BPBin) ->
                                    case PartName of
                                        % A properly encoded inlined body part MUST have a CRLF between
                                        % it and the header block, so we MUST use two CRLF:
                                        % - first to signal end of the Content-Disposition header
                                        % - second to signal the end of the header block
                                        <<"body">> -> 
                                            <<
                                                "Content-Disposition: ", Disposition/binary, ?CRLF/binary,
                                                ?CRLF/binary,
                                                BPBin/binary
                                            >>;
                                        % All other binary values are encoded as a header in their
                                        % respective sub-part
                                        _ ->
                                            <<
                                                "Content-Disposition: ", Disposition/binary, ?CRLF/binary,
                                                BPBin/binary
                                            >>
                                    end
                                    
                            end,
                            [
                                <<
                                    "--", Boundary/binary, ?CRLF/binary,
                                    EncodedBodyPart/binary
                                >>
                            |
                                Acc
                            ]
                    end,
                    [],
                    BodyMap
                ),
                % Finally, join each part of the multipart body into a single binary
                % to be used as the body of the Http Message
                BodyBin = iolist_to_binary(lists:join(?CRLF, lists:reverse(BodyList))),
                % Ensure we append the Content-Type to be a multipart response
                #{ 
                    <<"headers">> => [
                        {
                            <<"content-type">>,
                            <<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>
                        }
                        | maps:get(<<"headers">>, Http)
                    ],
                    % End the body with a final terminating Boundary
                    <<"body">> => <<BodyBin/binary, ?CRLF/binary, "--", Boundary/binary, "--">>
                }
        end,
    NewHttp.

encode_http_msg (_Http = #{ <<"headers">> := SubHeaders, <<"body">> := SubBody }) ->
    % Serialize the headers, to be included in the part of the multipart response
    HeaderList = lists:foldl(
        fun ({HeaderName, HeaderValue}, Acc) ->
            [<<HeaderName/binary, ": ", HeaderValue/binary>> | Acc]
        end,
        [],
        SubHeaders
    ),
    EncodedHeaders = iolist_to_binary(lists:join(?CRLF, lists:reverse(HeaderList))),
    case SubBody of
        <<>> -> EncodedHeaders;
        % Some-Headers: some-value
        % content-type: image/png
        % 
        % <body>
        _ -> <<EncodedHeaders/binary, ?DOUBLE_CRLF/binary, SubBody/binary>>
    end.

signatures_to_http(Http, Signatures) when is_map(Signatures) ->
    signatures_to_http(Http, maps:to_list(Signatures));
signatures_to_http(Http, Signatures) when is_list(Signatures) ->
    ?event({encoding_signatures, Signatures}),
    {SfSigInputs, SfSigs} = lists:foldl(
        fun ({SigName, SignatureMap = #{ <<"inputs">> := Inputs, <<"signature">> := Signature }}, {SfSigInputs, SfSigs}) ->
            NextSigInput = hb_http_signature:sf_signature_params(Inputs, SignatureMap),
            NextSig = hb_http_signature:sf_signature(Signature),
            NextName = hb_converge:normalize_key(SigName),
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
    WithSig = field_to_http(Http, {<<"signature">>, hb_http_structured_fields:dictionary(SfSigs)}, #{}),
    WithSigAndInput = field_to_http(WithSig, {<<"signature-input">>, hb_http_structured_fields:dictionary(SfSigInputs)}, #{}),
    WithSigAndInput.

% Force the value to be encoded into the body of the HTTP message
body_to_http(Http, Body) ->
    field_to_http(Http, {<<"body">>, Body}, #{ where => body }).

% All maps are encoded into the body of the HTTP message
% to be further encoded later.
field_to_http(Http, {Name, Value}, _Opts) when is_map(Value) ->
    NormalizedName = hb_converge:normalize_key(Name),
    BodyMap = maps:get(<<"body">>, Http, #{}),
    NewBody = maps:put(NormalizedName, Value, BodyMap),
    maps:put(<<"body">>, NewBody, Http);
field_to_http(Http, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_converge:normalize_key(Name),
    % The default location where the value is encoded within the HTTP
    % message depends on its size.
    % 
    % So we check whether the size of the value is within the threshold
    % to encode as a header, and otherwise default to encoding in the body.
    %
    % Note that a "where" Opts may force the location of the encoded
    % value -- this is only a default location if not specified in Opts 
    DefaultWhere =
        case {maps:get(where, Opts, headers), byte_size(Value)} of
            {headers, Fits} when Fits =< ?MAX_HEADER_LENGTH -> headers;
            _ -> body
        end,
    case maps:get(where, Opts, DefaultWhere) of
        headers ->
            Headers = maps:get(<<"headers">>, Http),
            NewHeaders = lists:append(Headers, [{NormalizedName, Value}]),
            maps:put(<<"headers">>, NewHeaders, Http);
        body ->
            BodyMap = maps:get(<<"body">>, Http, #{}),
            NewBodyMap = maps:put(NormalizedName, Value, BodyMap),
            maps:put(<<"body">>, NewBodyMap, Http)
    end.

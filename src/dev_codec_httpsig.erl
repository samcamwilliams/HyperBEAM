%%% @doc This module implements HTTP Message Signatures as described in RFC-9421
%%% (https://datatracker.ietf.org/doc/html/rfc9421), as an AO-Core device.
%%% It implements the codec standard (from/1, to/1), as well as the optional
%%% commitment functions (id/3, sign/3, verify/3). The commitment functions
%%% are found in this module, while the codec functions are relayed to the 
%%% `dev_codec_httpsig_conv' module.
-module(dev_codec_httpsig).
%%% Codec API functions
-export([to/3, from/3]).
%%% Commitment API functions
-export([commit/3, verify/3]).
%%% Public API functions
-export([add_content_digest/2]).
-export([add_derived_specifiers/1, remove_derived_specifiers/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
-define(EMPTY_QUERY_PARAMS, $?).
%%% A list of components that are `derived' in the context of RFC-9421 from the
%%% request message.
-define(DERIVED_COMPONENTS, [
    <<"method">>,
    <<"target-uri">>,
    <<"authority">>,
    <<"scheme">>,
    <<"request-target">>,
    <<"path">>,
    <<"query">>,
    <<"query-param">>,
    <<"status">>
]).

%%% Routing functions for the `dev_codec_httpsig_conv' module
to(Msg, Req, Opts) -> dev_codec_httpsig_conv:to(Msg, Req, Opts).
from(Msg, Req, Opts) -> dev_codec_httpsig_conv:from(Msg, Req, Opts).

% @doc Verify the signature of a commitment based on its `type' parameter.
verify(Base, Req = #{ <<"type">> := <<"hmac-sha256">>, <<"signature">> := ID }, Opts) ->
    % A hmac-sha256 commitment is verified simply by generating the ID from the
    % given committed keys.
    case hmac(Base, Req, Opts) of
        {ok, ID} -> {ok, true};
        {error, _} -> {ok, false}
    end;
verify(Base, Req = #{ <<"type">> := <<"rsa-pss-sha512">> }, Opts) ->
    % A rsa-pss-sha512 commitment is verified by regenerating the signature
    % base and validating against the signature.
    SigBase = signature_base(Base, Req, Opts),
    PubKey = maps:get(<<"keyid">>, Req),
    Signature = maps:get(<<"signature">>, Req),
    {ok, ar_wallet:verify({{rsa, 65537}, PubKey}, SigBase, Signature, sha512)};
verify(_Base, Req, _Opts) ->
    {error, {httpsig_unsupported_commitment_request, Req}}.

%% @doc Commit to a message using the HTTP-Signature format. We use the `type'
%% parameter to determine the type of commitment to use. If the `type' parameter
%% is `signed', we default to the rsa-pss-sha512 algorithm. If the `type'
%% parameter is `unsigned', we default to the hmac-sha256 algorithm.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"hmac-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"rsa-pss-sha512">> }, Opts);
commit(MsgToSign, Req = #{ <<"type">> := <<"rsa-pss-sha512">> }, Opts) ->
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    % Utilize the hashpath, if present, as the tag for the commitment.
    MaybeTagMap =
        case MsgToSign of
            #{ <<"priv">> := #{ <<"hashpath">> := HP }} ->
                #{ <<"tag">> => HP };
            _ -> #{}
        end,
    % Generate the unsigned commitment and signature base.
    UnsignedCommitment =
        MaybeTagMap#{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"type">> => <<"rsa-pss-sha512">>,
            <<"keyid">> => ar_wallet:to_pubkey(Wallet),
            <<"committer">> =>
                hb_util:human_id(ar_wallet:to_address(Wallet)),
            <<"committed">> =>
                hb_ao:normalize_keys(keys_to_commit(MsgToSign, Req, Opts))
        },
    ?event({encoded_to_httpsig_for_commitment, MsgToSign}),
    % Generate the signature base
    SignatureBase = signature_base(MsgToSign, UnsignedCommitment, Opts),
    % Sign the signature base
    Signature = ar_wallet:sign(Wallet, SignatureBase, sha512),
    % Generate the ID of the signature
    ID = hb_util:human_id(crypto:hash(sha256, Signature)),
    % Calculate the ID and place the signature into the `commitments' key of the
    % message. After, we call `commit' again to add the hmac to the new
    % message.
    commit(
        MsgToSign#{
            <<"commitments">> =>
                (maps:get(<<"commitments">>, MsgToSign, #{}))#{
                    ID => UnsignedCommitment#{ <<"signature">> => Signature }
                }
        },
        #{
            <<"type">> => <<"hmac-sha256">>
        },
        Opts
    );
commit(Msg, Req = #{ <<"type">> := <<"hmac-sha256">> }, Opts) ->
    % Find the ID of the message without hmac commitments, then add the hmac
    % using the set of all presently committed keys as the input. If no (other)
    % commitments are present, then use all keys from the encoded message.
    WithoutHmac =
        hb_message:without_commitments(
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"hmac-sha256">>
            },
            Msg,
            Opts
        ),
    % Extract the base commitments from the message.
    Commitments = maps:get(<<"commitments">>, WithoutHmac, #{}),
    CommittedKeys = keys_to_commit(Msg, Req, Opts),
    {ok, ID} = hmac(WithoutHmac, #{ <<"committed">> => CommittedKeys }, Opts),
    EncID = hb_util:human_id(ID),
    Res = {
        ok,
        hb_maps:put(
            <<"commitments">>,
            Commitments#{
                EncID =>
                    #{
                        <<"commitment-device">> => <<"httpsig@1.0">>,
                        <<"type">> => <<"hmac-sha256">>,
                        <<"keyid">> => <<"ao">>,
                        <<"signature">> => ID,
                        <<"committed">> => hb_ao:normalize_keys(CommittedKeys)
                    }
            },
            Msg
        )
    },
    ?event({reset_hmac_complete, Res}),
    Res.

%% @doc Derive the set of keys to commit to from a `commit` request and a 
%% base message.
%% 
%% We do this by:
%% 1. Checking if the user has set specific keys to commit to in the request 
%%    itself.
%% 2. Checking if there are existing commitments and replicate the `committed`
%%    if so.
%% 3. If neither of the previous sources are viable, returning the full list of
%%    keys.
keys_to_commit(Base, #{ <<"committed">> := Explicit}, _Opts) ->
    % Case 1: Explicitly provided keys to commit.
    % Add `+link` specifiers to the user given list as necessary, in order for
    % their given keys to match the HTTPSig encoed TABM form.
    lists:map(
        fun(Key) ->
            case maps:is_key(Key, Base) of
                true -> Key;
                false ->
                    case maps:is_key(<<Key/binary, "+link">>, Base) of
                        true -> <<Key/binary, "+link">>;
                        false ->
                            throw({requested_key_not_found, Key, Base})
                    end
            end
        end,
        Explicit
    );
keys_to_commit(Base, _Req, Opts) ->
    % Extract the set of committed keys from the message.
    ExistingCommittedReq =
        #{
            <<"committers">> => <<"all">>,
            <<"raw">> => true
        },
    case hb_message:committed(Base, ExistingCommittedReq, Opts) of
        [] ->
            % Case 3: Default to all keys in the TABM-encoded message, aside
            % metadata.
            maps:keys(Base) -- [<<"commitments">>, <<"priv">>];
        Keys ->
            % Case 2: Replicate the raw keys that the existing commitments have
            % used. This leads to a message whose commitments can be 'stacked'
            % and represented together in HTTPSig format.
            Keys
    end.

%% @doc If the `body' key is present and a binary, replace it with a
%% content-digest.
add_content_digest(Msg, Opts) ->
    case hb_maps:get(<<"body">>, Msg, not_found, Opts) of
        Body when is_binary(Body) ->
            % Remove the body from the message and add the content-digest,
            % encoded as a structured field.
            ?event({add_content_digest, {string, Body}}),
            (hb_maps:without([<<"body">>], Msg))#{
                <<"content-digest">> =>
                    hb_util:bin(hb_structured_fields:dictionary(
                        #{
                            <<"sha-256">> =>
                                {item, {binary, hb_crypto:sha256(Body)}, []}
                        }
                    ))
            };
        _ -> Msg
    end.

%% @doc Generate the ID of the message, with the current signature and signature
%% input as the components for the hmac.
hmac(Msg, Req, Opts) ->
    % Find the committed keys. Default: all keys.
    Committed = maps:get(<<"committed">>, Req, maps:keys(Msg)),
    ?event(hmac, {generating_hmac_on, {msg, Msg}, {keys, Committed}}),
    % Find the keys to use for the hmac. These should be set by the signature
    % input, but if that is not present, then use all the keys from the encoded
    % message.
    HMACSpecifiers = normalize_component_identifiers(Committed),
    % Generate the signature base
    SignatureBase =
        signature_base(
            Msg,
            #{
                <<"committed">> => HMACSpecifiers,
                <<"keyid">> => <<"ao">>,
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"hmac-sha256">>
            },
            Opts
        ),
    ?event(hmac, {hmac_base, {string, SignatureBase}}),
    HMacValue = crypto:mac(hmac, sha256, <<"ao">>, SignatureBase),
    ?event(hmac, {hmac_result, HMacValue}),
    {ok, HMacValue}.

%% @doc Takes a list of keys that will be used in the signature inputs and 
%% ensures that they have deterministic sorting, as well as the correct
%% component identifiers if applicable.
normalize_component_identifiers(ComponentIdentifiers) ->
    % Remove the @ prefix from the component identifiers, if present.
    Stripped =
        lists:map(
            fun(<<"@", Key/binary>>) -> Key; (Key) -> Key end,
            ComponentIdentifiers
        ),
    % Sort the component identifiers and add the correct specifiers back in,
    % if applicable.
    lists:sort(add_derived_specifiers(Stripped)).

%% @doc Normalize key parameters to ensure their names are correct.
add_derived_specifiers(ComponentIdentifiers) ->
    lists:flatten(
        lists:map(
            fun(Key) ->
                case lists:member(Key, ?DERIVED_COMPONENTS) of
                    true -> << "@", Key/binary >>;
                    false -> Key
                end
            end,
            ComponentIdentifiers
        )
    ).

%% @doc Remove derived specifiers from a list of component identifiers.
remove_derived_specifiers(ComponentIdentifiers) ->
    lists:map(
        fun(<<"@", Key/binary>>) ->
            Key;
        (Key) ->
            case hb_link:is_link_key(Key) of
                true -> binary:part(Key, 0, byte_size(Key) - 5);
                false -> Key
            end
        end,
        ComponentIdentifiers
    ).

%% @doc create the signature base that will be signed in order to create the
%% Signature and SignatureInput.
%%
%% This implements a portion of RFC-9421 see:
%% https://datatracker.ietf.org/doc/html/rfc9421#name-creating-the-signature-base
signature_base(Msg, Commitment, Opts) when is_map(Commitment) ->
    % Remove the body and set the content-digest as a field
    MsgWithContentDigest = add_content_digest(Msg, Opts),
    % If the commitment references the `body` key, then we remove it and replace
    % it with the content-digest.
    NewCommitment =
        Commitment#{
            <<"committed">> =>
                dev_codec_httpsig_siginfo:committed_keys_to_siginfo(
                    maps:get(<<"committed">>, Commitment, [])
                )
        },
	ComponentsLine =
        signature_components_line(
            NewCommitment,
            #{},
            MsgWithContentDigest,
            Opts
        ),
    ?event({component_identifiers_for_sig_base, ComponentsLine}),
	ParamsLine = signature_params_line(NewCommitment, Opts),
    SignatureBase = 
        <<
            ComponentsLine/binary, "\n",
            "\"@signature-params\": ", ParamsLine/binary
        >>,
    ?event(signature_base, {signature_base, {string, SignatureBase}}),
	SignatureBase.

%% @doc Given a list of Component Identifiers and a Request/Response Message
%% context, create the "signature-base-line" portion of the signature base
%% TODO: catch duplicate identifier:
%% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.1
%%
%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.1
signature_components_line(Commitment, Req, Res, Opts) ->
	ComponentsLines =
        lists:map(
            fun(Name) when is_map_key(Name, Res)->
                % Use `hb_maps' to get each value, such that we lookup data 
                % in the cache as necessary.
                Value = hb_maps:get(Name, Res, not_found, Opts),
                << Name/binary, <<": ">>/binary, Value/binary>>;
            (ComponentIdentifier) ->
                {ok, {I, V}} =
                    identifier_to_component(ComponentIdentifier, Req, Res, Opts),
                <<I/binary, <<": ">>/binary, V/binary>>
            end,
            hb_util:to_sorted_list(maps:get(<<"committed">>, Commitment))
        ),
	iolist_to_binary(lists:join(<<"\n">>, ComponentsLines)).

%% @doc construct the "signature-params-line" part of the signature base.
%%
%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
signature_params_line(Commitment, _Opts) ->
	hb_util:bin(
        hb_structured_fields:list(
            [
                {
                    list,
                    lists:map(
                        fun(Key) -> {item, {string, Key}, []} end,
                        maps:get(<<"committed">>, Commitment)
                    ),
                    lists:map(
                        fun ({Name, Param}) when is_binary(Param) ->
                            {Name, {string, Param}};
                        ({Name, Param}) when is_integer(Param) ->
                            {Name, Param}
                        end,
                        maps:to_list(
                            maps:with(
                                [
                                    <<"created">>,
                                    <<"expires">>,
                                    <<"nonce">>,
                                    <<"keyid">>,
                                    <<"tag">>
                                ],
                                Commitment
                            )
                        )
                    )
                }
            ]
        )
    ).

%% @doc Given a Component Identifier and a Request/Response Messages Context
%% extract the value represented by the Component Identifier, from the Messages
%% Context, and return the normalized form of the identifier, along with the
%% extracted encoded value.
%%
%% Generally speaking, a Component Identifier may reference a "Derived" Component,
%% a Message Field, or a sub-component of a Message Field.
%%
%% Since a Component Identifier is itself a Structured Field, it may also specify
%% parameters, which are used to describe behavior such as which Message to
%% derive a field or sub-component of the field, and how to encode the value as
%% part of the signature base.
identifier_to_component(Identifier, Req, Res, Opts) when is_list(Identifier) ->
	identifier_to_component(list_to_binary(Identifier), Req, Res, Opts);
identifier_to_component(Identifier, Req, Res, Opts) when is_atom(Identifier) ->
	identifier_to_component(atom_to_binary(Identifier), Req, Res, Opts);
identifier_to_component(Identifier, Req, Res, Opts) when is_binary(Identifier) ->
	identifier_to_component(
        {item, {string, Identifier}, []},
        Req,
        Res,
        Opts
    );
identifier_to_component(ParsedIdentifier = {item, {X, Value}, Params}, Req, Res, Opts) ->
	case Value of
		<<$@, Rest/bits>> -> 
            extract_field({item, {X, Rest}, Params}, Req, Res, Opts);
		_ -> extract_field(ParsedIdentifier, Req, Res, Opts)
	end.

%% @doc Given a Component Identifier and a Request/Response Messages Context
%% extract the value represented by the Component Identifier, from the Messages
%% Context, specifically a field on a Message within the Messages Context,
%% and return the normalized form of the identifier, along with the extracted
%% encoded value.
%%
%% This implements a portion of RFC-9421
%% See https://datatracker.ietf.org/doc/html/rfc9421#name-http-fields
extract_field({item, {_Kind, IParsed}, IParams}, Req, Res, Opts) ->
	IsStrictFormat = find_strict_format_param(IParams),
	IsByteSequenceEncoded = find_byte_sequence_param(IParams),
	DictKey = find_key_param(IParams),
	case (IsStrictFormat orelse DictKey =/= false) andalso IsByteSequenceEncoded of
		true ->
			% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.2
			{
                conflicting_params_error,
                <<
                    "Component Identifier parameter 'bs' MUST not ",
                    "be used with 'sf' or 'key'"
                >>
            };
		_ ->
			NormParsed = hb_ao:normalize_key(IParsed),
			NormalizedItem =
                hb_structured_fields:item(
                    {item, {string, NormParsed}, IParams}
                ),
            IsRequestIdentifier = find_request_param(IParams),
			% There may be multiple fields that match the identifier on the Msg,
			% so we filter, instead of find
            ContextMsg = if IsRequestIdentifier -> Req; true -> Res end,
			case hb_maps:get(NormParsed, ContextMsg, not_found, Opts) of
				not_found ->
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.6
					{
                        field_not_found_error,
                        <<"Component Identifier for a field MUST be ",
                            "present on the message">>,
                        {key, NormParsed},
                        {context, ContextMsg}
                    };
				FieldValue ->
					% The Field was found, but we still need to potentially
                    % parse it (it could be a Structured Field) and potentially
                    % extract subsequent values ie. specific dictionary key and
                    % its parameters, or further encode it
					case
						extract_field_value(
                            [FieldValue],
                            [DictKey, IsStrictFormat, IsByteSequenceEncoded])
					of
						{ok, Extracted} ->
                            {ok,
                                {hb_util:bin(NormalizedItem), hb_util:bin(Extracted)}
                            };
						E -> E
					end
			end
	end.

%% @doc Extract values from the field and return the normalized field,
%% along with encoded value
extract_field_value(RawFields, [Key, IsStrictFormat, IsByteSequenceEncoded]) ->
	% TODO: (maybe this already works?) empty string for empty header
	HasKey = case Key of false -> false; _ -> true end,
	case not (HasKey orelse IsStrictFormat orelse IsByteSequenceEncoded) of
        % No RFC-9421 parameterized encodings ie. "sf", "bs", "key"
        % (see https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-17)
        % So simply normalize and return the field values.
        % 
        % This takes into account the list-based fields serialization
        % described in https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-5
		true ->
			Normalized = [trim_and_normalize(Field) || Field <- RawFields],
			{ok, bin(lists:join(<<", ">>, Normalized))};
		_ ->
			case IsByteSequenceEncoded of
				% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.3-2
				true ->
					SfList = [
						{item, {binary, trim_and_normalize(Field)}, []}
					 || Field <- RawFields
					],
					sf_encode(SfList);
				_ ->
                    % In all cases, multiple fields MUST be combined
                    % into a single data structure, before serialization,
                    %
                    % See https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.1-2
                    % And https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.2-2
					Combined = bin(lists:join(<<", ">>, RawFields)),
					case sf_parse(Combined) of
						% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.1-3
						{error, _} ->
							{
                                sf_parsing_error,
                                <<"Component Identifier value could not ",
                                    "be parsed as a structured field">>
                            };
						{ok, SF} ->
							case HasKey of
                                % https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.1
								false -> case IsStrictFormat of
                                    % just re-serialize, which should properly
                                    % format the data in Strict-Formatting style
                                    true -> sf_encode(SF);
                                    _ -> Combined
                                end;
								_ -> extract_dictionary_field_value(SF, Key)
							end
					end
			end
	end.

%% @doc Extract a value from a Structured Field, and return the normalized field,
%% along with the encoded value
extract_dictionary_field_value(StructuredField = [Elem | _Rest], Key) ->
	case Elem of
		{Name, _} when is_binary(Name) ->
			case lists:keyfind(Key, 1, StructuredField) of
				% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.2-5
				false ->
					{
                        sf_dicionary_key_not_found_error,
                        <<"Component Identifier references key not ",
                            "found in dictionary structured field">>,
                        {key, Key},
                        {structured_field, StructuredField}
                    };
				{_, Value} ->
					sf_encode(Value)
			end;
		_ ->
			{
                sf_not_dictionary_error,
                <<"Component Identifier cannot reference key on a ",
                    "non-dictionary structured field">>
            }
	end.

%% @doc Given a Component Identifier and a Request/Response Messages Context
%% extract the value represented by the Component Identifier, from the Messages
%% Context, specifically a "Derived" Component within the Messages Context,
%% and return the normalized form of the identifier, along with the extracted
%% encoded value.
%%
%% This implements a portion of RFC-9421
%% See https://datatracker.ietf.org/doc/html/rfc9421#name-derived-components
derive_component(Identifier, Req, Res) when map_size(Res) == 0 ->
	derive_component(Identifier, Req, Res, req);
derive_component(Identifier, Req, Res) ->
	derive_component(Identifier, Req, Res, res).
derive_component({item, {_Kind, IParsed}, IParams}, Req, Res, Subject) ->
	case find_request_param(IParams) andalso Subject =:= req of
		% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.3
		true ->
			{
                req_identifier_error,
                <<"A Component Identifier may not contain a req parameter ",
                    "if the target is a request message">>
            };
		_ ->
			Lowered = lower_bin(IParsed),
			NormalizedItem =
                hb_structured_fields:item(
                    {item, {string, Lowered}, IParams}
                ),
			Result =
				case Lowered of
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.2.1
					<<"@method">> ->
						{ok, upper_bin(hb_maps:get(<<"method">>, Req, <<>>))};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.4.1
					<<"@target-uri">> ->
						{ok, bin(hb_maps:get(<<"path">>, Req, <<>>))};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.6.1
					<<"@authority">> ->
						URI = uri_string:parse(hb_maps:get(<<"path">>, Req, <<>>)),
						Authority = hb_maps:get(host, URI, <<>>),
						{ok, lower_bin(Authority)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.8.1
					<<"@scheme">> ->
						URI = uri_string:parse(hb_maps:get(<<"path">>, Req)),
						Scheme = hb_maps:get(scheme, URI, <<>>),
						{ok, lower_bin(Scheme)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.10.1
					<<"@request-target">> ->
						URI = uri_string:parse(hb_maps:get(<<"path">>, Req)),
						% If message contains the absolute form value, then
						% the value must be the absolut url
						%
						% TODO: maybe better way to distinguish besides a flag
						% on the request?
						%
						% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.5-10
						RequestTarget =
							case hb_maps:get(is_absolute_form, Req, false) of
								true -> hb_maps:get(url, Req);
								_ ->
                                    lists:join($?,
                                        [
                                            hb_maps:get(path, URI, <<>>),
                                            hb_maps:get(query, URI, ?EMPTY_QUERY_PARAMS)
                                        ]
                                    )
							end,
						{ok, bin(RequestTarget)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.12.1
					<<"@path">> ->
						URI = uri_string:parse(hb_maps:get(<<"path">>, Req)),
						Path = hb_maps:get(path, URI),
						{ok, bin(Path)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.14.1
					<<"@query">> ->
						URI = uri_string:parse(hb_maps:get(<<"path">>, Req)),
						% No query params results in a "?" value
						% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
						Query =
							case hb_maps:get(query, URI, <<>>) of
								<<>> -> ?EMPTY_QUERY_PARAMS;
								Q -> Q
							end,
						{ok, bin(Query)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.16.1
					<<"@query-param">> ->
						case find_name_param(IParams) of
							% The name parameter MUST be provided when specifiying a @query-param
							% Derived Component. See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-1
							false ->
								{
                                    req_identifier_error,
                                    <<"@query_param Derived Component Identifier ",
                                    "must specify a name parameter">>
                                };
							Name ->
								URI = uri_string:parse(hb_maps:get(<<"path">>, Req)),
								QueryParams =
                                    uri_string:dissect_query(hb_maps:get(query, URI, "")),
								QueryParam =
									case lists:keyfind(Name, 1, QueryParams) of
										{_, QP} -> QP;
										% An missing or empty query param value results in
										% an empty string value in the signature base
										% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-4
										_ -> ""
									end,
								{ok, bin(QueryParam)}
						end;
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.18.1
					<<"@status">> ->
						case Subject =:= req of
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.9-8
							true ->
								{
                                    res_identifier_error,
                                    <<"@status Derived Component must not be ",
                                    "used if target is a request message">>
                                };
							_ ->
								Status = hb_maps:get(<<"status">>, Res, <<"200">>),
								{ok, Status}
						end
				end,
            ?event({derive_component, IParsed, Result}),
			case Result of
				{ok, V} ->
                    ?event({derive_component, IParsed, {ok, V}}),
                    {ok, {bin(NormalizedItem), V}};
				E -> E
			end
	end.

%%%
%%% Strucutured Field Utilities
%%%

%% @doc Attempt to parse the binary into a data structure that represents
%% an HTTP Structured Field.
%%
%% Lacking some sort of "hint", there isn't a way to know which "kind" of
%% Structured Field the binary is, apriori. So we simply try each parser,
%% and return the first invocation that doesn't result in an error.
%%
%% If no parser is successful, then we return an error tuple
sf_parse(Raw) when is_list(Raw) -> sf_parse(list_to_binary(Raw));
sf_parse(Raw) when is_binary(Raw) ->
	Parsers = [
		fun hb_structured_fields:parse_list/1,
		fun hb_structured_fields:parse_dictionary/1,
		fun hb_structured_fields:parse_item/1
	],
	sf_parse(Parsers, Raw).

sf_parse([], _Raw) ->
    {error, undefined};
sf_parse([Parser | Rest], Raw) ->
    case catch Parser(Raw) of
        % skip parsers that fail
        {'EXIT', _} -> sf_parse(Rest, Raw);
        Parsed -> {ok, Parsed}
    end.

%% @doc Attempt to encode the data structure into an HTTP Structured Field.
%% This is the inverse of sf_parse.
sf_encode(StructuredField = {list, _, _}) ->
	% The value is an inner_list, and so needs to be wrapped with an outer list
	% before being serialized
	sf_encode(fun hb_structured_fields:list/1, [StructuredField]);
sf_encode(StructuredField = {item, _, _}) ->
	sf_encode(fun hb_structured_fields:item/1, StructuredField);
sf_encode(StructuredField = [Elem | _Rest]) ->
	sf_encode(
		% Both an sf list and dictionary is represented in Erlang as a List of
        % pairs but a dictionary's members will always be a pair whose first value
		% is a binary, so we can match on that to determine which serializer to use
		case Elem of
			{Name, _} when is_binary(Name) ->
                fun hb_structured_fields:dictionary/1;
			_ ->
                fun hb_structured_fields:list/1
		end,
		StructuredField
	).
sf_encode(Serializer, StructuredField) ->
	case catch Serializer(StructuredField) of
		{'EXIT', _} -> {error, <<"Could not serialize into structured field">>};
		Parsed -> {ok, Parsed}
	end.

%% @doc Attempt to parse the provided value into an HTTP Structured Field Item
sf_item(SfItem = {item, {_Kind, _Parsed}, _Params}) ->
	SfItem;
sf_item(ComponentIdentifier) when is_list(ComponentIdentifier) ->
	sf_item(list_to_binary(ComponentIdentifier));
sf_item(ComponentIdentifier) when is_binary(ComponentIdentifier) ->
    {item, {string, ComponentIdentifier}, []}.

%% @doc Given a parameter Name, extract the Parameter value from the HTTP
%% Structured Field data structure.
%%
%% If no value is found, then false is returned
find_sf_param(Name, Params, Default) when is_list(Name) ->
    find_sf_param(list_to_binary(Name), Params, Default);
find_sf_param(Name, Params, Default) ->
	% [{<<"name">>,{string,<<"baz">>}}]
	case lists:keyfind(Name, 1, Params) of
		false -> Default;
		{_, {string, Value}} -> Value;
		{_, {token, Value}} -> Value;
		{_, {binary, Value}} -> Value;
		{_, Value} -> Value
	end.

%%
%% https://datatracker.ietf.org/doc/html/rfc9421#section-6.5.2-1
%% using functions allows encapsulating default values
%%
find_strict_format_param(Params) -> find_sf_param(<<"sf">>, Params, false).
find_key_param(Params) -> find_sf_param(<<"key">>, Params, false).
find_byte_sequence_param(Params) -> find_sf_param(<<"bs">>, Params, false).
find_trailer_param(Params) -> find_sf_param(<<"tr">>, Params, false).
find_request_param(Params) -> find_sf_param(<<"req">>, Params, false).
find_name_param(Params) -> find_sf_param(<<"name">>, Params, false).

%%
%% Data Utilities
%%

% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-5
trim_and_normalize(Bin) ->
	binary:replace(trim_ws(Bin), <<$\n>>, <<" ">>, [global]).

upper_bin(Item) when is_atom(Item) -> upper_bin(atom_to_list(Item));
upper_bin(Item) when is_binary(Item) -> upper_bin(binary_to_list(Item));
upper_bin(Item) when is_list(Item) -> bin(string:uppercase(Item)).

lower_bin(Item) when is_atom(Item) -> lower_bin(atom_to_list(Item));
lower_bin(Item) when is_binary(Item) -> lower_bin(binary_to_list(Item));
lower_bin(Item) when is_list(Item) -> bin(string:lowercase(Item)).

bin(Item) when is_atom(Item) -> atom_to_binary(Item, utf8);
bin(Item) when is_integer(Item) ->
    case Item of
        % Treat integer as an ASCII code
        N when N > 0 andalso N < 256 -> <<N>>;
        N -> integer_to_binary(N)
    end;
bin(Item) ->
    iolist_to_binary(Item).

%% @doc Recursively trim space characters from the beginning of the binary
trim_ws(<<$\s, Bin/bits>>) -> trim_ws(Bin);
%% @doc No space characters at the beginning so now trim them from the end
%% recrusively
trim_ws(Bin) -> trim_ws_end(Bin, byte_size(Bin) - 1).

trim_ws_end(_, -1) ->
	<<>>;
trim_ws_end(Value, N) ->
	case binary:at(Value, N) of
		$\s ->
			trim_ws_end(Value, N - 1);
		% No more space characters matches on the end
		% So extract the bytes up to N, and this is our trimmed value
		_ ->
			S = N + 1,
			<<Trimmed:S/binary, _/bits>> = Value,
			Trimmed
	end.

%%%
%%% TESTS
%%%

%%% Integration Tests

%% @doc Ensure that we can validate a signature on an extremely large and complex
%% message that is sent over HTTP, signed with the codec.
validate_large_message_from_http_test() ->
    Node = hb_http_server:start_node(Opts = #{
        force_signed => true,
        commitment_device => <<"httpsig@1.0">>,
        extra =>
            [
                [
                    [
                        #{
                            <<"n">> => N,
                            <<"m">> => M,
                            <<"o">> => O
                        }
                    ||
                        O <- lists:seq(1, 3)
                    ]
                ||
                    M <- lists:seq(1, 3)
                ]
            ||
                N <- lists:seq(1, 3)
            ]
    }),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    Signers = hb_message:signers(Res, #{}),
    ?event({received, {signers, Signers}, {res, Res}}),
    ?assert(length(Signers) == 1),
    ?assert(hb_message:verify(Res, Signers, #{})),
    ?event({sig_verifies, Signers}),
    ?assert(hb_message:verify(Res)),
    ?event({hmac_verifies, <<"hmac-sha256">>}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Res, Opts),
    ?event({msg_with_only_committed, OnlyCommitted}),
    ?assert(hb_message:verify(OnlyCommitted, Signers)),
    ?event({msg_with_only_committed_verifies, Signers}),
    ?assert(hb_message:verify(OnlyCommitted)),
    ?event({msg_with_only_committed_verifies_hmac, <<"hmac-sha256">>}).

committed_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Signed = hb_message:commit(Msg, hb:wallet()),
    ?event({signed_msg, Signed}),
    UnsignedID = hb_message:id(Signed, none),
    SignedID = hb_message:id(Signed, all),
    ?event({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID).

multicommitted_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Signed1 = hb_message:commit(Msg, Wallet1 = ar_wallet:new()),
    Signed2 = hb_message:commit(Signed1, Wallet2 = ar_wallet:new()),
    Addr1 = hb_util:human_id(ar_wallet:to_address(Wallet1)),
    Addr2 = hb_util:human_id(ar_wallet:to_address(Wallet2)),
    ?event({signed_msg, Signed2}),
    UnsignedID = hb_message:id(Signed2, none),
    SignedID = hb_message:id(Signed2, all),
    ?event({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID),
    ?assert(hb_message:verify(Signed2, [])),
    ?assert(hb_message:verify(Signed2, [Addr1])),
    ?assert(hb_message:verify(Signed2, [Addr2])),
    ?assert(hb_message:verify(Signed2, [Addr1, Addr2])),
    ?assert(hb_message:verify(Signed2, [Addr2, Addr1])),
    ?assert(hb_message:verify(Signed2, all)).

%%% Unit Tests
trim_ws_test() ->
	?assertEqual(<<"hello world">>, trim_ws(<<"      hello world      ">>)),
	?assertEqual(<<>>, trim_ws(<<"">>)),
	?assertEqual(<<>>, trim_ws(<<"         ">>)),
	ok.

% join_signature_base_test() ->
% 	ParamsLine =
% 		<<"(\"@method\" \"@path\" \"foo\";req \"foo\";key=\"a\");"
% 		    "created=1733165109501;nonce=\"foobar\";keyid=\"key1\"">>,
% 	ComponentsLine = <<"\"@method\": GET\n\"@path\": /id-123/Data\n\"foo\";"
%         "req: req-b-bar\n\"foo\";key=\"a\": 1">>,
% 	?assertEqual(
% 		<<
%             ComponentsLine/binary,
%             <<"\n">>/binary,
%             <<"\"@signature-params\": ">>/binary,
%             ParamsLine/binary
%         >>,
% 		join_signature_base(ComponentsLine, ParamsLine)
% 	).

signature_params_line_test() ->
	Params = #{created => 1733165109501, nonce => "foobar", keyid => "key1"},
	ContentIdentifiers = [
		<<"Content-Length">>,
        <<"@method">>,
        <<"@Path">>,
        <<"content-type">>,
        <<"example-dict">>
	],
	Result = signature_params_line(ContentIdentifiers, Params),
	?assertEqual(
        <<
            "(\"content-length\" \"@method\" \"@path\" \"content-type\" \"example-dict\")"
            ";created=1733165109501;keyid=\"key1\";nonce=\"foobar\""
        >>,
		Result
	).

derive_component_error_req_param_on_request_target_test() ->
	Result =
        derive_component(
            {item, {string, <<"@query-param">>}, [{<<"req">>, true}]},
            #{}, #{}, req),
	?assertMatch(
		{req_identifier_error, _},
		Result
	).

derive_component_error_query_param_no_name_test() ->
	Result =
        derive_component(
            {item,
                {string, <<"@query-param">>},
                [{<<"noname">>, {string, <<"foo">>}}]
            }, #{}, #{}, req),
	?assertMatch(
		{req_identifier_error, _},
		Result
	).

derive_component_error_status_req_target_test() ->
	Result = derive_component({item, {string, <<"@status">>}, []}, #{}, #{}, req),
	{E, _M} = Result,
	?assertEqual(res_identifier_error, E).

%% @doc Test that we can sign and verify a message with a link. We use 
sign_and_verify_link_test() ->
    Msg = #{
        <<"normal">> => <<"typical-value">>,
        <<"untyped">> =>
            {link, hb_util:human_id(crypto:strong_rand_bytes(32)), #{}},
        <<"typed">> =>
            {link,
                hb_util:human_id(crypto:strong_rand_bytes(32)),
                #{ <<"type">> => <<"integer">> }
            }
    },
    ?event({msg, Msg}),
    Signed = hb_message:commit(Msg, hb:wallet()),
    ?event({signed_msg, Signed}),
    ?assert(hb_message:verify(Signed)).

-module(hb_http_signature).

-export([authority/3, sign/2, sign/3, signature_params_line/2]).

% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
-define(EMPTY_QUERY_PARAMS, $?).

%%%
%%% Ideal API
%%% authority(ComponentIdentifiers, Params) -> Authority
%%%
%%% sign(Authority, Req, Res) -> {ok, {SigName, SigInput, Sig}
%%% verify(Authority, SigName, Msg) -> {ok}
%%%

authority(ComponentIdentifiers, SigParams, Key) ->
	#{
		component_identifiers => ComponentIdentifiers,
		sig_params => SigParams,
		key => Key
	}.

sign(Authority, Req) ->
	sign(Authority, Req, #{}).
sign(Authority, Req, Res) ->
	ComponentIdentifiers = maps:get(component_identifiers, Authority),
	SignatureComponentsLine = signature_components_line(ComponentIdentifiers, Req, Res),
	SignatureParamsLine = signature_params_line(ComponentIdentifiers, maps:get(sig_params, Authority)),
	SignatureBase =
		<<SignatureComponentsLine/binary, <<"\n">>, <<"\"@signature-params\": ">>, SignatureParamsLine/binary>>,
	Name = random_an_binary(5),
	SignatureInput = SignatureParamsLine,
	% Create signature using SignatureBase and authority#key
	Signature = create_signature(Authority, SignatureBase),
	{ok, {Name, SignatureInput, Signature}}.

create_signature(Authority, SignatureBase) ->
	Key = maps:get(key, Authority),
	% TODO: implement
	Signature = <<"SIGNED", SignatureBase/binary>>,
	Signature.

%%%
%%% TODO: Ensure error cases are covered
%%% - dup CI: https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.1
%%%
signature_components_line(ComponentIdentifiers, Req, Res) ->
	ComponentsLine = lists:foldl(
		fun(Line, Identifier) ->
			% TODO: handle errors?
			{ok, {I, V}} = identifier_to_component(Identifier, Req, Res),
			% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.1
			<<Line, I/binary, <<": ">>, V/binary, <<"\n">>>>
		end,
		<<>>,
		ComponentIdentifiers
	),
	bin(ComponentsLine).

%%%
%%% @doc construct the signature-params line part of the signature base.
%%%
%%% ComponentIdentifiers: a list of "component identifiers" to be included
%%% in the signature.
%%%
%%% SigParams: a map or list of pairs that contain the metadata parameters
%%% for the signature
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
%%%
signature_params_line(ComponentIdentifiers, SigParams) when is_map(SigParams) ->
	signature_params_line(ComponentIdentifiers, maps:to_list(SigParams));
signature_params_line(ComponentIdentifiers, SigParams) when is_list(SigParams) ->
	SfList = [
		{
			list,
			lists:map(fun sf_item/1, ComponentIdentifiers),
			lists:map(
				fun
					({K, V}) when is_integer(V) -> {bin(K), V};
					({K, V}) -> {bin(K), {string, bin(V)}}
				end,
				SigParams
			)
		}
	],
	Res = hb_http_structured_fields:list(SfList),
	bin(Res).

identifier_to_component(Identifier = <<"@", _R/bits>>, Req, Res) -> derive_component(Identifier, Req, Res);
identifier_to_component(Identifier, Req, Res) -> extract_header(Identifier, Req, Res).

extract_header(Identifier, Req, Res = #{}) ->
	extract_header(Identifier, Req, Res, req);
extract_header(Identifier, Req, Res) ->
	extract_header(Identifier, Req, Res, res).
extract_header(Identifier, Req, Res, _Subject) ->
	% The Identifier may have params and so we need to parse it
	% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-6
	{item, {string, IParsed}, IParams} = sf_item(Identifier),
	IsStrictFormat = get_sf_item_param(<<"sf">>, IParams, false) andalso true,
	IsByteSequenceEncoded = get_sf_item_param(<<"bs">>, IParams, false) andalso true,
	DictKey = get_sf_item_param(<<"key">>, IParams, false),
	case (IsStrictFormat orelse DictKey) andalso IsByteSequenceEncoded of
		true ->
			% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.2
			{conflicting_params_error, <<"Component Identifier MUST not contain both 'sf' and 'bs' parameters">>};
		_ ->
			Lowered = lower_bin(IParsed),
			IsRequestIdentifier = get_sf_item_param(<<"req">>, IParams, false),
			Msg = IsRequestIdentifier andalso Req orelse Res,
			% Fields are case-insensitive, so we perform a search across the Msg fields
			MaybeField = lists:keyfind(Lowered, 1, [
				{lower_bin(Key), Value}
			 || {Key, Value} <- maps:to_list(maps:get(fields, Msg, #{}))
			]),
			case MaybeField of
				false ->
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.6
					{field_not_found_error, <<"Component Identifier for a field MUST be present on the message">>};
				_ ->
					% The Field was found, but we still need to potentially parse it
					% (it could be a Structured Field) and potentially extract
					% subsequent values ie. specific dictionary key and its parameters
					% TODO: implement
					ok
			end,
			ok
	end.

derive_component(Identifier, Req, Res = #{}) ->
	derive_component(Identifier, Req, Res, req);
derive_component(Identifier, Req, Res) ->
	derive_component(Identifier, Req, Res, res).
derive_component(Identifier, Req, Res, Subject) when is_list(Identifier) ->
	derive_component(list_to_binary(Identifier), Req, Res, Subject);
derive_component(Identifier, Req, Res, Subject) when is_atom(Identifier) ->
	derive_component(atom_to_binary(Identifier), Req, Res, Subject);
derive_component(Identifier, Req, Res, Subject) when is_binary(Identifier) ->
	% The Identifier may have params and so we need to parse it
	% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-6
	{item, {string, IParsed}, IParams} = sf_item(Identifier),
	case get_sf_item_param(<<"req">>, IParams, false) andalso Subject =:= req of
		% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.3
		true ->
			{req_identifier_error,
				<<"A Component Identifier may not contain a req parameter if the target is a response message">>};
		_ ->
			Lowered = lower_bin(IParsed),
			NormalizedItem = hb_http_structured_fields:item({item, {string, Lowered}, IParams}),
			Derived =
				case Lowered of
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.2.1
					<<"@method">> ->
						{ok, {NormalizedItem, upper_bin(maps:get(method, Req))}};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.4.1
					<<"@target-uri">> ->
						{ok, {NormalizedItem, bin(maps:get(url, Req))}};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.6.1
					<<"@authority">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						Authority = maps:get(host, URI),
						{ok, {NormalizedItem, lower_bin(Authority)}};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.8.1
					<<"@scheme">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						Scheme = maps:get(schema, URI),
						{ok, {NormalizedItem, lower_bin(Scheme)}};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.10.1
					<<"@request-target">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						% If message contains the absolute form value, then
						% the value must be the absolut url
						%
						% TODO: maybe better way to distinguish besides a flag
						% on the request?
						%
						% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.5-10
						RequestTarget =
							case maps:get(is_absolute_form, Req) of
								true -> URI;
								_ -> lists:join($?, [maps:get(path, URI), maps:get(query, URI, ?EMPTY_QUERY_PARAMS)])
							end,
						{ok, {NormalizedItem, bin(RequestTarget)}};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.12.1
					<<"@path">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						Path = maps:get(path, URI),
						{ok, {NormalizedItem, bin(Path)}};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.14.1
					<<"@query">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						Query = maps:get(query, URI, ?EMPTY_QUERY_PARAMS),
						{ok, {NormalizedItem, bin(Query)}};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.16.1
					<<"@query-param">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						case get_sf_item_param(<<"name">>, IParams, false) of
							% The name parameter MUST be provided when specifiying a @query-param
							% Derived Component. See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-1
							false ->
								{req_identifier_error, <<"@query_param Derived Component Identifier must specify a name parameter">>};
							Name ->
								QueryParams = uri_string:dissect_query(maps:get(query, URI, "")),
								QueryParam =
									case lists:keyfind(Name, 1, QueryParams) of
										{_, QP} -> QP;
										% An missing or empty query param value results in
										% an empty string value in the signature base
										% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-4
										_ -> ""
									end,
								{ok, {NormalizedItem, bin(QueryParam)}}
						end;
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.18.1
					<<"@status">> ->
						case Subject =:= req of
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.9-8
							true ->
								{res_identifier_error, <<"@status Derived Component must not be used if target is a request message">>};
							_ ->
								Status = maps:get(status, Res, <<"200">>),
								{ok, {NormalizedItem, Status}}
						end
				end,
			Derived
	end.

%%%
%%% Data Utilities
%%%

sf_item(ComponentIdentifier = {item, _Item, _Params}) ->
	ComponentIdentifier;
% TODO: should we check whether the string is already quoted?
sf_item(ComponentIdentifier) when is_list(ComponentIdentifier) ->
	sf_item(<<$", (lower_bin(ComponentIdentifier))/binary, $">>);
sf_item(ComponentIdentifier) when is_binary(ComponentIdentifier) ->
	hb_http_structured_fields:parse_item(ComponentIdentifier).

get_sf_item_param(Name, SfItemParams, Default) when is_list(Name) ->
	get_sf_item_param(list_to_binary(Name), SfItemParams, Default);
get_sf_item_param(Name, SfItemParams, Default) ->
	% [{<<"name">>,{string,<<"baz">>}}]
	case lists:keyfind(Name, 1, SfItemParams) of
		{_, {_, Value}} -> Value;
		_ -> Default
	end.

upper_bin(Item) when is_binary(Item) -> upper_bin(binary_to_list(Item));
upper_bin(Item) when is_list(Item) -> bin(string:uppercase(Item)).

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

random_an_binary(Length) ->
	Characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
	ListLength = length(Characters),
	RandomIndexes = [rand:uniform(ListLength) || _ <- lists:seq(1, Length)],
	RandomChars = [lists:nth(Index, Characters) || Index <- RandomIndexes],
	list_to_binary(RandomChars).

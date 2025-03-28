%%% @doc A cache of Converge Protocol messages and compute results.
%%%
%%% HyperBEAM stores all paths in key value stores, abstracted by the `hb_store'
%%% module. Each store has its own storage backend, but each works with simple
%%% key-value pairs. Each store can write binary keys at paths, and link between
%%% paths.
%%%
%%% There are three layers to HyperBEAMs internal data representation on-disk:
%%%
%%% 1. The raw binary data, written to the store at the hash of the content.
%%%    Storing binary paths in this way effectively deduplicates the data.
%%% 2. The hashpath-graph of all content, stored as a set of links between
%%%    hashpaths, their keys, and the data that underlies them. This allows
%%%    all messages to share the same hashpath space, such that all requests
%%%    from users additively fill-in the hashpath space, minimizing duplicated
%%%    compute.
%%% 3. Messages, referrable by their IDs (attested or unattested). These are
%%%    stored as a set of links attestation IDs and the unattested message.
%%%
%%% Before writing a message to the store, we convert it to Type-Annotated
%%% Binary Messages (TABMs), such that each of the keys in the message is
%%% either a map or a direct binary.
-module(hb_cache).
-export([read/2, read_resolved/3]).
-export([write/2, write_binary/3, write_hashpath/2]).
-export([link/3]).
-export([list/2, list_numbered/2]).
-export([test_unsigned/1, test_signed/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc List all items in a directory, assuming they are numbered.
%%
%% @param Path A binary representing the directory path.
%% @param Opts A map of options (including store configuration).
%% @returns A list of integers converted from the names of the directory items.
list_numbered(Path, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(hb_cache, {list_numbered, {store, Store}, {path, Path}}),
    SlotDir = hb_store:path(Store, Path),
    ?event(hb_cache, {list_numbered, {slot_dir, SlotDir}}),
    RawNames = list(SlotDir, Opts),
    ?event(hb_cache, {list_numbered, {raw_names, RawNames}}),
    [to_integer(Name) || Name <- RawNames].


%% @doc List all items under a given path.
%%
%% @param Path A binary representing the path to list.
%% @param Opts A map of options (including store configuration).
%% @returns A list of item names, or an empty list if the store is unavailable.
list(Path, Opts) when is_map(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(hb_cache, {list, {path, Path}, {opts, Opts}, {store, Store}}),
    case Store of
        no_viable_store ->
            ?event(hb_cache, {list, {result, []}}),
            [];
        _ ->
            list(Path, Store)
    end;
list(Path, Store) ->
    ResolvedPath = hb_store:resolve(Store, Path),
    ?event(hb_cache, {list, {resolved_path, ResolvedPath}}),
    case hb_store:list(Store, ResolvedPath) of
        {ok, Names} ->
            ?event(hb_cache, {list, {result, Names}}),
            Names;
        {error, Err} ->
            ?event(hb_cache, {list, {error, Err}}),
            [];
        no_viable_store ->
            ?event(hb_cache, {list, {result, []}}),
            []
    end.

%% @doc Write a message to the cache.
%%
%% For raw binaries, writes the data at its hashpath (by default, the
%% SHA2-256 hash of the data). The function links the unattended ID's
%% hashpath for keys (including `/attestations') to the underlying data and
%% recursively processes submessages. Additionally, it creates links from
%% each attestation ID to the unattested message, ensuring that both attested
%% and unattested IDs can be read and that all attestations are available in
%% memory. For deep messages, the attestations of inner messages are also
%% read, so that the ID of the outer message (which does not include its
%% attestations) is built upon the attestations of the inner messages.
%% Note: IDs from attestations on signed _inner_ messages are not stored.
%%       (This behavior may be revisited in the future.)
%%
%% @param RawMsg The raw message (binary or map) to be written.
%% @param Opts A map of configuration options.
%% @returns {ok, Path} on success or {error, Reason} on failure.
write(RawMsg, Opts) ->
    ?event(hb_cache, {write, {start, RawMsg}}),
    case hb_message:with_only_attested(RawMsg, Opts) of
        {ok, Msg} ->
            AltIDs = calculate_alt_ids(RawMsg, Opts),
            ?event(hb_cache, {write, {alt_ids, AltIDs}, {msg, Msg}}),
            Tabm = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
            ?event({tabm, Tabm}),
            try do_write_message(
                Tabm,
                AltIDs,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
            catch
                Type:Reason:Stacktrace ->
                    ?event(error,
                        {cache_write_error,
                            {type, Type},
                            {reason, Reason},
                            {stacktrace, Stacktrace}
                        },
                        Opts
                    ),
                    {error, no_viable_store}
            end;
        {error, Err} ->
            ?event(hb_cache, {write, {error, Err}}),
            {error, Err}
    end.

%% @doc Helper function to write a message or binary to the store.
%%
%% For binary data, writes it at its hashpath and creates links for
%% alternative IDs. For map messages, handles both remote and local store
%% cases by writing subkeys, creating groups, and establishing links for
%% attestations.
%%
%% @param BinOrMsg A binary or map representing the message.
%% @param AltIDs A list of alternative IDs derived from attestations.
%% @param Store The store configuration.
%% @param Opts A map of configuration options.
%% @returns {ok, Path} on success or {error, Reason} on failure.
do_write_message(Bin, AltIDs, Store, Opts) when is_binary(Bin) ->
    ?event(hb_cache, 
		{do_write_message_binary, 
			{start, Bin}, 
			{alt_ids, AltIDs},
			{store, Store}
		}
	),
    Hashpath = hb_path:hashpath(Bin, Opts),
    ?event(hb_cache, {do_write_message_binary, {hashpath, Hashpath}}),
    case hb_store:write(Store, Path = <<"data/", Hashpath/binary>>, Bin) of
        ok ->
            ?event(hb_cache, 
				{do_write_message_binary, 
					{write_result, ok}, {path, Path}
				}
			),
            lists:map(
                fun(AltID) ->
                    ?event(hb_cache, 
						{do_write_message_binary, 
							{making_link, 
								{alt_id, AltID}, 
								{path, Path}
							}
						}
					),
                    hb_store:make_link(Store, Path, AltID)
                end,
                AltIDs
            ),
            {ok, Path};
        {ok, RemotePath} ->
            ?event(hb_cache, 
				{do_write_message_binary, 
					{write_result, {ok, RemotePath}}
				}
			),
            {ok, RemotePath};
        {error, Err} ->
            ?event(hb_cache, 
				{do_write_message_binary, 
					{write_result, {error, Err}}
				}
			),
            {error, Err}
    end;

do_write_message(Msg, AltIDs, Store, Opts) when is_map(Msg) ->
    ?event(hb_cache, 
		{do_write_message_map, 
			{start, Msg}, 
			{alt_ids, AltIDs}, 
			{store, Store}
		}
	),
    case is_list(Store) andalso 
		length(Store) > 0 andalso 
		maps:get(
			<<"store-module">>, 
			hd(Store), undefined
		) == hb_store_remote_node of
        true ->
            ?event(hb_cache, {do_write_message_map, {remote_store, true}}),
            Hashpath = hb_path:hashpath(Msg, Opts),
            ?event(hb_cache, {do_write_message_map, {hashpath, Hashpath}}),
            case hb_store:write(Store, <<"data/", Hashpath/binary>>, Msg) of
                {ok, RemotePath} ->
                    ?event(hb_cache, 
						{do_write_message_map, 
							{remote_write_success, RemotePath}
						}
					),
                    {ok, RemotePath};
                {error, Err} ->
                    ?event(hb_cache, 
						{do_write_message_map, 
							{remote_write_error, Err}
						}
					),
                    {error, Err}
            end;
        false ->
            ?event(hb_cache, {do_write_message_map, {remote_store, false}}),
            {ok, UnattestedID} = 
				dev_message:id(Msg, #{ <<"attestors">> => <<"none">> }, Opts),
            ?event(hb_cache, 
				{do_write_message_map, 
					{unsigned_id, UnattestedID}, 
					{alt_ids, AltIDs}
				}
			),
            MsgHashpathAlg = hb_path:hashpath_alg(Msg),
            hb_store:make_group(Store, UnattestedID),
            hb_store:make_group(Store, UnattestedID),
            maps:map(
                fun(<<"device">>, Map) when is_map(Map) ->
                    ?event(hb_cache, 
						{do_write_message_map, 
							{error, "device_map_cannot_be_written"}, 
							{id, hb_message:id(Map)}
						}
					),
                    throw(
						{
							device_map_cannot_be_written, 
							{id, hb_message:id(Map)}
						}
					);
                (Key, Value) ->
                    ?event(hb_cache, 
						{do_write_message_map, 
							{writing_subkey, 
								{key, Key}, 
								{value, Value}
							}
						}
					),
                    KeyHashPath =
                        hb_path:hashpath(
                            UnattestedID,
                            hb_path:to_binary(Key),
                            MsgHashpathAlg,
                            Opts
                        ),
                    ?event(hb_cache, 
						{do_write_message_map, 
							{key_hashpath, KeyHashPath}
							}
						),
					% Note: We do not pass the AltIDs here, as we cannot 
					% calculate them based on the TABM that we have in-memory
                    % at this point. We could turn the TABM back into a
                    % structured message, but this is expensive.
                    {ok, Path} = do_write_message(Value, [], Store, Opts),
                    hb_store:make_link(Store, Path, KeyHashPath),
                    ?event(hb_cache, 
						{do_write_message_map, 
							{link_created, 
								{key_hashpath, KeyHashPath}, 
								{data_path, Path}
							}
						}
					),
                    Path
                end,
                hb_private:reset(Msg)
            ),
			% Write the attestations to the store, linking each 
			% attestation ID to the unattested message.
            lists:map(
                fun(AltID) ->
                    ?event(hb_cache, 
						{do_write_message_map, 
							{linking_attestation, 
								{unattested_id, UnattestedID}, 
								{attested_id, AltID}
							}
						}
					),
                    hb_store:make_link(Store, UnattestedID, AltID)
                end,
                AltIDs
            ),
            {ok, UnattestedID}
    end.

%% @doc Calculate the alternative IDs for a message.
%%
%% For binary input, returns an empty list.
%% For map input, extracts alternative IDs from the message's attestations.
%%
%% @param Msg A binary or map representing the message.
%% @param Opts A map of configuration options.
%% @returns A list of alternative IDs.
calculate_alt_ids(Bin, _Opts) when is_binary(Bin) ->
    ?event(hb_cache, {calculate_alt_ids, {binary_input, Bin}}),
    [];
calculate_alt_ids(Msg, _Opts) ->
    ?event(hb_cache, {calculate_alt_ids, {msg_input, Msg}}),
    Attestations =
		 maps:without(
			[<<"priv">>],
			maps:get(<<"attestations">>, Msg, #{})
		),
    ?event(hb_cache, {calculate_alt_ids, {attestations, Attestations}}),
    Result = lists:filtermap(
        fun(Attestor) ->
            Att = maps:get(Attestor, Attestations, #{}),
            case maps:get(<<"id">>, Att, undefined) of
                undefined ->
                    ?event(hb_cache, 
						{calculate_alt_ids, 
							{attestor, Attestor}, 
							{result, undefined}
						}
					),
                    false;
                ID ->
                    ?event(hb_cache, 
						{calculate_alt_ids, 
							{attestor, Attestor}, 
							{result, ID}
						}
					),
                    {true, ID}
            end
        end,
        maps:keys(Attestations)
    ),
    ?event(hb_cache, {calculate_alt_ids, {result, Result}}),
    Result.

%% @doc Write a hashpath and its message to the store and create a link.
%%
%% If the message contains a hashpath in its private data, that hashpath is
%% used. Otherwise, the message is written normally.
%%
%% @param Msg A map representing the message to be written.
%% @param Opts A map of configuration options.
%% @returns {ok, Path} on success.
write_hashpath(Msg = #{ <<"priv">> := #{ <<"hashpath">> := HP } }, Opts) ->
    ?event(hb_cache, {write_hashpath, {detected_hashpath, HP}, {msg, Msg}}),
    write_hashpath(HP, Msg, Opts);
write_hashpath(MsgWithoutHP, Opts) ->
    ?event(hb_cache, {write_hashpath, {no_hashpath, MsgWithoutHP}}),
    write(MsgWithoutHP, Opts).

write_hashpath(HP, Msg, Opts) when is_binary(HP) or is_list(HP) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(hb_cache, 
		{write_hashpath, 
			{writing_hashpath, HP}, 
			{msg, Msg}, 
			{store, Store}
		}
	),
    {ok, Path} = write(Msg, Opts),
    hb_store:make_link(Store, Path, HP),
    {ok, Path}.

%% @doc Write a raw binary key into the store and create a link at a given
%% hashpath.
%%
%% @param Hashpath The hashpath at which to write the binary.
%% @param Bin The binary data to be stored.
%% @param Opts A map of configuration options.
%% @returns The result of writing the binary.
write_binary(Hashpath, Bin, Opts) ->
    ?event(hb_cache, 
		{write_binary, 
			{start, 
				{hashpath, Hashpath}, 
				{bin, Bin}, 
				{opts, Opts}
			}
		}
	),
    write_binary(
		Hashpath, 
		Bin, 
		hb_opts:get(store, no_viable_store, Opts), 
		Opts
	).

write_binary(Hashpath, Bin, Store, Opts) ->
    ?event(hb_cache, 
		{write_binary, 
			{start_with_store, 
				{hashpath, Hashpath}, 
				{bin, Bin}, 
				{store, Store}
			}
		}
	),
    do_write_message(Bin, [Hashpath], Store, Opts).


%% @doc Read the message at a given path.
%%
%% Retrieves the message in `structured@1.0' format, which may be a richly
%% typed map or a direct binary.
%%
%% @param Path The path or key to read.
%% @param Opts A map of configuration options.
%% @returns {ok, Structured} on success, or not_found if the message is missing.
read(Path, Opts) ->
    ?event(hb_cache, {read, {start, {path, Path}, {opts, Opts}}}),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(hb_cache, {read, {store, Store}}),
    case store_read(Path, Store, Opts) of
        not_found ->
            ?event(hb_cache, {read, {result, not_found}}),
            not_found;
        {ok, Res} ->
            ?event(hb_cache, {read, {raw_result, Res}}),
            Structured = dev_codec_structured:to(Res),
            ?event(hb_cache, {read, {structured_result, Structured}}),
            {ok, Structured}
    end.

%% @doc Recursively read a path from the store, tracking visited paths to
%% avoid circular links.
%%
%% @param Path The path to be read.
%% @param Store The store configuration.
%% @param Opts A map of configuration options.
%% @returns {ok, Result} on success, or not_found if the path cannot be read.
store_read(Path, Store, Opts) ->
    ?event(hb_cache, 
		{store_read, 
			{start, 
				{path, Path}, 
				{store, Store},
				{opts, Opts}
			}
		}
	),
    store_read(Path, Store, Opts, []).

store_read(_Path, no_viable_store, _, _AlreadyRead) ->
    ?event(hb_cache, {store_read, {result, no_viable_store}}),
    not_found;
store_read(Path, Store, Opts, AlreadyRead) ->
    case lists:member(Path, AlreadyRead) of
        true ->
            ?event(hb_cache, 
				{store_read, 
					{circular_links_detected, 
					{path, Path},
					{already_read, AlreadyRead}
					}
				}
			),
            throw({circular_links_detected, Path, {already_read, AlreadyRead}});
        false ->
            ?event(hb_cache, {store_read, {proceeding, {path, Path}}}),
            do_read(Path, Store, Opts, AlreadyRead)
    end.

%% @doc Read a path from the store.
%%
%% This function is unsafe as it may recurse indefinitely if circular links
%% are present.
%%
%% @param Path The path to read.
%% @param Store The store configuration.
%% @param Opts A map of configuration options.
%% @param AlreadyRead A list of paths already visited.
%% @returns The read result, or not_found if unsuccessful.
do_read(Path, Store, Opts, AlreadyRead) ->
    ResolvedFullPath = 
		hb_store:resolve(Store, PathToBin = hb_path:to_binary(Path)),
    ?event(hb_cache, 
		{do_read, 
			{path, PathToBin}, 
			{resolved, ResolvedFullPath}
		}
	),
    case hb_store:type(Store, ResolvedFullPath) of
        not_found ->
            ?event(hb_cache, {do_read, {result, not_found}}),
            not_found;
        no_viable_store ->
            ?event(hb_cache, {do_read, {result, no_viable_store}}),
            not_found;
        simple ->
            ?event(hb_cache, {do_read, {type, simple}}),
            hb_store:read(Store, ResolvedFullPath);
        _ ->
            case hb_store:list(Store, ResolvedFullPath) of
                {ok, Subpaths} ->
                    ?event(hb_cache, 
					{do_read, 
						{listed, 
							{original_path, Path}, 
							{subpaths, {explicit, Subpaths}}
						}
					}
				),
				Msg =
					maps:from_list(
					lists:map(
						fun(Subpath) ->
							?event(hb_cache, 
								{do_read, 
									{reading_subpath, 
										{path, Subpath}, 
										{store, Store}
									}
								}
							),
							{ok, Res} = store_read(
								[ResolvedFullPath, Subpath],
								 Store, 
								 Opts, 
								 [ResolvedFullPath | AlreadyRead]
							),
							{iolist_to_binary([Subpath]), Res}
						end,
						Subpaths
					)
				),
				?event(hb_cache, {do_read, {read_message, Msg}}),
				{ok, Msg};
			_ ->
				?event(hb_cache, {do_read, {list_error, ResolvedFullPath}}),
				not_found
		end
    end.

%% @doc Read the output of a prior computation given two message identifiers
%% or messages.
%%
%% Constructs a composite path from the provided message IDs (or messages)
%% and retrieves the result.
%%
%% @param MsgID1 The first message ID or message.
%% @param MsgID2 The second message ID or message.
%% @param Opts A map of configuration options.
%% @returns The resolved message, or not_found if not available.
read_resolved(MsgID1, MsgID2, Opts) when ?IS_ID(MsgID1) and ?IS_ID(MsgID2) ->
    ?event(hb_cache, 
		{read_resolved, 
			{msg1, MsgID1}, 
			{msg2, MsgID2}, 
			{opts, Opts}
		}
	),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_resolved(MsgID1, Msg2, Opts) when ?IS_ID(MsgID1) and is_map(Msg2) ->
    {ok, MsgID2} = 
		dev_message:id(Msg2, #{ <<"attestors">> => <<"all">> }, Opts),
    ?event(hb_cache, {read_resolved, {derived_msgid2, MsgID2}}),
    read(<<MsgID1/binary, "/", MsgID2/binary>>, Opts);
read_resolved(Msg1, Msg2, Opts) when is_map(Msg1) and is_map(Msg2) ->
    ?event(hb_cache, {read_resolved, {msg1, Msg1}, {msg2, Msg2}}),
    read(hb_path:hashpath(Msg1, Msg2, Opts), Opts);
read_resolved(_, _, _) ->
    ?event(hb_cache, {read_resolved, {result, not_found}}),
    not_found.

%% @doc Create a link from one path to another in the store.
%%
%% @param Existing The existing path.
%% @param New The new path to link to.
%% @param Opts A map of configuration options.
%% @returns The result of the link creation.
link(Existing, New, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event(hb_cache, {link, {existing, Existing}, {new, New}, {store, Store}}),
    hb_store:make_link(
		Store, 
		Existing, 
		New
	).

%% @doc Convert a value to an integer.
%%
%% @param Value A binary or list representing an integer.
%% @returns The integer value.
to_integer(Value) when is_list(Value) ->
    ?event(hb_cache, {to_integer, {list, Value}}),
    list_to_integer(Value);
to_integer(Value) when is_binary(Value) ->
    ?event(hb_cache, {to_integer, {binary, Value}}),
    binary_to_integer(Value).



%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"deep-test-key">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) -> test_signed(Data, ar_wallet:new()).
test_signed(Data, Wallet) ->
    hb_message:attest(test_unsigned(Data), Wallet).

test_store_binary(Opts) ->
    Bin = <<"Simple unsigned data item">>,
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin).

test_store_unsigned_empty_message(Opts) ->
    Item = #{},
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Item, RetrievedItem)).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_message(Opts) ->
    Item = test_unsigned(#{ <<"key">> => <<"Simple unsigned data item">> }),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_converge:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem)),
    ok.

test_store_ans104_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Item = 
		#{ <<"type">> => <<"ANS104">>, <<"content">> => <<"Hello, world!">> },
    Attested = hb_message:attest(Item, hb:wallet()),
    {ok, _Path} = write(Attested, Opts),
    AttestedID = hb_util:human_id(hb_message:id(Attested, all)),
    UnattestedID = hb_util:human_id(hb_message:id(Attested, none)),
    ?event({test_message_ids, 
		{unattested, UnattestedID}, 
		{attested, AttestedID}
	}),
    {ok, RetrievedItem} = read(AttestedID, Opts),
    {ok, RetrievedItemU} = read(UnattestedID, Opts),
    ?assert(hb_message:match(Attested, RetrievedItem)),
    ?assert(hb_message:match(Attested, RetrievedItemU)),
    ok.

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_signed_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }, Wallet),
    ?event({writing_message, Item}),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    {ok, UID} = dev_message:id(Item, #{ <<"attestors">> => <<"none">> }, Opts),
    {ok, RetrievedItemU} = read(UID, Opts),
    ?event({retreived_unsigned_message, 
		{expected, Item}, 
		{got, RetrievedItemU}
	}),
    ?assert(hb_message:match(Item, RetrievedItemU)),
    {ok, AttestedID} = 
		dev_message:id(Item, #{ <<"attestors">> => [Address] }, Opts),
    {ok, RetrievedItemS} = read(AttestedID, Opts),
    ?assert(hb_message:match(Item, RetrievedItemS)),
    ok.

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Wallet),
    Outer =
        hb_message:attest(
            #{
                <<"level1">> =>
                    hb_message:attest(
                        #{
                            <<"level2">> =>
                                #{
                                    <<"level3">> => Level3SignedSubmessage,
                                    <<"e">> => <<"f">>,
                                    <<"z">> => [1,2,3]
                                },
                            <<"c">> => <<"d">>,
                            <<"g">> => [<<"h">>, <<"i">>],
                            <<"j">> => 1337
                        },
                        ar_wallet:new()
                    ),
                <<"a">> => <<"b">>
            },
            Wallet
        ),
    {ok, UID} = dev_message:id(Outer, #{ <<"attestors">> => <<"none">> }, Opts),
    ?event({string, <<"================================================">>}),
    {ok, AttestedID} = 
		dev_message:id(Outer, #{ <<"attestors">> => [Address] }, Opts),
    ?event({string, <<"================================================">>}),
    ?event({test_message_ids, {unattested, UID}, {attested, AttestedID}}),
    %% Write the nested item
    {ok, _} = write(Outer, Opts),
    %% Read the deep value back using subpath
    {ok, DeepMsg} =
        read(
            [
                OuterID = hb_util:human_id(UID),
                <<"level1">>,
                <<"level2">>,
                <<"level3">>
            ],
            Opts
        ),
    ?event({deep_message, DeepMsg}),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual([1,2,3], hb_converge:get(<<"deep-test-key">>, DeepMsg)),
    ?event({deep_message_match, 
		{read, DeepMsg}, 
		{write, Level3SignedSubmessage}
	}),
    ?assert(hb_message:match(Level3SignedSubmessage, DeepMsg)),
    {ok, OuterMsg} = read(OuterID, Opts),
    ?assert(hb_message:match(Outer, OuterMsg)),
    ?event({reading_attested_outer, {id, AttestedID}, {expect, Outer}}),
    {ok, AttestedMsg} = read(hb_util:human_id(AttestedID), Opts),
    ?assert(hb_message:match(Outer, AttestedMsg)).

test_message_with_message(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:reset(Store),
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem)).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store unsigned empty message", 
			fun test_store_unsigned_empty_message/1},
        {"store binary", 
			fun test_store_binary/1},
        {"store simple unsigned message", 
			fun test_store_simple_unsigned_message/1},
        {"store simple signed message", 
			fun test_store_simple_signed_message/1},
        {"deeply nested complex message", 
			fun test_deeply_nested_complex_message/1},
        {"message with message", 
			fun test_message_with_message/1}
    ]).

%% @doc Test that message whose device is `#{}` cannot be written. If it were to
%% be written, it would cause an infinite loop.
test_device_map_cannot_be_written_test() ->
    try
        Opts = #{ store => StoreOpts =
            [#{ 
				<<"store-module">> => hb_store_fs, 
				<<"prefix">> => <<"cache-TEST">> 
			}] 
		},
        hb_store:reset(StoreOpts),
        Danger = #{ <<"device">> => #{}},
        write(Danger, Opts),
        ?assert(false)
    catch
        _:_:_ -> ?assert(true)
    end.

run_test() ->
    Opts = #{ store => StoreOpts = 
        [#{
			<<"store-module">> => hb_store_fs, 
			<<"prefix">> => <<"cache-TEST">> 
		}]
	},
    test_store_ans104_message(Opts),
    hb_store:reset(StoreOpts).
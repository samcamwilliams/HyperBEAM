%%% @doc A device that looks up an ID from a local store and returns it,
%%% honoring the `accept' key to return the correct format. The cache also
%%% supports writing messages to the store, if the node message has the
%%% writer's address in its `cache_writers' key.
-module(dev_cache).
-export([read/3, write/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Read data from the cache.
%% Retrieves data corresponding to a key from a local store.
%% The key is extracted from the incoming message under <<"target">>.
%% The options map may include store configuration.
%% If the "accept" header is set to <<"application/aos-2">>, the result is 
%% converted to a JSON structure and encoded.
%%
%% @param M1 Ignored parameter.
%% @param M2 The request message containing the key (<<"target">>) and an
%%            optional "accept" header.
%% @param Opts A map of configuration options.
%% @returns {ok, Data} on success,
%%          not_found if the key does not exist,
%%          {error, Reason} on failure.
read(_M1, M2, Opts) ->
    ID = hb_converge:get(<<"target">>, M2, Opts),
    ?event(dev_cache, {read, {key_extracted, ID}}),
    case hb_cache:read(ID, Opts) of
        {ok, Res} ->
            ?event(dev_cache, {read, {cache_result, ok, Res}}),
            case hb_converge:get(<<"accept">>, M2, Opts) of
                <<"application/aos-2">> ->
                    ?event(dev_cache, 
						{read, 
							{accept_header, <<"application/aos-2">>}
						}
					),
                    Struct = dev_json_iface:message_to_json_struct(Res),
                    ?event(dev_cache, {read, {json_struct, Struct}}),
                    {ok,
                        #{
                            <<"body">> => jiffy:encode(Struct),
                            <<"content-type">> => <<"application/aos-2">>
                        }
					};
                _ ->
                    {ok, Res}
            end;
        not_found ->
            ?event(dev_cache, {read, {cache_result, not_found}}),
            {error, not_found}
    end.

%% @doc Write data to the cache.
%% Processes a write request by first verifying that the request comes from a
%% trusted writer (as defined by the `cache_writers' configuration in the
%% options). The write type is determined from the message ("single" or "batch")
%% and the data is stored accordingly.
%%
%% @param M1 Ignored parameter.
%% @param M2 The request message containing the data to write, the write type,
%%            and any additional parameters.
%% @param Opts A map of configuration options.
%% @returns {ok, Path} on success, where Path indicates where the data was
%%          stored, {error, Reason} on failure.
write(_M1, M2, Opts) ->
    case is_trusted_writer(M2, Opts) of
        true ->
            ?event(dev_cache, {write, {trusted_writer, true}}),
            Type = hb_converge:get(<<"type">>, M2, <<"single">>, Opts),
            ?event(dev_cache, {write, {write_type, Type}}),
            case Type of
                <<"single">> ->
                    ?event(dev_cache, {write, {write_single_called}}),
                    write_single(M2, Opts);
                <<"batch">> ->
                    ?event(dev_cache, {write, {write_batch_called}}),
                    maps:map(
                        fun(_, Value) ->
                            ?event(dev_cache, {write, {batch_item, Value}}),
                            write_single(Value, Opts)
                        end,
                        hb_converge:get(<<"body">>, M2, Opts)
                    );
                _ ->
                    ?event(dev_cache, {write, {invalid_write_type, Type}}),
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> => <<"Invalid write type.">>
                        }
                    }
            end;
        false ->
            ?event(dev_cache, {write, {trusted_writer, false}}),
            {error,
                #{
                    <<"status">> => 403,
                    <<"body">> => <<"Not authorized to write to the cache.">>
                }
            }
    end.

%% @doc Helper function to write a single data item to the cache.
%% Extracts the body, location, and operation from the message.
%% Depending on the type of data (map or binary) or if a link operation is
%% requested, it writes the data to the store using the appropriate function.
%%
%% @param Msg The message containing data to be written.
%% @param Opts A map of configuration options.
%% @returns {ok, #{status := 200, path := Path}} on success,
%%          {error, Reason} on failure.
write_single(Msg, Opts) ->
    Body = hb_converge:get(<<"body">>, Msg, Opts),
    ?event(dev_cache, {write_single, {body_extracted, Body}}),
    Location = hb_converge:get(<<"location">>, Msg, Opts),
    ?event(dev_cache, {write_single, {location_extracted, Location}}),
    Operation = hb_converge:get(<<"operation">>, Msg, <<"write">>, Opts),
    ?event(dev_cache, {write_single, {operation, Operation}}),
    case {Operation, Body, Location} of
        {<<"write">>, not_found, _} ->
            ?event(dev_cache, {write_single, {error, "No body to write"}}),
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"No body to write.">>
                }
            };
        {<<"write">>, Map, _Location} when is_map(Map) ->
            ?event(dev_cache, {write_single, {processing_map, Map}}),
            {ok, Path} = hb_cache:write(Map, Opts),
            ?event(dev_cache, {write_single, {map_written, Path}}),
            {ok, #{ <<"status">> => 200, <<"path">> => Path }};
        {<<"write">>, Binary, Location} when is_binary(Binary) ->
            ?event(dev_cache, 
				{write_single, 
					{processing_binary, Binary, Location}
				}
			),
            {ok, Path} = hb_cache:write_binary(Location, Binary, Opts),
            ?event(dev_cache, {write_single, {binary_written, Path}}),
            {ok, #{ <<"status">> => 200, <<"path">> => Path }};
        {<<"link">>, _, _} ->
            ?event(dev_cache, {write_single, {processing_link}}),
            Source = hb_converge:get(<<"source">>, Msg, Opts),
            Destination = hb_converge:get(<<"destination">>, Msg, Opts),
            ?event(dev_cache, 
				{write_single, 
					{link_params, Source, Destination}
				}
			),
            ok = hb_cache:link(Source, Destination, Opts),
            ?event(dev_cache, {write_single, {link_success}}),
            {ok, #{ <<"status">> => 200 }};
        _ ->
            ?event(dev_cache, {write_single, {error, "Invalid write type"}}),
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Invalid write type.">>
                }
            }
    end.

%% @doc Verify that the request originates from a trusted writer.
%% Checks that the single signer of the request is present in the list
%% of trusted cache writer addresses specified in the options.
%%
%% @param Req The request message.
%% @param Opts A map of configuration options.
%% @returns true if the request is from an authorized writer, false
%%          otherwise.
is_trusted_writer(Req, Opts) ->
    Signers = hb_message:signers(Req),
    ?event(dev_cache, {is_trusted_writer, {signers, Signers}}),
    CacheWriters = hb_opts:get(cache_writers, [], Opts),
    ?event(dev_cache, {is_trusted_writer, {cache_writers, CacheWriters}}),
    case Signers of
        [Signer] ->
            Trusted = lists:member(Signer, CacheWriters),
            ?event(dev_cache, {is_trusted_writer, {trusted, Trusted}}),
            Trusted;
        _ ->
            ?event(dev_cache, {is_trusted_writer, {trusted, false}}),
            false
    end.

%%%--------------------------------------------------------------------
%%% Test Helpers
%%%--------------------------------------------------------------------

%% @doc Create a test environment with a local store and node.
%% Ensures that the required application is started, configures a local
%% file-system store, resets the store for a clean state, creates a wallet
%% for signing requests, and starts a node with the store and trusted cache
%% writer configuration.
%%
%% @param StorePrefix A binary specifying the prefix for the local store.
%% @returns {LocalStore, Wallet, Address, Node}
setup_test_env(StorePrefix) ->
    ?event(dev_cache, {setup_test_env, {start, StorePrefix}}),
    application:ensure_all_started(hb),
    ?event(dev_cache, {setup_test_env, {hb_started}}),
    LocalStore = 
		#{ <<"store-module">> => hb_store_fs, <<"prefix">> => StorePrefix },
    ?event(dev_cache, {setup_test_env, {local_store_configured, LocalStore}}),
    hb_store:reset(LocalStore),
    ?event(dev_cache, {setup_test_env, {store_reset}}),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ?event(dev_cache, {setup_test_env, {address, Address}}),
    Node = hb_http_server:start_node(#{ 
        store => LocalStore,
        cache_writers => [
			Address,
			hb_util:human_id(ar_wallet:to_address(hb:wallet()))
		],
        port => 10001
    }),
    ?event(dev_cache, {setup_test_env, {node_started, Node}}),
    {LocalStore, Wallet, Address, Node}.

%% @doc Write data to the cache via HTTP.
%% Constructs a write request message with the provided data, signs it with the
%% given wallet, sends it to the node, and verifies that the response indicates
%% a successful write.
%%
%% @param Node The target node.
%% @param Data The data to be written.
%% @param Wallet The wallet used to sign the request.
%% @returns {ok, WriteResponse} on success.
write_to_cache(Node, Data, Wallet) ->
    ?event(dev_cache, {write_to_cache, {start, Node}}),
    WriteMsg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Data
    },
    ?event(dev_cache, {write_to_cache, {message_created, WriteMsg}}),
    SignedMsg = hb_message:attest(WriteMsg, Wallet),
    ?event(dev_cache, {write_to_cache, {message_signed}}),
    WriteResult = hb_http:post(Node, SignedMsg, #{}),
    ?event(dev_cache, {write_to_cache, {http_post, WriteResult}}),
    {ok, WriteResponse} = WriteResult,
    ?event(dev_cache, {write_to_cache, {response_received, WriteResponse}}),
    Status = hb_converge:get(<<"status">>, WriteResponse, 0, #{}),
    ?assertEqual(200, Status),
    Path = hb_converge:get(<<"path">>, WriteResponse, not_found, #{}),
    ?assertNotEqual(not_found, Path),
    ?event(dev_cache, {write_to_cache, {write_success, Path}}),
    {WriteResponse, Path}.

%% @doc Read data from the cache via HTTP.
%% Constructs a GET request using the provided path, sends it to the node,
%% and returns the response.
%%
%% @param Node The target node.
%% @param Path The key or location where the data is stored.
%% @returns The response read from the cache (either binary or wrapped in
%%          {ok, Response}).
read_from_cache(Node, Path) ->
    ?event(dev_cache, {read_from_cache, {start, Node, Path}}),
    ReadMsg = #{
        <<"path">> => <<"/~cache@1.0/read">>,
        <<"method">> => <<"GET">>,
        <<"target">> => Path
    },
    ?event(dev_cache, {read_from_cache, {request_created, ReadMsg}}),
    ?event({test_read, request, ReadMsg}),
    ReadResult = hb_http:get(Node, ReadMsg, #{}),
    ?event(dev_cache, {read_from_cache, {http_get, ReadResult}}),
    case ReadResult of
        ReadResponse when is_binary(ReadResponse) ->
            ?event(dev_cache, 
				{read_from_cache, 
					{response_binary, ReadResponse}
				}
			),
            ReadResponse;
        {ok, ReadResponse} ->
            ?event(dev_cache, {read_from_cache, {response_ok, ReadResponse}}),
            ReadResponse;
        {error, Reason} ->
            ?event(dev_cache, {read_from_cache, {response_error, Reason}}),
            {error, Reason}
    end.

%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

%% @doc Test writing a map to the cache and then reading it back.
%% This test writes a nested map to the cache, reads it back,
%% and verifies that the written data matches the retrieved data.
cache_write_read_map_test() ->
    ?event({cache_write_test, start}),
    {LocalStore, _Wallet, _Address, _Node} = 
		setup_test_env(<<"cache-TEST-WRITE">>),
    TestData = #{
        <<"test_key">> => <<"test_value">>,
        <<"nested">> => #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => 42
        }
    },
    ?event(dev_cache, {cache_write_read_map_test, {test_data, TestData}}),
    {_WriteResponse, Path} = 
		write_to_cache("http://localhost:10000/", TestData, hb:wallet()),
    ?event(dev_cache, {cache_write_read_map_test, {data_written, Path}}),
    ReadResponse = read_from_cache("http://localhost:10000/", Path),
    ?event(dev_cache, {cache_write_read_map_test, {data_read, ReadResponse}}),
    ExpectedValue = hb_converge:get(<<"test_key">>, TestData, not_found, #{}),
    TestValue = hb_converge:get(<<"test_key">>, ReadResponse, not_found, #{}),
    ?assertEqual(ExpectedValue, TestValue),
    ExpectedNested = hb_converge:get(<<"nested">>, TestData, not_found, #{}),
    NestedResponse = 
		hb_converge:get(<<"nested">>, ReadResponse, not_found, #{}),
    ?assertNotEqual(not_found, NestedResponse),
    ExpectedKey1 = hb_converge:get(<<"key1">>, ExpectedNested, not_found, #{}),
    Key1 = hb_converge:get(<<"key1">>, NestedResponse, not_found, #{}),
    ?assertEqual(ExpectedKey1, Key1),
    ExpectedKey2 = hb_converge:get(<<"key2">>, ExpectedNested, not_found, #{}),
    Key2 = hb_converge:get(<<"key2">>, NestedResponse, not_found, #{}),
    ?assertEqual(ExpectedKey2, Key2),
    hb_store:reset(LocalStore),
    ?event(dev_cache, {cache_write_read_map_test}),
    ok.

%% @doc Test writing binary data to the cache and then reading it back.
%% This test writes binary data, reads it from the cache,
%% and asserts that the retrieved data matches the original.
cache_write_read_binary_test() ->
    ?event(dev_cache, {cache_write_read_binary_test, {start}}),
    {LocalStore, _Wallet, _Address, _Node} = 
		setup_test_env(<<"cache-TEST-WRITE">>),
    TestData = <<"test_value">>,
    ?event(dev_cache, {cache_write_read_binary_test, {test_data, TestData}}),
    {_WriteResponse, Path} = 
		write_to_cache("http://localhost:10000/", TestData, hb:wallet()),
    ?event(dev_cache, {cache_write_read_binary_test, {data_written, Path}}),
    ReadResponse = read_from_cache("http://localhost:10000/", Path),
    ?event(dev_cache, 
		{cache_write_read_binary_test, 
			{data_read, ReadResponse}
		}
	),
    ?assertEqual(TestData, ReadResponse),
    hb_store:reset(LocalStore),
    ?event(dev_cache, {cache_write_read_binary_test}),
    ok.

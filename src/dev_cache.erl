%%% @doc A device that looks up an ID from a local store and returns it, honoring
%%% the `accept' key to return the correct format. The cache also supports
%%% writing messages to the store, if the node message has the writer's address
%%% in its `cache_writers' key.
-module(dev_cache).
-export([read/3, write/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Read data from the cache using the given key and options.
%% The options map can specify which store to use, with appropriate configuration.
%%
%% Example:
%%   {ok, Data} = dev_cache:read(Key, #{store => StoreConfig})
%%
%% Returns:
%%   {ok, Data} on success
%%   not_found if the key doesn't exist
%%   {error, Reason} on failure
read(_M1, M2, Opts) ->
    ID = hb_converge:get(<<"target">>, M2, Opts),
    ?event({cache_read, start, {id, ID}, {m2, M2}}),
    case hb_cache:read(ID, Opts) of
        {ok, Res} ->
            ?event({cache_read, success, {result, Res}}),
            case hb_converge:get(<<"accept">>, M2, Opts) of
                <<"application/aos-2">> ->
                    ?event({cache_read, format, aos2}),
                    Struct = dev_json_iface:message_to_json_struct(Res),
                    {ok,
                        #{
                            <<"body">> => jiffy:encode(Struct),
                            <<"content-type">> => <<"application/aos-2">>
                        }};
                _ ->
                    ?event({cache_read, format, default}),
                    {ok, Res}
            end;
        not_found ->
            ?event({cache_read, not_found, ID}),
            {error, not_found}
    end.

%% @doc Write data to the cache with the given options.
%% The options map can specify which store to use, with appropriate configuration.
%%
%% Example:
%%   {ok, Path} = dev_cache:write(Data, #{store => StoreConfig})
%%
%% Returns:
%%   {ok, Path} on success where Path is the location the data was stored
%%   {error, Reason} on failure
write(_M1, M2, Opts) ->
    case is_trusted_writer(M2, Opts) of
        true ->
            Type = hb_converge:get(<<"type">>, M2, <<"single">>, Opts),
            case Type of
                <<"single">> -> 
                    write_single(M2, Opts);
                <<"batch">> ->
                    maps:map(
                        fun(_, Value) ->
                            write_single(Value, Opts)
                        end,
                        hb_converge:get(<<"body">>, M2, Opts)
                    );
                _ ->
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> => <<"Invalid write type.">>
                        }
                    }
            end;
        false ->
            {error, 
                #{
                    <<"status">> => 403,
                    <<"body">> => <<"Not authorized to write to the cache.">>
                }
            }
    end.

write_single(Msg, Opts) ->
    Body = hb_converge:get(<<"body">>, Msg, Opts),
    Location = hb_converge:get(<<"location">>, Msg, Opts),
    Operation = hb_converge:get(<<"operation">>, Msg, <<"write">>, Opts),
    ?event({write_single, start, {body, Body}, {location, Location}, {operation, Operation}}),
    
    case {Operation, Body, Location} of
        {<<"write">>, not_found, _} ->
            ?event({write_single, error, no_body}),
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"No body to write.">>
                }
            };
        {<<"write">>, Map, _Location} when is_map(Map) ->
            {ok, Path} = hb_cache:write(Map, Opts),
            {ok, #{ <<"status">> => 200, <<"path">> => Path }};
        {<<"write">>, Binary, Location} when is_binary(Binary) ->
            {ok, Path} = hb_cache:write_binary(Location, Binary, Opts),
            {ok, #{ <<"status">> => 200, <<"path">> => Path }};
        {<<"link">>, _, _} ->
            Source = hb_converge:get(<<"source">>, Msg, Opts),
            Destination = hb_converge:get(<<"destination">>, Msg, Opts),
            ok = hb_cache:link(Source, Destination, Opts),
            {ok, #{ <<"status">> => 200 }};
        _ ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Invalid write type.">>
                }
            }
    end.

is_trusted_writer(Req, Opts) ->
    Signers = hb_message:signers(Req),
    CacheWriters = hb_opts:get(cache_writers, [], Opts),
    case Signers of
        [Signer] ->
            IsTrusted = lists:member(Signer, CacheWriters),
            IsTrusted;
        _ -> 
            false
    end.

%% Test helpers

%% @doc Create a test environment with a local store and node
setup_test_env(StorePrefix) ->
	% Ensure that node is started using ensure_all_started
	application:ensure_all_started(hb),

    % Set up a local store configuration
    LocalStore = #{ <<"store-module">> => hb_store_fs, <<"prefix">> => StorePrefix },
    
    % Reset the store to ensure a clean test environment
    hb_store:reset(LocalStore),
    ?event({test_setup, store_reset, {store, LocalStore}}),
    
    % Create a wallet for signing requests
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ?event({test_setup, wallet_created, {address, Address}}),
    
    % Start a node with our local store
    Node = hb_http_server:start_node(#{ 
        store => LocalStore,
        cache_writers => [Address, hb_util:human_id(ar_wallet:to_address(hb:wallet()))],
		port => 10001
    }),
    ?event({test_setup, node_started, {node, Node}}),
    
    {LocalStore, Wallet, Address, Node}.

%% @doc Create test data for cache tests
create_test_data() ->
    TestData = #{
        <<"test_key">> => <<"test_value">>,
        <<"nested">> => #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => 42
        }
    },
    ?event({test_data, created, TestData}),
    TestData.

%% @doc Write data to cache via HTTP
write_to_cache(Node, Data, Wallet) ->
    % Create a write request message
    WriteMsg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Data
    },
    ?event({test_write, request_created, WriteMsg}),
    
    % Sign the message with our wallet to authorize the write
    SignedMsg = hb_message:attest(WriteMsg, Wallet),
    ?event({test_write, msg_signed, {signers, hb_message:signers(SignedMsg)}}),
    
    % Send the write request
    ?event({test_write, sending_request}),
    WriteResult = hb_http:post(Node, SignedMsg, #{}),
    ?event(pete, {test_write, {result, {explicit, WriteResult}}}),
    
    {ok, WriteResponse} = WriteResult,
    
    % Verify write was successful
    Status = hb_converge:get(<<"status">>, WriteResponse, 0, #{}),
    ?event({test_write, status, Status}),
    ?assertEqual(200, Status),
    
    % Extract the path from the response
    Path = hb_converge:get(<<"path">>, WriteResponse, not_found, #{}),
    ?event({test_write, path, Path}),
    ?assertNotEqual(not_found, Path),
    
    {WriteResponse, Path}.

%% @doc Read data from cache via HTTP and verify contents
read_from_cache(Node, Path) ->
    % Create read request
    ReadMsg = #{
        <<"path">> => <<"/~cache@1.0/read">>,
        <<"method">> => <<"GET">>,
        <<"target">> => Path
    },
    ?event({test_read, request, ReadMsg}),
    
    % Send the read request
    ReadResult = hb_http:get(Node, ReadMsg, #{}),
    ?event({test_read, result, ReadResult}),
    {ok, ReadResponse} = ReadResult,
    
    ReadResponse.

%% @doc Verify that response data matches expected data
verify_data_match(Response, ExpectedData) ->
    % Verify the test_key value
    ExpectedValue = hb_converge:get(<<"test_key">>, ExpectedData, not_found, #{}),
    TestValue = hb_converge:get(<<"test_key">>, Response, not_found, #{}),
    ?assertEqual(ExpectedValue, TestValue),
    
    % Verify the nested object
    ExpectedNested = hb_converge:get(<<"nested">>, ExpectedData, not_found, #{}),
    NestedResponse = hb_converge:get(<<"nested">>, Response, not_found, #{}),
    ?assertNotEqual(not_found, NestedResponse),
    
    % Verify nested key1
    ExpectedKey1 = hb_converge:get(<<"key1">>, ExpectedNested, not_found, #{}),
    Key1 = hb_converge:get(<<"key1">>, NestedResponse, not_found, #{}),
    ?assertEqual(ExpectedKey1, Key1),
    
    % Verify nested key2
    ExpectedKey2 = hb_converge:get(<<"key2">>, ExpectedNested, not_found, #{}),
    Key2 = hb_converge:get(<<"key2">>, NestedResponse, not_found, #{}),
    ?assertEqual(ExpectedKey2, Key2).

%% @doc Test writing to cache via HTTP POST and then reading it back
cache_write_test() ->
    ?event({cache_write_test, start}),
    
    % Setup test environment
    {LocalStore, Wallet, _Address, Node} = setup_test_env(<<"cache-TEST-WRITE">>),
    
    % Create test data
    TestData = create_test_data(),
    
    % Write data to cache
    {_WriteResponse, Path} = write_to_cache("http://localhost:10000/", TestData, hb:wallet()),
	?event({cache_write_test, path, Path}),
    
    % Read data back and verify
    ReadResponse = read_from_cache(Node, Path),
    
	% Verify the data is correct
	verify_data_match(ReadResponse, TestData),

    % Clean up
    hb_store:reset(LocalStore),
    ?event({cache_write_test, complete}),
    ok.

%% @doc Test reading from cache via HTTP
cache_read_test() ->
    ?event({cache_read_test, start}),
    
    % Setup test environment
    {LocalStore, Wallet, _Address, Node} = setup_test_env(<<"cache-TEST">>),
    
    % Create test data
    TestData = create_test_data(),
    
    % Write to the remote node cache.
	{_WriteResponse, Path} = write_to_cache(Node, TestData, Wallet),
    ?event({cache_read_test, data_written, {path, Path}}),

    % Read via HTTP and verify
    ReadResponse = read_from_cache("http://localhost:10000/", Path),

	% Verify the data is correct
	verify_data_match(ReadResponse, TestData),
    
    % % Clean up
    hb_store:reset(LocalStore),
    ?event({cache_read_test, complete}),
    ok.


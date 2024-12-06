-module(dev_meta).
-export([execute/2]).
-include("include/hb.hrl").

%%% The hyperbeam meta device, which is the default entry point
%%% for all messages processed by the machine. This device executes a 'path' of
%%% functions upon a message, sequentially, returning the message resulting
%%% from the last function.

%%% The ideal API should support:
%%% GET /id/Execute/Report?1.Mode=Fast&2.Mode=Terse <- Edge case?
%%% GET /id/Outbox/1/Target <- Getting results from a process. Common case.
%%% GET /id/Execute/Output?Action=Transfer <- 'Normal' dry-run request.
%%% POST /id/Execute/Output?Action=Transfer <- 'Normal' push request.
%%% POST /id/Schedule <- Schedule the contained message to be executed.
%%% GET /id/Schedule <- Get the schedule of a process by its ID.
%%% POST /id/Schedule <- Send a new message to the schedule of a process.
%%% POST /id/Push <- Push a new message on a process.
%%% POST /Push <- Push a new message on the referenced message.
%%% GET /Execute <- Execute an assignment on the referenced process?
%%% GET /Thing/in/cache <- Get a thing from the cache by its path.

%% @doc Execute a message on hyperbeam.
execute(CarrierMsg, S) ->
    ?event({executing_message, CarrierMsg}),
    {Msg, Path} = parse_carrier_msg(CarrierMsg, S),
    execute_path(Path, Msg, S).

%% @doc Execute a path on a message, yielding a new message.
execute_path([], Msg, _) -> {ok, Msg};
execute_path([FuncName|Path], Msg, S) ->
    ?event({meta_executing_on_path, {function, FuncName}, {path, Path}}),
    Func = parse_path_to_func(FuncName),
    execute_path(Path, hb_pam:resolve(Msg, Func, [Msg], S), S).

parse_path_to_func(BinName) when is_binary(BinName) ->
    binary_to_existing_atom(string:lowercase(BinName), utf8);
parse_path_to_func(AtomName) when is_atom(AtomName) ->
    AtomName.

%% @doc Resolve the carrier message to an executable message, either by extracting
%% from its body or reading from its referenced ID.
parse_carrier_msg(CarrierMsg, S) ->
    case lists:keyfind(<<"Device">>, 1, CarrierMsg#tx.tags) of
        {_, _} ->
            % If the carrier message itself contains a device, we should execute
            % the path on that device.
            ?event({carrier_msg_contains_device, CarrierMsg}),
            {
                path_from_carrier_message(CarrierMsg),
                CarrierMsg
            };
        false ->
            % Otherwise, we try to parse the message from the path + body.
            load_executable_message_from_carrier(CarrierMsg, S)
    end.

%% @doc The carrier message itself does not contain a device, so we try to
%% load the message from the first element of the path, then apply the carrier
%% message that. For example, GET /id/Schedule?From=0&To=1 will result in
%% MessageWithID.Schedule(#{From => 0, To => 1}).
load_executable_message_from_carrier(CarrierMsg, #{ store := RawStore }) ->
    Path = path_from_carrier_message(CarrierMsg),
    Store =
        case hb_opts:get(access_remote_cache_for_client) of
            true -> RawStore;
            false -> hb_store:scope(RawStore, local)
        end,
    load_path(Store, Path).

%% @doc Load a path from the cache. Load the whole path if possible, 
%% backing off to smaller parts of the path until a viable component
%% (eventually just the message ID) is found.
load_path(Store, PathParts) -> load_path(Store, PathParts, []).
load_path(Store, PathParts, Unresolved) ->
    ?event({loading_path, Store, PathParts, Unresolved}),
    % First, try to read the path directly from the cache.
    case hb_cache:read(Store, PathParts) of
        {ok, Msg} -> {Msg, Unresolved};
        not_found ->
            % If that fails, try to read it as a message.
            case hb_cache:read_message(Store, PathParts) of
                {ok, Msg} -> {Msg, Unresolved};
                not_found ->
                    load_path(
                        Store,
                        lists:droplast(PathParts),
                        Unresolved ++ [lists:last(PathParts)]
                    )
            end
    end.

%% @doc Extract the components of the path from the carrier message.
path_from_carrier_message(CarrierMsg) ->
    {_, Path} = lists:keyfind(<<"Path">>, 1, CarrierMsg#tx.tags),
    lists:filter(fun(Part) -> byte_size(Part) > 0 end,
        binary:split(Path, <<"/">>, [global])).
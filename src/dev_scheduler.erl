%%% @doc A simple scheduler scheme for AO.
%%% This device expects a message of the form:
%%%     Process: `#{ id, Scheduler: #{ Authority } }'
%%% ```
%%% It exposes the following keys for scheduling:
%%%     `#{ method: GET, path: <<"/info">> }' ->
%%%         Returns information about the scheduler.
%%%     `#{ method: GET, path: <<"/slot">> }' -> `slot(Msg1, Msg2, Opts)'
%%%         Returns the current slot for a process.
%%%     `#{ method: GET, path: <<"/schedule">> }' -> `get_schedule(Msg1, Msg2, Opts)'
%%%         Returns the schedule for a process in a cursor-traversable format.
%%%    ` #{ method: POST, path: <<"/schedule">> }' -> `post_schedule(Msg1, Msg2, Opts)'
%%%         Schedules a new message for a process, or starts a new scheduler
%%%         for the given message.
%%% '''

-module(dev_scheduler).
%%% Converge API functions:
-export([info/0]).
%%% Local scheduling functions:
-export([schedule/3, router/4, register/3]).
%%% CU-flow functions:
-export([slot/3, status/3, next/3]).
-export([start/0, checkpoint/1]).
%%% Test helper exports:
-export([test_process/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

%% @doc Helper to ensure that the environment is started.
start() ->
    % We need the rocksdb backend to run for hb_cache module to work
    application:ensure_all_started(hb),
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>>
        = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}),
    ok.

%% @doc This device uses a default_handler to route requests to the correct
%% function.
info() -> 
    #{
        exports =>
            [
                register,
                status,
                next,
                schedule,
                slot,
                init,
                checkpoint
            ],
        excludes => [set, keys],
        default => fun router/4
    }.

%% @doc The default handler for the scheduler device.
router(_, Msg1, Msg2, Opts) ->
    ?event({scheduler_router_called, {msg2, Msg2}, {opts, Opts}}),
    schedule(Msg1, Msg2, Opts).

%% @doc Load the schedule for a process into the cache, then return the next
%% assignment. Assumes that Msg1 is a `dev_process` or similar message, having
%% a `Current-Slot' key. It stores a local cache of the schedule in the
%% `priv/To-Process' key.
next(Msg1, Msg2, Opts) ->
    ?event(next, {scheduler_next_called, {msg1, Msg1}, {msg2, Msg2}}),
    Schedule =
        hb_private:get(
            <<"priv/scheduler/assignments">>,
            Msg1,
            Opts
        ),
    LastProcessed = hb_util:int(hb_converge:get(<<"current-slot">>, Msg1, Opts)),
    ?event(next, {local_schedule_cache, {schedule, Schedule}}),
    Assignments =
        case Schedule of
            X when is_map(X) and map_size(X) > 0 -> X;
            _ ->
                {ok, RecvdAssignments} =
                    hb_converge:resolve(
                        Msg1,
                        #{
                            <<"method">> => <<"GET">>,
                            <<"path">> => <<"schedule/assignments">>,
                            <<"from">> => LastProcessed
                        },
                        Opts#{ scheduler_follow_redirects => true }
                    ),
                RecvdAssignments
        end,
    NormAssignments =
        maps:from_list(
            lists:map(
                fun({Slot, Assignment}) ->
                    {hb_util:int(Slot), Assignment}
                end,
                maps:to_list(maps:without([<<"priv">>, <<"attestations">>], Assignments))
            )
        ),
    ValidKeys =
        lists:filter(
            fun(Slot) -> Slot > LastProcessed end,
            maps:keys(NormAssignments)
        ),
    % Remove assignments that are below the last processed slot.
    FilteredAssignments = maps:with(ValidKeys, NormAssignments),
    ?event(next, {filtered_assignments, FilteredAssignments}),
    Slot =
        case ValidKeys of
            [] -> hb_util:int(LastProcessed);
            Slots -> lists:min(Slots)
        end,
    ?event(next,
        {next_slot_to_process, Slot, {last_processed, LastProcessed}}),
    case (LastProcessed + 1) == Slot of
        true ->
            NextMessage =
                hb_converge:get(
                    Slot,
                    FilteredAssignments,
                    Opts
                ),
            NextState =
                hb_private:set(
                    Msg1,
                    <<"schedule/assignments">>,
                    hb_converge:remove(FilteredAssignments, Slot),
                    Opts
                ),
            ?event(next,
                {next_returning, {slot, Slot}, {message, NextMessage}}),
            {ok, #{ <<"body">> => NextMessage, <<"state">> => NextState }};
        false ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"body">> => <<"No assignment found for next slot.">>
                }
            }
    end.

%% @doc Returns information about the entire scheduler.
status(_M1, _M2, _Opts) ->
    ?event(getting_scheduler_status),
    Wallet = dev_scheduler_registry:get_wallet(),
    {ok,
        #{
            <<"address">> => hb_util:id(ar_wallet:to_address(Wallet)),
            <<"processes">> =>
                lists:map(
                    fun hb_util:id/1,
                    dev_scheduler_registry:get_processes()
                ),
            <<"cache-control">> => <<"no-store">>
        }
    }.

%% @doc Generate a new scheduler location record and register it. We both send 
%% the new scheduler-location to the given registry, and return it to the caller.
register(_Msg1, Req, Opts) ->
    % Ensure that the request is signed by the operator.
    ?event({registering_scheduler, {msg1, _Msg1}, {req, Req}, {opts, Opts}}),
    {ok, OnlyAttested} = hb_message:with_only_attested(Req),
    ?event({only_attested, OnlyAttested}),
    Signers = hb_message:signers(OnlyAttested),
    Operator =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, hb:wallet(), Opts)
            )
        ),
    ExistingNonce = 
        case hb_gateway_client:scheduler_location(Operator, Opts) of
            {ok, SchedulerLocation} ->
                hb_converge:get(<<"nonce">>, SchedulerLocation, 0, Opts);
            {error, _} -> -1
        end,
    NewNonce = hb_converge:get(<<"nonce">>, OnlyAttested, 0, Opts),
    case lists:member(Operator, Signers) andalso NewNonce > ExistingNonce of
        false ->
            {ok,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Invalid request.">>,
                    <<"requested-nonce">> => NewNonce,
                    <<"existing-nonce">> => ExistingNonce,
                    <<"signers">> => Signers
                }
            };
        true ->
            % The operator has asked to replace the scheduler location. Get the
            % details and register the new location.
            DefaultTTL = hb_opts:get(scheduler_location_ttl, 1000 * 60 * 60, Opts),
            TimeToLive = hb_converge:get(
                    <<"time-to-live">>,
                    OnlyAttested,
                    DefaultTTL,
                    Opts
                ),
            URL =
                case hb_converge:get(<<"url">>, OnlyAttested, Opts) of
                    not_found ->
                        Port = hb_opts:get(port, 8734, Opts),
                        Host = hb_opts:get(host, <<"localhost">>, Opts),
                        Protocol = hb_opts:get(protocol, http1, Opts),
                        ProtoStr =
                            case Protocol of
                                http1 -> <<"http">>;
                                _ -> <<"https">>
                            end,
                        <<ProtoStr/binary, "://", Host/binary, ":", Port/binary>>;
                    GivenURL -> GivenURL
                end,
            % Construct the new scheduler location message.
            Codec = hb_converge:get(<<"accept-codec">>, OnlyAttested, <<"httpsig@1.0">>, Opts),
            NewSchedulerLocation =
                #{
                    <<"data-protocol">> => <<"ao">>,
                    <<"variant">> => <<"ao.N.1">>,
                    <<"type">> => <<"scheduler-location">>,
                    <<"url">> => URL,
                    <<"nonce">> => NewNonce,
                    <<"time-to-live">> => TimeToLive,
                    <<"codec-device">> => Codec
                },
            Signed = hb_message:attest(NewSchedulerLocation, Opts, Codec),
            ?event({uploading_signed_scheduler_location, Signed}),
            Res = hb_client:upload(Signed, Opts),
            ?event({upload_response, Res}),
            {ok, Signed}
    end.

%% @doc A router for choosing between getting the existing schedule, or
%% scheduling a new message.
schedule(Msg1, Msg2, Opts) ->
    ?event({resolving_schedule_request, {msg2, Msg2}, {state_msg, Msg1}}),
    case hb_converge:get(<<"method">>, Msg2, <<"GET">>, Opts) of
        <<"POST">> -> post_schedule(Msg1, Msg2, Opts);
        <<"GET">> -> get_schedule(Msg1, Msg2, Opts)
    end.

%% @doc Schedules a new message on the SU. Searches Msg1 for the appropriate ID,
%% then uses the wallet address of the scheduler to determine if the message is
%% for this scheduler. If so, it schedules the message and returns the assignment.
post_schedule(Msg1, Msg2, Opts) ->
    ?event(scheduling_message),
    % Find the target message to schedule:
    ToSched = find_message_to_schedule(Msg1, Msg2, Opts),
    ?event({to_sched, ToSched}),
    % Find the ProcessID of the target message:
    % - If it is a Process, use the ID of the message.
    % - If not, use the target as the ProcessID.
    ProcID =
        case hb_converge:get(<<"type">>, ToSched, not_found, Opts) of
            <<"Process">> -> hb_message:id(ToSched, all);
            _ ->
                case hb_converge:get(<<"target">>, ToSched, not_found, Opts) of
                    not_found -> find_target_id(Msg1, Msg2, Opts);
                    Target -> Target
                end
        end,
    ?event({proc_id, ProcID}),
    % Filter all unsigned keys from the source message.
    case hb_message:with_only_attested(ToSched) of
        {ok, OnlyAttested} ->
            ?event(
                {post_schedule,
                    {schedule_id, ProcID},
                    {message, ToSched}
                }
            ),
            % Find the relevant scheduler server for the given process and
            % message, start a new one if necessary, or return a redirect to the
            % correct remote scheduler.
            case find_server(ProcID, Msg1, ToSched, Opts) of
                {local, PID} ->
                    ?event({scheduling_message_locally, {proc_id, ProcID}, {pid, PID}}),
                    do_post_schedule(ProcID, PID, OnlyAttested, Opts);
                {redirect, Redirect} ->
                    ?event({redirecting_to_scheduler, {redirect, Redirect}}),
                    {ok, Redirect};
                {error, Error} ->
                    ?event({error_finding_scheduler, {error, Error}}),
                    {error, Error}
            end;
        {error, _} ->
            {ok,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Message is not valid.">>
                }
            }
    end.

%% @doc Post schedule the message. `Msg2` by this point has been refined to only
%% attested keys, and to only include the `target' message that is to be
%% scheduled.
do_post_schedule(ProcID, PID, Msg2, Opts) ->
    % Should we verify the message again before scheduling?
    Verified =
        case hb_opts:get(verify_assignments, true, Opts) of
            true ->
                ?event({verifying_message_before_scheduling, Msg2}),
                hb_message:verify(Msg2, signers);
            false -> true
        end,
    % Handle scheduling of the message if the message is valid.
    case {Verified, hb_converge:get(<<"type">>, Msg2, Opts)} of
        {false, _} ->
            {ok,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Message is not valid.">>
                }
            };
        {true, <<"Process">>} ->
            hb_cache:write(Msg2, Opts),
            spawn(fun() -> hb_client:upload(Msg2, Opts) end),
            ?event(
                {registering_new_process,
                    {proc_id, ProcID},
                    {pid, PID},
                    {is_alive, is_process_alive(PID)}
                }
            ),
            {ok, dev_scheduler_server:schedule(PID, Msg2)};
        {true, _} ->
            % If Message2 is not a process, use the ID of Message1 as the PID
            {ok, dev_scheduler_server:schedule(PID, Msg2)}
    end.

%% @doc Locate the correct scheduling server for a given process.
find_server(ProcID, Msg1, Opts) ->
    find_server(ProcID, Msg1, undefined, Opts).
find_server(ProcID, Msg1, ToSched, Opts) ->
    case get_hint(ProcID, Opts) of
        {ok, Hint} ->
            ?event({found_hint_in_proc_id, Hint}),
            generate_redirect(ProcID, Hint, Opts);
        not_found ->
            ?event({no_hint_in_proc_id, ProcID}),
            case dev_scheduler_registry:find(ProcID, false, Opts) of
                PID when is_pid(PID) ->
                    ?event({found_pid_in_local_registry, PID}),
                    {local, PID};
                not_found ->
                    ?event({no_pid_in_local_registry, ProcID}),
                    % Find the process from the message.
                    Proc =
                        case hb_converge:get(<<"process">>, Msg1, not_found, Opts#{ hashpath => ignore }) of
                            not_found ->
                                ?event({reading_cache, {proc_id, ProcID}}),
                                case hb_cache:read(ProcID, Opts) of
                                    {ok, P} -> P;
                                    not_found -> Msg1
                                end;
                            P -> P
                        end,
                    ?event({found_process, {process, Proc}, {msg1, Msg1}}),
                    % Check if we are the scheduler for this process.
                    Address = hb_util:human_id(ar_wallet:to_address(
                        hb_opts:get(priv_wallet, hb:wallet(), Opts))),
                    ?event({local_address, Address}),
                    SchedLoc =
                        hb_converge:get_first(
                            [
                                {Proc, <<"scheduler">>},
                                {Proc, <<"scheduler-location">>}
                            ] ++
                            case ToSched of
                                undefined -> [];
                                _ -> [{ToSched, <<"scheduler-location">>}]
                            end,
                            not_found,
                            Opts#{ hashpath => ignore }
                        ),
                    ?event({sched_loc, SchedLoc}),
                    case SchedLoc of
                        not_found ->
                            {error, <<"No scheduler information provided.">>};
                        Address ->
                            % We are the scheduler. Start the server if it has not already
                            % been started.
                            {local, dev_scheduler_registry:find(ProcID, true, Opts)};
                        _ ->
                            % We are not the scheduler. Find it and return a redirect.
                            find_remote_scheduler(ProcID, SchedLoc, Opts)
                    end
            end
    end.

%% @doc If a hint is present in the string, return it. Else, return not_found.
get_hint(Str, Opts) when is_binary(Str) ->
    case hb_opts:get(scheduler_follow_hints, true, Opts) of
        true ->
            case binary:split(Str, <<"?">>, [global]) of
                [_, QS] ->
                    QueryMap = maps:from_list(uri_string:dissect_query(QS)),
                    case maps:get(<<"hint">>, QueryMap, not_found) of
                        not_found -> not_found;
                        Hint -> {ok, Hint}
                    end;
                _ -> not_found
            end;
        false -> not_found
    end;
get_hint(Str, _Opts) -> not_found.

%% @doc Generate a redirect message to a scheduler.
generate_redirect(ProcID, URL, Opts) ->
    generate_redirect(ProcID, URL, #{}, Opts).
generate_redirect(ProcID, URL, SchedulerLocation, Opts) ->
    AcceptCodec =
        case hb_converge:get(<<"variant">>, SchedulerLocation, <<"ao.N.1">>, Opts) of
            <<"ao.N.1">> -> <<"httpsig@1.0">>;
            <<"ao.TN.1">> -> <<"ans104@1.0">>
        end,
    ProcWithoutHint = without_hint(ProcID),
    Sep = case binary:last(URL) of $/ -> <<"">>; _ -> <<"/">> end,
    {redirect,
        #{
            <<"status">> => 307,
            <<"location">> =>
                case AcceptCodec of
                    <<"httpsig@1.0">> ->
                        <<
                            URL/binary, Sep/binary, ProcWithoutHint/binary,
                            "/schedule"
                        >>;
                    <<"ans104@1.0">> ->
                        <<
                            URL/binary, Sep/binary, ProcWithoutHint/binary,
                            "?proc-id=", ProcWithoutHint/binary
                        >>
                end,
            <<"body">> => <<"Redirecting to scheduler: ", URL/binary>>,
            <<"accept-codec">> => AcceptCodec
        }
    }.

without_hint(Target) ->
    hd(binary:split(Target, [<<"?">>, <<"&">>], [global])).

%% @doc Use the SchedulerLocation to the remote path and return a redirect.
find_remote_scheduler(ProcID, SchedulerLocation, Opts) ->
    % Parse the SchedulerLocation to see if it has a hint. If there is a hint,
    % we will use it to construct a redirect message.
    case get_hint(SchedulerLocation, Opts) of
        {ok, Hint} ->
            % We have a hint. Construct a redirect message.
            generate_redirect(ProcID, Hint, Opts);
        not_found ->
            {ok, SchedMsg} =
                hb_gateway_client:scheduler_location(SchedulerLocation, Opts),
            {ok, SchedURL} = hb_converge:resolve(SchedMsg, <<"url">>, Opts),
            % We have a valid path. Construct a redirect message.
            generate_redirect(ProcID, SchedURL, SchedMsg, Opts)
    end.

%% @doc Returns information about the current slot for a process.
slot(M1, M2, Opts) ->
    ?event({getting_current_slot, {msg, M1}}),
    ProcID = find_target_id(M1, M2, Opts),
    case find_server(ProcID, M1, Opts) of
        {local, PID} ->
            ?event({getting_current_slot, {proc_id, ProcID}}),
            {Timestamp, Hash, Height} = ar_timestamp:get(),
            #{ current := CurrentSlot, wallet := Wallet } =
                dev_scheduler_server:info(PID),
            {ok, #{
                <<"process">> => ProcID,
                <<"current-slot">> => CurrentSlot,
                <<"timestamp">> => Timestamp,
                <<"block-height">> => Height,
                <<"block-hash">> => Hash,
                <<"cache-control">> => <<"no-store">>,
                <<"wallet-address">> => hb_util:human_id(ar_wallet:to_address(Wallet))
            }};
        {redirect, Redirect} ->
            {ok, Redirect}
    end.

%% @doc Generate and return a schedule for a process, optionally between
%% two slots -- labelled as `from' and `to'. If the schedule is not local,
%% we redirect to the remote scheduler or proxy based on the node opts.
get_schedule(Msg1, Msg2, Opts) ->
    ProcID = find_target_id(Msg1, Msg2, Opts),
    From =
        case hb_converge:get(<<"from">>, Msg2, not_found, Opts) of
            not_found -> 0;
            X when X < 0 -> 0;
            FromRes -> hb_util:int(FromRes)
        end,
    To =
        case hb_converge:get(<<"to">>, Msg2, not_found, Opts) of
            not_found ->
                ?event({getting_current_slot, {proc_id, ProcID}}),
                case dev_scheduler_registry:find(ProcID) of
                    not_found ->
                        {Slot, _} = dev_scheduler_cache:latest(ProcID, Opts),
                        Slot;
                    Pid -> maps:get(current, dev_scheduler_server:info(Pid))
                end;
            ToRes -> hb_util:int(ToRes)
        end,
    Format = hb_converge:get(<<"accept">>, Msg2, <<"application/http">>, Opts),
    ?event({parsed_get_schedule, {process, ProcID}, {from, From}, {to, To}, {format, Format}}),
    case find_server(ProcID, Msg1, Opts) of
        {local, _PID} ->
            generate_local_schedule(Format, ProcID, From, To, Opts);
        {redirect, Redirect} ->
            case hb_opts:get(scheduler_follow_redirects, true, Opts) of
                true ->
                    case get_remote_schedule(ProcID, From, To, Redirect, Opts) of
                        {ok, Res} ->
                            case Format of
                                <<"application/aos-2">> ->
                                    dev_scheduler_formats:assignments_to_aos2(
                                        ProcID,
                                        hb_converge:get(
                                            <<"assignments">>, Res, [], Opts),
                                        hb_util:atom(hb_converge:get(
                                            <<"continues">>, Res, false, Opts)),
                                        Opts
                                    );
                                _ ->
                                    {ok, Res}
                            end;
                        {error, Res} ->
                            {error, Res}
                    end;
                false ->
                    {ok, Redirect}
            end
    end.

%% @doc Get a schedule from a remote scheduler.
get_remote_schedule(ProcID, From, To, Redirect, Opts) ->
    Location = hb_converge:get(<<"location">>, Redirect, Opts),
    Parsed = uri_string:parse(Location),
    Node =
        uri_string:recompose(
            (maps:remove(query, Parsed))#{
                path => <<"/">>
            }
        ),
    ?event({getting_remote_schedule, {proc_id, ProcID}, {from, From}, {to, To}}),
    ToBin = integer_to_binary(From+1),
    FromBin = integer_to_binary(From),
    Path = <<
            ProcID/binary,
            "/schedule"
            "&from+integer=",
            FromBin/binary,
            "&to+integer=",
            ToBin/binary
        >>,
    case hb_http:get(Node, Path, Opts) of
        {ok, Res} ->
            ?event(push, {remote_schedule_result, {res, Res}}, Opts),
            case hb_converge:get(<<"status">>, Res, 200, Opts) of
                200 -> {ok, Res};
                307 -> get_remote_schedule(ProcID, From, To, Redirect, Opts)
            end;
        {error, Res} ->
            ?event(push, {remote_schedule_result, {res, Res}}, Opts),
            {error, Res}
    end.

%%% Private methods

%% @doc Find the schedule ID from a given request. The precidence order for 
%% search is as follows:
%% [1. `ToSched/id' -- in the case of `POST schedule', handled locally]
%% 2. `Msg2/target'
%% 3. `Msg2/id' when `Msg2' has `type: Process'
%% 4. `Msg1/process/id'
%% 5. `Msg1/id' when `Msg1' has `type: Process'
%% 6. `Msg2/id'
find_target_id(Msg1, Msg2, Opts) ->
    TempOpts = Opts#{ hashpath => ignore },
    Res = case hb_converge:resolve(Msg2, <<"target">>, TempOpts) of
        {ok, Target} ->
            % ID found at Msg2/target
            Target;
        _ ->
            case hb_converge:resolve(Msg2, <<"type">>, TempOpts) of
                {ok, <<"Process">>} ->
                    % Msg2 is a Process, so the ID is at Msg2/id
                    hb_message:id(Msg2, all);
                _ ->
                    case hb_converge:resolve(Msg1, <<"process">>, TempOpts) of
                        {ok, Process} ->
                            % ID found at Msg1/process/id
                            hb_message:id(Process, all);
                        _ ->
                            % Does the message have a type of Process?
                            case hb_converge:get(<<"type">>, Msg1, TempOpts) of
                                <<"Process">> ->
                                    % Yes, so try Msg1/id
                                    hb_message:id(Msg1, all);
                                _ ->
                                    % No, so the ID is at Msg2/id
                                    hb_message:id(Msg2, all)
                            end
                end
            end
    end,
    ?event({found_id, {id, Res}}),
    Res.

%% @doc Search the given base and request message pair to find the message to
%% schedule. The precidence order for search is as follows:
%% 1. `Msg2/body'
%% 2. `Msg2'
find_message_to_schedule(_Msg1, Msg2, Opts) ->
    case hb_converge:resolve(Msg2, <<"body">>, Opts#{ hashpath => ignore }) of
        {ok, Body} ->
            Body;
        _ -> Msg2
    end.

%% @doc Generate a `GET /schedule' response for a process.
generate_local_schedule(Format, ProcID, From, To, Opts) ->
    ?event(
        {servicing_request_for_assignments,
            {proc_id, ProcID},
            {from, From},
            {to, To}
        }
    ),
    ?event(generating_schedule_from_local_server),
    {Assignments, More} = get_local_assignments(ProcID, From, To, Opts),
    ?event({got_assignments, length(Assignments), {more, More}}),
    % Determine and apply the formatting function to use for generation 
    % of the response, based on the `Accept' header.
    FormatterFun =
        case Format of
            <<"application/aos-2">> ->
                fun dev_scheduler_formats:assignments_to_aos2/4;
            _ ->
                fun dev_scheduler_formats:assignments_to_bundle/4
        end,
    Res = FormatterFun(ProcID, Assignments, More, Opts),
    ?event({assignments_bundle_outbound, {format, Format}, {res, Res}}),
    Res.

%% @doc Get the assignments for a process, and whether the request was truncated.
get_local_assignments(ProcID, From, RequestedTo, Opts) ->
    ?event({handling_req_to_get_assignments, ProcID, From, RequestedTo}),
    ComputedTo =
        case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
            true -> RequestedTo + ?MAX_ASSIGNMENT_QUERY_LEN;
            false -> RequestedTo
        end,
    {
        do_get_assignments(ProcID, From, ComputedTo, Opts),
        ComputedTo =/= RequestedTo
    }.

%% @doc Get the assignments for a process.
do_get_assignments(_ProcID, From, To, _Opts) when From > To ->
    [];
do_get_assignments(ProcID, From, To, Opts) ->
    case dev_scheduler_cache:read(ProcID, From, Opts) of
        not_found ->
            [];
        {ok, Assignment} ->
            [
                Assignment
                | do_get_assignments(
                    ProcID,
                    From + 1,
                    To,
                    Opts
                )
            ]
    end.

%% @doc Returns the current state of the scheduler.
checkpoint(State) -> {ok, State}.

%%% Tests

%% @doc Generate a _transformed_ process message, not as they are generated 
%% by users. See `dev_process' for examples of AO process messages.
test_process() -> test_process(hb:wallet()).
test_process(Wallet) when not is_binary(Wallet) ->
    test_process(hb_util:human_id(ar_wallet:to_address(Wallet)));
test_process(Address) ->
    #{
        <<"device">> => <<"scheduler@1.0">>,
        <<"device-stack">> => [<<"Cron@1.0">>, <<"WASM-64@1.0">>, <<"PODA@1.0">>],
        <<"image">> => <<"wasm-image-id">>,
        <<"type">> => <<"Process">>,
        <<"scheduler-location">> => Address,
        <<"test-random-seed">> => rand:uniform(1337)
    }.

status_test() ->
    start(),
    ?assertMatch(
        #{<<"processes">> := Processes,
            <<"address">> := Address}
            when is_list(Processes) and is_binary(Address),
        hb_converge:get(status, test_process())
    ).

register_new_process_test() ->
    start(),
    Msg1 = test_process(),
    ?event({test_registering_new_process, {msg, Msg1}}),
    ?assertMatch({ok, _},
        hb_converge:resolve(
            Msg1,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Msg1
            },
            #{}
        )
    ),
    ?event({status_response, Msg1}),
    Procs = hb_converge:get(<<"processes">>, hb_converge:get(status, Msg1)),
    ?event({procs, Procs}),
    ?assert(
        lists:member(
            hb_util:id(Msg1, all),
            hb_converge:get(<<"processes">>, hb_converge:get(status, Msg1))
        )
    ).

schedule_message_and_get_slot_test() ->
    start(),
    Msg1 = test_process(),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"true">>
            }, hb:wallet())
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    Msg3 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>,
        <<"process">> => hb_util:id(Msg1)
    },
    ?event({pg, dev_scheduler_registry:get_processes()}),
    ?event({getting_schedule, {msg, Msg3}}),
    ?assertMatch({ok, #{ <<"current-slot">> := CurrentSlot }}
            when CurrentSlot > 0,
        hb_converge:resolve(Msg1, Msg3, #{})).

redirect_to_hint_test() ->
    start(),
    RandAddr = hb_util:human_id(crypto:strong_rand_bytes(32)),
    TestLoc = <<"http://test.computer">>,
    Msg1 = test_process(<< RandAddr/binary, "?hint=", TestLoc/binary>>),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Msg1
    },
    ?assertMatch(
        {ok, #{ <<"location">> := Location }} when is_binary(Location),
        hb_converge:resolve(Msg1, Msg2, #{ scheduler_follow_hints => true })).

redirect_from_graphql_test() ->
    start(),
    Opts =
        #{ store =>
            [
                {hb_store_fs, #{ prefix => "mainnet-cache" }},
                {hb_store_gateway, #{}}
            ]
        },
    {ok, Msg} = hb_cache:read(<<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>, Opts),
    ?assertMatch(
        {ok, #{ <<"location">> := Location }} when is_binary(Location),
        hb_converge:resolve(
            Msg,
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:attest(#{
                        <<"type">> => <<"Message">>,
                        <<"target">> =>
                            <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>,
                        <<"test-key">> => <<"Test-Val">>
                    }, hb:wallet())
            },
            #{}
        )
    ).

get_local_schedule_test() ->
    start(),
    Msg1 = test_process(),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"Test-Val">>
            }, hb:wallet())
    },
    Msg3 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"Test-Val-2">>
            }, hb:wallet())
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg3, #{})),
    ?assertMatch(
        {ok, _},
        hb_converge:resolve(Msg1, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>,
            <<"target">> => hb_util:id(Msg1)
        },
        #{})
    ).

%%% HTTP tests

http_init() -> http_init(#{}).
http_init(Opts) ->
    start(),
    Wallet = ar_wallet:new(),
    Node = hb_http_server:start_node(Opts#{ priv_wallet => Wallet }),
    {Node, Wallet}.

register_scheduler_test() ->
    start(),
    {Node, Wallet} = http_init(),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/register">>,
        <<"url">> => <<"https://hyperbeam-test-ignore.com">>,
        <<"method">> => <<"POST">>,
        <<"nonce">> => 1,
        <<"accept-codec">> => <<"ans104@1.0">>
    }, Wallet),
    {ok, Res} = hb_http:get(Node, Msg1, #{}),
    ?assertMatch(#{ <<"url">> := Location } when is_binary(Location), Res).

http_post_schedule_sign(Node, Msg, ProcessMsg, Wallet) ->
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(
                Msg#{
                    <<"target">> => hb_util:human_id(hb_message:id(ProcessMsg)),
                    <<"type">> => <<"Message">>
                },
                Wallet
            )
    }, Wallet),
    hb_http:post(Node, Msg1, #{}).

http_get_slot(N, PMsg) ->
    ID = hb_message:id(PMsg, all),
    Wallet = hb:wallet(),
    {ok, _} = hb_http:get(N, hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/slot">>,
        <<"method">> => <<"GET">>,
        <<"target">> => ID
    }, Wallet), #{}).

http_get_schedule(N, PMsg, From, To) ->
    http_get_schedule(N, PMsg, From, To, <<"application/http">>).

http_get_schedule(N, PMsg, From, To, Format) ->
    ID = hb_message:id(PMsg, all),
    Wallet = hb:wallet(),
    {ok, _} = hb_http:get(N, hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"GET">>,
        <<"target">> => hb_util:human_id(ID),
        <<"from">> => From,
        <<"to">> => To,
        <<"accept">> => Format
    }, Wallet), #{}).

http_get_schedule_redirect_test() ->
    Opts =
        #{
            store =>
                [
                    {hb_store_fs, #{ prefix => "mainnet-cache" }},
                    {hb_store_gateway, #{}}
                ],
                scheduler_follow_redirects => false
        },
    {N, _Wallet} = http_init(Opts),
    start(),
    ProcID = <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>,
    Res = hb_http:get(N, <<"/", ProcID/binary, "/schedule">>, #{}),
    ?assertMatch({ok, #{ <<"location">> := Location }} when is_binary(Location), Res).

http_post_schedule_test() ->
    {N, W} = http_init(),
    PMsg = hb_message:attest(test_process(W), W),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => PMsg
    }, W),
    {ok, Res} = hb_http:post(N, Msg1, #{}),
    {ok, Res2} =
        http_post_schedule_sign(
            N,
            #{ <<"inner">> => <<"test-message">> },
            PMsg,
            W
        ),
    ?assertEqual(<<"test-message">>, hb_converge:get(<<"body/inner">>, Res2, #{})),
    ?assertMatch({ok, #{ <<"current-slot">> := 1 }}, http_get_slot(N, PMsg)).

http_get_schedule_test() ->
    {Node, Wallet} = http_init(),
    PMsg = hb_message:attest(test_process(Wallet), Wallet),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => PMsg
    }, Wallet),
    Msg2 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => PMsg
    }, Wallet),
    {ok, _} = hb_http:post(Node, Msg1, #{}),
    lists:foreach(
        fun(_) -> {ok, _} = hb_http:post(Node, Msg2, #{}) end,
        lists:seq(1, 10)
    ),
    ?assertMatch({ok, #{ <<"current-slot">> := 10 }}, http_get_slot(Node, PMsg)),
    {ok, Schedule} = http_get_schedule(Node, PMsg, 0, 10),
    Assignments = hb_converge:get(<<"assignments">>, Schedule, #{}),
    ?assertEqual(
        12, % +1 for the hashpath
        length(maps:values(Assignments))
    ).

http_get_json_schedule_test() ->
    {Node, Wallet} = http_init(),
    PMsg = hb_message:attest(test_process(Wallet), Wallet),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => PMsg
    }, Wallet),
    {ok, _} = hb_http:post(Node, Msg1, #{}),
    Msg2 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(
                #{
                    <<"inner">> => <<"test">>,
                    <<"target">> => hb_util:human_id(hb_message:id(PMsg, all))
                },
                Wallet
            )
        },
        Wallet
    ),
    lists:foreach(
        fun(_) -> {ok, _} = hb_http:post(Node, Msg2, #{}) end,
        lists:seq(1, 10)
    ),
    ?assertMatch({ok, #{ <<"current-slot">> := 10 }}, http_get_slot(Node, PMsg)),
    {ok, Schedule} = http_get_schedule(Node, PMsg, 0, 10, <<"application/aos-2">>),
    ?event({schedule, Schedule}),
    JSON = hb_converge:get(<<"body">>, Schedule, #{}),
    Assignments = jiffy:decode(JSON, [return_maps]),
    ?assertEqual(
        11, % +1 for the hashpath
        length(maps:get(<<"edges">>, Assignments))
    ).

%%% Benchmarks

single_converge(Opts) ->
    start(),
    BenchTime = 1,
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Msg1 = test_process(Wallet),
    ?event({benchmark_start, ?MODULE}),
    MsgToSchedule = hb_message:attest(#{
        <<"type">> => <<"Message">>,
        <<"test-key">> => <<"test-val">>
    }, Wallet),
    Iterations = hb:benchmark(
        fun(_) ->
            MsgX = #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => MsgToSchedule
            },
            ?assertMatch({ok, _}, hb_converge:resolve(Msg1, MsgX, Opts))
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    Msg3 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>,
        <<"process">> => hb_util:human_id(hb_message:id(Msg1, all))
    },
    ?assertMatch({ok, #{ <<"current-slot">> := CurrentSlot }}
            when CurrentSlot == Iterations - 1,
        hb_converge:resolve(Msg1, Msg3, Opts)),
    ?event(bench, {res, Iterations - 1}),
    hb_util:eunit_print(
        "Scheduled ~p messages through Converge in ~p seconds (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 3).

many_clients(Opts) ->
    BenchTime = 1,
    Processes = hb_opts:get(workers, 25, Opts),
    {Node, Wallet} = http_init(Opts),
    PMsg = hb_message:attest(test_process(Wallet), Wallet),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"process">> => PMsg,
        <<"body">> => hb_message:attest(#{ <<"inner">> => <<"test">> }, Wallet)
    }, Wallet),
    {ok, _} = hb_http:post(Node, Msg1, Opts),
    Iterations = hb:benchmark(
        fun(X) ->
            {ok, _} = hb_http:post(Node, Msg1, Opts),
            ?event(bench, {iteration, X, self()})
        end,
        BenchTime,
        Processes
    ),
    ?event({iterations, Iterations}),
    hb_util:eunit_print(
        "Scheduled ~p messages with ~p workers through HTTP in ~ps (~.2f msg/s)",
        [Iterations, Processes, BenchTime, Iterations / BenchTime]
    ),
    {ok, Res} = http_get_slot(Node, PMsg),
    ?event(bench, {res, Res}),
    ?assert(Iterations > 10).

benchmark_suite_test_() ->
    rand:seed(exsplus, erlang:timestamp()),
    Port = 30000 + rand:uniform(10000),
    Bench = [
        {benchmark, "benchmark", fun single_converge/1},
        {multihttp_benchmark, "multihttp_benchmark", fun many_clients/1}
    ],
    OptSpecs = [
        #{
            name => fs,
            opts => #{
                store => {hb_store_fs, #{
                    prefix => <<"TEST-cache/fs-",
                        (integer_to_binary(Port))/binary>>
                }},
                scheduling_mode => local_confirmation,
                port => Port
            },
            desc => "FS store, local conf."
        },
        #{
            name => fs_aggressive,
            opts => #{
                store => {hb_store_fs, #{
                    prefix => <<"TEST-cache/fs-",
                        (integer_to_binary(Port))/binary>>
                }},
                scheduling_mode => aggressive,
                port => Port + 1
            },
            desc => "FS store, aggressive conf."
        },
        #{
            name => rocksdb,
            opts => #{
                store => {hb_store_rocksdb, #{
                    prefix => <<"TEST-cache-rocksdb-",
                        (integer_to_binary(Port+1))/binary>>
                }},
                scheduling_mode => local_confirmation,
                port => Port + 2
            },
            desc => "RocksDB store, local conf."
        },
        #{
            name => rocksdb_aggressive,
            opts => #{
                store => {hb_store_rocksdb, #{
                    prefix => <<"TEST-cache-rocksdb-",
                        (integer_to_binary(Port+2))/binary>>
                }},
                scheduling_mode => aggressive,
                port => Port + 3
            },
            desc => "RocksDB store, aggressive conf."
        }
        % #{
        %     name => rocksdb_extreme_aggressive_h3,
        %     opts => #{
        %         store => {hb_store_rocksdb, #{
        %             prefix => <<"TEST-cache-rocksdb-",
        %                 (integer_to_binary(Port+3))/binary>>
        %         }},
        %         scheduling_mode => aggressive,
        %         protocol => http3,
        %         workers => 100
        %     },
        %     desc => "100xRocksDB store, aggressive conf, http/3."
        % }
    ],
    hb_test_utils:suite_with_opts(Bench, OptSpecs).
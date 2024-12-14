-module(dev_process_cache).
-export([latest/2, read/2, read/3, write/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% @moduledoc A wrapper around the hb_cache module that provides a more
%%% convenient interface for reading the result of a process at a given slot or
%%% message ID.

-define(COMPUTE_CACHE_KEY, "computed").

%% @doc Read the result of a process at a given slot or message ID.
read(ProcID, Opts) ->
    hb_util:ok(latest(ProcID, Opts)).
read(ProcID, Slot, Opts) when is_integer(Slot) ->
    read(ProcID, ["computed", "slot", integer_to_list(Slot)], Opts);
read(ProcID, MessageID, Opts) when ?IS_ID(MessageID) ->
    read(ProcID, hb_util:human_id(MessageID), Opts);
read(ProcID, SlotBin, Opts) when is_binary(SlotBin) ->
    read(
        hb_util:human_id(ProcID),
        ["computed", "slot", binary_to_list(SlotBin)],
        Opts
    );
read(ProcID, SlotRef, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event({reading_computed_result, ProcID, SlotRef}),
    ResolvedPath =
        P2 = hb_store:resolve(
            Store,
            P1 = hb_store:path(Store, [?COMPUTE_CACHE_KEY, ProcID, SlotRef])
        ),
    ?event({resolved_path, {p1, P1}, {p2, P2}, {resolved, ResolvedPath}}),
    case hb_store:type(Store, ResolvedPath) of
        not_found -> not_found;
        _ -> hb_cache:read(ResolvedPath, Opts)
    end.

%% @doc Write a process computation result to the cache.
write(ProcID, Slot, Item, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    Path = hb_store:path(
        Store,
        [
            "computed",
            hb_util:human_id(ProcID),
            "slot",
            integer_to_list(Slot)
        ]
    ),
    hb_cache:write(Path, Item, Opts).

%% @doc Retrieve the latest slot for a given process.
latest(ProcID, Opts) -> latest(ProcID, undefined, Opts).
latest(ProcID, Limit, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    Path = [hb_util:human_id(ProcID), "computed", "slot"],
    case hb_store:list(Path, Opts) of
        [] -> not_found;
        AllOutputSlots ->
            ?event(
                {searching_for_latest_slot,
                    {proc_id, hb_util:human_id(ProcID)},
                    {limit, Limit},
                    {path, Path},
                    {all_output_slots, AllOutputSlots}
                }
            ),
            case first_slot_with_path(
                    Store,
                    hb_util:human_id(ProcID),
                    lists:reverse(lists:sort(AllOutputSlots)),
                    Limit,
                    Path
                ) of
                not_found -> not_found;
                Slot ->
                    ResolvedPath =
                        hb_store:resolve(
                            Store,
                            hb_store:path(
                                Store,
                                [
                                    "computed",
                                    hb_util:human_id(ProcID),
                                    "slot",
                                    integer_to_list(Slot)
                                ]
                            )
                        ),
                    ?event({resolved_path, ResolvedPath}),
                    {ok, Msg} = hb_cache:read(ResolvedPath, Opts),
                    ?event(got_message),
                    {Slot, Msg}
            end
    end.

%% @doc Find the first slot with a given sub-path element.
first_slot_with_path(_Store, _ProcID, [], _Limit, _Path) ->
    not_found;
first_slot_with_path(Store, ProcID, [AfterLimit | Rest], Limit, Path) 
        when AfterLimit > Limit ->
    first_slot_with_path(Store, ProcID, Rest, Limit, Path);
first_slot_with_path(Store, ProcID, [LatestSlot | Rest], Limit, Path) ->
    ?event({trying_slot, LatestSlot, Path}),
    RawPath =
        build_path(
            ["computed", process, "slot", slot] ++ Path,
            #{
                slot => integer_to_list(LatestSlot),
                process => hb_util:human_id(ProcID)
            }
        ),
    ResolvedPath = hb_store:resolve(Store, RawPath),
    case hb_store:type(Store, ResolvedPath) of
        not_found -> first_slot_with_path(Store, ProcID, Rest, Limit, Path);
        _ -> LatestSlot
    end.

%% @doc Takes a path list and a map of values, inserting the values into 
%% the path list as appropriate.
build_path(PathList, Map) ->
    lists:map(
        fun(Ref) when is_atom(Ref) -> maps:get(Ref, Map);
            (Other) -> Other
        end,
        PathList
    ).

%%% Tests

write_and_read_output_test() ->
    Opts = hb_cache:test_opts(),
    Proc = hb_cache:test_signed(
        #{ <<"test-item">> => hb_cache:test_unsigned(<<"test-body-data">>) }),
    ProcID = hb_util:human_id(ar_bundles:id(Proc, signed)),
    Item1 = hb_cache:test_signed(<<"Simple signed output #1">>),
    Item2 = hb_cache:test_signed(<<"Simple signed output #2">>),
    {ok, _} = write(ProcID, 0, Item1, Opts),
    {ok, _} = write(ProcID, 1, Item2, Opts),
    ?assertEqual({ok, Item1}, read(ProcID, ar_bundles:id(Item1, unsigned), Opts)),
    ?assertEqual({ok, Item2}, read(ProcID, ar_bundles:id(Item2, unsigned), Opts)),
    ?assertEqual({ok, Item2}, read(ProcID, 1, Opts)),
    ?assertEqual({ok, Item1}, read(ProcID, ar_bundles:id(Item1, unsigned), Opts)).

latest_output_retrieval_test() ->
    Opts = hb_cache:test_opts(),
    Proc = hb_cache:test_signed(
        #{ <<"test-item">> => hb_cache:test_unsigned(<<"test-body-data">>) }),
    ProcID = hb_util:human_id(ar_bundles:id(Proc, signed)),
    Item1 = hb_cache:test_signed(<<"Simple signed output #1">>),
    Item2 = hb_cache:test_signed(<<"Simple signed output #2">>),
    {ok, _} = write(Opts, ProcID, 0, Item1),
    {ok, _} = write(Opts, ProcID, 1, Item2),
    ?assertEqual({1, Item2}, latest(ProcID, Opts)),
    ?assertEqual({0, Item1}, latest(ProcID, 0, Opts)).
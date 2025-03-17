-module(dev_scheduler_cache).
-export([write/2, read/3, list/2, latest/2]).
-include("include/hb.hrl").

%%% Assignment cache functions

%% @doc Write an assignment message into the cache.
write(Assignment, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    % Write the message into the main cache
    ProcID = hb_converge:get(<<"process">>, Assignment),
    Slot = hb_converge:get(<<"slot">>, Assignment),
    ?event(
        {writing_assignment,
            {proc_id, ProcID},
            {slot, Slot},
            {assignment, Assignment}
        }
    ),
    {ok, RootPath} = hb_cache:write(Assignment, Opts),
    % Create symlinks from the message on the process and the 
    % slot on the process to the underlying data.
    hb_store:make_link(
        Store,
        RootPath,
        hb_store:path(
            Store,
            [
                <<"assignments">>,
                hb_util:human_id(ProcID),
                hb_converge:normalize_key(Slot)
            ]
        )
    ),
    ok.

%% @doc Get an assignment message from the cache.
read(ProcID, Slot, Opts) when is_integer(Slot) ->
    read(ProcID, integer_to_list(Slot), Opts);
read(ProcID, Slot, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ResolvedPath =
        P2 = hb_store:resolve(
            Store,
            P1 = hb_store:path(Store, [
                "assignments",
                hb_util:human_id(ProcID),
                Slot
            ])
        ),
    ?event({resolved_path, {p1, P1}, {p2, P2}, {resolved, ResolvedPath}}),
    hb_cache:read(ResolvedPath, Opts).

%% @doc Get the assignments for a process.
list(ProcID, Opts) ->
    hb_cache:list_numbered(
        hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
            "assignments",
            hb_util:human_id(ProcID)
        ]),
        Opts
    ).

%% @doc Get the latest assignment from the cache.
latest(ProcID, Opts) ->
    ?event({getting_assignments_from_cache, {proc_id, ProcID}, {opts, Opts}}),
    case dev_scheduler_cache:list(ProcID, Opts) of
        [] ->
            ?event({no_assignments_in_cache, {proc_id, ProcID}}),
            not_found;
        Assignments ->
            AssignmentNum = lists:max(Assignments),
            ?event(
                {found_assignment_from_cache,
                    {proc_id, ProcID},
                    {assignment_num, AssignmentNum}
                }
            ),
            {ok, Assignment} = dev_scheduler_cache:read(
                ProcID,
                AssignmentNum,
                Opts
            ),
            {
                AssignmentNum,
                hb_converge:get(
                    <<"hash-chain">>, Assignment, #{ hashpath => ignore })
            }
    end.
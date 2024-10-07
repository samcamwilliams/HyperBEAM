-module(ao_cache).
-export([read/3, latest/3, latest/4, checkpoints/2, write_output/5, write_message/2]).
-include("src/include/ao.hrl").

-define(DEFAULT_DATA_DIR, "data").
-define(COMPUTE_CACHE_DIR, "computed").
-define(DEFAULT_FREQ, 2).

%%% A cache of AO messages and compute results.
%%% 
%%% In AO, every message is a combinator: The message itself represents a
%%% 'processor' that can be applied to a new message, yielding a result.
%%% As a consequence, a simple way of understanding AO's computation model is to
%%% think of it as a dictionary: Every message is a key, yielding its computed value.
%%% 
%%% Each message itself can be raw data with an associated header (containing metadata),
%%% or a bundle of other messages (its children). These children are expressed as 
%%% either maps or list of other messages.
%%% 
%%% We store each of the messages in a cache on disk. The cache is a simple 
%%% wrapper that allows us to look up either the direct key (a message's ID -- 
%%% either signed or unsigned) or a 'subpath'. We also store a cache of the linkages
%%% between messages as symlinks. In the backend, we store each message as either a 
%%% directory -- if it contains further data items inside -- or as a file, if it is 
%%% a simple value.
%%% 
%%% The file structure of the store is as follows:
%%%
%%% Root: ?DEFAULT_DATA_DIR
%%% Messages: ?DEFAULT_DATA_DIR/messages
%%% Computed outputs: ?DEFAULT_DATA_DIR/computed
%%% 
%%% Outputs by process: ?DEFAULT_DATA_DIR/computed/ProcessID
%%% Outputs by slot on process: ?DEFAULT_DATA_DIR/computed/ProcessID/slot/[n]
%%% Outputs by message on process: ?DEFAULT_DATA_DIR/computed/ProcessID/MessageID[/Subpath]
%%% 
%%% Outputs are stored as symlinks to the actual file or directory containing the message.
%%% Messages that are composite are represented as directories containing their childen
%%% (by ID and by subpath), as well as their base message stored at `.base`.
%%% 
%%% 

read(DirBase, ProcID, Msg) -> read(DirBase, ProcID, Msg, all).
read(DirBase, ProcID, Msg, AtomName) when is_atom(AtomName) ->
    read(DirBase, ProcID, Msg, atom_to_binary(AtomName));
read(DirBase, ProcID, Msg, Subpath) ->
    CachePath = filename:join([DirBase, ?COMPUTE_CACHE_DIR, ProcID, Msg]),
    case file:read_link(CachePath) of
        {ok, Target} ->
            case Subpath of
                all -> {ok, su_data:read_message(Target)};
                _ -> {ok, su_data:read_message(filename:join(Target, Subpath))}
            end;
        {error, _} ->
            unavailable
    end.

latest(DirBase, ProcID, Subpath) ->
    latest(DirBase, ProcID, Subpath, inf).

latest(DirBase, ProcID, Subpath, Limit) ->
    CPs = checkpoints(DirBase, ProcID),
    LatestSlot = lists:max(
        case Limit of
            inf -> CPs;
            _ -> lists:filter(fun(Slot) -> Slot < Limit end, CPs)
        end
    ),
    read(DirBase, ProcID, LatestSlot, Subpath).

checkpoints(DirBase, ProcID) ->
    SlotDir = filename:join([DirBase, ?COMPUTE_CACHE_DIR, ProcID, "slot"]),
    case file:list_dir(SlotDir) of
        {ok, Names} -> [ list_to_integer(Name) || Name <- Names ];
        {error, _} -> []
    end.

%% Write a full message to the cache.
write_output(DirBase, ProcID, MessageID, Slot, Item) ->
    CachePath = filename:join([DirBase, ?COMPUTE_CACHE_DIR, fmt_id(ProcID), fmt_id(MessageID)]),
    MessagePath = write(CachePath, Item),
    SlotPath = filename:join([DirBase, ?COMPUTE_CACHE_DIR, fmt_id(ProcID), "slot", integer_to_list(Slot)]),
    ln(MessagePath, SlotPath).

write_message(Message) ->
    write_message(?DEFAULT_DATA_DIR, Message).
write_message(CacheBase, Item) ->
    write(CacheBase ++ "/messages/", Item).

write(ItemBase, Item) when not is_record(Item, tx) -> write(ItemBase, ar_bundles:normalize(Item));
write(ItemBase, Item) when Item#tx.id == ?DEFAULT_ID -> write(ItemBase, ar_bundles:normalize(Item));
write(ItemBase, Item) ->
    ok = mkdir(ItemBase ++ "/" ++ fmt_id(Item)),
    case ar_bundles:type(Item) of
        binary ->
            file:write_file(ItemBase ++ "/" ++ fmt_id(ar_bundles:id(Item, signed)), ar_bundles:serialize(Item)),
            ln(
                ItemBase ++ "/" ++ fmt_id(ar_bundles:id(Item, signed)),
                ItemBase ++ "/" ++ fmt_id(ar_bundles:id(Item, unsigned))
            );
        map ->
            BasePath = ItemBase ++ "/" ++ ".base",
            ok = mkdir(BasePath),
            ok = file:write_file(BasePath, ar_bundles:serialize(Item#tx{ data = <<>>})),
            maps:map(fun(Key, Subitem) ->
                ok = mkdir(Subpath = ItemBase ++ "/" ++ Key),
                SubmessagePath = write(Subpath, Subitem),
                ln(SubmessagePath, ItemBase ++ "/" ++ fmt_id(Subitem))
            end, Item#tx.data);
        list ->
            BasePath = ItemBase ++ "/" ++ ".base",
            ok = mkdir(BasePath),
            ok = file:write_file(BasePath, ar_bundles:serialize(Item#tx{ data = <<>>})),
            lists:map(fun(Subitem) ->
                write(ItemBase ++ "/" ++ fmt_id(Subitem), Subitem)
            end, Item)
    end,
    case Item#tx.signature of
        ?DEFAULT_SIG -> do_nothing;
        _ ->
            ln(
                ItemBase ++ "/" ++ fmt_id(ar_bundles:id(Item, signed)),
                ItemBase ++ "/" ++ fmt_id(ar_bundles:id(Item, unsigned))
            )
    end.

fmt_id(ID) when is_record(ID, tx) -> fmt_id(ar_bundles:id(ID));
fmt_id(ID) ->
    binary_to_list(ar_util:encode(ID)).

mkdir(Path) ->
    ?c({mkdir, Path}),
    ok = filelib:ensure_dir(Path).

ln(Target, Link) ->
    ?c({symlink, Target, Link}),
    file:make_symlink(Target, Link).
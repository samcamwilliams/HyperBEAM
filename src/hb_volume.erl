-module(hb_volume).
-moduledoc """
Module for managing physical disks and volumes, providing operations
for partitioning, formatting, mounting, and managing encrypted volumes.
""".
-export([list_partitions/0, create_partition/2]).
-export([format_disk/2, mount_disk/4, change_node_store/2]).
-export([check_for_device/1, check_lmdb_exists/1]).
-export([copy_lmdb_store/2]).
-include("include/hb.hrl").

-doc """
List available partitions in the system.
@returns {ok, Map} where Map contains the partition information,
         or {error, Reason} if the operation fails.
""".
-spec list_partitions() -> {ok, map()} | {error, binary()}.
list_partitions() ->
    ?event(disk, {list_partitions, start}),
    
    % Get the partition information using fdisk -l
    case os:cmd("sudo fdisk -l") of
        [] ->
            % Empty output indicates an error
            Reason = <<"Failed to list partitions: no output">>,
            ?event(disk, {list_partitions, error, Reason}),
            {error, Reason};
        Output ->
            ?event(disk, {list_partitions, complete}),
            
            % Split output into lines
            Lines = string:split(Output, "\n", all),
            
            % Process the output to group information by disk
            {_, DiskData} = lists:foldl(
                fun process_disk_line/2,
                {undefined, []},
                Lines
            ),
            
            % Process each disk's data to extract all information
            DiskObjects = lists:filtermap(
                fun(DiskEntry) ->
                    Device = maps:get(<<"device">>, DiskEntry),
                    DiskLines = lists:reverse(maps:get(<<"data">>, DiskEntry)),
                    DiskInfo = parse_disk_info(Device, DiskLines),
                    {true, DiskInfo}
                end,
                DiskData
            ),
            
            % Return the partition information
            {ok, #{
                <<"status">> => 200,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => hb_json:encode(#{<<"disks">> => DiskObjects})
            }}
    end.

%%% Helper functions for list_partitions

% Process a line of fdisk output to group by disk
process_disk_line(Line, {CurrentDisk, Acc}) ->
    % Match for a new disk entry
    DiskPattern = "^Disk (/dev/(?!ram)\\S+):",
    case re:run(Line, DiskPattern, [{capture, [1], binary}]) of
        {match, [Device]} ->
            % Start a new disk entry
            NewDisk = #{
                <<"device">> => Device,
                <<"data">> => [Line]
            },
            {NewDisk, [NewDisk | Acc]};
        _ when CurrentDisk =:= undefined ->
            % Not a disk line and no current disk
            {undefined, Acc};
        _ ->
            % Add line to current disk's data
            CurrentData = maps:get(<<"data">>, CurrentDisk),
            UpdatedDisk = CurrentDisk#{
                <<"data">> => [Line | CurrentData]
            },
            % Update the list with the modified disk entry
            UpdatedAcc = [UpdatedDisk | lists:delete(CurrentDisk, Acc)],
            {UpdatedDisk, UpdatedAcc}
    end.

% Parse detailed disk information from fdisk output lines
parse_disk_info(Device, Lines) ->
    % Initialize with device ID
    DiskInfo = #{<<"device">> => Device},
    
    % Process each line to extract information
    lists:foldl(
        fun parse_disk_line/2,
        DiskInfo,
        Lines
    ).

% Parse a single line of disk information
parse_disk_line(Line, Info) ->
    % Extract disk size and bytes
    SizePattern = "^Disk .+: ([0-9.]+ [KMGT]iB), ([0-9]+) bytes, ([0-9]+) sectors",
    case re:run(Line, SizePattern, [{capture, [1, 2, 3], binary}]) of
        {match, [Size, Bytes, Sectors]} ->
            Info#{
                <<"size">> => Size,
                <<"bytes">> => binary_to_integer(Bytes),
                <<"sectors">> => binary_to_integer(Sectors)
            };
        _ -> 
            parse_disk_model_line(Line, Info)
    end.

% Parse disk model information
parse_disk_model_line(Line, Info) ->
    % Extract disk model
    ModelPattern = "^Disk model: (.+)\\s*$",
    case re:run(Line, ModelPattern, [{capture, [1], binary}]) of
        {match, [Model]} ->
            Info#{<<"model">> => string:trim(Model)};
        _ ->
            parse_disk_units_line(Line, Info)
    end.

% Parse disk units information
parse_disk_units_line(Line, Info) ->
    % Extract units information
    UnitsPattern = "^Units: (.+)$",
    case re:run(Line, UnitsPattern, [{capture, [1], binary}]) of
        {match, [Units]} ->
            Info#{<<"units">> => Units};
        _ ->
            parse_sector_size_line(Line, Info)
    end.

% Parse sector size information
parse_sector_size_line(Line, Info) ->
    % Extract sector size
    SectorPattern = "^Sector size \\(logical/physical\\): ([^/]+)/(.+)$",
    case re:run(Line, SectorPattern, [{capture, [1, 2], binary}]) of
        {match, [LogicalSize, PhysicalSize]} ->
            Info#{
                <<"sector_size">> => #{
                    <<"logical">> => string:trim(LogicalSize),
                    <<"physical">> => string:trim(PhysicalSize)
                }
            };
        _ ->
            parse_io_size_line(Line, Info)
    end.

% Parse I/O size information
parse_io_size_line(Line, Info) ->
    % Extract I/O size
    IOPattern = "^I/O size \\(minimum/optimal\\): ([^/]+)/(.+)$",
    case re:run(Line, IOPattern, [{capture, [1, 2], binary}]) of
        {match, [MinSize, OptSize]} ->
            Info#{
                <<"io_size">> => #{
                    <<"minimum">> => string:trim(MinSize),
                    <<"optimal">> => string:trim(OptSize)
                }
            };
        _ -> 
            Info
    end.

-doc """
Create a partition on a disk device.
@param Device The path to the device, e.g. "/dev/sdb".
@param PartType The partition type to create, defaults to "ext4".
@returns {ok, Map} on success where Map includes status and partition information,
         or {error, Reason} if the operation fails.
""".
-spec create_partition(Device :: binary(), PartType :: binary()) ->
    {ok, map()} | {error, binary()}.
create_partition(undefined, _PartType) ->
    {error, <<"Device path not specified">>};
create_partition(Device, PartType) ->
    ?event(disk, {create_partition, start}),
    ?event(disk, {create_partition, device, Device}),
    ?event(disk, {create_partition, part_type, PartType}),
    
    % Create a GPT partition table
    DeviceStr = binary_to_list(Device),
    MklabelCmd = "sudo parted " ++ DeviceStr ++ " mklabel gpt",
    MklabelResult = os:cmd(MklabelCmd),
    
    % Check if creating the partition table succeeded
    case string:find(MklabelResult, "Error") of
        nomatch ->
            create_actual_partition(Device, PartType);
        _ ->
            ?event(disk, {create_partition, error, list_to_binary(MklabelResult)}),
            {error, list_to_binary(MklabelResult)}
    end.

% Create the actual partition after making the GPT label
create_actual_partition(Device, PartType) ->
    DeviceStr = binary_to_list(Device),
    PartTypeStr = binary_to_list(PartType),
    
    % Build the parted command to create the partition
    MkpartCmd = "sudo parted -a optimal " ++ DeviceStr ++ 
               " mkpart primary " ++ PartTypeStr ++ " 0% 100%",
    MkpartResult = os:cmd(MkpartCmd),
    
    % Check if creating the partition succeeded
    case string:find(MkpartResult, "Error") of
        nomatch ->
            get_partition_info(Device);
        _ ->
            ?event(disk, {create_partition, error, list_to_binary(MkpartResult)}),
            {error, list_to_binary(MkpartResult)}
    end.

% Get the partition information after creating a partition
get_partition_info(Device) ->
    DeviceStr = binary_to_list(Device),
    
    % Print partition information
    PrintCmd = "sudo parted " ++ DeviceStr ++ " print",
    PartitionInfo = os:cmd(PrintCmd),
    
    ?event(disk, {create_partition, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Partition created successfully.">>,
        <<"device_path">> => Device,
        <<"partition_info">> => list_to_binary(PartitionInfo)
    }}.

-doc """
Format a disk or partition with LUKS encryption.
@param Partition The path to the partition, e.g. "/dev/sdc1".
@param EncKey The encryption key to use for LUKS.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec format_disk(Partition :: binary(), EncKey :: binary()) ->
    {ok, map()} | {error, binary()}.
format_disk(undefined, _EncKey) ->
    {error, <<"Partition path not specified">>};
format_disk(_Partition, undefined) ->
    {error, <<"Encryption key not specified">>};
format_disk(Partition, EncKey) ->
    ?event(disk, {format, start}),
    ?event(disk, {format, partition, Partition}),
    
    % Ensure tmp directory exists
    os:cmd("sudo mkdir -p /root/tmp"),
    KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),
    file:write_file(KeyFile, EncKey, [raw]),
    
    % Format with LUKS
    PartitionStr = binary_to_list(Partition),
    FormatCmd = "sudo cryptsetup luksFormat --batch-mode --key-file " ++ 
                KeyFile ++ " " ++ PartitionStr,
    FormatResult = os:cmd(FormatCmd),
    
    % Remove the temporary key file 
    os:cmd("sudo shred -u " ++ KeyFile),
    
    % Check if the command succeeded
    case string:find(FormatResult, "failed") of
        nomatch ->
            ?event(disk, {format, complete}),
            {ok, #{
                <<"status">> => 200,
                <<"message">> => 
                    <<"Partition formatted with LUKS encryption successfully.">>
            }};
        _ ->
            ?event(disk, {format, error, list_to_binary(FormatResult)}),
            {error, list_to_binary(FormatResult)}
    end.

-doc """
Mount a LUKS-encrypted disk.
@param Partition The path to the partition, e.g. "/dev/sdc1".
@param EncKey The encryption key for LUKS.
@param MountPoint The directory where the disk should be mounted.
@param VolumeName The name to use for the decrypted LUKS volume.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec mount_disk(
    Partition :: binary(),
    EncKey :: binary(),
    MountPoint :: binary(),
    VolumeName :: binary()
) -> {ok, map()} | {error, binary()}.
mount_disk(undefined, _EncKey, _MountPoint, _VolumeName) ->
    {error, <<"Partition path not specified">>};
mount_disk(_Partition, undefined, _MountPoint, _VolumeName) ->
    {error, <<"Encryption key not specified">>};
mount_disk(_Partition, _EncKey, undefined, _VolumeName) ->
    {error, <<"Mount point not specified">>};
mount_disk(Partition, EncKey, MountPoint, VolumeName) ->
    ?event(disk, {mount, start}),
    ?event(disk, {mount, partition, Partition}),
    ?event(disk, {mount, mount_point, MountPoint}),
    ?event(disk, {mount, volume_name, VolumeName}),
    
    % Ensure tmp directory exists
    os:cmd("sudo mkdir -p /root/tmp"),
    KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),
    file:write_file(KeyFile, EncKey, [raw]),
    
    % Open the LUKS volume
    PartitionStr = binary_to_list(Partition),
    VolumeNameStr = binary_to_list(VolumeName),
    OpenCmd = "sudo cryptsetup luksOpen --key-file " ++ KeyFile ++ " " ++ 
               PartitionStr ++ " " ++ VolumeNameStr,
    OpenResult = os:cmd(OpenCmd),
    
    % Remove the temporary key file
    os:cmd("sudo shred -u " ++ KeyFile),
    
    % Check if opening the LUKS volume succeeded
    case string:find(OpenResult, "failed") of
        nomatch ->
            mount_opened_volume(Partition, MountPoint, VolumeName);
        _ ->
            ?event(disk, {mount, error, list_to_binary(OpenResult)}),
            {error, list_to_binary(OpenResult)}
    end.

% Mount an already opened LUKS volume
mount_opened_volume(Partition, MountPoint, VolumeName) ->
    % Create mount point if it doesn't exist
    MountPointStr = binary_to_list(MountPoint),
    os:cmd("sudo mkdir -p " ++ MountPointStr),
    
    % Mount the unlocked LUKS volume
    VolumeNameStr = binary_to_list(VolumeName),
    MountCmd = "sudo mount /dev/mapper/" ++ VolumeNameStr ++ " " ++ 
                MountPointStr,
    MountResult = os:cmd(MountCmd),
    
    % Check if mounting succeeded
    case string:find(MountResult, "failed") of
        nomatch ->
            create_mount_info(Partition, MountPoint, VolumeName);
        _ ->
            % Close the LUKS volume if mounting failed
            VolumeNameStr = binary_to_list(VolumeName),
            os:cmd("sudo cryptsetup luksClose " ++ VolumeNameStr),
            ?event(disk, {mount, error, list_to_binary(MountResult)}),
            {error, list_to_binary(MountResult)}
    end.

% Create mount info response
create_mount_info(Partition, MountPoint, VolumeName) ->
    ?event(disk, {mount, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Encrypted partition mounted successfully.">>,
        <<"mount_point">> => MountPoint,
        <<"mount_info">> => #{
            partition => Partition,
            mount_point => MountPoint,
            volume_name => VolumeName
        }
    }}.

-doc """
Change the node's data store location to the mounted encrypted disk.
@param StorePath The new path for the store directory.
@param CurrentStore The current store configuration.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec change_node_store(StorePath :: binary(), CurrentStore :: list()) ->
    {ok, map()} | {error, binary()}.
change_node_store(undefined, _CurrentStore) ->
    {error, <<"Store path not specified">>};
change_node_store(StorePath, CurrentStore) ->
    ?event(disk, {change_store, start}),
    ?event(disk, {change_store, store_path, StorePath}),
    
    % Create the store directory if it doesn't exist
    StorePathStr = binary_to_list(StorePath),
    os:cmd("sudo mkdir -p " ++ StorePathStr),
    
    % Update the store configuration with the new path
    NewStore = update_store_config(CurrentStore, StorePath),
    
    % Return the result
    ?event(disk, {change_store, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Node store updated to use encrypted disk.">>,
        <<"store_path">> => StorePath,
        <<"store">> => NewStore
    }}.

%%% Helper functions

% Update the store configuration with a new base path
-spec update_store_config(StoreConfig :: term(), NewPath :: binary()) -> term().
update_store_config(StoreConfig, NewPath) when is_list(StoreConfig) ->
    % For a list, update each element
    [update_store_config(Item, NewPath) || Item <- StoreConfig];
update_store_config(#{<<"store-module">> := Module} = StoreConfig, NewPath) 
  when is_map(StoreConfig) ->
    % Handle various store module types differently
    case Module of
        hb_store_fs ->
            % For filesystem store, prefix the existing path with the new path
            ExistingPath = maps:get(<<"name">>, StoreConfig, <<"">>),
            NewName = <<NewPath/binary, "/", ExistingPath/binary>>,
            ?event(debug_volume, {fs, StoreConfig, NewPath, NewName}),
            StoreConfig#{<<"name">> => NewName};
        hb_store_lmdb ->
            % For LMDB store, handle migration to new encrypted mount location
            ExistingPath = maps:get(<<"name">>, StoreConfig, <<"">>),
            NewName = <<NewPath/binary, "/", ExistingPath/binary>>,
            ?event(debug_volume, 
                {migrate_start, ExistingPath, NewName}
            ),
            % Step 1: Stop current LMDB store to ensure clean migration
            ?event(debug_volume, {stopping_current_store, StoreConfig}),
            try 
                hb_store_lmdb:stop(StoreConfig)
            catch 
                error:StopReason ->
                    ?event(debug_volume, {stop_error, StopReason})
            end,
            % Step 2: Check if new location already contains an LMDB store
            NewLocationExists = check_lmdb_exists(NewName),
            ?event(debug_volume, 
                {new_location_exists, NewLocationExists}
            ),
            case NewLocationExists of
                true ->
                    % LMDB store already exists at new location, just use it
                    ?event(debug_volume, 
                        {using_existing_store, NewName}
                    ),
                    StoreConfig#{<<"name">> => NewName};
                false ->
                    % No LMDB store at new location, copy existing data
                    ?event(debug_volume, 
                        {copying_store, ExistingPath, NewName}
                    ),
                    case copy_lmdb_store(ExistingPath, NewName) of
                        ok ->
                            ?event(debug_volume, 
                                {copy_success, NewName}
                            ),
                            StoreConfig#{<<"name">> => NewName};
                        {error, Reason} ->
                            ?event(debug_volume, 
                                {copy_error, Reason}
                            ),
                            % Fall back to original path if copy fails
                            ?event(debug_volume, 
                                {fallback_to_original, ExistingPath}
                            ),
                            StoreConfig
                    end
            end,
            % Step 3: Start the new LMDB store
            ?event(debug_volume, {starting_new_store, NewName}),
            hb_store_lmdb:start(StoreConfig#{<<"name">> => NewName});
        hb_store_rocksdb ->
            StoreConfig;
        hb_store_gateway ->
            % For gateway store, recursively update nested store configurations
            NestedStore = maps:get(<<"store">>, StoreConfig, []),
            StoreConfig#{
                <<"store">> => update_store_config(NestedStore, NewPath)
            };
        _ ->
            % For any other store type, update the prefix
            % StoreConfig#{<<"name">> => NewPath}
            ?event(debug_volume, {other, StoreConfig, NewPath}),
            StoreConfig
    end;
update_store_config({Type, _OldPath, Opts}, NewPath) ->
    % For tuple format with options
    {Type, NewPath, Opts};
update_store_config({Type, _OldPath}, NewPath) ->
    % For tuple format without options
    {Type, NewPath};
update_store_config(StoreConfig, _NewPath) ->
    % Return unchanged for any other format
    StoreConfig.

-doc """
Check if a device exists on the system.
@param Device The path to the device to check (binary).
@returns true if the device exists, false otherwise.
""".
-spec check_for_device(Device :: binary()) -> boolean().
check_for_device(Device) ->
    Command = io_lib:format(
        "ls -l ~s 2>/dev/null || echo 'not_found'", 
        [binary_to_list(Device)]
    ),
	?event(disk, {check_for_device, command, Command}),
    Result = os:cmd(Command),
    string:find(Result, "not_found") =:= nomatch.

%% @doc Check if an LMDB database exists at the given path.
%%
%% LMDB databases consist of at least a data.mdb file and optionally a lock.mdb file.
%% This function checks for the presence of these files to determine if a valid
%% LMDB database exists at the specified location.
%%
%% @param Path Binary path to check for LMDB database
%% @returns true if LMDB database exists, false otherwise
-spec check_lmdb_exists(Path :: binary()) -> boolean().
check_lmdb_exists(Path) ->
    PathStr = binary_to_list(Path),
    DataFile = PathStr ++ "/data.mdb",    
    % Check if the directory exists first
    case filelib:is_dir(PathStr) of
        false -> 
            false;
        true ->
            % Check if data.mdb exists (required for LMDB)
            % lock.mdb is optional and might not exist when DB is not in use
            filelib:is_regular(DataFile)
    end.

%% @doc Copy an LMDB database from source to destination.
%%
%% This function performs a safe copy of an LMDB database by copying all
%% the database files (data.mdb, lock.mdb if present) from the source
%% directory to the destination directory. It ensures the destination
%% directory exists before copying.
%%
%% @param SourcePath Binary path to source LMDB database
%% @param DestPath Binary path to destination location
%% @returns ok on success, {error, Reason} on failure
-spec copy_lmdb_store(
    SourcePath :: binary(), 
    DestPath :: binary()
) -> ok | {error, term()}.
copy_lmdb_store(SourcePath, DestPath) ->
    SourceStr = binary_to_list(SourcePath),
    DestStr = binary_to_list(DestPath),
    ?event(debug_volume, {start, SourceStr, DestStr}),
    % Check if source LMDB database exists
    case check_lmdb_exists(SourcePath) of
        false ->
            ?event(debug_volume, {source_not_found, SourceStr}),
            {error, <<"Source LMDB database not found">>};
        true ->
            % Ensure destination directory exists
            ok = filelib:ensure_dir(DestStr ++ "/"),
            
            % Copy the LMDB database files
            case copy_lmdb_files(SourceStr, DestStr) of
                ok ->
                    ?event(debug_volume, {success}),
                    ok;
                {error, Reason} ->
                    ?event(debug_volume, {error, Reason}),
                    {error, Reason}
            end
    end.

%% @doc Helper function to copy LMDB database files.
%%
%% This function copies the actual LMDB files (data.mdb and lock.mdb if present)
%% from source to destination using system commands for reliability.
%%
%% @param SourceDir String path to source directory
%% @param DestDir String path to destination directory
%% @returns ok on success, {error, Reason} on failure
-spec copy_lmdb_files(
    SourceDir :: string(), 
    DestDir :: string()
) -> ok | {error, term()}.
copy_lmdb_files(SourceDir, DestDir) ->
    % Use rsync for reliable copying of LMDB files
    % --archive preserves permissions and timestamps
    % --sparse handles sparse files efficiently
    CopyCommand = io_lib:format(
        "rsync -av --sparse '~s/' '~s/'", 
        [SourceDir, DestDir]
    ),
    ?event(debug_volume, {copy_lmdb_files, CopyCommand}),
    case os:cmd(CopyCommand) of
        Result ->
            % Check if rsync completed successfully by looking for error indicators
            case {
                    string:find(Result, "rsync error"), 
                    string:find(Result, "failed")
                } of
                {nomatch, nomatch} ->
                    ok;
                _ ->
                    {error, list_to_binary(Result)}
            end
    end. 
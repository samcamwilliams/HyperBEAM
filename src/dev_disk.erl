-module(dev_disk).
-moduledoc """
Device module for disk operations. Provides functions to manage physical disks,
including listing partitions, creating partitions, formatting with LUKS encryption,
mounting encrypted volumes, and configuring storage locations.
""".
-export([list_partitions/3, create_partition/3]).
-export([format_disk/3, mount_disk/3, change_node_store/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-doc """
List available partitions in the system.
This function uses the system command 'fdisk -l' to get a list of all partitions
and returns the formatted output.
@param M1 Ignored parameter.
@param M2 Ignored parameter.
@param Opts A map of configuration options.
@returns {ok, Map} where Map contains the partition information.
""".
-spec list_partitions(M1 :: term(), M2 :: term(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
list_partitions(_M1, _M2, _Opts) ->
    ?event(disk, {list_partitions, start}),
    case os:cmd("sudo fdisk -l") of
        {error, Reason} ->
            ?event(disk, {list_partitions, error, Reason}),
            Error = io_lib:format("Failed to list partitions: ~p", [Reason]),
            {error, list_to_binary(Error)};
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
            
            % Encode the response as JSON
            EncodedJSON = hb_json:encode(#{<<"disks">> => DiskObjects}),
            
            {ok, #{
                <<"status">> => 200,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => EncodedJSON
            }}
    end.

-doc """
Process a line of fdisk output to group by disk.
Helper function for list_partitions/3.
@param Line A line from fdisk output.
@param {CurrentDisk, Acc} A tuple containing the current disk being processed
       and the accumulator of processed disks.
@returns {NewCurrentDisk, NewAcc} The updated current disk and accumulator.
""".
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

-doc """
Parse detailed disk information from fdisk output lines.
@param Device The device path.
@param Lines The lines of fdisk output for the device.
@returns A map containing parsed disk information.
""".
parse_disk_info(Device, Lines) ->
    % Initialize with device ID
    DiskInfo = #{<<"device">> => Device},
    
    % Process each line to extract information
    lists:foldl(
        fun parse_disk_line/2,
        DiskInfo,
        Lines
    ).

-doc """
Parse a single line of disk information.
Helper function for parse_disk_info/2.
@param Line A line from fdisk output.
@param Info The current disk info map.
@returns The updated disk info map.
""".
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

-doc """
Parse disk model information.
Helper function for parse_disk_line/2.
@param Line A line from fdisk output.
@param Info The current disk info map.
@returns The updated disk info map.
""".
parse_disk_model_line(Line, Info) ->
    % Extract disk model
    ModelPattern = "^Disk model: (.+)\\s*$",
    case re:run(Line, ModelPattern, [{capture, [1], binary}]) of
        {match, [Model]} ->
            Info#{<<"model">> => string:trim(Model)};
        _ ->
            parse_disk_units_line(Line, Info)
    end.

-doc """
Parse disk units information.
Helper function for parse_disk_model_line/2.
@param Line A line from fdisk output.
@param Info The current disk info map.
@returns The updated disk info map.
""".
parse_disk_units_line(Line, Info) ->
    % Extract units information
    UnitsPattern = "^Units: (.+)$",
    case re:run(Line, UnitsPattern, [{capture, [1], binary}]) of
        {match, [Units]} ->
            Info#{<<"units">> => Units};
        _ ->
            parse_sector_size_line(Line, Info)
    end.

-doc """
Parse sector size information.
Helper function for parse_disk_units_line/2.
@param Line A line from fdisk output.
@param Info The current disk info map.
@returns The updated disk info map.
""".
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

-doc """
Parse I/O size information.
Helper function for parse_sector_size_line/2.
@param Line A line from fdisk output.
@param Info The current disk info map.
@returns The updated disk info map.
""".
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
This function creates a GPT partition table on the specified device and
creates a single partition that occupies the entire disk.
@param M1 A map containing the device path.
@param M2 A map containing optional parameters like partition type.
@param Opts A map of configuration options.
@returns {ok, Map} on success where Map includes status and partition information.
""".
-spec create_partition(M1 :: map(), M2 :: map(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
create_partition(M1, M2, Opts) ->
    ?event(disk, {create_partition, start}),
    
    % Get the device path from the request
    Device = hb_ao:get(<<"device_path">>, M1, undefined, Opts),
    
    % Get optional parameters
    PartType = hb_ao:get(<<"partition_type">>, M2, <<"ext4">>, Opts),
    
    % Validate device path
    case Device of
        undefined ->
            ?event(disk, {create_partition, error, <<"no device specified">>}),
            {error, <<"No device path specified.">>};
        _ ->
            create_partition_with_device(Device, PartType, Opts)
    end.

-doc """
Create a partition with validated device path.
Helper function for create_partition/3.
@param Device The device path.
@param PartType The partition type.
@param Opts A map of configuration options.
@returns {ok, Map} on success or {error, Reason} on failure.
""".
create_partition_with_device(Device, PartType, Opts) ->
    % Create a GPT partition table
    DeviceStr = binary_to_list(Device),
    MklabelCmd = "sudo parted " ++ DeviceStr ++ " mklabel gpt",
    MklabelResult = os:cmd(MklabelCmd),
    
    % Check if creating the partition table succeeded
    case string:find(MklabelResult, "Error") of
        nomatch ->
            create_actual_partition(Device, PartType, Opts);
        _ ->
            ErrorEvent = {create_partition, error, list_to_binary(MklabelResult)},
            ?event(disk, ErrorEvent),
            {error, list_to_binary(MklabelResult)}
    end.

-doc """
Create the actual partition after making the GPT label.
Helper function for create_partition_with_device/3.
@param Device The device path.
@param PartType The partition type.
@param Opts A map of configuration options.
@returns {ok, Map} on success or {error, Reason} on failure.
""".
create_actual_partition(Device, PartType, Opts) ->
    DeviceStr = binary_to_list(Device),
    PartTypeStr = binary_to_list(PartType),
    
    % Build the parted command to create the partition
    MkpartCmd = "sudo parted -a optimal " ++ DeviceStr ++ 
               " mkpart primary " ++ PartTypeStr ++ " 0% 100%",
    MkpartResult = os:cmd(MkpartCmd),
    
    % Check if creating the partition succeeded
    case string:find(MkpartResult, "Error") of
        nomatch ->
            get_partition_info(Device, Opts);
        _ ->
            ErrorEvent = {create_partition, error, list_to_binary(MkpartResult)},
            ?event(disk, ErrorEvent),
            {error, list_to_binary(MkpartResult)}
    end.

-doc """
Get the partition information after creating a partition.
Helper function for create_actual_partition/3.
@param Device The device path.
@param Opts A map of configuration options.
@returns {ok, Map} with partition information.
""".
get_partition_info(Device, _Opts) ->
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
This function formats the specified device with LUKS encryption using the
provided AES key for encryption.
@param M1 A map containing the device path.
@param M2 A map containing encryption options.
@param Opts A map of configuration options, must include the AES key.
@returns {ok, Map} on success where Map includes the status and confirmation message.
""".
-spec format_disk(M1 :: map(), M2 :: map(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
format_disk(M1, M2, Opts) ->
    ?event(disk, {format, start}),
    
    % Get the device path from the request
    Device = hb_ao:get(<<"device_path">>, M1, undefined, Opts),
    
    % Get the encryption key (use the green zone AES key if not specified)
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    EncKey = hb_ao:get(<<"encryption_key">>, M2, GreenZoneAES, Opts),
    
    % Validate device path and encryption key
    case {Device, EncKey} of
        {undefined, _} ->
            ?event(disk, {format, error, <<"no device specified">>}),
            {error, <<"No device path specified.">>};
        {_, undefined} ->
            ?event(disk, {format, error, <<"no encryption key">>}),
            ErrorMsg = <<"No encryption key available for LUKS formatting.">>,
            {error, ErrorMsg};
        _ ->
            format_disk_with_key(Device, EncKey, Opts)
    end.

-doc """
Format disk with validated device and encryption key.
Helper function for format_disk/3.
@param Device The device path.
@param EncKey The encryption key.
@param Opts A map of configuration options.
@returns {ok, Map} on success or {error, Reason} on failure.
""".
format_disk_with_key(Device, EncKey, _Opts) ->
    % Ensure tmp directory exists
    os:cmd("sudo mkdir -p /root/tmp"),
    KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),
    file:write_file(KeyFile, EncKey, [raw]),
    
    % Format with LUKS
    DeviceStr = binary_to_list(Device),
    FormatCmd = "sudo cryptsetup luksFormat --batch-mode --key-file " ++ 
                KeyFile ++ " " ++ DeviceStr,
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
                    <<"Disk formatted with LUKS encryption successfully.">>
            }};
        _ ->
            ErrorEvent = {format, error, list_to_binary(FormatResult)},
            ?event(disk, ErrorEvent),
            {error, list_to_binary(FormatResult)}
    end.

-doc """
Mount a LUKS-encrypted disk.
This function opens a LUKS-encrypted partition using the provided AES key,
mounts it to the specified mount point, and updates the configuration with
the mount information.
@param M1 A map containing the device path and mount point.
@param M2 A map containing encryption options.
@param Opts A map of configuration options, must include the AES key.
@returns {ok, Map} on success where Map includes the status and confirmation message.
""".
-spec mount_disk(M1 :: map(), M2 :: map(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
mount_disk(M1, M2, Opts) ->
    ?event(disk, {mount, start}),
    
    % Get parameters from the request
    Device = hb_ao:get(<<"device_path">>, M1, undefined, Opts),
    MountPoint = hb_ao:get(
        <<"mount_point">>, 
        M1, 
        <<"/root/mnt/encrypted_data">>, 
        Opts
    ),
    VolumeName = hb_ao:get(
        <<"volume_name">>, 
        M1,
        <<"primary_encrypted_volume">>, 
        Opts
    ),
    
    % Get the encryption key (use the green zone AES key if not specified)
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    EncKey = hb_ao:get(<<"encryption_key">>, M2, GreenZoneAES, Opts),
    
    % Validate device path and encryption key
    case {Device, EncKey} of
        {undefined, _} ->
            ?event(disk, {mount, error, <<"no device specified">>}),
            {error, <<"No device path specified.">>};
        {_, undefined} ->
            ?event(disk, {mount, error, <<"no encryption key">>}),
            ErrorMsg = <<"No encryption key available to open LUKS volume.">>,
            {error, ErrorMsg};
        _ ->
            mount_disk_with_key(Device, EncKey, MountPoint, VolumeName, Opts)
    end.

-doc """
Mount disk with validated parameters.
Helper function for mount_disk/3.
@param Device The device path.
@param EncKey The encryption key.
@param MountPoint The mount point.
@param VolumeName The name for the LUKS volume.
@param Opts A map of configuration options.
@returns {ok, Map} on success or {error, Reason} on failure.
""".
mount_disk_with_key(Device, EncKey, MountPoint, VolumeName, Opts) ->
    % Ensure tmp directory exists
    os:cmd("sudo mkdir -p /root/tmp"),
    KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),
    file:write_file(KeyFile, EncKey, [raw]),
    
    % Open the LUKS volume
    DeviceStr = binary_to_list(Device),
    VolumeNameStr = binary_to_list(VolumeName),
    OpenCmd = "sudo cryptsetup luksOpen --key-file " ++ KeyFile ++ " " ++ 
               DeviceStr ++ " " ++ VolumeNameStr,
    OpenResult = os:cmd(OpenCmd),
    
    % Remove the temporary key file
    os:cmd("sudo shred -u " ++ KeyFile),
    
    % Check if opening the LUKS volume succeeded
    case string:find(OpenResult, "failed") of
        nomatch ->
            mount_opened_volume(Device, MountPoint, VolumeName, Opts);
        _ ->
            ErrorEvent = {mount, error, list_to_binary(OpenResult)},
            ?event(disk, ErrorEvent),
            {error, list_to_binary(OpenResult)}
    end.

-doc """
Mount an already opened LUKS volume.
Helper function for mount_disk_with_key/5.
@param Device The device path.
@param MountPoint The mount point.
@param VolumeName The name of the LUKS volume.
@param Opts A map of configuration options.
@returns {ok, Map} on success or {error, Reason} on failure.
""".
mount_opened_volume(Device, MountPoint, VolumeName, Opts) ->
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
            save_mount_info(Device, MountPoint, VolumeName, Opts);
        _ ->
            % Close the LUKS volume if mounting failed
            VolumeNameStr = binary_to_list(VolumeName),
            os:cmd("sudo cryptsetup luksClose " ++ VolumeNameStr),
            ErrorEvent = {mount, error, list_to_binary(MountResult)},
            ?event(disk, ErrorEvent),
            {error, list_to_binary(MountResult)}
    end.

-doc """
Save mounted disk info to configuration.
Helper function for mount_opened_volume/4.
@param Device The device path.
@param MountPoint The mount point.
@param VolumeName The name of the LUKS volume.
@param Opts A map of configuration options.
@returns {ok, Map} with mount information.
""".
save_mount_info(Device, MountPoint, VolumeName, Opts) ->
    % Save the mount information in the configuration
    ok = hb_http_server:set_opts(Opts#{
        priv_encrypted_disk => #{
            device => Device,
            mount_point => MountPoint,
            volume_name => VolumeName
        }
    }),
    ?event(disk, {mount, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Encrypted disk mounted successfully.">>,
        <<"mount_point">> => MountPoint
    }}.

-doc """
Change the node's data store location to the mounted encrypted disk.
This function updates the node's configuration to use the mounted encrypted
disk as its data store.
@param M1 A map containing the new store path.
@param M2 Ignored parameter.
@param Opts A map of configuration options.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec change_node_store(M1 :: map(), M2 :: term(), Opts :: map()) ->
    {ok, map()} | {error, binary()}.
change_node_store(M1, _M2, Opts) ->
    ?event(disk, {change_store, start}),
    
    % Get the mounted disk information
    EncryptedDisk = hb_opts:get(priv_encrypted_disk, undefined, Opts),
    
    % Get the new store path (default to the mount point + '/store')
    NewStorePath = case EncryptedDisk of
        undefined ->
            hb_ao:get(<<"store_path">>, M1, undefined, Opts);
        #{mount_point := MountPoint} ->
            hb_ao:get(
                <<"store_path">>, 
                M1, 
                <<MountPoint/binary, "/store">>, 
                Opts
            )
    end,
    
    % Validate the store path
    case NewStorePath of
        undefined ->
            ?event(disk, {change_store, error, <<"no store path">>}),
            ErrorMsg = <<"No encrypted disk mounted or store path specified.">>,
            {error, ErrorMsg};
        _ ->
            change_store_with_path(NewStorePath, Opts)
    end.

-doc """
Change store with a validated path.
Helper function for change_node_store/3.
@param NewStorePath The new store path.
@param Opts A map of configuration options.
@returns {ok, Map} on success with store path information.
""".
change_store_with_path(NewStorePath, Opts) ->
    % Create the store directory if it doesn't exist
    StorePathStr = binary_to_list(NewStorePath),
    os:cmd("sudo mkdir -p " ++ StorePathStr),
    
    % Get the current store configuration
    CurrentStore = hb_opts:get(store, [], Opts),
    
    % Update the store configuration with the new path
    NewStore = update_store_config(CurrentStore, NewStorePath),
    
    % Update the node's configuration with the new store
    ok = hb_http_server:set_opts(Opts#{
        store => NewStore
    }),
    
    ?event(disk, {change_store, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Node store updated to use encrypted disk.">>,
        <<"store_path">> => NewStorePath
    }}.

-doc """
Update the store configuration with a new base path.
This is a helper function for change_node_store.
@param StoreConfig The current store configuration.
@param NewPath The new base path for the store.
@returns Updated store configuration.
""".
-spec update_store_config(StoreConfig :: term(), NewPath :: binary()) -> term().
update_store_config(StoreConfig, NewPath) when is_list(StoreConfig) ->
    % For a list, update each element
    [update_store_config(Item, NewPath) || Item <- StoreConfig];
update_store_config(#{<<"store-module">> := Module} = StoreConfig, NewPath) 
  when is_map(StoreConfig) ->
    % Handle various store module types differently
    case Module of
        hb_store_fs ->
            % For filesystem store, replace prefix with the new path
            StoreConfig#{<<"prefix">> => NewPath};
        hb_store_rocksdb ->
            % For RocksDB store, replace prefix with the new path
            StoreConfig#{<<"prefix">> => NewPath};
        hb_store_gateway ->
            % For gateway store, recursively update nested store configurations
            NestedStore = maps:get(<<"store">>, StoreConfig, []),
            StoreConfig#{
                <<"store">> => update_store_config(NestedStore, NewPath)
            };
        _ ->
            % For any other store type, update the prefix
            StoreConfig#{<<"prefix">> => NewPath}
    end;
update_store_config(StoreConfig, _NewPath) ->
    % Return unchanged for any other format
    StoreConfig. 
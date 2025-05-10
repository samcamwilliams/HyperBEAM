%%% @doc Secure Volume Management for HyperBEAM Nodes
%%%
%%% This module handles encrypted storage operations for HyperBEAM, providing 
%%% a robust and secure approach to data persistence. It manages the complete 
%%% lifecycle of encrypted volumes from detection to creation, formatting, and 
%%% mounting.
%%%
%%% Key responsibilities:
%%% - Volume detection and initialization
%%% - Encrypted partition creation and formatting
%%% - Secure mounting using cryptographic keys
%%% - Store path reconfiguration to use mounted volumes
%%% - Automatic handling of various system states 
%%%   (new device, existing partition, etc.)
%%%
%%% The primary entry point is the `mount/3' function, which orchestrates the 
%%% entire process based on the provided configuration parameters. This module 
%%% works alongside `hb_volume' which provides the low-level operations for 
%%% device manipulation.
%%%
%%% Security considerations:
%%% - Ensures data at rest is protected through LUKS encryption
%%% - Provides proper volume sanitization and secure mounting
-module(dev_volume).
-export([mount/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Handles the complete process of secure encrypted volume mounting.
%%
%% This function performs the following operations depending on the state:
%% 1. Validates the encryption key is present
%% 2. Checks if the base device exists
%% 3. Checks if the partition exists on the device
%% 4. If the partition exists, attempts to mount it
%% 5. If the partition doesn't exist, creates it, formats it with encryption 
%%    and mounts it
%% 6. Updates the node's store configuration to use the mounted volume
%%
%% Config options in Opts map:
%% - volume_key: (Required) The encryption key
%% - volume_device: Base device path (default: `/dev/sdc')
%% - volume_partition: Partition path (default: `/dev/sdc1') 
%% - volume_partition_type: Filesystem type (default: `ext4')
%% - volume_name: Name for encrypted volume (default: `hyperbeam_secure')
%% - volume_mount_point: Where to mount (default: `/root/mnt/hyperbeam_secure')
%% - volume_store_path: Store path on volume (default: `/root/mnt/hyperbeam_secure/store')
%%
%% @param M1 Base message for context.
%% @param M2 Request message with operation details.
%% @param Opts A map of configuration options for volume operations.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec mount(term(), term(), map()) -> {ok, binary()} | {error, binary()}.
mount(_M1, _M2, Opts) ->
	Key = hb_opts:get(volume_key, not_found, Opts),
	case Key of
	  not_found ->
		?event(mount, {error, <<"Volume key not found">>}),
		{error, <<"Volume key not found">>};
	  _ -> 
		Device = hb_opts:get(volume_device, <<"/dev/sdc">>, Opts),
		Partition = hb_opts:get(volume_partition, <<"/dev/sdc1">>, Opts),
		PartitionType = hb_opts:get(volume_partition_type, <<"ext4">>, Opts),
		VolumeName = hb_opts:get(volume_name, <<"hyperbeam_secure">>, Opts),
		MountPoint = hb_opts:get(
			volume_mount_point, 
			<<"/root/mnt/hyperbeam_secure">>, 
			Opts
		),
		StorePath = hb_opts:get(
			volume_store_path, 
			<<"/root/mnt/hyperbeam_secure/store">>, 
			Opts
		),
		?event(debug_mount, 
			{mount, device, Device}
		),
		?event(debug_mount, 
			{mount, partition, Partition}
		),
		?event(debug_mount, 
			{mount, partition_type, PartitionType}
		),
		?event(debug_mount, 
			{mount, mount_point, MountPoint}
		),	
		check_base_device(
			Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
			Key, Opts
		)
	end.

%% @doc Check if the base device exists and if it does, check if the partition exists.
%% @param Device The base device to check.
%% @param Partition The partition to check.
%% @param PartitionType The type of partition to check.
%% @param VolumeName The name of the volume to check.
%% @param MountPoint The mount point to check.
%% @param StorePath The store path to check.
%% @param Key The key to check.
%% @param Opts The options to check.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec check_base_device(
	term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
check_base_device(
	Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
	Key, Opts
) ->
	case hb_volume:check_for_device(Device) of
		false ->
			% Base device doesn't exist
			?event(debug_mount, 
				{device_check, error, <<"Base device not found">>}
			),
			{error, <<"Base device not found">>};
		true ->
			check_partition(
				Device, Partition, PartitionType, VolumeName, 
				MountPoint, StorePath, Key, Opts
			)
	end.

%% @doc Check if the partition exists. If it does, attempt to mount it.
%% If it doesn't exist, create it, format it with encryption and mount it.
%% @param Device The base device to check.
%% @param Partition The partition to check.
%% @param PartitionType The type of partition to check.
%% @param VolumeName The name of the volume to check.
%% @param MountPoint The mount point to check.
%% @param StorePath The store path to check.
%% @param Key The key to check.
%% @param Opts The options to check.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec check_partition(
	term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
check_partition(
	Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
	Key, Opts
) ->
	case hb_volume:check_for_device(Partition) of
		true ->
			% Partition exists, try mounting it
			mount_existing_partition(
				Partition, Key, MountPoint, VolumeName, StorePath, Opts
			);
		false ->
			% Partition doesn't exist, create it
			create_and_mount_partition(
				Device, Partition, PartitionType, Key, 
				MountPoint, VolumeName, StorePath, Opts
			)
	end.

%% @doc Mount an existing partition.
%% @param Partition The partition to mount.
%% @param Key The key to mount.
%% @param MountPoint The mount point to mount.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec mount_existing_partition(
	term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
mount_existing_partition(
	Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
	?event(debug_mount, {mount_volume, attempt, Partition}),
	case hb_volume:mount_disk(Partition, Key, MountPoint, VolumeName) of
		{ok, MountResult} ->
			?event(debug_mount, {mount_volume, success, MountResult}),
			update_store_path(StorePath, Opts);
		{error, MountError} ->
			?event(debug_mount, {mount_volume, error, MountError}),
			{error, <<"Failed to mount volume">>}
	end.

%% @doc Create, format and mount a new partition.
%% @param Device The device to create the partition on.
%% @param Partition The partition to create.
%% @param PartitionType The type of partition to create.
%% @param Key The key to create the partition with.
%% @param MountPoint The mount point to mount the partition to.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec create_and_mount_partition(
	term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
create_and_mount_partition(
	Device, Partition, PartitionType, Key, 
	MountPoint, VolumeName, StorePath, Opts
) ->
	?event(debug_mount, {create_partition, attempt, Device}),
	case hb_volume:create_partition(Device, PartitionType) of
		{ok, PartitionResult} ->
			?event(debug_mount, {partition_create, success, PartitionResult}),
			format_and_mount(
				Partition, Key, MountPoint, VolumeName, StorePath, Opts
			);
		{error, PartitionError} ->
			?event(debug_mount, {partition_create, error, PartitionError}),
			{error, <<"Failed to create partition">>}
	end.

%% @doc Format and mount a newly created partition.
%% @param Partition The partition to format and mount.
%% @param Key The key to format and mount the partition with.
%% @param MountPoint The mount point to mount the partition to.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec format_and_mount(
	term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
format_and_mount(
	Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
	case hb_volume:format_disk(Partition, Key) of
		{ok, FormatResult} ->
			?event(debug_mount, {format_disk, success, FormatResult}),
			mount_formatted_partition(
				Partition, Key, MountPoint, VolumeName, StorePath, Opts
			);
		{error, FormatError} ->
			?event(debug_mount, {format_disk, error, FormatError}),
			{error, <<"Failed to format disk">>}
	end.

%% @doc Mount a newly formatted partition.
%% @param Partition The partition to mount.
%% @param Key The key to mount the partition with.
%% @param MountPoint The mount point to mount the partition to.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec mount_formatted_partition(
	term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
mount_formatted_partition(
	Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
	case hb_volume:mount_disk(Partition, Key, MountPoint, VolumeName) of
		{ok, RetryMountResult} ->
			?event(debug_mount, {mount_volume, success, RetryMountResult}),
			update_store_path(StorePath, Opts);
		{error, RetryMountError} ->
			?event(debug_mount, {mount_volume, error, RetryMountError}),
			{error, <<"Failed to mount newly formatted volume">>}
	end.

%% @doc Update the store path to use the mounted volume.
%% @param StorePath The store path to update.
%% @param Opts The options to update.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec update_store_path(term(), map()) -> {ok, binary()} | {error, binary()}.
update_store_path(StorePath, Opts) ->
	CurrentStore = hb_opts:get(store, [], Opts),
	case hb_volume:change_node_store(StorePath, CurrentStore) of
		{ok, #{<<"store">> := NewStore} = StoreResult} ->
			?event(debug_mount, {store_update, success, StoreResult}),
			update_node_config(NewStore, Opts);
		{error, StoreError} ->
			?event(debug_mount, {store_update, error, StoreError}),
			{error, <<"Failed to update store">>}
	end.

%% @doc Update the node's configuration with the new store.
%% @param NewStore The new store to update the node's configuration with.
%% @param Opts The options to update the node's configuration with.
%% @returns {ok, Binary} on success with operation result message.
%% @returns {error, Binary} on failure with error message.
-spec update_node_config(term(), map()) -> {ok, binary()} | {error, binary()}.
update_node_config(NewStore, Opts) ->
	ok = hb_http_server:set_opts(Opts#{store => NewStore}),
	?event(debug_mount, {store_update, config_updated}),
	{ok, <<"Volume mounted and store updated successfully">>}.

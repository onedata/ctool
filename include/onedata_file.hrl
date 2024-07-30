%%%--------------------------------------------------------------------
%%% @author Michal Stanisz, Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Common definitions regarding files in Onedata.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEDATA_FILE_HRL).
-define(ONEDATA_FILE_HRL, 1).


%% file path related macros
-define(DIRECTORY_SEPARATOR, "/").
-define(DIRECTORY_SEPARATOR_CHAR, $/).
-define(DIRECTORY_SEPARATOR_BIN, <<?DIRECTORY_SEPARATOR>>).

-define(CURRENT_DIRECTORY, ".").
-define(PARENT_DIRECTORY, "..").


%% file types
-define(REGULAR_FILE_TYPE, 'REG').
-define(DIRECTORY_TYPE, 'DIR').
-define(LINK_TYPE, 'LNK'). % hard link
-define(SYMLINK_TYPE, 'SYMLNK'). % symbolic link


%% file attributes
-define(attr_guid, guid).
-define(attr_index, index).
-define(attr_type, type).
-define(attr_active_permissions_type, active_permissions_type).
-define(attr_mode, mode).
-define(attr_acl, acl).
-define(attr_name, name).
-define(attr_conflicting_name, conflicting_name).
-define(attr_path, path).
-define(attr_parent_guid, parent_guid).
-define(attr_gid, gid).
-define(attr_uid, uid).
-define(attr_creation_time, creation_time).
-define(attr_atime, atime).
-define(attr_mtime, mtime).
-define(attr_ctime, ctime).
-define(attr_size, size).
-define(attr_is_fully_replicated, is_fully_replicated).
-define(attr_local_replication_rate, local_replication_rate).
-define(attr_provider_id, provider_id).
-define(attr_shares, shares).
-define(attr_owner_id, owner_id).
-define(attr_hardlink_count, hardlink_count).
-define(attr_symlink_value, symlink_value).
-define(attr_has_custom_metadata, has_custom_metadata).
-define(attr_eff_protection_flags, eff_protection_flags).
-define(attr_eff_dataset_protection_flags, eff_dataset_protection_flags).
-define(attr_eff_dataset_inheritance_path, eff_dataset_inheritance_path).
-define(attr_eff_qos_inheritance_path, eff_qos_inheritance_path).
-define(attr_qos_status, qos_status).
-define(attr_recall_root_id, recall_root_id).
-define(attr_is_deleted, is_deleted).
-define(attr_conflicting_files, conflicting_files).
-define(attr_xattrs(XattrNames), {xattrs, XattrNames}).

-define(IMPLICIT_FILE_ATTRS, [
    ?attr_guid
]).
-define(FILE_META_ATTRS, [
    ?attr_index, ?attr_type, ?attr_active_permissions_type, ?attr_mode, ?attr_acl, ?attr_parent_guid,
    ?attr_provider_id, ?attr_shares, ?attr_owner_id, ?attr_hardlink_count, ?attr_symlink_value, ?attr_is_deleted
]).
-define(LINK_TREE_FILE_ATTRS, [
    ?attr_name, ?attr_conflicting_name, ?attr_conflicting_files
]).
-define(PATH_FILE_ATTRS, [
    ?attr_path
]).
-define(LUMA_FILE_ATTRS, [
    ?attr_gid, ?attr_uid
]).
-define(TIMES_FILE_ATTRS, [
    ?attr_creation_time, ?attr_atime, ?attr_mtime, ?attr_ctime
]).
-define(LOCATION_FILE_ATTRS, [
    ?attr_size, ?attr_is_fully_replicated, ?attr_local_replication_rate
]).
-define(METADATA_FILE_ATTRS, [
    ?attr_has_custom_metadata
]).
-define(DATASET_FILE_ATTRS, [
    ?attr_eff_dataset_inheritance_path, ?attr_eff_dataset_protection_flags, ?attr_eff_protection_flags
]).
-define(QOS_EFF_VALUE_FILE_ATTRS, [
    ?attr_eff_qos_inheritance_path
]).
-define(QOS_STATUS_FILE_ATTRS, [
    ?attr_qos_status
]).
-define(ARCHIVE_RECALL_FILE_ATTRS, [
    ?attr_recall_root_id
]).

% Below list does not contain xattrs
-define(ALL_FILE_ATTRS, lists:flatten([
    ?IMPLICIT_FILE_ATTRS, ?FILE_META_ATTRS, ?LINK_TREE_FILE_ATTRS, ?PATH_FILE_ATTRS,
    ?LUMA_FILE_ATTRS, ?TIMES_FILE_ATTRS, ?LOCATION_FILE_ATTRS, ?METADATA_FILE_ATTRS,
    ?DATASET_FILE_ATTRS, ?QOS_EFF_VALUE_FILE_ATTRS, ?QOS_STATUS_FILE_ATTRS, ?ARCHIVE_RECALL_FILE_ATTRS
])).

% attrs that are used internally by op-worker mechanisms (like events, storage import); are not visible in API
-define(INTERNAL_FILE_ATTRS, [
    ?attr_is_deleted, ?attr_conflicting_files
]).

-define(API_FILE_ATTRS, (?ALL_FILE_ATTRS -- ?INTERNAL_FILE_ATTRS)).
-define(PUBLIC_API_FILE_ATTRS, [
    ?attr_guid, ?attr_index, ?attr_type, ?attr_active_permissions_type, ?attr_mode, ?attr_name, ?attr_conflicting_name,
    ?attr_parent_guid, ?attr_creation_time, ?attr_atime, ?attr_mtime, ?attr_ctime, ?attr_size, ?attr_shares,
    ?attr_symlink_value, ?attr_has_custom_metadata
]).

%% @TODO VFS-11378 remove when all usages provide their custom required attrs
-define(ONECLIENT_FILE_ATTRS, [
    ?attr_guid, ?attr_type, ?attr_mode, ?attr_name, ?attr_parent_guid, ?attr_gid, ?attr_uid, ?attr_atime, ?attr_mtime,
    ?attr_ctime, ?attr_size, ?attr_provider_id, ?attr_shares, ?attr_owner_id
]).

%% @TODO VFS-11377 deprecated, remove when possible
-define(DEPRECATED_ALL_FILE_ATTRS, [
    ?attr_guid, ?attr_parent_guid, ?attr_name, ?attr_mode, ?attr_atime, ?attr_mtime, ?attr_ctime, ?attr_type, ?attr_size,
    ?attr_shares, ?attr_index, ?attr_uid, ?attr_gid, ?attr_owner_id, ?attr_provider_id, ?attr_hardlink_count
]).
-define(DEPRECATED_PUBLIC_FILE_ATTRS, [
    ?attr_guid, ?attr_parent_guid, ?attr_name, ?attr_mode, ?attr_atime, ?attr_mtime, ?attr_ctime, ?attr_type, ?attr_size,
    ?attr_shares, ?attr_index
]).


-endif.

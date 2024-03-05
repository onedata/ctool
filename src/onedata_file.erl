%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common functions and types regarding files in Onedata.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_file).
-author("Lukasz Opiola").

-include("onedata_file.hrl").
-include("errors.hrl").


%% API
-export([type_to_json/1, type_from_json/1]).

-export([sanitize_attr_names/4]).
-export([attr_name_to_json/2, attr_name_to_json/1]).
-export([attr_name_from_json/2, attr_name_from_json/1]).


-type type() :: ?REGULAR_FILE_TYPE | ?DIRECTORY_TYPE | ?SYMLINK_TYPE | ?LINK_TYPE.

-type attr_generation() :: current | deprecated.
-type attr_name() :: ?attr_guid | ?attr_index | ?attr_type | ?attr_active_permissions_type | ?attr_mode | ?attr_acl |
?attr_name | ?attr_conflicting_name | ?attr_path | ?attr_parent_guid | ?attr_gid | ?attr_uid | ?attr_atime |
?attr_mtime | ?attr_ctime | ?attr_size | ?attr_is_fully_replicated | ?attr_local_replication_rate | ?attr_provider_id |
?attr_shares | ?attr_owner_id | ?attr_hardlink_count | ?attr_symlink_value | ?attr_has_custom_metadata |
?attr_eff_protection_flags | ?attr_eff_dataset_protection_flags | ?attr_eff_dataset_inheritance_path |
?attr_eff_qos_inheritance_path | ?attr_qos_status | ?attr_recall_root_id | ?attr_is_deleted |
?attr_conflicting_files | ?attr_xattrs([xattr_name()]).

-type xattr_name() :: binary().
-type xattr_value() :: binary().

-export_type([type/0]).
-export_type([attr_name/0, attr_generation/0]).
-export_type([xattr_name/0, xattr_value/0]).


%%%===================================================================
%%% API
%%%===================================================================


-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(?REGULAR_FILE_TYPE) -> <<"REG">>;
type_to_json(?DIRECTORY_TYPE) -> <<"DIR">>;
type_to_json(?LINK_TYPE) -> <<"LNK">>;
type_to_json(?SYMLINK_TYPE) -> <<"SYMLNK">>.


%% @private
-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"REG">>) -> ?REGULAR_FILE_TYPE;
type_from_json(<<"DIR">>) -> ?DIRECTORY_TYPE;
type_from_json(<<"LNK">>) -> ?LINK_TYPE;
type_from_json(<<"SYMLNK">>) -> ?SYMLINK_TYPE.


-spec sanitize_attr_names(binary(), [binary()], attr_generation(), [attr_name()]) ->
    [attr_name()] | no_return().
sanitize_attr_names(DataKey, Attributes, AttrGeneration, AllowedAttributes) ->
    Result = lists_utils:foldl_while(fun
        (<<"xattr.", XattrName/binary>>, {ok, AttrAcc, XattrAcc}) ->
            {cont, {ok, AttrAcc, [XattrName | XattrAcc]}};
        (Attr, {ok, AttrAcc, XattrAcc}) ->
            try
                TranslatedAttr = attr_name_from_json(AttrGeneration, Attr),
                true = lists:member(TranslatedAttr, AllowedAttributes),
                {cont, {ok, [TranslatedAttr | AttrAcc], XattrAcc}}
            catch _:_ ->
                AllowedValuesJson = [attr_name_to_json(AttrGeneration, A) || A <- AllowedAttributes],
                % add xattr.* to end of list, so allowed values are printed in correct order
                {halt, {error, AllowedValuesJson ++ [<<"xattr.*">>]}}
            end
    end, {ok, [], []}, utils:ensure_list(Attributes)),
    case Result of
        {ok, TranslatedAttrs, []} -> TranslatedAttrs;
        {ok, TranslatedAttrs, Xattrs} -> [?attr_xattrs(Xattrs) | TranslatedAttrs];
        {error, AllowedValuesJson} -> throw(?ERROR_BAD_VALUE_NOT_ALLOWED(DataKey, AllowedValuesJson))
    end.


%% @TODO VFS-11377 deprecated, remove when possible
-spec attr_name_from_json(attr_generation(), binary()) -> attr_name().
attr_name_from_json(current, AttrJson) ->
    attr_name_from_json(AttrJson);
attr_name_from_json(deprecated, AttrJson) ->
    attr_name_from_json_deprecated(AttrJson).


%% @TODO VFS-11377 deprecated, remove when possible
-spec attr_name_to_json(attr_generation(), attr_name()) -> binary().
attr_name_to_json(current, Attr) ->
    attr_name_to_json(Attr);
attr_name_to_json(deprecated, Attr) ->
    attr_name_to_json_deprecated(Attr).


-spec attr_name_from_json(binary()) -> attr_name().
attr_name_from_json(<<"fileId">>)                    -> ?attr_guid;
attr_name_from_json(<<"index">>)                     -> ?attr_index;
attr_name_from_json(<<"type">>)                      -> ?attr_type;
attr_name_from_json(<<"activePermissionsType">>)     -> ?attr_active_permissions_type;
attr_name_from_json(<<"posixPermissions">>)          -> ?attr_mode;
attr_name_from_json(<<"acl">>)                       -> ?attr_acl;
attr_name_from_json(<<"name">>)                      -> ?attr_name;
attr_name_from_json(<<"conflictingName">>)           -> ?attr_conflicting_name;
attr_name_from_json(<<"path">>)                      -> ?attr_path;
attr_name_from_json(<<"parentFileId">>)              -> ?attr_parent_guid;
attr_name_from_json(<<"displayGid">>)                -> ?attr_gid;
attr_name_from_json(<<"displayUid">>)                -> ?attr_uid;
attr_name_from_json(<<"atime">>)                     -> ?attr_atime;
attr_name_from_json(<<"mtime">>)                     -> ?attr_mtime;
attr_name_from_json(<<"ctime">>)                     -> ?attr_ctime;
attr_name_from_json(<<"size">>)                      -> ?attr_size;
attr_name_from_json(<<"isFullyReplicatedLocally">>)  -> ?attr_is_fully_replicated;
attr_name_from_json(<<"localReplicationRate">>)      -> ?attr_local_replication_rate;
attr_name_from_json(<<"originProviderId">>)          -> ?attr_provider_id;
attr_name_from_json(<<"directShareIds">>)            -> ?attr_shares;
attr_name_from_json(<<"ownerUserId">>)               -> ?attr_owner_id;
attr_name_from_json(<<"hardlinkCount">>)             -> ?attr_hardlink_count;
attr_name_from_json(<<"symlinkValue">>)              -> ?attr_symlink_value;
attr_name_from_json(<<"hasCustomMetadata">>)         -> ?attr_has_custom_metadata;
attr_name_from_json(<<"effProtectionFlags">>)        -> ?attr_eff_protection_flags;
attr_name_from_json(<<"effDatasetProtectionFlags">>) -> ?attr_eff_dataset_protection_flags;
attr_name_from_json(<<"effDatasetInheritancePath">>) -> ?attr_eff_dataset_inheritance_path;
attr_name_from_json(<<"effQosInheritancePath">>)     -> ?attr_eff_qos_inheritance_path;
attr_name_from_json(<<"aggregateQosStatus">>)        -> ?attr_qos_status;
attr_name_from_json(<<"archiveRecallRootFileId">>)   -> ?attr_recall_root_id.


-spec attr_name_to_json(attr_name()) -> binary().
attr_name_to_json(?attr_guid)                         -> <<"fileId">>;
attr_name_to_json(?attr_index)                        -> <<"index">>;
attr_name_to_json(?attr_type)                         -> <<"type">>;
attr_name_to_json(?attr_active_permissions_type)      -> <<"activePermissionsType">>;
attr_name_to_json(?attr_mode)                         -> <<"posixPermissions">>;
attr_name_to_json(?attr_acl)                          -> <<"acl">>;
attr_name_to_json(?attr_name)                         -> <<"name">>;
attr_name_to_json(?attr_conflicting_name)             -> <<"conflictingName">>;
attr_name_to_json(?attr_path)                         -> <<"path">>;
attr_name_to_json(?attr_parent_guid)                  -> <<"parentFileId">>;
attr_name_to_json(?attr_gid)                          -> <<"displayGid">>;
attr_name_to_json(?attr_uid)                          -> <<"displayUid">>;
attr_name_to_json(?attr_atime)                        -> <<"atime">>;
attr_name_to_json(?attr_mtime)                        -> <<"mtime">>;
attr_name_to_json(?attr_ctime)                        -> <<"ctime">>;
attr_name_to_json(?attr_size)                         -> <<"size">>;
attr_name_to_json(?attr_is_fully_replicated)          -> <<"isFullyReplicatedLocally">>;
attr_name_to_json(?attr_local_replication_rate)       -> <<"localReplicationRate">>;
attr_name_to_json(?attr_provider_id)                  -> <<"originProviderId">>;
attr_name_to_json(?attr_shares)                       -> <<"directShareIds">>;
attr_name_to_json(?attr_owner_id)                     -> <<"ownerUserId">>;
attr_name_to_json(?attr_hardlink_count)               -> <<"hardlinkCount">>;
attr_name_to_json(?attr_symlink_value)                -> <<"symlinkValue">>;
attr_name_to_json(?attr_has_custom_metadata)          -> <<"hasCustomMetadata">>;
attr_name_to_json(?attr_eff_protection_flags)         -> <<"effProtectionFlags">>;
attr_name_to_json(?attr_eff_dataset_protection_flags) -> <<"effDatasetProtectionFlags">>;
attr_name_to_json(?attr_eff_dataset_inheritance_path) -> <<"effDatasetInheritancePath">>;
attr_name_to_json(?attr_eff_qos_inheritance_path)     -> <<"effQosInheritancePath">>;
attr_name_to_json(?attr_qos_status)                   -> <<"aggregateQosStatus">>;
attr_name_to_json(?attr_recall_root_id)               -> <<"archiveRecallRootFileId">>.


%%%===================================================================
%%% API
%%%===================================================================


%% @TODO VFS-11377 deprecated, remove when possible
%% @private
-spec attr_name_from_json_deprecated(binary()) -> attr_name().
attr_name_from_json_deprecated(<<"file_id">>)             -> ?attr_guid;
attr_name_from_json_deprecated(<<"path">>)                -> ?attr_path;
attr_name_from_json_deprecated(<<"name">>)                -> ?attr_name;
attr_name_from_json_deprecated(<<"atime">>)               -> ?attr_atime;
attr_name_from_json_deprecated(<<"mtime">>)               -> ?attr_mtime;
attr_name_from_json_deprecated(<<"ctime">>)               -> ?attr_ctime;
attr_name_from_json_deprecated(<<"type">>)                -> ?attr_type;
attr_name_from_json_deprecated(<<"size">>)                -> ?attr_size;
attr_name_from_json_deprecated(<<"shares">>)              -> ?attr_shares;
attr_name_from_json_deprecated(<<"index">>)               -> ?attr_index;
attr_name_from_json_deprecated(<<"storage_user_id">>)     -> ?attr_uid;
attr_name_from_json_deprecated(<<"storage_group_id">>)    -> ?attr_gid;
attr_name_from_json_deprecated(<<"owner_id">>)            -> ?attr_owner_id;
attr_name_from_json_deprecated(<<"parent_id">>)           -> ?attr_parent_guid;
attr_name_from_json_deprecated(<<"provider_id">>)         -> ?attr_provider_id;
attr_name_from_json_deprecated(<<"hardlinks_count">>)     -> ?attr_hardlink_count;
attr_name_from_json_deprecated(<<"is_fully_replicated">>) -> ?attr_is_fully_replicated;
attr_name_from_json_deprecated(<<"mode">>)                -> ?attr_mode.


%% @TODO VFS-11377 deprecated, remove when possible
%% @private
-spec attr_name_to_json_deprecated(attr_name()) -> binary().
attr_name_to_json_deprecated(?attr_guid)                -> <<"file_id">>;
attr_name_to_json_deprecated(?attr_path)                -> <<"path">>;
attr_name_to_json_deprecated(?attr_name)                -> <<"name">>;
attr_name_to_json_deprecated(?attr_atime)               -> <<"atime">>;
attr_name_to_json_deprecated(?attr_mtime)               -> <<"mtime">>;
attr_name_to_json_deprecated(?attr_ctime)               -> <<"ctime">>;
attr_name_to_json_deprecated(?attr_type)                -> <<"type">>;
attr_name_to_json_deprecated(?attr_size)                -> <<"size">>;
attr_name_to_json_deprecated(?attr_shares)              -> <<"shares">>;
attr_name_to_json_deprecated(?attr_index)               -> <<"index">>;
attr_name_to_json_deprecated(?attr_uid)                 -> <<"storage_user_id">>;
attr_name_to_json_deprecated(?attr_gid)                 -> <<"storage_group_id">>;
attr_name_to_json_deprecated(?attr_owner_id)            -> <<"owner_id">>;
attr_name_to_json_deprecated(?attr_parent_guid)         -> <<"parent_id">>;
attr_name_to_json_deprecated(?attr_provider_id)         -> <<"provider_id">>;
attr_name_to_json_deprecated(?attr_hardlink_count)      -> <<"hardlinks_count">>;
attr_name_to_json_deprecated(?attr_is_fully_replicated) -> <<"is_fully_replicated">>;
attr_name_to_json_deprecated(?attr_mode)                -> <<"mode">>.

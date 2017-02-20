%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions concerning privileges in onedata.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(PRIVILEGES_HRL).
-define(PRIVILEGES_HRL, 1).

% Group privileges of members (users or groups)
% TODO VFS-2918
%%-define(GROUP_VIEW, group_view).
-define(GROUP_VIEW, group_view_data).
% TODO VFS-2918
%%-define(GROUP_UPDATE, group_update).
-define(GROUP_UPDATE, group_change_data).
% TODO VFS-2918
%%-define(GROUP_DELETE, group_delete).
-define(GROUP_DELETE, group_remove).
-define(GROUP_SET_PRIVILEGES, group_set_privileges).

-define(GROUP_INVITE_USER, group_invite_user).
-define(GROUP_REMOVE_USER, group_remove_user).

-define(GROUP_JOIN_GROUP, group_join_group).
% TODO VFS-2918
%%-define(GROUP_LEAVE_GROUP, group_leave_group).
-define(GROUP_INVITE_GROUP, group_invite_group).
-define(GROUP_REMOVE_GROUP, group_remove_group).

-define(GROUP_CREATE_SPACE, group_create_space).
-define(GROUP_JOIN_SPACE, group_join_space).
-define(GROUP_LEAVE_SPACE, group_leave_space).

% TODO VFS-2918
%%-define(GROUP_LEAVE_HANDLE_SERVICE, group_leave_handle_service).
% TODO VFS-2918
%%-define(GROUP_LEAVE_HANDLE, group_leave_handle).

% Space privileges of members (users or groups)
% TODO VFS-2918
%%-define(SPACE_VIEW, space_view).
-define(SPACE_VIEW, space_view_data).
% TODO VFS-2918
%%-define(SPACE_UPDATE, space_update).
-define(SPACE_UPDATE, space_change_data).
% TODO VFS-2918
%%-define(SPACE_DELETE, space_delete).
-define(SPACE_DELETE, space_remove).
-define(SPACE_SET_PRIVILEGES, space_set_privileges).
% TODO VFS-2918
%%-define(SPACE_WRITE_DATA, space_write_data).
-define(SPACE_WRITE_DATA, space_write_files).
-define(SPACE_MANAGE_SHARES, space_manage_shares).

-define(SPACE_INVITE_USER, space_invite_user).
-define(SPACE_REMOVE_USER, space_remove_user).

-define(SPACE_INVITE_GROUP, space_invite_group).
-define(SPACE_REMOVE_GROUP, space_remove_group).

% TODO VFS-2918
%%-define(SPACE_INVITE_PROVIDER, space_invite_provider).
-define(SPACE_INVITE_PROVIDER, space_add_provider).
-define(SPACE_REMOVE_PROVIDER, space_remove_provider).

% Handle service privileges of members (users or groups)
% TODO VFS-2918
%%-define(HANDLE_SERVICE_VIEW, handle_service_view).
-define(HANDLE_SERVICE_VIEW, view_handle_service).
% TODO VFS-2918
%%-define(HANDLE_SERVICE_UPDATE, handle_service_update).
-define(HANDLE_SERVICE_UPDATE, modify_handle_service).
% TODO VFS-2918
%%-define(HANDLE_SERVICE_DELETE, handle_service_delete).
-define(HANDLE_SERVICE_DELETE, delete_handle_service).
% TODO VFS-2918
%%-define(HANDLE_SERVICE_REGISTER_HANDLE, handle_service_register_handle).
-define(HANDLE_SERVICE_REGISTER_HANDLE, register_handle).
-define(HANDLE_SERVICE_LIST_HANDLES, handle_service_list_handles).

% Handle privileges of members (users or groups)
% TODO VFS-2918
%%-define(HANDLE_VIEW, handle_view).
-define(HANDLE_VIEW, view_handle).
% TODO VFS-2918
%%-define(HANDLE_UPDATE, handle_update).
-define(HANDLE_UPDATE, modify_handle).
% TODO VFS-2918
%%-define(HANDLE_DELETE, handle_delete).
-define(HANDLE_DELETE, delete_handle).

% OZ privileges of users or groups in onezone (typically privileges for admins)
% TODO VFS-2918
%%-define(OZ_VIEW_PRIVILEGES, oz_view_privileges).
-define(OZ_VIEW_PRIVILEGES, view_privileges).
% TODO VFS-2918
%%-define(OZ_SET_PRIVILEGES, oz_set_privileges).
-define(OZ_SET_PRIVILEGES, set_privileges).

% TODO VFS-2918
%%-define(OZ_USERS_LIST, oz_users_list).
-define(OZ_USERS_LIST, list_users).
-define(OZ_USERS_DELETE, oz_users_delete).

% TODO VFS-2918
%%-define(OZ_GROUPS_LIST, oz_groups_list).
-define(OZ_GROUPS_LIST, list_groups).
-define(OZ_GROUPS_LIST_USERS, oz_groups_list_users).
-define(OZ_GROUPS_LIST_GROUPS, oz_groups_list_groups).
-define(OZ_GROUPS_ADD_MEMBERS, oz_groups_add_members).
-define(OZ_GROUPS_REMOVE_MEMBERS, oz_groups_remove_members).

% TODO VFS-2918
%%-define(OZ_SPACES_LIST, oz_spaces_list).
-define(OZ_SPACES_LIST, list_spaces).
-define(OZ_SPACES_LIST_USERS, oz_spaces_list_users).
-define(OZ_SPACES_LIST_GROUPS, oz_spaces_list_groups).
% TODO VFS-2918
%%-define(OZ_SPACES_LIST_PROVIDERS, oz_spaces_list_providers).
-define(OZ_SPACES_LIST_PROVIDERS, list_providers_of_space).
% TODO VFS-2918
%%-define(OZ_SPACES_ADD_MEMBERS, oz_spaces_add_members).
-define(OZ_SPACES_ADD_MEMBERS, add_member_to_space).
% TODO VFS-2918
%%-define(OZ_SPACES_REMOVE_MEMBERS, oz_spaces_remove_members).
-define(OZ_SPACES_REMOVE_MEMBERS, remove_member_from_space).

-define(OZ_SHARES_LIST, oz_shares_list).

% TODO VFS-2918
%%-define(OZ_PROVIDERS_LIST, oz_providers_list).
-define(OZ_PROVIDERS_LIST, list_providers).
% TODO VFS-2918
%%-define(OZ_PROVIDERS_LIST_USERS, oz_providers_list_users).
-define(OZ_PROVIDERS_LIST_USERS, list_users_of_provider).
% TODO VFS-2918
%%-define(OZ_PROVIDERS_LIST_GROUPS, oz_providers_list_groups).
-define(OZ_PROVIDERS_LIST_GROUPS, list_groups_of_provider).
% TODO VFS-2918
%%-define(OZ_PROVIDERS_LIST_SPACES, oz_providers_list_spaces).
-define(OZ_PROVIDERS_LIST_SPACES, list_spaces_of_provider).
-define(OZ_PROVIDERS_DELETE, oz_providers_delete).

-define(OZ_HANDLE_SERVICES_CREATE, oz_handle_services_create).
-define(OZ_HANDLE_SERVICES_LIST, oz_handle_services_list).

-define(OZ_HANDLES_LIST, oz_shares_list).


-endif.
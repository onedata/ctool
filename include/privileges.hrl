%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
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
-define(GROUP_VIEW, group_view).
-define(GROUP_UPDATE, group_update).
-define(GROUP_DELETE, group_delete).
-define(GROUP_VIEW_PRIVILEGES, group_view_privileges).
-define(GROUP_SET_PRIVILEGES, group_set_privileges).

-define(GROUP_ADD_PARENT, group_add_parent).
-define(GROUP_LEAVE_PARENT, group_leave_parent).

-define(GROUP_ADD_CHILD, group_add_child).
-define(GROUP_REMOVE_CHILD, group_remove_child).

-define(GROUP_ADD_USER, group_add_user).
-define(GROUP_REMOVE_USER, group_remove_user).

-define(GROUP_ADD_SPACE, group_add_space).
-define(GROUP_LEAVE_SPACE, group_leave_space).

-define(GROUP_ADD_CLUSTER, group_add_cluster).
-define(GROUP_LEAVE_CLUSTER, group_leave_cluster).

-define(GROUP_CREATE_HANDLE_SERVICE, group_create_handle_service).
-define(GROUP_LEAVE_HANDLE_SERVICE, group_leave_handle_service).

-define(GROUP_CREATE_HANDLE, group_create_handle).
-define(GROUP_LEAVE_HANDLE, group_leave_handle).

-define(GROUP_ADD_HARVESTER, group_add_harvester).
-define(GROUP_REMOVE_HARVESTER, group_remove_harvester).

% Space privileges of members (users or groups)
-define(SPACE_VIEW, space_view).
-define(SPACE_UPDATE, space_update).
-define(SPACE_DELETE, space_delete).
-define(SPACE_VIEW_PRIVILEGES, space_view_privileges).
-define(SPACE_SET_PRIVILEGES, space_set_privileges).

-define(SPACE_READ_DATA, space_read_data).
-define(SPACE_WRITE_DATA, space_write_data).
-define(SPACE_MANAGE_SHARES, space_manage_shares).

-define(SPACE_VIEW_VIEWS, space_view_views).
-define(SPACE_MANAGE_VIEWS, space_manage_views).
-define(SPACE_QUERY_VIEWS, space_query_views).

-define(SPACE_VIEW_STATISTICS, space_view_statistics).
-define(SPACE_VIEW_CHANGES_STREAM, space_view_changes_stream).

-define(SPACE_VIEW_TRANSFERS, space_view_transfers).
-define(SPACE_SCHEDULE_REPLICATION, space_schedule_replication).
-define(SPACE_CANCEL_REPLICATION, space_cancel_replication).
-define(SPACE_SCHEDULE_EVICTION, space_schedule_eviction).
-define(SPACE_CANCEL_EVICTION, space_cancel_eviction).

-define(SPACE_ADD_USER, space_add_user).
-define(SPACE_REMOVE_USER, space_remove_user).

-define(SPACE_ADD_GROUP, space_add_group).
-define(SPACE_REMOVE_GROUP, space_remove_group).

-define(SPACE_ADD_SUPPORT, space_add_provider).
-define(SPACE_REMOVE_SUPPORT, space_remove_provider).

-define(SPACE_ADD_HARVESTER, space_add_harvester).
-define(SPACE_REMOVE_HARVESTER, space_remove_harvester).

% Handle service privileges of members (users or groups)
-define(HANDLE_SERVICE_VIEW, handle_service_view).
-define(HANDLE_SERVICE_UPDATE, handle_service_update).
-define(HANDLE_SERVICE_DELETE, handle_service_delete).
-define(HANDLE_SERVICE_REGISTER_HANDLE, handle_service_register_handle).
-define(HANDLE_SERVICE_LIST_HANDLES, handle_service_list_handles).

% Handle privileges of members (users or groups)
-define(HANDLE_VIEW, handle_view).
-define(HANDLE_UPDATE, handle_update).
-define(HANDLE_DELETE, handle_delete).

% Harvester privileges of members (users or groups)
-define(HARVESTER_VIEW, harvester_view).
-define(HARVESTER_UPDATE, harvester_update).
-define(HARVESTER_DELETE, harvester_delete).
-define(HARVESTER_VIEW_PRIVILEGES, harvester_view_privileges).
-define(HARVESTER_SET_PRIVILEGES, harvester_set_privileges).

-define(HARVESTER_ADD_USER, harvester_add_user).
-define(HARVESTER_REMOVE_USER, harvester_remove_user).

-define(HARVESTER_ADD_GROUP, harvester_add_group).
-define(HARVESTER_REMOVE_GROUP, harvester_remove_group).

-define(HARVESTER_ADD_SPACE, harvester_add_space).
-define(HARVESTER_REMOVE_SPACE, harvester_remove_space).

% Cluster privileges of members (users or groups)
-define(CLUSTER_VIEW, cluster_view).
-define(CLUSTER_UPDATE, cluster_update).
-define(CLUSTER_DELETE, cluster_delete).
-define(CLUSTER_VIEW_PRIVILEGES, cluster_view_privileges).
-define(CLUSTER_SET_PRIVILEGES, cluster_set_privileges).

-define(CLUSTER_ADD_USER, cluster_add_user).
-define(CLUSTER_REMOVE_USER, cluster_remove_user).

-define(CLUSTER_ADD_GROUP, cluster_add_group).
-define(CLUSTER_REMOVE_GROUP, cluster_remove_group).

% OZ privileges to administrate privileges of users or groups in onezone
-define(OZ_VIEW_PRIVILEGES, oz_view_privileges).
-define(OZ_SET_PRIVILEGES, oz_set_privileges).

% OZ privileges to administrate access / invite tokens of users and providers
-define(OZ_MANAGE_TOKENS, oz_manage_tokens).

%% Privileges to administrate users in OZ
-define(OZ_USERS_LIST, oz_users_list).
-define(OZ_USERS_VIEW, oz_users_view).
-define(OZ_USERS_CREATE, oz_users_create).
-define(OZ_USERS_MANAGE_PASSWORDS, oz_users_manage_passwords).
-define(OZ_USERS_UPDATE, oz_users_update).
-define(OZ_USERS_DELETE, oz_users_delete).
-define(OZ_USERS_LIST_RELATIONSHIPS, oz_users_list_relationships).
-define(OZ_USERS_ADD_RELATIONSHIPS, oz_users_add_relationships).
-define(OZ_USERS_REMOVE_RELATIONSHIPS, oz_users_remove_relationships).

%% Privileges to administrate groups in OZ
-define(OZ_GROUPS_LIST, oz_groups_list).
-define(OZ_GROUPS_VIEW, oz_groups_view).
-define(OZ_GROUPS_CREATE, oz_groups_create).
-define(OZ_GROUPS_UPDATE, oz_groups_update).
-define(OZ_GROUPS_DELETE, oz_groups_delete).
-define(OZ_GROUPS_VIEW_PRIVILEGES, oz_groups_view_privileges).
-define(OZ_GROUPS_SET_PRIVILEGES, oz_groups_set_privileges).
-define(OZ_GROUPS_LIST_RELATIONSHIPS, oz_groups_list_relationships).
-define(OZ_GROUPS_ADD_RELATIONSHIPS, oz_groups_add_relationships).
-define(OZ_GROUPS_REMOVE_RELATIONSHIPS, oz_groups_remove_relationships).

%% Privileges to administrate spaces in OZ
-define(OZ_SPACES_LIST, oz_spaces_list).
-define(OZ_SPACES_VIEW, oz_spaces_view).
-define(OZ_SPACES_CREATE, oz_spaces_create).
-define(OZ_SPACES_UPDATE, oz_spaces_update).
-define(OZ_SPACES_DELETE, oz_spaces_delete).
-define(OZ_SPACES_VIEW_PRIVILEGES, oz_spaces_view_privileges).
-define(OZ_SPACES_SET_PRIVILEGES, oz_spaces_set_privileges).
-define(OZ_SPACES_LIST_RELATIONSHIPS, oz_spaces_list_relationships).
-define(OZ_SPACES_ADD_RELATIONSHIPS, oz_spaces_add_relationships).
-define(OZ_SPACES_REMOVE_RELATIONSHIPS, oz_spaces_remove_relationships).

%% Privileges to administrate shares in OZ
-define(OZ_SHARES_LIST, oz_shares_list).
-define(OZ_SHARES_VIEW, oz_shares_view).
-define(OZ_SHARES_CREATE, oz_shares_create).
-define(OZ_SHARES_UPDATE, oz_shares_update).
-define(OZ_SHARES_DELETE, oz_shares_delete).

%% Privileges to administrate providers in OZ
-define(OZ_PROVIDERS_LIST, oz_providers_list).
-define(OZ_PROVIDERS_VIEW, oz_providers_view).
-define(OZ_PROVIDERS_UPDATE, oz_providers_update).
-define(OZ_PROVIDERS_DELETE, oz_providers_delete).
-define(OZ_PROVIDERS_LIST_RELATIONSHIPS, oz_providers_list_relationships).
-define(OZ_PROVIDERS_INVITE, oz_providers_invite).

%% Privileges to administrate handle services in OZ
-define(OZ_HANDLE_SERVICES_LIST, oz_handle_services_list).
-define(OZ_HANDLE_SERVICES_VIEW, oz_handle_services_view).
-define(OZ_HANDLE_SERVICES_CREATE, oz_handle_services_create).
-define(OZ_HANDLE_SERVICES_UPDATE, oz_handle_services_update).
-define(OZ_HANDLE_SERVICES_DELETE, oz_handle_services_delete).
-define(OZ_HANDLE_SERVICES_VIEW_PRIVILEGES, oz_handle_services_view_privileges).
-define(OZ_HANDLE_SERVICES_SET_PRIVILEGES, oz_handle_services_set_privileges).
-define(OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS, oz_handle_services_list_relationships).
-define(OZ_HANDLE_SERVICES_ADD_RELATIONSHIPS, oz_handle_services_add_relationships).
-define(OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS, oz_handle_services_remove_relationships).

%% Privileges to administrate handles in OZ
-define(OZ_HANDLES_LIST, oz_handles_list).
-define(OZ_HANDLES_VIEW, oz_handles_view).
-define(OZ_HANDLES_CREATE, oz_handles_create).
-define(OZ_HANDLES_UPDATE, oz_handles_update).
-define(OZ_HANDLES_DELETE, oz_handles_delete).
-define(OZ_HANDLES_VIEW_PRIVILEGES, oz_handles_view_privileges).
-define(OZ_HANDLES_SET_PRIVILEGES, oz_handles_set_privileges).
-define(OZ_HANDLES_LIST_RELATIONSHIPS, oz_handles_list_relationships).
-define(OZ_HANDLES_ADD_RELATIONSHIPS, oz_handles_add_relationships).
-define(OZ_HANDLES_REMOVE_RELATIONSHIPS, oz_handles_remove_relationships).

%% Privileges to administrate harvesters in OZ
-define(OZ_HARVESTERS_LIST, oz_harvesters_list).
-define(OZ_HARVESTERS_VIEW, oz_harvesters_view).
-define(OZ_HARVESTERS_CREATE, oz_harvesters_create).
-define(OZ_HARVESTERS_UPDATE, oz_harvesters_update).
-define(OZ_HARVESTERS_DELETE, oz_harvesters_delete).
-define(OZ_HARVESTERS_VIEW_PRIVILEGES, oz_harvesters_view_privileges).
-define(OZ_HARVESTERS_SET_PRIVILEGES, oz_harvesters_set_privileges).
-define(OZ_HARVESTERS_LIST_RELATIONSHIPS, oz_harvesters_list_relationships).
-define(OZ_HARVESTERS_ADD_RELATIONSHIPS, oz_harvesters_add_relationships).
-define(OZ_HARVESTERS_REMOVE_RELATIONSHIPS, oz_harvesters_remove_relationships).

%% Privileges to administrate clusters in OZ
-define(OZ_CLUSTERS_LIST, oz_clusters_list).
-define(OZ_CLUSTERS_VIEW, oz_clusters_view).
-define(OZ_CLUSTERS_UPDATE, oz_clusters_update).
-define(OZ_CLUSTERS_VIEW_PRIVILEGES, oz_clusters_view_privileges).
-define(OZ_CLUSTERS_SET_PRIVILEGES, oz_clusters_set_privileges).
-define(OZ_CLUSTERS_LIST_RELATIONSHIPS, oz_clusters_list_relationships).
-define(OZ_CLUSTERS_ADD_RELATIONSHIPS, oz_clusters_add_relationships).
-define(OZ_CLUSTERS_REMOVE_RELATIONSHIPS, oz_clusters_remove_relationships).


-endif.

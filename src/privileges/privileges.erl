%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module describing user privileges in group and Space.
%%%-------------------------------------------------------------------
-module(privileges).
-author("Konrad Zemek").


-export([space_user/0, space_manager/0, space_admin/0, space_privileges/0]).
-export([group_user/0, group_manager/0, group_admin/0, group_privileges/0]).
-export([handle_service_user/0, handle_service_admin/0,
    handle_service_privileges/0]).
-export([handle_user/0, handle_admin/0, handle_privileges/0]).
-export([oz_viewer/0, oz_privileges/0]).

%% User privileges with regards to Space management.
-type space_privilege() :: space_invite_user | space_remove_user |
space_invite_group | space_remove_group | space_set_privileges |
space_remove | space_add_provider | space_remove_provider |
space_change_data | space_view_data | space_manage_shares | space_write_files.

%% User privileges with regards to group management.
-type group_privilege() :: group_change_data | group_invite_user |
group_remove_user | group_join_space | group_create_space |
group_set_privileges | group_remove | group_leave_space |
group_view_data | group_create_space_token |
group_join_group | group_invite_group | group_remove_group.

%% User privileges with regards to handle service.
-type handle_service_privilege() ::
%%register_handle_service | list_handle_services | % we may need those
%% privileges for admins in oz_privileges
delete_handle_service | modify_handle_service | view_handle_service |
register_handle.

%% User privileges with regards to handle.
-type handle_privilege() :: delete_handle | modify_handle |
view_handle.

%% User/group privileges to admin OZ API
-type oz_privilege() :: view_privileges | set_privileges | list_spaces |
list_providers | list_providers_of_space | add_member_to_space |
remove_member_from_space.

-export_type([
    space_privilege/0,
    group_privilege/0,
    handle_service_privilege/0,
    handle_privilege/0,
    oz_privilege/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc A privilege level of a Space user.
%%--------------------------------------------------------------------
-spec space_user() -> [space_privilege()].
space_user() ->
    [space_view_data, space_write_files].

%%--------------------------------------------------------------------
%% @doc A privilege level of a Space manager.
%%--------------------------------------------------------------------
-spec space_manager() -> [space_privilege()].
space_manager() ->
    ordsets:union(
        space_user(),
        ordsets:from_list([
            space_invite_user,
            space_remove_user,
            space_invite_group,
            space_remove_group,
            space_manage_shares
        ])
    ).

%%--------------------------------------------------------------------
%% @doc A privilege level of a Space administrator. This level contains all
%% atoms representing space privileges.
%% @end
%%--------------------------------------------------------------------
-spec space_admin() -> [space_privilege()].
space_admin() ->
    ordsets:union(
        space_manager(),
        ordsets:from_list([
            space_add_provider,
            space_remove_provider,
            space_set_privileges,
            space_change_data,
            space_remove
        ])
    ).

%%--------------------------------------------------------------------
%% @doc All atoms representing space privileges.
%% @equiv space_admin()
%%--------------------------------------------------------------------
-spec space_privileges() -> [space_privilege()].
space_privileges() ->
    space_admin().


%%--------------------------------------------------------------------
%% @doc A privilege level of a group user.
%%--------------------------------------------------------------------
-spec group_user() -> [group_privilege()].
group_user() ->
    [group_view_data].

%%--------------------------------------------------------------------
%% @doc A privilege level of a group manager.
%%--------------------------------------------------------------------
-spec group_manager() -> [group_privilege()].
group_manager() ->
    ordsets:union(
        group_user(),
        ordsets:from_list([
            group_invite_user,
            group_remove_user,
            group_invite_group,
            group_remove_group
        ])
    ).

%%--------------------------------------------------------------------
%% @doc A privilege level of a group administrator. This level contains all
%% atoms representing group privileges.
%% @end
%%--------------------------------------------------------------------
-spec group_admin() -> [group_privilege()].
group_admin() ->
    ordsets:union(
        group_manager(),
        ordsets:from_list([
            group_create_space,
            group_create_space_token,
            group_join_space,
            group_leave_space,
            group_set_privileges,
            group_change_data,
            group_remove,
            group_join_group
        ])
    ).

%%--------------------------------------------------------------------
%% @doc All atoms representing group privileges.
%% @equiv group_admin()
%%--------------------------------------------------------------------
-spec group_privileges() -> [group_privilege()].
group_privileges() ->
    group_admin().


%%--------------------------------------------------------------------
%% @doc A privilege level of a handle_service user.
%%--------------------------------------------------------------------
-spec handle_service_user() -> [handle_service_privilege()].
handle_service_user() ->
    [view_handle_service, register_handle].

%%--------------------------------------------------------------------
%% @doc A privilege level of a handle_service administrator. This level contains all
%% atoms representing handle_service privileges.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_admin() -> [handle_service_privilege()].
handle_service_admin() ->
    ordsets:union(
        handle_service_user(),
        ordsets:from_list([
%%            register_handle_service, % we may need those privileges for admins in oz_privileges
%%            list_handle_services,
            delete_handle_service,
            modify_handle_service,
            view_handle_service
        ])
    ).

%%--------------------------------------------------------------------
%% @doc All atoms representing handle_service privileges.
%% @equiv handle_service_admin()
%%--------------------------------------------------------------------
-spec handle_service_privileges() -> [handle_service_privilege()].
handle_service_privileges() ->
    handle_service_admin().


%%--------------------------------------------------------------------
%% @doc A privilege level of a handle user.
%%--------------------------------------------------------------------
-spec handle_user() -> [handle_privilege()].
handle_user() ->
    [view_handle].

%%--------------------------------------------------------------------
%% @doc A privilege level of a handle administrator. This level contains all
%% atoms representing handle privileges.
%% @end
%%--------------------------------------------------------------------
-spec handle_admin() -> [handle_privilege()].
handle_admin() ->
    ordsets:union(
        handle_user(),
        ordsets:from_list([
            list_handles,
            delete_handle,
            modify_handle,
            view_handle
        ])
    ).

%%--------------------------------------------------------------------
%% @doc All atoms representing handle privileges.
%% @equiv handle_admin()
%%--------------------------------------------------------------------
-spec handle_privileges() -> [handle_privilege()].
handle_privileges() ->
    handle_admin().


%%--------------------------------------------------------------------
%% @doc All view privileges in OZ API.
%%--------------------------------------------------------------------
-spec oz_viewer() -> [oz_privilege()].
oz_viewer() -> [
    list_users,
    list_users_of_provider,
    list_groups,
    list_groups_of_provider,
    list_spaces,
    list_spaces_of_provider,
    list_providers,
    list_providers_of_space
].


%%--------------------------------------------------------------------
%% @doc All OZ API privileges.
%%--------------------------------------------------------------------
-spec oz_privileges() -> [oz_privilege()].
oz_privileges() ->
    ordsets:union(
        oz_viewer(),
        ordsets:from_list([
            view_privileges,
            set_privileges,
            add_member_to_space,
            remove_member_from_space
        ])
    ).


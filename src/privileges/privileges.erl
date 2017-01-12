%%%-------------------------------------------------------------------
%%% @author Konrad Zemek, Lukasz Opiola
%%% @copyright (C): 2014-2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% The module describes privileges of users and groups in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(privileges).
-author("Konrad Zemek").
-author("Lukasz Opiola").

-include("privileges.hrl").

-type privileges(Type) :: ordsets:ordset(Type).

%% User privileges with regards to group management.
-type group_privilege() :: ?GROUP_VIEW | ?GROUP_UPDATE | ?GROUP_DELETE |
?GROUP_SET_PRIVILEGES |
?GROUP_INVITE_USER | ?GROUP_REMOVE_USER |
?GROUP_JOIN_GROUP | ?GROUP_LEAVE_GROUP |
?GROUP_INVITE_GROUP | ?GROUP_REMOVE_GROUP |
?GROUP_CREATE_SPACE | ?GROUP_JOIN_SPACE | ?GROUP_LEAVE_SPACE |
?GROUP_LEAVE_HANDLE_SERVICE | ?GROUP_LEAVE_HANDLE.


% Group privileges of members (users or groups)

%% User privileges with regards to Space management.
-type space_privilege() :: ?SPACE_VIEW | ?SPACE_UPDATE | ?SPACE_DELETE |
?SPACE_SET_PRIVILEGES |
?SPACE_WRITE_DATA | ?SPACE_MANAGE_SHARES |
?SPACE_INVITE_USER | ?SPACE_REMOVE_USER |
?SPACE_INVITE_GROUP | ?SPACE_REMOVE_GROUP |
?SPACE_INVITE_PROVIDER | ?SPACE_REMOVE_PROVIDER.

%% User privileges with regards to handle service.
-type handle_service_privilege() ::
%%register_handle_service | list_handle_services | % we may need those
%% privileges for admins in oz_privileges
?HANDLE_SERVICE_VIEW | ?HANDLE_SERVICE_UPDATE | ?HANDLE_SERVICE_DELETE |
?HANDLE_SERVICE_REGISTER_HANDLE | ?HANDLE_SERVICE_LIST_HANDLES.

%% User privileges with regards to handle.
-type handle_privilege() :: ?HANDLE_VIEW | ?HANDLE_UPDATE | ?HANDLE_DELETE.

%% User/group privileges to admin OZ API
-type oz_privilege() :: ?OZ_VIEW_PRIVILEGES | ?OZ_SET_PRIVILEGES |
?OZ_USERS_LIST | ?OZ_USERS_DELETE |
?OZ_GROUPS_LIST | ?OZ_GROUPS_LIST_USERS | ?OZ_GROUPS_LIST_GROUPS |
?OZ_GROUPS_ADD_MEMBERS | ?OZ_GROUPS_REMOVE_MEMBERS |
?OZ_SPACES_LIST | ?OZ_SPACES_LIST_USERS | ?OZ_SPACES_LIST_GROUPS |
?OZ_SPACES_LIST_PROVIDERS | ?OZ_SPACES_ADD_MEMBERS | ?OZ_SPACES_REMOVE_MEMBERS |
?OZ_SHARES_LIST |
?OZ_PROVIDERS_LIST | ?OZ_PROVIDERS_LIST_USERS | ?OZ_PROVIDERS_LIST_GROUPS |
?OZ_PROVIDERS_LIST_SPACES | ?OZ_PROVIDERS_DELETE |
?OZ_HANDLES_LIST |
?OZ_HANDLE_SERVICES_LIST.

-export_type([
    privileges/1,
    space_privilege/0,
    group_privilege/0,
    handle_service_privilege/0,
    handle_privilege/0,
    oz_privilege/0
]).

% Privileges manipulation
-export([from_list/1, union/2, subtract/2]).
% Privileges in the system
-export([group_user/0, group_manager/0, group_admin/0, group_privileges/0]).
-export([space_user/0, space_manager/0, space_admin/0, space_privileges/0]).
-export([handle_service_user/0, handle_service_admin/0,
    handle_service_privileges/0]).
-export([handle_user/0, handle_admin/0, handle_privileges/0]).
-export([oz_viewer/0, oz_admin/0, oz_privileges/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns privileges from list. It is required so that privileges are
%% always sorted.
%% @end
%%--------------------------------------------------------------------
-spec from_list(PrivilegesList :: [atom()]) -> privileges(atom()).
from_list(PrivilegesList) ->
    ordsets:from_list(PrivilegesList).


%%--------------------------------------------------------------------
%% @doc Returns privileges from list. It is required so that privileges are
%% always sorted.
%% @end
%%--------------------------------------------------------------------
-spec union(PrivsA :: [atom()], PrivsB :: [atom()]) -> privileges(atom()).
union(PrivilegesA, PrivilegesB) ->
    ordsets:union(from_list(PrivilegesA), from_list(PrivilegesB)).


%%--------------------------------------------------------------------
%% @doc Returns privileges from list. It is required so that privileges are
%% always sorted.
%% @end
%%--------------------------------------------------------------------
-spec subtract(PrivsA :: [atom()], PrivsB :: [atom()]) -> privileges(atom()).
subtract(PrivilegesA, PrivilegesB) ->
    ordsets:subtract(from_list(PrivilegesA), from_list(PrivilegesB)).


%%--------------------------------------------------------------------
%% @doc A privilege level of a group user.
%%--------------------------------------------------------------------
-spec group_user() -> privileges(group_privilege()).
group_user() ->
    from_list([?GROUP_VIEW]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a group manager.
%%--------------------------------------------------------------------
-spec group_manager() -> privileges(group_privilege()).
group_manager() ->
    union(group_user(), [
        ?GROUP_INVITE_USER, ?GROUP_REMOVE_USER,
        ?GROUP_JOIN_GROUP, ?GROUP_LEAVE_GROUP,
        ?GROUP_INVITE_GROUP, ?GROUP_REMOVE_GROUP
    ]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a group administrator. This level contains all
%% atoms representing group privileges.
%% @end
%%--------------------------------------------------------------------
-spec group_admin() -> privileges(group_privilege()).
group_admin() ->
    union(group_manager(), [
        ?GROUP_UPDATE, ?GROUP_DELETE, ?GROUP_SET_PRIVILEGES,
        ?GROUP_CREATE_SPACE, ?GROUP_JOIN_SPACE, ?GROUP_LEAVE_SPACE,
        ?GROUP_LEAVE_HANDLE_SERVICE, ?GROUP_LEAVE_HANDLE
    ]).

%%--------------------------------------------------------------------
%% @doc All atoms representing group privileges.
%% @equiv group_admin()
%%--------------------------------------------------------------------
-spec group_privileges() -> privileges(group_privilege()).
group_privileges() ->
    group_admin().


%%--------------------------------------------------------------------
%% @doc A privilege level of a Space user.
%%--------------------------------------------------------------------
-spec space_user() -> privileges(space_privilege()).
space_user() ->
    from_list([?SPACE_VIEW, ?SPACE_WRITE_DATA]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a Space manager.
%%--------------------------------------------------------------------
-spec space_manager() -> privileges(space_privilege()).
space_manager() ->
    union(space_user(), [
        ?SPACE_INVITE_USER, ?SPACE_REMOVE_USER,
        ?SPACE_INVITE_GROUP, ?SPACE_REMOVE_GROUP,
        ?SPACE_MANAGE_SHARES
    ]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a Space administrator. This level contains all
%% atoms representing space privileges.
%% @end
%%--------------------------------------------------------------------
-spec space_admin() -> privileges(space_privilege()).
space_admin() ->
    union(space_manager(), [
        ?SPACE_UPDATE, ?SPACE_DELETE, ?SPACE_SET_PRIVILEGES,
        ?SPACE_INVITE_PROVIDER, ?SPACE_REMOVE_PROVIDER
    ]).

%%--------------------------------------------------------------------
%% @doc All atoms representing space privileges.
%% @equiv space_admin()
%%--------------------------------------------------------------------
-spec space_privileges() -> privileges(space_privilege()).
space_privileges() ->
    space_admin().


%%--------------------------------------------------------------------
%% @doc A privilege level of a handle_service user.
%%--------------------------------------------------------------------
-spec handle_service_user() -> privileges(handle_service_privilege()).
handle_service_user() ->
    from_list([?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a handle_service administrator. This level contains all
%% atoms representing handle_service privileges.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_admin() -> privileges(handle_service_privilege()).
handle_service_admin() ->
    union(handle_service_user(), [
%%        register_handle_service, % we may need those privileges for admins in oz_privileges
%%        list_handle_services,
        ?HANDLE_SERVICE_UPDATE,
        ?HANDLE_SERVICE_DELETE,
        ?HANDLE_SERVICE_LIST_HANDLES
    ]).

%%--------------------------------------------------------------------
%% @doc All atoms representing handle_service privileges.
%% @equiv handle_service_admin()
%%--------------------------------------------------------------------
-spec handle_service_privileges() -> privileges(handle_service_privilege()).
handle_service_privileges() ->
    handle_service_admin().


%%--------------------------------------------------------------------
%% @doc A privilege level of a handle user.
%%--------------------------------------------------------------------
-spec handle_user() -> privileges(handle_privilege()).
handle_user() ->
    from_list([?HANDLE_VIEW]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a handle administrator. This level contains all
%% atoms representing handle privileges.
%% @end
%%--------------------------------------------------------------------
-spec handle_admin() -> privileges(handle_privilege()).
handle_admin() ->
    union(handle_user(), [
        ?HANDLE_UPDATE,
        ?HANDLE_DELETE
    ]).

%%--------------------------------------------------------------------
%% @doc All atoms representing handle privileges.
%% @equiv handle_admin()
%%--------------------------------------------------------------------
-spec handle_privileges() -> privileges(handle_privilege()).
handle_privileges() ->
    handle_admin().


%%--------------------------------------------------------------------
%% @doc All view privileges in OZ API.
%%--------------------------------------------------------------------
-spec oz_viewer() -> privileges(oz_privilege()).
oz_viewer() ->
    from_list([
        ?OZ_USERS_LIST,

        ?OZ_GROUPS_LIST, ?OZ_GROUPS_LIST_USERS, ?OZ_GROUPS_LIST_GROUPS,

        ?OZ_SPACES_LIST, ?OZ_SPACES_LIST_USERS, ?OZ_SPACES_LIST_GROUPS,
        ?OZ_SPACES_LIST_PROVIDERS,

        ?OZ_SHARES_LIST,

        ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_LIST_USERS, ?OZ_PROVIDERS_LIST_GROUPS,
        ?OZ_PROVIDERS_LIST_SPACES,

        ?OZ_HANDLES_LIST,

        ?OZ_HANDLE_SERVICES_LIST
    ]).


%%--------------------------------------------------------------------
%% @doc Admin privileges in OZ API.
%%--------------------------------------------------------------------
-spec oz_admin() -> privileges(oz_privilege()).
oz_admin() ->
    union(oz_viewer(), [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES,
        ?OZ_USERS_DELETE,
        ?OZ_GROUPS_ADD_MEMBERS, ?OZ_GROUPS_REMOVE_MEMBERS,
        ?OZ_SPACES_ADD_MEMBERS, ?OZ_SPACES_REMOVE_MEMBERS,
        ?OZ_PROVIDERS_DELETE
    ]).


%%--------------------------------------------------------------------
%% @doc All OZ API privileges.
%% @equiv oz_admin()
%%--------------------------------------------------------------------
-spec oz_privileges() -> privileges(oz_privilege()).
oz_privileges() ->
    oz_admin().



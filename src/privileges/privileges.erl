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
-type group_privilege() ::
?GROUP_VIEW | ?GROUP_UPDATE | ?GROUP_DELETE |
?GROUP_VIEW_PRIVILEGES | ?GROUP_SET_PRIVILEGES |

?GROUP_INVITE_USER | ?GROUP_REMOVE_USER |

?GROUP_ADD_PARENT | ?GROUP_LEAVE_PARENT |

?GROUP_ADD_CHILD | ?GROUP_REMOVE_CHILD |

?GROUP_ADD_SPACE | ?GROUP_LEAVE_SPACE |

?GROUP_ADD_CLUSTER | ?GROUP_LEAVE_CLUSTER |

?GROUP_CREATE_HANDLE_SERVICE | ?GROUP_LEAVE_HANDLE_SERVICE |

?GROUP_CREATE_HANDLE | ?GROUP_LEAVE_HANDLE.

%% User privileges with regards to Space management.
-type space_privilege() ::
?SPACE_VIEW | ?SPACE_UPDATE | ?SPACE_DELETE |
?SPACE_VIEW_PRIVILEGES | ?SPACE_SET_PRIVILEGES |

?SPACE_READ_DATA | ?SPACE_WRITE_DATA |
?SPACE_MANAGE_SHARES |
?SPACE_MANAGE_INDEXES | ?SPACE_QUERY_INDEXES |
?SPACE_VIEW_STATISTICS |

?SPACE_VIEW_TRANSFERS |
?SPACE_SCHEDULE_REPLICATION | ?SPACE_CANCEL_REPLICATION |
?SPACE_SCHEDULE_EVICTION | ?SPACE_CANCEL_EVICTION |

?SPACE_INVITE_USER | ?SPACE_REMOVE_USER |

?SPACE_ADD_GROUP | ?SPACE_REMOVE_GROUP |

?SPACE_INVITE_PROVIDER | ?SPACE_REMOVE_PROVIDER.

%% User privileges with regards to handle service.
-type handle_service_privilege() ::
%%register_handle_service | list_handle_services | % we may need those
%% privileges for admins in oz_privileges
?HANDLE_SERVICE_VIEW | ?HANDLE_SERVICE_UPDATE | ?HANDLE_SERVICE_DELETE |
?HANDLE_SERVICE_REGISTER_HANDLE | ?HANDLE_SERVICE_LIST_HANDLES.

%% User privileges with regards to handle.
-type handle_privilege() :: ?HANDLE_VIEW | ?HANDLE_UPDATE | ?HANDLE_DELETE.

%% User privileges with regards to Cluster management.
-type cluster_privilege() ::
?CLUSTER_VIEW | ?CLUSTER_UPDATE | ?CLUSTER_DELETE |
?CLUSTER_VIEW_PRIVILEGES | ?CLUSTER_SET_PRIVILEGES |

?CLUSTER_INVITE_USER | ?CLUSTER_REMOVE_USER |

?CLUSTER_ADD_GROUP | ?CLUSTER_REMOVE_GROUP.

-type oz_privilege() ::
?OZ_VIEW_PRIVILEGES | ?OZ_SET_PRIVILEGES |

%% Privileges to administrate users in OZ
?OZ_USERS_LIST | ?OZ_USERS_VIEW |
?OZ_USERS_UPDATE | ?OZ_USERS_DELETE |
?OZ_USERS_LIST_RELATIONSHIPS | ?OZ_USERS_ADD_RELATIONSHIPS | ?OZ_USERS_REMOVE_RELATIONSHIPS |

%% Privileges to administrate groups in OZ
?OZ_GROUPS_LIST | ?OZ_GROUPS_VIEW |
?OZ_GROUPS_CREATE | ?OZ_GROUPS_UPDATE | ?OZ_GROUPS_DELETE |
?OZ_GROUPS_VIEW_PRIVILEGES | ?OZ_GROUPS_SET_PRIVILEGES |
?OZ_GROUPS_LIST_RELATIONSHIPS | ?OZ_GROUPS_ADD_RELATIONSHIPS | ?OZ_GROUPS_REMOVE_RELATIONSHIPS |

%% Privileges to administrate spaces in OZ
?OZ_SPACES_LIST | ?OZ_SPACES_VIEW |
?OZ_SPACES_CREATE | ?OZ_SPACES_UPDATE | ?OZ_SPACES_DELETE |
?OZ_SPACES_VIEW_PRIVILEGES | ?OZ_SPACES_SET_PRIVILEGES |
?OZ_SPACES_LIST_RELATIONSHIPS | ?OZ_SPACES_ADD_RELATIONSHIPS | ?OZ_SPACES_REMOVE_RELATIONSHIPS |

%% Privileges to administrate shares in OZ
?OZ_SHARES_LIST | ?OZ_SHARES_VIEW |
?OZ_SHARES_CREATE | ?OZ_SHARES_UPDATE | ?OZ_SHARES_DELETE |

%% Privileges to administrate providers in OZ
?OZ_PROVIDERS_LIST | ?OZ_PROVIDERS_VIEW |
?OZ_PROVIDERS_UPDATE | ?OZ_PROVIDERS_DELETE |
?OZ_PROVIDERS_LIST_RELATIONSHIPS | ?OZ_PROVIDERS_INVITE |

%% Privileges to administrate handle services in OZ
?OZ_HANDLE_SERVICES_LIST | ?OZ_HANDLE_SERVICES_VIEW |
?OZ_HANDLE_SERVICES_CREATE | ?OZ_HANDLE_SERVICES_UPDATE | ?OZ_HANDLE_SERVICES_DELETE |
?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES | ?OZ_HANDLE_SERVICES_SET_PRIVILEGES |
?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS | ?OZ_HANDLE_SERVICES_ADD_RELATIONSHIPS | ?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS |

%% Privileges to administrate handles in OZ
?OZ_HANDLES_LIST | ?OZ_HANDLES_VIEW |
?OZ_HANDLES_CREATE | ?OZ_HANDLES_UPDATE | ?OZ_HANDLES_DELETE |
?OZ_HANDLES_VIEW_PRIVILEGES | ?OZ_HANDLES_SET_PRIVILEGES |
?OZ_HANDLES_LIST_RELATIONSHIPS | ?OZ_HANDLES_ADD_RELATIONSHIPS | ?OZ_HANDLES_REMOVE_RELATIONSHIPS |

%% Privileges to administrate clusters in OZ
?OZ_CLUSTERS_LIST | ?OZ_CLUSTERS_VIEW |
?OZ_CLUSTERS_UPDATE | ?OZ_CLUSTERS_DELETE |
?OZ_CLUSTERS_VIEW_PRIVILEGES | ?OZ_CLUSTERS_SET_PRIVILEGES |
?OZ_CLUSTERS_LIST_RELATIONSHIPS | ?OZ_CLUSTERS_ADD_RELATIONSHIPS | ?OZ_CLUSTERS_REMOVE_RELATIONSHIPS.


-export_type([
    privileges/1,
    space_privilege/0,
    group_privilege/0,
    handle_service_privilege/0,
    handle_privilege/0,
    cluster_privilege/0,
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
-export([cluster_user/0, cluster_manager/0, cluster_admin/0, cluster_privileges/0]).
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
        ?GROUP_VIEW_PRIVILEGES,
        ?GROUP_INVITE_USER, ?GROUP_REMOVE_USER,
        ?GROUP_ADD_PARENT, ?GROUP_LEAVE_PARENT,
        ?GROUP_ADD_CHILD, ?GROUP_REMOVE_CHILD
    ]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a group administrator. This level contains all
%% atoms representing group privileges.
%% @end
%%--------------------------------------------------------------------
-spec group_admin() -> privileges(group_privilege()).
group_admin() ->
    union(group_manager(), [
        ?GROUP_UPDATE, ?GROUP_DELETE,
        ?GROUP_SET_PRIVILEGES,
        ?GROUP_ADD_SPACE, ?GROUP_LEAVE_SPACE,
        ?GROUP_ADD_CLUSTER, ?GROUP_LEAVE_CLUSTER,
        ?GROUP_CREATE_HANDLE_SERVICE, ?GROUP_LEAVE_HANDLE_SERVICE,
        ?GROUP_CREATE_HANDLE, ?GROUP_LEAVE_HANDLE
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
    from_list([
        ?SPACE_VIEW,
        ?SPACE_READ_DATA, ?SPACE_WRITE_DATA,
        ?SPACE_VIEW_TRANSFERS
    ]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a Space manager.
%%--------------------------------------------------------------------
-spec space_manager() -> privileges(space_privilege()).
space_manager() ->
    union(space_user(), [
        ?SPACE_VIEW_PRIVILEGES,
        ?SPACE_INVITE_USER, ?SPACE_REMOVE_USER,
        ?SPACE_ADD_GROUP, ?SPACE_REMOVE_GROUP,
        ?SPACE_MANAGE_SHARES,
        ?SPACE_QUERY_INDEXES,
        ?SPACE_VIEW_STATISTICS,
        ?SPACE_SCHEDULE_REPLICATION
    ]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a Space administrator. This level contains all
%% atoms representing space privileges.
%% @end
%%--------------------------------------------------------------------
-spec space_admin() -> privileges(space_privilege()).
space_admin() ->
    union(space_manager(), [
        ?SPACE_UPDATE, ?SPACE_DELETE,
        ?SPACE_SET_PRIVILEGES,
        ?SPACE_INVITE_PROVIDER, ?SPACE_REMOVE_PROVIDER,
        ?SPACE_MANAGE_INDEXES,
        ?SPACE_CANCEL_REPLICATION,
        ?SPACE_SCHEDULE_EVICTION, ?SPACE_CANCEL_EVICTION
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
%% @doc A privilege level of a Cluster user.
%%--------------------------------------------------------------------
-spec cluster_user() -> privileges(cluster_privilege()).
cluster_user() ->
    from_list([
        ?CLUSTER_VIEW
    ]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a Cluster manager.
%%--------------------------------------------------------------------
-spec cluster_manager() -> privileges(cluster_privilege()).
cluster_manager() ->
    union(cluster_user(), [
        ?CLUSTER_INVITE_USER, ?CLUSTER_REMOVE_USER,
        ?CLUSTER_ADD_GROUP, ?CLUSTER_REMOVE_GROUP
    ]).

%%--------------------------------------------------------------------
%% @doc A privilege level of a Cluster administrator. This level contains all
%% atoms representing cluster privileges.
%% @end
%%--------------------------------------------------------------------
-spec cluster_admin() -> privileges(cluster_privilege()).
cluster_admin() ->
    union(cluster_manager(), [
        ?CLUSTER_UPDATE, ?CLUSTER_DELETE,
        ?CLUSTER_VIEW_PRIVILEGES, ?CLUSTER_SET_PRIVILEGES
    ]).

%%--------------------------------------------------------------------
%% @doc All atoms representing Cluster privileges.
%% @equiv cluster_admin()
%%--------------------------------------------------------------------
-spec cluster_privileges() -> privileges(cluster_privilege()).
cluster_privileges() ->
    cluster_admin().


%%--------------------------------------------------------------------
%% @doc All view privileges in OZ API.
%%--------------------------------------------------------------------
-spec oz_viewer() -> privileges(oz_privilege()).
oz_viewer() ->
    from_list([
        ?OZ_USERS_LIST, ?OZ_USERS_VIEW, ?OZ_USERS_LIST_RELATIONSHIPS,

        ?OZ_GROUPS_LIST, ?OZ_GROUPS_VIEW, ?OZ_GROUPS_LIST_RELATIONSHIPS,

        ?OZ_SPACES_LIST, ?OZ_SPACES_VIEW, ?OZ_SPACES_LIST_RELATIONSHIPS,

        ?OZ_SHARES_LIST, ?OZ_SHARES_VIEW,

        ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_VIEW, ?OZ_PROVIDERS_LIST_RELATIONSHIPS,

        ?OZ_HANDLE_SERVICES_LIST, ?OZ_HANDLE_SERVICES_VIEW, ?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS,

        ?OZ_HANDLES_LIST, ?OZ_HANDLES_VIEW, ?OZ_HANDLES_LIST_RELATIONSHIPS,

        ?OZ_CLUSTERS_LIST, ?OZ_CLUSTERS_VIEW, ?OZ_CLUSTERS_LIST_RELATIONSHIPS
    ]).


%%--------------------------------------------------------------------
%% @doc Admin privileges in OZ API.
%%--------------------------------------------------------------------
-spec oz_admin() -> privileges(oz_privilege()).
oz_admin() ->
    union(oz_viewer(), [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES,

        ?OZ_USERS_UPDATE, ?OZ_USERS_DELETE,
        ?OZ_USERS_ADD_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS,

        ?OZ_GROUPS_CREATE, ?OZ_GROUPS_UPDATE, ?OZ_GROUPS_DELETE,
        ?OZ_GROUPS_VIEW_PRIVILEGES, ?OZ_GROUPS_SET_PRIVILEGES,
        ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS,

        ?OZ_SPACES_CREATE, ?OZ_SPACES_UPDATE, ?OZ_SPACES_DELETE,
        ?OZ_SPACES_VIEW_PRIVILEGES, ?OZ_SPACES_SET_PRIVILEGES,
        ?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS,

        ?OZ_SHARES_CREATE, ?OZ_SHARES_UPDATE, ?OZ_SHARES_DELETE,

        ?OZ_PROVIDERS_UPDATE, ?OZ_PROVIDERS_DELETE, ?OZ_PROVIDERS_INVITE,

        ?OZ_HANDLE_SERVICES_CREATE, ?OZ_HANDLE_SERVICES_UPDATE, ?OZ_HANDLE_SERVICES_DELETE,
        ?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES, ?OZ_HANDLE_SERVICES_SET_PRIVILEGES,
        ?OZ_HANDLE_SERVICES_ADD_RELATIONSHIPS, ?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS,

        ?OZ_HANDLES_CREATE, ?OZ_HANDLES_UPDATE, ?OZ_HANDLES_DELETE,
        ?OZ_HANDLES_VIEW_PRIVILEGES, ?OZ_HANDLES_SET_PRIVILEGES,
        ?OZ_HANDLES_ADD_RELATIONSHIPS, ?OZ_HANDLES_REMOVE_RELATIONSHIPS,


        ?OZ_CLUSTERS_UPDATE, ?OZ_CLUSTERS_DELETE,
        ?OZ_CLUSTERS_VIEW_PRIVILEGES, ?OZ_CLUSTERS_SET_PRIVILEGES,
        ?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_CLUSTERS_REMOVE_RELATIONSHIPS
    ]).


%%--------------------------------------------------------------------
%% @doc All OZ API privileges.
%% @equiv oz_admin()
%%--------------------------------------------------------------------
-spec oz_privileges() -> privileges(oz_privilege()).
oz_privileges() ->
    oz_admin().



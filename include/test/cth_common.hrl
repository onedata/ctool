%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This file contains common macros used in ct hooks.
%%% @end
%%%-------------------------------------------------------------------
-author("Jakub Kudzia").

-ifndef(CTH_COMMON_HRL).
-define(CTH_COMMON_HRL, 1).

%% nodes on which mock_manager will be started
-define(CTH_MOCK_MANAGER_NODES, [
    op_worker_nodes,
    cluster_worker_nodes,
    oz_worker_nodes,
    onepanel_nodes,
    all_nodes % this key is used in onepanel
]).


%% Priorities for cth modules.
%% Modules installation order is determined by increasing priorities (lowest first).
-define(CTH_LOGGER_PRIORITY, 0).
-define(CTH_ENV_UP_PRIORITY, 1).
-define(CTH_MOCK_PRIORITY, 2).

-endif.
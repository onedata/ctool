%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains global definitions of component names, macros and types
%%% used in applications based on cluster managers - node_managers distribution model.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(GLOBAL_DEFINITIONS_CTOOL_HRL).
-define(GLOBAL_DEFINITIONS_CTOOL_HRL, 1).

%%%===================================================================
%%% Global names
%%%===================================================================

%% Global name of gen_server that provides ccm functionality.
-define(CCM, cluster_manager).

%% Local name (name and node is used to identify it) of gen_server that
%% coordinates node life cycle.
-define(NODE_MANAGER_NAME, node_manager).

-endif.

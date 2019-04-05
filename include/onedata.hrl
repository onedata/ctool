%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains macros related to Onedata services and clusters.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(ONEDATA_CTOOL_HRL).
-define(ONEDATA_CTOOL_HRL, 1).

%% Types of clusters in Onedata - every cluster is made up of services
-define(ONEZONE, onezone).
-define(ONEPROVIDER, oneprovider).

-define(ONEZONE_CLUSTER_ID, <<"onezone">>).
%% Oneprovider's ClusterId is the same as ProviderId

%% Services in Onedata
-define(OZ_WORKER, oz_worker).
-define(OZ_PANEL, oz_panel).
-define(OP_WORKER, op_worker).
-define(OP_PANEL, op_panel).

%% Types of services in Onedata
-define(WORKER, worker).
-define(ONEPANEL, onepanel).

-endif.

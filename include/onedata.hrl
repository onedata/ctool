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

%% Types of products in Onedata
%% ONEZONE and ONEPROVIDER are also used as cluster types
-define(ONEZONE, onezone).
-define(ONEPROVIDER, oneprovider).
-define(ONECLIENT, oneclient).

-define(ONEZONE_CLUSTER_ID, <<"onezone">>).
%% Oneprovider's ClusterId is the same as ProviderId

%% Services in Onedata - every cluster is made up of services
-define(OZ_WORKER, oz_worker).
-define(OZ_PANEL, oz_panel).
-define(OP_WORKER, op_worker).
-define(OP_PANEL, op_panel).
-define(HARVESTER, harvester).

%% Types of services in Onedata
-define(WORKER, worker).
-define(ONEPANEL, onepanel).

%% Graphical User Interfaces in Onedata
-define(OZ_WORKER_GUI, oz_worker_gui).
-define(OP_WORKER_GUI, op_worker_gui).
-define(ONEPANEL_GUI, onepanel_gui).
-define(HARVESTER_GUI, harvester_gui).

%% Mode in which GUI is operating. Unified GUI is served by Onezone for all
%% services, emergency GUI refers only to limited Onepanel interface used for
%% emergencies (typically when Onezone is not available).
-define(UNIFIED, unified).
-define(EMERGENCY, emergency).

%% Macros with known major release lines of Onedata software
-define(LINE_19_02, <<"19.02.*">>).
-define(LINE_20_02(Suffix), <<"20.02.", Suffix/binary>>).
-define(LINE_20_02, <<"20.02.*">>).
-define(LINE_21_02(Suffix), <<"21.02.", Suffix/binary>>).
-define(LINE_21_02, <<"21.02.*">>).

%% Macros used for file manipulation
-define(DIRECTORY_SEPARATOR, "/").
-define(DIRECTORY_SEPARATOR_CHAR, $/).
-define(DIRECTORY_SEPARATOR_BIN, <<?DIRECTORY_SEPARATOR>>).

-define(CURRENT_DIRECTORY, ".").
-define(PARENT_DIRECTORY, "..").

-endif.

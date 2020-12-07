%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains type definitions related to services and clusters in
%%% Onedata, along with corresponding convenience functions.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata).
-author("Lukasz Opiola").

-include("onedata.hrl").

%% Types of Onedata products
-type product() :: ?ONEZONE | ?ONEPROVIDER | ?ONECLIENT.
%% Release version of a product, for example <<"19.02.0-beta1">>
-type release_version() :: binary().
%% Types of clusters in Onedata - every cluster is made up of services
-type cluster_type() :: ?ONEZONE | ?ONEPROVIDER.
%% Services in Onedata
-type service() :: ?OZ_WORKER | ?OZ_PANEL | ?OP_WORKER | ?OP_PANEL.
%% Types of services in Onedata
-type service_type() :: ?WORKER | ?ONEPANEL.
%% Identifier of a service instance - either Oneprovider Id or ?ONEZONE_CLUSTER_ID
-type service_id() :: binary().
%% Graphical User Interfaces in Onedata
-type gui() :: ?OZ_WORKER_GUI | ?OP_WORKER_GUI | ?ONEPANEL_GUI | ?HARVESTER_GUI.
%% SHA-256 checksum of a GUI package tarball, used for integrity checks
-type gui_hash() :: binary().
%% Mode in which GUI is operating. Unified GUI is served by Onezone for all
%% services, emergency GUI refers only to limited Onepanel interface used for
%% emergencies (typically when Onezone is not available).
-type gui_mode() :: ?UNIFIED | ?EMERGENCY.

-export_type([product/0, release_version/0]).
-export_type([cluster_type/0]).
-export_type([service/0, service_type/0, service_id/0]).
-export_type([gui/0, gui_hash/0, gui_mode/0]).

%% API
-export([service_by_type/2, service_type/1, service_to_cluster_type/1]).
-export([service_gui/1, service_by_gui/2]).
-export([service_shortname/1, service_by_shortname/1]).
-export([gui_prefix/1, gui_by_prefix/1]).
-export([compare_major_versions/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec service_by_type(cluster_type(), service_type()) -> service().
service_by_type(?ONEZONE, ?WORKER) -> ?OZ_WORKER;
service_by_type(?ONEZONE, ?ONEPANEL) -> ?OZ_PANEL;
service_by_type(?ONEPROVIDER, ?WORKER) -> ?OP_WORKER;
service_by_type(?ONEPROVIDER, ?ONEPANEL) -> ?OP_PANEL;
service_by_type(_, _) -> error(badarg).


-spec service_type(service()) -> service_type().
service_type(?OZ_WORKER) -> ?WORKER;
service_type(?OZ_PANEL) -> ?ONEPANEL;
service_type(?OP_WORKER) -> ?WORKER;
service_type(?OP_PANEL) -> ?ONEPANEL;
service_type(_) -> error(badarg).


-spec service_to_cluster_type(service()) -> cluster_type().
service_to_cluster_type(?OZ_WORKER) -> ?ONEZONE;
service_to_cluster_type(?OZ_PANEL) -> ?ONEZONE;
service_to_cluster_type(?OP_WORKER) -> ?ONEPROVIDER;
service_to_cluster_type(?OP_PANEL) -> ?ONEPROVIDER;
service_to_cluster_type(_) -> error(badarg).


-spec service_gui(service()) -> gui().
service_gui(?OZ_WORKER) -> ?OZ_WORKER_GUI;
service_gui(?OP_WORKER) -> ?OP_WORKER_GUI;
service_gui(?OZ_PANEL) -> ?ONEPANEL_GUI;
service_gui(?OP_PANEL) -> ?ONEPANEL_GUI;
service_gui(_) -> error(badarg).


-spec service_by_gui(gui(), ClusterId :: binary()) -> service().
service_by_gui(?OZ_WORKER_GUI, _) -> ?OZ_WORKER;
service_by_gui(?OP_WORKER_GUI, _) -> ?OP_WORKER;
service_by_gui(?ONEPANEL_GUI, ?ONEZONE_CLUSTER_ID) -> ?OZ_PANEL;
service_by_gui(?ONEPANEL_GUI, _) -> ?OP_PANEL;
service_by_gui(_, _) -> error(badarg).


-spec service_shortname(service()) -> <<_:24>>.
service_shortname(?OZ_WORKER) -> <<"ozw">>;
service_shortname(?OP_WORKER) -> <<"opw">>;
service_shortname(?OZ_PANEL) -> <<"ozp">>;
service_shortname(?OP_PANEL) -> <<"opp">>;
service_shortname(_) -> error(badarg).


-spec service_by_shortname(<<_:24>>) -> service().
service_by_shortname(<<"ozw">>) -> ?OZ_WORKER;
service_by_shortname(<<"opw">>) -> ?OP_WORKER;
service_by_shortname(<<"ozp">>) -> ?OZ_PANEL;
service_by_shortname(<<"opp">>) -> ?OP_PANEL;
service_by_shortname(_) -> error(badarg).


-spec gui_prefix(gui()) -> binary().
gui_prefix(?OZ_WORKER_GUI) -> <<"ozw">>;
gui_prefix(?OP_WORKER_GUI) -> <<"opw">>;
gui_prefix(?ONEPANEL_GUI) -> <<"onp">>;
gui_prefix(?HARVESTER_GUI) -> <<"hrv">>;
gui_prefix(_) -> error(badarg).


-spec gui_by_prefix(binary()) -> gui().
gui_by_prefix(<<"ozw">>) -> ?OZ_WORKER_GUI;
gui_by_prefix(<<"opw">>) -> ?OP_WORKER_GUI;
gui_by_prefix(<<"onp">>) -> ?ONEPANEL_GUI;
gui_by_prefix(<<"hrv">>) -> ?HARVESTER_GUI;
gui_by_prefix(_) -> error(badarg).


-spec compare_major_versions(release_version(), release_version()) -> lower | equal | greater.
compare_major_versions(<<A:5/binary, _/binary>>, <<B:5/binary, _/binary>>) when A < B -> lower;
compare_major_versions(<<A:5/binary, _/binary>>, <<B:5/binary, _/binary>>) when A =:= B -> equal;
compare_major_versions(<<A:5/binary, _/binary>>, <<B:5/binary, _/binary>>) when A > B -> greater;
compare_major_versions(_, _) -> error(badarg).
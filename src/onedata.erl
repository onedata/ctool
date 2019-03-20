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

%% Types of clusters in Onedata - every cluster is made up of services
-type cluster_type() :: ?ONEZONE | ?ONEPROVIDER.
%% Services in Onedata
-type service() :: ?OZ_WORKER | ?OZ_PANEL | ?OP_WORKER | ?OP_PANEL.
%% Types of services in Onedata
-type service_type() :: ?WORKER | ?ONEPANEL.

-export_type([cluster_type/0, service/0, service_type/0]).

%% API
-export([service_shortname/1, service_from_shortname/1]).
-export([service_by_type/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec service_shortname(service()) -> binary().
service_shortname(?OZ_WORKER) -> <<"ozw">>;
service_shortname(?OZ_PANEL) -> <<"ozp">>;
service_shortname(?OP_WORKER) -> <<"opw">>;
service_shortname(?OP_PANEL) -> <<"opp">>;
service_shortname(_) -> error(badarg).


-spec service_from_shortname(binary()) -> service().
service_from_shortname(<<"ozw">>) -> ?OZ_WORKER;
service_from_shortname(<<"ozp">>) -> ?OZ_PANEL;
service_from_shortname(<<"opw">>) -> ?OP_WORKER;
service_from_shortname(<<"opp">>) -> ?OP_PANEL;
service_from_shortname(_) -> error(badarg).


-spec service_by_type(cluster_type(), service_type()) -> service().
service_by_type(?ONEZONE, ?WORKER) -> ?OZ_WORKER;
service_by_type(?ONEZONE, ?ONEPANEL) -> ?OZ_PANEL;
service_by_type(?ONEPROVIDER, ?WORKER) -> ?OP_WORKER;
service_by_type(?ONEPROVIDER, ?ONEPANEL) -> ?OP_PANEL;
service_by_type(_, _) -> error(badarg).

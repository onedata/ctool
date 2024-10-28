%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_node_not_compatible'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_node_not_compatible).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").
-include("onedata.hrl").

-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_NODE_NOT_COMPATIBLE(Hostname, NodeClusterType)) when
    NodeClusterType == ?ONEPROVIDER;
    NodeClusterType == ?ONEZONE
->
    #{
        <<"id">> => ?ERROR_NODE_NOT_COMPATIBLE_ID,
        <<"details">> => #{
            <<"hostname">> => Hostname, <<"clusterType">> => NodeClusterType},
        <<"description">> => ?fmt("Cannot add \"~ts\", it is a ~ts node.",
            [Hostname, NodeClusterType])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_NODE_NOT_COMPATIBLE_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Hostname = maps:get(<<"hostname">>, DetailsJson),
    ClusterTypeJson = maps:get(<<"clusterType">>, DetailsJson),
    ClusterType = binary_to_existing_atom(ClusterTypeJson, utf8),

    ?ERROR_NODE_NOT_COMPATIBLE(Hostname, ClusterType).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

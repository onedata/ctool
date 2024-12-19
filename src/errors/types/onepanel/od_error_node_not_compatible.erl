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

-type t() :: {error, #od_error{type :: ?MODULE}}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERR_NODE_NOT_COMPATIBLE(ErrorCtx, Hostname, NodeClusterType)) when
    NodeClusterType == ?ONEPROVIDER;
    NodeClusterType == ?ONEZONE
->
    #{
        <<"id">> => ?ERR_NODE_NOT_COMPATIBLE_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"hostname">> => Hostname, <<"clusterType">> => NodeClusterType
        },
        <<"description">> => od_error:format_description(
            "Cannot add \"~ts\", it is a ~ts node.",
            [Hostname, NodeClusterType]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_NODE_NOT_COMPATIBLE_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Hostname = maps:get(<<"hostname">>, DetailsJson),
    ClusterTypeJson = maps:get(<<"clusterType">>, DetailsJson),
    ClusterType = erlang:binary_to_existing_atom(ClusterTypeJson, utf8),

    ?ERR_NODE_NOT_COMPATIBLE(ErrorCtx, Hostname, ClusterType).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

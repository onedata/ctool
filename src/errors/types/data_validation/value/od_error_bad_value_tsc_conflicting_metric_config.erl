%%%-------------------------------------------------------------------
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for ?MODULE.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_value_tsc_conflicting_metric_config).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(TSName, MetricName, ExistingMConfig, ConflictingMConfig)) ->
    #{
        <<"id">> => ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID,
        <<"details">> => #{
            <<"timeSeriesName">> => TSName,
            <<"metricName">> => MetricName,
            <<"existingMetricConfig">> => jsonable_record:to_json(ExistingMConfig, metric_config),
            <<"conflictingMetricConfig">> => jsonable_record:to_json(ConflictingMConfig, metric_config)
        },
        <<"description">> => ?fmt(
            "Provided metric config for 'time series' ~ts and metric '~ts' conflicts with existing metric config (see details).", [
                TSName, MetricName
            ])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID, <<"details">> := #{
    <<"timeSeriesName">> := TSName,
    <<"metricName">> := MetricName,
    <<"existingMetricConfig">> := ExistingMetricConfig,
    <<"conflictingMetricConfig">> := ConflictingMetricConfig
}}) ->
    ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(
        TSName, MetricName,
        jsonable_record:from_json(ExistingMetricConfig, metric_config),
        jsonable_record:from_json(ConflictingMetricConfig, metric_config)
    ).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

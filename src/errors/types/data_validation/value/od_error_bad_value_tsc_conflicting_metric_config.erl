%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_value_tsc_conflicting_metric_config'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_value_tsc_conflicting_metric_config).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_MATCH(TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig)) ->
    ExistingMetricConfigJson = jsonable_record:to_json(ExistingMetricConfig, metric_config),
    ConflictingMetricConfigJson = jsonable_record:to_json(ConflictingMetricConfig, metric_config),

    #{
        <<"id">> => ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID,
        <<"details">> => #{
            <<"timeSeriesName">> => TimeSeriesName,
            <<"metricName">> => MetricName,
            <<"existingMetricConfig">> => ExistingMetricConfigJson,
            <<"conflictingMetricConfig">> => ConflictingMetricConfigJson
        },
        <<"description">> => od_error:format_description(
            "Provided metric config for 'time series' ~ts and metric '~ts' conflicts with existing metric config (see details).",
            [TimeSeriesName, MetricName]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    TimeSeriesName = maps:get(<<"timeSeriesName">>, DetailsJson),
    MetricName = maps:get(<<"metricName">>, DetailsJson),
    ExistingMetricConfigJson = maps:get(<<"existingMetricConfig">>, DetailsJson),
    ExistingMetricConfig = jsonable_record:from_json(ExistingMetricConfigJson, metric_config),
    ConflictingMetricConfigJson = maps:get(<<"conflictingMetricConfig">>, DetailsJson),
    ConflictingMetricConfig = jsonable_record:from_json(ConflictingMetricConfigJson, metric_config),

    ?new_ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.

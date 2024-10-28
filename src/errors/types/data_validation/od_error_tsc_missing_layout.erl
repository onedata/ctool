%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_tsc_missing_layout'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_tsc_missing_layout).

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
to_json(?ERROR_TSC_MISSING_LAYOUT(MissingLayout)) ->
    MissingLayoutPrint = ?fmt_csv(maps:fold(fun(TimeSeriesName, MetricNames, Acc) ->
        Acc ++ [?fmt("~ts -> [~ts]", [TimeSeriesName, ?fmt_csv(MetricNames)])]
    end, [], MissingLayout)),

    #{
        <<"id">> => ?ERROR_TSC_MISSING_LAYOUT_ID,
        <<"details">> => #{
            <<"missingLayout">> => MissingLayout
        },
        <<"description">> => ?fmt(
            "The request refers to a layout that is not reflected in the time series collection; the following part of the layout is missing (time series name -> metric names): ~ts.",
            [MissingLayoutPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_TSC_MISSING_LAYOUT_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    MissingLayout = maps:get(<<"missingLayout">>, DetailsJson),

    ?ERROR_TSC_MISSING_LAYOUT(MissingLayout).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

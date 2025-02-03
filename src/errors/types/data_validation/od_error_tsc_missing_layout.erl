%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
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


-type t() :: {error, #od_error{type :: ?MODULE}}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERR_TSC_MISSING_LAYOUT(ErrorCtx, MissingLayout)) ->
    MissingLayoutPrint = od_error:format_csv(maps:fold(fun(TimeSeriesName, MetricNames, Acc) ->
        Acc ++ [str_utils:format_bin("~ts -> [~ts]", [TimeSeriesName, od_error:format_csv(MetricNames)])]
    end, [], MissingLayout)),

    #{
        <<"id">> => ?ERR_TSC_MISSING_LAYOUT_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"missingLayout">> => MissingLayout
        },
        <<"description">> => od_error:format_description(
            "The request refers to a layout that is not reflected in the time series collection; the following part of the layout is missing (time series name -> metric names): ~ts.",
            [MissingLayoutPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_TSC_MISSING_LAYOUT_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    MissingLayout = maps:get(<<"missingLayout">>, DetailsJson),

    ?ERR_TSC_MISSING_LAYOUT(ErrorCtx, MissingLayout).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.

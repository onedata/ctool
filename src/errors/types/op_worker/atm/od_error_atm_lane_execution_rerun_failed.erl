%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_lane_execution_rerun_failed'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_lane_execution_rerun_failed).

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
to_json(?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED) ->
    #{
        <<"id">> => ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED_ID,
        <<"description">> => <<"Failed to rerun specified lane execution. Lane execution can be rerun only if it is in one of the following states: 'finished', 'failed', 'cancelled'.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED_ID}) ->
    ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED.


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_job_batch_crashed'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_job_batch_crashed).

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
to_json(?ERR_ATM_JOB_BATCH_CRASHED(ErrorCtx, Reason)) ->
    #{
        <<"id">> => ?ERR_ATM_JOB_BATCH_CRASHED_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"reason">> => Reason
        },
        <<"description">> => od_error:format_description(
            "Job batch execution has crashed: ~ts.",
            [Reason]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_ATM_JOB_BATCH_CRASHED_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Reason = maps:get(<<"reason">>, DetailsJson),

    ?ERR_ATM_JOB_BATCH_CRASHED(ErrorCtx, Reason).


-spec to_http_code(t()) -> ?HTTP_500_INTERNAL_SERVER_ERROR.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

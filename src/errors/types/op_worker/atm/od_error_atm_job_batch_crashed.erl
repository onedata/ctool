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
-module(od_error_atm_job_batch_crashed).

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
to_json(?ERROR_ATM_JOB_BATCH_CRASHED(Reason)) ->
    #{
        <<"id">> => ?ERROR_ATM_JOB_BATCH_CRASHED_ID,
        <<"details">> => #{
            <<"reason">> => Reason
        },
        <<"description">> => <<"Job batch execution has crashed.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_ATM_JOB_BATCH_CRASHED_ID,
    <<"details">> := #{<<"reason">> := Reason}
}) ->
    ?ERROR_ATM_JOB_BATCH_CRASHED(Reason).


-spec to_http_code(t()) -> 500.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.

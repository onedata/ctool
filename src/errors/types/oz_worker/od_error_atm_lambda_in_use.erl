%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_lambda_in_use'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_lambda_in_use).

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
to_json(?ERR_ATM_LAMBDA_IN_USE(ErrorCtx, AtmWorkflowSchemas)) ->
    AtmWorkflowSchemasPrint = od_error:format_csv(AtmWorkflowSchemas),

    #{
        <<"id">> => ?ERR_ATM_LAMBDA_IN_USE_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"atmWorkflowSchemas">> => AtmWorkflowSchemas
        },
        <<"description">> => od_error:format_description(
            "This lambda cannot be removed because it is used by the following workflow schemas: ~ts.",
            [AtmWorkflowSchemasPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_ATM_LAMBDA_IN_USE_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    AtmWorkflowSchemas = maps:get(<<"atmWorkflowSchemas">>, DetailsJson),

    ?ERR_ATM_LAMBDA_IN_USE(ErrorCtx, AtmWorkflowSchemas).


-spec to_http_code(t()) -> ?HTTP_403_FORBIDDEN.
to_http_code(_) ->
    ?HTTP_403_FORBIDDEN.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

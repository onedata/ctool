%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_parallel_box_execution_creation_failed'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_parallel_box_execution_creation_failed).

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
to_json(?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(ErrorCtx, AtmParallelBoxSchemaId, SpecificError)) ->
    SpecificErrorJson = errors:to_json(SpecificError),
    SpecificErrorPrint = maps:get(<<"description">>, SpecificErrorJson),

    #{
        <<"id">> => ?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"atmParallelBoxSchemaId">> => AtmParallelBoxSchemaId,
            <<"specificError">> => SpecificErrorJson
        },
        <<"description">> => od_error:format_description(
            "Failed to create automation parallel box execution (ID: \"~ts\"): ~ts",
            [AtmParallelBoxSchemaId, SpecificErrorPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    AtmParallelBoxSchemaId = maps:get(<<"atmParallelBoxSchemaId">>, DetailsJson),
    SpecificErrorJson = maps:get(<<"specificError">>, DetailsJson),
    SpecificError = errors:from_json(SpecificErrorJson),

    ?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(ErrorCtx, AtmParallelBoxSchemaId, SpecificError).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

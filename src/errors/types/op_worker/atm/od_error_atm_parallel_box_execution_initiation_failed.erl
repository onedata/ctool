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
-module(od_error_atm_parallel_box_execution_initiation_failed).

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
to_json(?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(
    AtmParallelBoxSchemaId,
    SpecificError
)) ->
    #{
        <<"id">> => ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_ID,
        <<"details">> => #{
            <<"atmParallelBoxSchemaId">> => AtmParallelBoxSchemaId,
            <<"specificError">> => errors:to_json(SpecificError)
        },
        <<"description">> => ?fmt(
            "Failed to initiate automation parallel box execution (id: \"~ts\") (see details).",
            [AtmParallelBoxSchemaId]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_ID,
    <<"details">> := #{
        <<"atmParallelBoxSchemaId">> := AtmParallelBoxSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(
        AtmParallelBoxSchemaId,
        errors:from_json(SpecificErrorJson)
    ).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

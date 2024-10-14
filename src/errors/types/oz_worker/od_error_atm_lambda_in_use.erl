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
-module(od_error_atm_lambda_in_use).

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
to_json(?ERROR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas)) ->
    #{
        <<"id">> => ?ERROR_ATM_LAMBDA_IN_USE_ID,
        <<"details">> => #{
            <<"atmWorkflowSchemas">> => AtmWorkflowSchemas
        },
        <<"description">> => ?fmt(
            "This lambda cannot be removed because it is used by the following workflow schemas: ~ts.",
            [?fmt_csv(AtmWorkflowSchemas)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_ATM_LAMBDA_IN_USE_ID,
    <<"details">> := #{<<"atmWorkflowSchemas">> := AtmWorkflowSchemas}
}) ->
    ?ERROR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas).


-spec to_http_code(t()) -> 403.
to_http_code(_) ->
    ?HTTP_403_FORBIDDEN.

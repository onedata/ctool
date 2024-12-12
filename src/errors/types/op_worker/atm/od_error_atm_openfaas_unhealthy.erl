%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_openfaas_unhealthy'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_openfaas_unhealthy).

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
to_json(?ERR_ATM_OPENFAAS_UNHEALTHY(ErrorCtx)) ->
    #{
        <<"id">> => ?ERR_ATM_OPENFAAS_UNHEALTHY_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"description">> => <<"The OpenFaaS service is unhealthy.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_ATM_OPENFAAS_UNHEALTHY_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),
    ?ERR_ATM_OPENFAAS_UNHEALTHY(ErrorCtx).


-spec to_http_code(t()) -> ?HTTP_503_SERVICE_UNAVAILABLE.
to_http_code(_) ->
    ?HTTP_503_SERVICE_UNAVAILABLE.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

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
-module(od_error_atm_openfaas_query_failed).

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
to_json(?ERROR_ATM_OPENFAAS_QUERY_FAILED(Reason)) ->
    #{
        <<"id">> => ?ERROR_ATM_OPENFAAS_QUERY_FAILED_ID,
        <<"details">> => #{
            <<"reason">> => utils:undefined_to_null(Reason)
        },
        <<"description">> => <<"Failed to query OpenFaaS service (see details).">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(ErrorJson = #{<<"id">> := ?ERROR_ATM_OPENFAAS_QUERY_FAILED_ID}) ->
    DetailsJson = maps:get(<<"details">>, ErrorJson, #{}),
    Reason = utils:null_to_undefined(maps:get(<<"reason">>, DetailsJson, null)),

    ?ERROR_ATM_OPENFAAS_QUERY_FAILED(Reason).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

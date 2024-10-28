%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_token_service_forbidden'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_token_service_forbidden).

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
to_json(?ERROR_TOKEN_SERVICE_FORBIDDEN(Service)) ->
    ServiceJson = aai:service_to_json(Service),
    ServicePrint = aai:service_to_printable(Service),

    #{
        <<"id">> => ?ERROR_TOKEN_SERVICE_FORBIDDEN_ID,
        <<"details">> => #{
            <<"service">> => ServiceJson
        },
        <<"description">> => ?fmt(
            "The service ~ts is forbidden for this subject.",
            [ServicePrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_TOKEN_SERVICE_FORBIDDEN_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ServiceJson = maps:get(<<"service">>, DetailsJson),
    Service = aai:service_from_json(ServiceJson),

    ?ERROR_TOKEN_SERVICE_FORBIDDEN(Service).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

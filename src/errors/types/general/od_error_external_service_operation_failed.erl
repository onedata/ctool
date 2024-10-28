%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_external_service_operation_failed'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_external_service_operation_failed).

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
to_json(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName)) ->
    #{
        <<"id">> => ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED_ID,
        <<"details">> => #{
            <<"serviceName">> => ServiceName
        },
        <<"description">> => ?fmt(
            "Your request could not be fulfilled due to problems with the external service '~ts'. This might be a temporary problem or a misconfiguration. Please try again later or contact the site administrators if the problem persists.",
            [ServiceName]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ServiceName = maps:get(<<"serviceName">>, DetailsJson),

    ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName).


-spec to_http_code(t()) -> ?HTTP_503_SERVICE_UNAVAILABLE.
to_http_code(_) ->
    ?HTTP_503_SERVICE_UNAVAILABLE.

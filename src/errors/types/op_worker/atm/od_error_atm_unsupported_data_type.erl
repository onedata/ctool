%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_unsupported_data_type'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_unsupported_data_type).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_ATM_UNSUPPORTED_DATA_TYPE_MATCH(Type, Allowed)) ->
    TypeJson = atm_data_type:type_to_json(Type),
    AllowedJson = lists:map(fun atm_data_type:type_to_json/1, Allowed),
    AllowedPrint = od_error:format_csv(AllowedJson),

    #{
        <<"id">> => ?ERROR_ATM_UNSUPPORTED_DATA_TYPE_ID,
        <<"details">> => #{
            <<"type">> => TypeJson,
            <<"allowed">> => AllowedJson
        },
        <<"description">> => od_error:format_description(
            "Bad automation data type: provided \"~ts\" is not one of: ~ts.",
            [Type, AllowedPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_ATM_UNSUPPORTED_DATA_TYPE_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    TypeJson = maps:get(<<"type">>, DetailsJson),
    Type = atm_data_type:type_from_json(TypeJson),
    AllowedJson = maps:get(<<"allowed">>, DetailsJson),
    Allowed = lists:map(fun atm_data_type:type_from_json/1, AllowedJson),

    ?new_ERROR_ATM_UNSUPPORTED_DATA_TYPE(Type, Allowed).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

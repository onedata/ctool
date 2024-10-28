%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_value_json'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_value_json).

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
to_json(?ERROR_BAD_VALUE_JSON(Key)) ->
    #{
        <<"id">> => ?ERROR_BAD_VALUE_JSON_ID,
        <<"details">> => #{
            <<"key">> => Key
        },
        <<"description">> => ?fmt(
            "Bad value: provided \"~ts\" must be a valid JSON.",
            [Key]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_BAD_VALUE_JSON_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Key = maps:get(<<"key">>, DetailsJson),

    ?ERROR_BAD_VALUE_JSON(Key).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

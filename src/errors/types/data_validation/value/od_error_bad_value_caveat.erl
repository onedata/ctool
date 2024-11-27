%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_value_caveat'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_value_caveat).

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
to_json(?ERROR_BAD_VALUE_CAVEAT_MATCH(Caveat)) ->
    CaveatPrint = json_utils:encode(Caveat),

    #{
        <<"id">> => ?ERROR_BAD_VALUE_CAVEAT_ID,
        <<"details">> => #{
            <<"caveat">> => Caveat
        },
        <<"description">> => od_error:format_description(
            "Provided caveat is invalid: '~ts'.",
            [CaveatPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_BAD_VALUE_CAVEAT_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Caveat = maps:get(<<"caveat">>, DetailsJson),

    ?new_ERROR_BAD_VALUE_CAVEAT(Caveat).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.

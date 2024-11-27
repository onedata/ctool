%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_forbidden'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_forbidden).

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
to_json(?ERROR_FORBIDDEN_MATCH(Hint)) ->
    HintJson = utils:undefined_to_null(Hint),

    #{
        <<"id">> => ?ERROR_FORBIDDEN_ID,
        <<"details">> => #{
            <<"hint">> => HintJson
        },
        <<"description">> => od_error:format_description(
            "You are not authorized to perform this operation: ~ts",
            [HintJson]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_FORBIDDEN_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson, #{}),

    Hint = utils:null_to_undefined(maps:get(<<"hint">>, DetailsJson, null)),

    ?new_ERROR_FORBIDDEN(Hint).


-spec to_http_code(t()) -> ?HTTP_403_FORBIDDEN.
to_http_code(_) ->
    ?HTTP_403_FORBIDDEN.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EACCES}.

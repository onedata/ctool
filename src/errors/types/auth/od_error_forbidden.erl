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


-type t() :: {error, #od_error{type :: ?MODULE}}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERR_FORBIDDEN(ErrorCtx, Hint)) ->
    {HintJson, HintPrint} = case Hint of
        undefined ->
            {null, <<"no details available.">>};
        _ ->
            {Hint, Hint}
    end,

    #{
        <<"id">> => ?ERR_FORBIDDEN_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"hint">> => HintJson
        },
        <<"description">> => od_error:format_description(
            "You are not authorized to perform this operation: ~ts",
            [HintPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_FORBIDDEN_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson, #{}),

    Hint = utils:null_to_undefined(maps:get(<<"hint">>, DetailsJson, null)),

    ?ERR_FORBIDDEN(ErrorCtx, Hint).


-spec to_http_code(t()) -> ?HTTP_403_FORBIDDEN.
to_http_code(_) ->
    ?HTTP_403_FORBIDDEN.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EACCES}.

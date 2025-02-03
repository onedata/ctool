%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_space_not_supported_by'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_space_not_supported_by).

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
to_json(?ERR_SPACE_NOT_SUPPORTED_BY(ErrorCtx, SpaceId, ProviderId)) ->
    #{
        <<"id">> => ?ERR_SPACE_NOT_SUPPORTED_BY_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"spaceId">> => SpaceId,
            <<"providerId">> => ProviderId
        },
        <<"description">> => od_error:format_description(
            "Specified space: \"~ts\" is not supported by provider \"~ts\".",
            [SpaceId, ProviderId]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_SPACE_NOT_SUPPORTED_BY_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    SpaceId = maps:get(<<"spaceId">>, DetailsJson),
    ProviderId = maps:get(<<"providerId">>, DetailsJson),

    ?ERR_SPACE_NOT_SUPPORTED_BY(ErrorCtx, SpaceId, ProviderId).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.

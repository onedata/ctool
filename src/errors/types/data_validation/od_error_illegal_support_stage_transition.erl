%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_illegal_support_stage_transition'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_illegal_support_stage_transition).

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
to_json(?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ErrorCtx, CurrentProviderStage, CurrentStorageStage)) ->
    CurrentProviderStageJson = support_stage:serialize(provider, CurrentProviderStage),
    CurrentStorageStageJson = support_stage:serialize(storage, CurrentStorageStage),

    #{
        <<"id">> => ?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"currentProviderStage">> => CurrentProviderStageJson,
            <<"currentStorageStage">> => CurrentStorageStageJson
        },
        <<"description">> => od_error:format_description(
            "Illegal support stage transition: this operation cannot be performed while the storage is in stage \"~w\" and the provider is in stage \"~w\".",
            [CurrentStorageStage, CurrentProviderStage]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    CurrentProviderStageJson = maps:get(<<"currentProviderStage">>, DetailsJson),
    CurrentProviderStage = support_stage:deserialize(provider, CurrentProviderStageJson),
    CurrentStorageStageJson = maps:get(<<"currentStorageStage">>, DetailsJson),
    CurrentStorageStage = support_stage:deserialize(storage, CurrentStorageStageJson),

    ?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ErrorCtx, CurrentProviderStage, CurrentStorageStage).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.

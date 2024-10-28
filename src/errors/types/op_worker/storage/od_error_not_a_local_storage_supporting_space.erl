%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_not_a_local_storage_supporting_space'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_not_a_local_storage_supporting_space).

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
to_json(?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId)) ->
    #{
        <<"id">> => ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_ID,
        <<"details">> => #{
            <<"providerId">> => ProviderId,
            <<"storageId">> => StorageId,
            <<"spaceId">> => SpaceId
        },
        <<"description">> => ?fmt(
            "Storage ~ts does not belong to this Oneprovider (~ts) and/or does not support the space ~ts.",
            [StorageId, ProviderId, SpaceId]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ProviderId = maps:get(<<"providerId">>, DetailsJson),
    StorageId = maps:get(<<"storageId">>, DetailsJson),
    SpaceId = maps:get(<<"spaceId">>, DetailsJson),

    ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

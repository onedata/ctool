%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_cannot_delete_entity'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_cannot_delete_entity).

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
to_json(?ERROR_CANNOT_DELETE_ENTITY_MATCH(EntityType, EntityId)) ->
    EntityTypeJson = gri:serialize_type(EntityType),

    #{
        <<"id">> => ?ERROR_CANNOT_DELETE_ENTITY_ID,
        <<"details">> => #{
            <<"entityType">> => EntityTypeJson,
            <<"entityId">> => EntityId
        },
        <<"description">> => od_error:format_description(
            "Cannot delete ~ts:~ts; failed to delete some dependent relations.",
            [EntityTypeJson, EntityId]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_CANNOT_DELETE_ENTITY_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    EntityTypeJson = maps:get(<<"entityType">>, DetailsJson),
    EntityType = gri:deserialize_type(EntityTypeJson),
    EntityId = maps:get(<<"entityId">>, DetailsJson),

    ?new_ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId).


-spec to_http_code(t()) -> ?HTTP_500_INTERNAL_SERVER_ERROR.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

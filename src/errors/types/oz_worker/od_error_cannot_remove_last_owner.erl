%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_cannot_remove_last_owner'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_cannot_remove_last_owner).

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
to_json(?ERROR_CANNOT_REMOVE_LAST_OWNER_MATCH(EntityType, EntityId)) ->
    EntityTypeJson = atom_to_binary(EntityType, utf8),

    #{
        <<"id">> => ?ERROR_CANNOT_REMOVE_LAST_OWNER_ID,
        <<"details">> => #{
            <<"entityType">> => EntityTypeJson,
            <<"entityId">> => EntityId
        },
        <<"description">> => <<"Cannot remove the last owner - another owner must be assigned first. Ownership can be granted to any direct or effective member.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_CANNOT_REMOVE_LAST_OWNER_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    EntityTypeJson = maps:get(<<"entityType">>, DetailsJson),
    EntityType = binary_to_existing_atom(EntityTypeJson, utf8),
    EntityId = maps:get(<<"entityId">>, DetailsJson),

    ?new_ERROR_CANNOT_REMOVE_LAST_OWNER(EntityType, EntityId).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

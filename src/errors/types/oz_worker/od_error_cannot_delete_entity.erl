%%%-------------------------------------------------------------------
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for ?MODULE.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_cannot_delete_entity).

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
to_json(?ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId)) ->
    #{
        <<"id">> => ?ERROR_CANNOT_DELETE_ENTITY_ID,
        <<"details">> => #{
            <<"entityType">> => EntityType,
            <<"entityId">> => EntityId
        },
        <<"description">> => ?fmt("Cannot delete ~ts:~ts; failed to delete some dependent relations.", [
            gri:serialize_type(EntityType), EntityId
        ])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_CANNOT_DELETE_ENTITY_ID, <<"details">> := #{
    <<"entityType">> := EntType,
    <<"entityId">> := EntId
}}) ->
    ?ERROR_CANNOT_DELETE_ENTITY(binary_to_existing_atom(EntType, utf8), EntId).


-spec to_http_code(t()) -> 500.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.

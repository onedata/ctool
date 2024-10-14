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
-module(od_error_cannot_remove_last_owner).

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
to_json(?ERROR_CANNOT_REMOVE_LAST_OWNER(EntityType, EntityId)) ->
    #{
        <<"id">> => ?ERROR_CANNOT_REMOVE_LAST_OWNER_ID,
        <<"details">> => #{
            <<"entityType">> => EntityType,
            <<"entityId">> => EntityId
        },
        <<"description">> => <<
            "Cannot remove the last owner - another owner must be assigned first. "
            "Ownership can be granted to any direct or effective member."
        >>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_CANNOT_REMOVE_LAST_OWNER_ID, <<"details">> := #{
    <<"entityType">> := EntType,
    <<"entityId">> := EntId
}}) ->
    ?ERROR_CANNOT_REMOVE_LAST_OWNER(binary_to_existing_atom(EntType, utf8), EntId).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

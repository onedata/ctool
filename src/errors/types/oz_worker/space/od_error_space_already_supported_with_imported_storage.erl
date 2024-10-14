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
-module(od_error_space_already_supported_with_imported_storage).

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
to_json(?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId)) ->
    #{
        <<"id">> => ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID,
        <<"details">> => #{
            <<"spaceId">> => SpaceId,
            <<"storageId">> => StorageId
        },
        <<"description">> => ?fmt("Space ~ts is already supported with an imported storage ~ts.", [SpaceId, StorageId])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID, <<"details">> := #{
    <<"spaceId">> := SpaceId, <<"storageId">> := StorageId}
}) ->
    ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId).


-spec to_http_code(t()) -> 409.
to_http_code(_) ->
    ?HTTP_409_CONFLICT.

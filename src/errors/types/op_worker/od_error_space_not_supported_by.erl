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
-module(od_error_space_not_supported_by).

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
to_json(?ERROR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId)) ->
    #{
        <<"id">> => ?ERROR_SPACE_NOT_SUPPORTED_BY_ID,
        <<"details">> => #{
            <<"spaceId">> => SpaceId,
            <<"providerId">> => ProviderId
        },
        <<"description">> => ?fmt("Specified space: ~ts is not supported by provider ~ts.", [SpaceId, ProviderId])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_SPACE_NOT_SUPPORTED_BY_ID, <<"details">> := #{
    <<"spaceId">> := SpaceId,
    <<"providerId">> := ProviderId
}}) ->
    ?ERROR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

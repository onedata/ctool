%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_cannot_delete_non_empty_handle_service'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_cannot_delete_non_empty_handle_service).

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
to_json(?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE(ErrorCtx)) ->
    #{
        <<"id">> => ?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"description">> => <<"This handle service cannot be deleted as it still has some handles registered. All the handles would have to be deleted first, but proceed with caution as Public Data records should be persistent.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),
    ?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE(ErrorCtx).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

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
-module(od_error_cannot_delete_non_empty_handle_service).

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
to_json(?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE) ->
    #{
        <<"id">> => ?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID,
        <<"description">> => <<
            "This handle service cannot be deleted as it still has some handles registered. "
            "All the handles would have to be deleted first, but proceed with caution as Open Data "
            "records should be persistent."
        >>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID}) ->
    ?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE.


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

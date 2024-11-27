%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_dir_stats_disabled_for_space'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_dir_stats_disabled_for_space).

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
to_json(?ERROR_DIR_STATS_DISABLED_FOR_SPACE_MATCH) ->
    #{
        <<"id">> => ?ERROR_DIR_STATS_DISABLED_FOR_SPACE_ID,
        <<"description">> => <<"Directory statistics collection is disabled for this space.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_DIR_STATS_DISABLED_FOR_SPACE_ID}) ->
    ?new_ERROR_DIR_STATS_DISABLED_FOR_SPACE().


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

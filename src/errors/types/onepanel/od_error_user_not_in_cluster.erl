%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_user_not_in_cluster'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_user_not_in_cluster).

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
to_json(?ERROR_USER_NOT_IN_CLUSTER_MATCH) ->
    #{
        <<"id">> => ?ERROR_USER_NOT_IN_CLUSTER_ID,
        <<"description">> => <<"Authenticated user is not a member of this cluster.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_USER_NOT_IN_CLUSTER_ID}) ->
    ?new_ERROR_USER_NOT_IN_CLUSTER().


-spec to_http_code(t()) -> ?HTTP_403_FORBIDDEN.
to_http_code(_) ->
    ?HTTP_403_FORBIDDEN.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

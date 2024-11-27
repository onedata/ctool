%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_lets_encrypt_not_reachable'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_lets_encrypt_not_reachable).

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
to_json(?ERROR_LETS_ENCRYPT_NOT_REACHABLE_MATCH) ->
    #{
        <<"id">> => ?ERROR_LETS_ENCRYPT_NOT_REACHABLE_ID,
        <<"description">> => <<"Connection to Let's Encrypt server failed.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_LETS_ENCRYPT_NOT_REACHABLE_ID}) ->
    ?new_ERROR_LETS_ENCRYPT_NOT_REACHABLE().


-spec to_http_code(t()) -> ?HTTP_503_SERVICE_UNAVAILABLE.
to_http_code(_) ->
    ?HTTP_503_SERVICE_UNAVAILABLE.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

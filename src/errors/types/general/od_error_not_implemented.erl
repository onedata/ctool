%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_not_implemented'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_not_implemented).

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
to_json(?ERROR_NOT_IMPLEMENTED_MATCH) ->
    #{
        <<"id">> => ?ERROR_NOT_IMPLEMENTED_ID,
        <<"description">> => <<"This operation is not implemented.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_NOT_IMPLEMENTED_ID}) ->
    ?new_ERROR_NOT_IMPLEMENTED().


-spec to_http_code(t()) -> ?HTTP_501_NOT_IMPLEMENTED.
to_http_code(_) ->
    ?HTTP_501_NOT_IMPLEMENTED.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?ENOSYS}.

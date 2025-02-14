%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_already_exists'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_already_exists).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: {error, already_exists}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_ALREADY_EXISTS) ->
    #{
        <<"id">> => ?ERROR_ALREADY_EXISTS_ID,
        <<"description">> => <<"The resource already exists.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_ALREADY_EXISTS_ID}) ->
    ?ERROR_ALREADY_EXISTS.


-spec to_http_code(t()) -> ?HTTP_409_CONFLICT.
to_http_code(_) ->
    ?HTTP_409_CONFLICT.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.

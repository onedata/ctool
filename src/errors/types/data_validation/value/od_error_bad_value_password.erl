%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_value_password'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_value_password).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").
-include("validation.hrl").

-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_BAD_VALUE_PASSWORD) ->
    #{
        <<"id">> => ?ERROR_BAD_VALUE_PASSWORD_ID,
        <<"description">> => ?fmt(
            "Bad value: ~ts",
            [?PASSWORD_REQUIREMENTS_DESCRIPTION]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_BAD_VALUE_PASSWORD_ID}) ->
    ?ERROR_BAD_VALUE_PASSWORD.


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

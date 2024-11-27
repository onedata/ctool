%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_storage_test_failed'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_storage_test_failed).

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
to_json(?ERROR_STORAGE_TEST_FAILED_MATCH(Operation)) ->
    #{
        <<"id">> => ?ERROR_STORAGE_TEST_FAILED_ID,
        <<"details">> => #{<<"operation">> => str_utils:to_binary(Operation)},
        <<"description">> => od_error:format_description("Failed to ~ts test file on storage.", [Operation])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_STORAGE_TEST_FAILED_ID, <<"details">> := #{<<"operation">> := Operation}}) when
    Operation == <<"read">>;
    Operation == <<"write">>;
    Operation == <<"remove">>
->
    ?new_ERROR_STORAGE_TEST_FAILED(binary_to_atom(Operation, utf8)).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EIO}.

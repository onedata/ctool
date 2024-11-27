%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_internal_server_error'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_internal_server_error).

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
to_json(?ERROR_INTERNAL_SERVER_ERROR_MATCH(Reference)) ->
    ReferenceJson = utils:undefined_to_null(Reference),

    #{
        <<"id">> => ?ERROR_INTERNAL_SERVER_ERROR_ID,
        <<"details">> => #{
            <<"reference">> => ReferenceJson
        },
        <<"description">> => od_error:format_description(
            "The server has encountered an error while processing this request. If the problem persists, please contact the site's administrators, citing the following reference: ~ts",
            [ReferenceJson]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_INTERNAL_SERVER_ERROR_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson, #{}),

    Reference = utils:null_to_undefined(maps:get(<<"reference">>, DetailsJson, null)),

    ?new_ERROR_INTERNAL_SERVER_ERROR(Reference).


-spec to_http_code(t()) -> ?HTTP_500_INTERNAL_SERVER_ERROR.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EAGAIN}.

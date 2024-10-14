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
-module(od_error_internal_server_error).

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
to_json(?ERROR_INTERNAL_SERVER_ERROR(ErrorRef)) ->
    ErrorRefJson = utils:undefined_to_null(ErrorRef),

    #{
        <<"id">> => ?ERROR_INTERNAL_SERVER_ERROR_ID,
        <<"details">> => #{
            <<"reference">> => ErrorRefJson
        },
        <<"description">> => ?fmt(
            "The server has encountered an error while processing this request. "
            "If the problem persists, please contact the site's administrators, citing the following reference: ~ts.", [ErrorRefJson]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(ErrorJson = #{<<"id">> := ?ERROR_INTERNAL_SERVER_ERROR_ID}) ->
    DetailsJson = maps:get(<<"details">>, ErrorJson, #{}),
    Reference = utils:null_to_undefined(maps:get(<<"reference">>, DetailsJson, null)),

    ?ERROR_INTERNAL_SERVER_ERROR(Reference).


-spec to_http_code(t()) -> 500.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.

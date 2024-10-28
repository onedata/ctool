%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_view_query_failed'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_view_query_failed).

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
to_json(?ERROR_VIEW_QUERY_FAILED(Category, Description)) ->
    #{
        <<"id">> => ?ERROR_VIEW_QUERY_FAILED_ID,
        <<"details">> => #{
            <<"category">> => Category,
            <<"description">> => Description
        },
        <<"description">> => ?fmt(
            "Query on view failed. Error category: ~ts. Description: ~ts.",
            [Category, Description]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_VIEW_QUERY_FAILED_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Category = maps:get(<<"category">>, DetailsJson),
    Description = maps:get(<<"description">>, DetailsJson),

    ?ERROR_VIEW_QUERY_FAILED(Category, Description).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

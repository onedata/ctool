%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_nested_archive_deletion_forbidden'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_nested_archive_deletion_forbidden).

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
to_json(?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ParentArchiveId)) ->
    #{
        <<"id">> => ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN_ID,
        <<"details">> => #{
            <<"parentArchiveId">> => ParentArchiveId
        },
        <<"description">> => <<"This archive cannot be deleted since it is nested in another archive.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ParentArchiveId = maps:get(<<"parentArchiveId">>, DetailsJson),

    ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ParentArchiveId).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

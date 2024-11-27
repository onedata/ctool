%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_relation_does_not_exist'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_relation_does_not_exist).

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
to_json(?ERROR_RELATION_DOES_NOT_EXIST_MATCH(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is not supported by">>;
        {_, _} -> <<"is not a member of">>
    end,
    #{
        <<"id">> => ?ERROR_RELATION_DOES_NOT_EXIST_ID,
        <<"details">> => #{
            <<"childType">> => ChType,
            <<"childId">> => ChId,
            <<"parentType">> => ParType,
            <<"parentId">> => ParId
        },
        <<"description">> => od_error:format_description("Bad value: ~ts:~ts ~ts ~ts:~ts.", [
            gri:serialize_type(ChType), ChId,
            RelationToString,
            gri:serialize_type(ParType), ParId
        ])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_RELATION_DOES_NOT_EXIST_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ChildTypeJson = maps:get(<<"childType">>, DetailsJson),
    ChildType = binary_to_existing_atom(ChildTypeJson, utf8),
    ChildId = maps:get(<<"childId">>, DetailsJson),
    ParentTypeJson = maps:get(<<"parentType">>, DetailsJson),
    ParentType = binary_to_existing_atom(ParentTypeJson, utf8),
    ParentId = maps:get(<<"parentId">>, DetailsJson),

    ?new_ERROR_RELATION_DOES_NOT_EXIST(ChildType, ChildId, ParentType, ParentId).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.

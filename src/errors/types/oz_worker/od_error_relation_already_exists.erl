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
-module(od_error_relation_already_exists).

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
to_json(?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is already supported by">>;
        {_, _} -> <<"is already a member of">>
    end,
    #{
        <<"id">> => ?ERROR_RELATION_ALREADY_EXISTS_ID,
        <<"details">> => #{
            <<"childType">> => gri:serialize_type(ChType),
            <<"childId">> => ChId,
            <<"parentType">> => gri:serialize_type(ParType),
            <<"parentId">> => ParId
        },
        <<"description">> => ?fmt("Bad value: ~ts:~ts ~ts ~ts:~ts.", [
            gri:serialize_type(ChType), ChId,
            RelationToString,
            gri:serialize_type(ParType), ParId
        ])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_RELATION_ALREADY_EXISTS_ID, <<"details">> := #{
    <<"childType">> := ChType, <<"childId">> := ChId, <<"parentType">> := ParType, <<"parentId">> := ParId}
}) ->
    ChTypeAtom = gri:deserialize_type(ChType),
    ParTypeAtom = gri:deserialize_type(ParType),

    ?ERROR_RELATION_ALREADY_EXISTS(ChTypeAtom, ChId, ParTypeAtom, ParId).


-spec to_http_code(t()) -> 409.
to_http_code(_) ->
    ?HTTP_409_CONFLICT.

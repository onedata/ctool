%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing user_form operation spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_user_form_operation_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).


-type record() :: #atm_user_form_operation_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Spec) ->
    #{
        <<"userFormId">> => Spec#atm_user_form_operation_spec.user_form_id
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    #atm_user_form_operation_spec{
        user_form_id = maps:get(<<"userFormId">>, SpecJson)
    }.

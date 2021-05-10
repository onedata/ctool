%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing atm_workflow operation spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_operation_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_workflow_operation_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Spec) ->
    #{
        <<"atmWorkflowId">> => Spec#atm_workflow_operation_spec.atm_workflow_id
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    #atm_workflow_operation_spec{
        atm_workflow_id = maps:get(<<"atmWorkflowId">>, SpecJson)
    }.

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Spec, _NestedRecordEncoder) ->
    to_json(Spec).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(SpecJson, _NestedRecordDecoder) ->
    from_json(SpecJson).

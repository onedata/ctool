%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing workflow schema revision used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_workflow_schema_revision).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% API
-export([fold_tasks/3, map_tasks/2, extract_atm_lambda_references/1]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_workflow_schema_revision{}.
-export_type([record/0]).

% Number assigned to a workflow schema revision by the user that creates or edits the revision.
% It is recommended that every edition of the workflow schema is saved as a new revision with
% sequentially incremented revision number, but in general it is up to the users how they use revisions.
-type revision_number() :: pos_integer().

%% Carries information what atm_lambdas in what revisions are referenced by a workflow schema.
%% Structurally, a map where the key is the referenced lambda id
%% and the value is the corresponding list of revisions of the lambda
%% (different revision numbers of the same lambda can be referenced in different tasks).
-type atm_lambda_references() :: #{AtmLambdaId :: automation:id() => [atm_lambda_revision:revision_number()]}.

-export_type([revision_number/0, atm_lambda_references/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec fold_tasks(
    fun((atm_task_schema:record(), AccIn :: term()) -> AccOut :: term()),
    InitialAcc :: term(),
    record()
) -> FinalAcc :: term().
fold_tasks(Callback, InitialAcc, #atm_workflow_schema_revision{lanes = Lanes}) ->
    lists:foldl(fun(#atm_lane_schema{parallel_boxes = ParallelBoxes}, TopAcc) ->
        lists:foldl(fun(#atm_parallel_box_schema{tasks = Tasks}, MiddleAcc) ->
            lists:foldl(fun(#atm_task_schema{} = AtmTaskSchema, BottomAcc) ->
                Callback(AtmTaskSchema, BottomAcc)
            end, MiddleAcc, Tasks)
        end, TopAcc, ParallelBoxes)
    end, InitialAcc, Lanes).


-spec map_tasks(
    fun((atm_task_schema:record()) -> atm_task_schema:record()),
    record()
) -> record().
map_tasks(MappingFunction, #atm_workflow_schema_revision{lanes = Lanes} = AtmWorkflowSchemaRevision) ->
    NewLanes = lists:map(fun(Lane = #atm_lane_schema{parallel_boxes = ParallelBoxes}) ->
        Lane#atm_lane_schema{
            parallel_boxes = lists:map(fun(ParallelBox = #atm_parallel_box_schema{tasks = Tasks}) ->
                ParallelBox#atm_parallel_box_schema{
                    tasks = lists:map(MappingFunction, Tasks)
                }
            end, ParallelBoxes)
        }
    end, Lanes),
    AtmWorkflowSchemaRevision#atm_workflow_schema_revision{lanes = NewLanes}.


-spec extract_atm_lambda_references(record()) -> atm_lambda_references().
extract_atm_lambda_references(AtmWorkflowSchemaRevision) ->
    fold_tasks(fun(#atm_task_schema{lambda_id = AtmLambdaId, lambda_revision_number = RevisionNumber}, Acc) ->
        maps:update_with(AtmLambdaId, fun(ReferencedRevisionNumbers) ->
            lists_utils:union([RevisionNumber], ReferencedRevisionNumbers)
        end, [RevisionNumber], Acc)
    end, #{}, AtmWorkflowSchemaRevision).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(validate, RecordJson, fun jsonable_record:from_json/2).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, NestedRecordEncoder) ->
    encode_with(Record, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, NestedRecordDecoder) ->
    decode_with(skip_validation, RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"description">> => Record#atm_workflow_schema_revision.description,
        <<"state">> => automation:lifecycle_state_to_json(Record#atm_workflow_schema_revision.state),
        <<"stores">> => [NestedRecordEncoder(S, atm_store_schema) || S <- Record#atm_workflow_schema_revision.stores],
        <<"lanes">> => [NestedRecordEncoder(S, atm_lane_schema) || S <- Record#atm_workflow_schema_revision.lanes],
        <<"dashboardSpec">> => case Record#atm_workflow_schema_revision.dashboard_spec of
            undefined -> null;
            DashboardSpec -> NestedRecordEncoder(DashboardSpec, ts_dashboard_spec)
        end
    }.


%% @private
-spec decode_with(jsonable_record:validation_strategy(), json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(ValidationStrategy, RecordJson, NestedRecordDecoder) ->
    %% @TODO VFS-8507 Rework along with new data sanitizers for all atm models (data_spec callback?)
    InputDescription = maps:get(<<"description">>, RecordJson, ?DEFAULT_DESCRIPTION),
    Description = case ValidationStrategy of
        validate -> automation:sanitize_binary_text(<<"description">>, InputDescription, ?DESCRIPTION_SIZE_LIMIT);
        skip_validation -> InputDescription
    end,
    #atm_workflow_schema_revision{
        description = Description,
        state = automation:lifecycle_state_from_json(maps:get(<<"state">>, RecordJson)),
        stores = [NestedRecordDecoder(S, atm_store_schema) || S <- maps:get(<<"stores">>, RecordJson)],
        lanes = [NestedRecordDecoder(S, atm_lane_schema) || S <- maps:get(<<"lanes">>, RecordJson)],
        dashboard_spec = case maps:get(<<"dashboardSpec">>, RecordJson, null) of
            null -> undefined;
            DashboardSpecJson -> NestedRecordDecoder(DashboardSpecJson, ts_dashboard_spec)
        end
    }.

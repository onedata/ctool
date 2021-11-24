%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of automation related modules.
%%% @end
%%%-------------------------------------------------------------------
-module(automation_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("automation/automation.hrl").
-include("errors.hrl").
-include("logging.hrl").

%% @TODO VFS-7687 Add tests for all automation data types and validators

encode_decode_atm_resource_spec_test() ->
    [Example | _] = ExampleResourceSpecs = atm_test_utils:example_resource_specs(),
    encode_decode_test_base(ExampleResourceSpecs),

    % CPU spec should be accepted as integers and converted to floats
    ExampleJson = atm_resource_spec:to_json(Example),
    ExampleWithRoundCpuResources = Example#atm_resource_spec{
        cpu_requested = 1.0,
        cpu_limit = 8.0
    },
    ExampleJsonWithIntegerCpuResources = ExampleJson#{
        <<"cpuRequested">> => 1,
        <<"cpuLimit">> => 8
    },
    ?assertEqual(ExampleWithRoundCpuResources, atm_resource_spec:from_json(ExampleJsonWithIntegerCpuResources)),

    % test validation of values
    ?assertThrow(?ERROR_BAD_DATA(<<"atmResourceSpec">>), atm_resource_spec:to_json(Example#atm_resource_spec{
        cpu_requested = undefined
    })),
    ?assertThrow(?ERROR_BAD_DATA(<<"atmResourceSpec">>), atm_resource_spec:from_json(ExampleJson#{
        <<"cpuLimit">> => <<"text">>
    })),
    ?assertThrow(?ERROR_BAD_DATA(<<"atmResourceSpec">>), atm_resource_spec:from_json(ExampleJson#{
        <<"memoryRequested">> => 17.8
    })),
    ?assertThrow(?ERROR_BAD_DATA(<<"atmResourceSpec">>), atm_resource_spec:to_json(Example#atm_resource_spec{
        memory_limit = #{<<"map">> => <<"val">>}
    })),
    ?assertThrow(?ERROR_BAD_DATA(<<"atmResourceSpec">>), atm_resource_spec:to_json(Example#atm_resource_spec{
        ephemeral_storage_requested = null
    })),
    ?assertThrow(?ERROR_BAD_DATA(<<"atmResourceSpec">>), atm_resource_spec:from_json(ExampleJson#{
        <<"ephemeralStorageLimit">> => 0
    })).


encode_decode_atm_data_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_data_specs()).


encode_decode_operation_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_operation_specs()),

    DisallowedOperationSpecs = [
        jsonable_record:to_json(#atm_onedata_function_operation_spec{}, atm_lambda_operation_spec),
        jsonable_record:to_json(#atm_workflow_operation_spec{}, atm_lambda_operation_spec),
        jsonable_record:to_json(#atm_user_form_operation_spec{}, atm_lambda_operation_spec)
    ],
    ExpDisallowedOperationSpecError = ?ERROR_BAD_VALUE_NOT_ALLOWED(
        <<"operationSpec.engine">>,
        lists:map(
            fun atm_lambda_operation_spec:engine_to_json/1,
            atm_lambda_operation_spec:allowed_engines_for_custom_lambdas()
        )
    ),
    lists:foreach(fun(DisallowedOperationSpec) ->
        ?assertThrow(
            ExpDisallowedOperationSpecError,
            jsonable_record:from_json(DisallowedOperationSpec, atm_lambda_operation_spec)
        )
    end, DisallowedOperationSpecs).


encode_decode_docker_execution_options_test() ->
    encode_decode_test_base(atm_test_utils:example_docker_execution_options()).


encode_decode_lambda_argument_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_argument_specs()).


encode_decode_lambda_result_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_result_specs()).


encode_decode_store_schema_test() ->
    encode_decode_test_base(atm_test_utils:example_store_schemas()).


encode_decode_store_iterator_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_store_iterator_specs()).


encode_decode_task_argument_mapper_test() ->
    encode_decode_test_base(atm_test_utils:example_argument_mappers()).


encode_decode_task_result_mapper_test() ->
    encode_decode_test_base(atm_test_utils:example_result_mappers()).


encode_decode_task_schema_test() ->
    encode_decode_test_base(atm_test_utils:example_task_schemas()).


encode_decode_parallel_box_schema_test() ->
    encode_decode_test_base(atm_test_utils:example_parallel_box_schemas()).


encode_decode_lane_schema_test() ->
    encode_decode_test_base(atm_test_utils:example_lane_schemas()).


encode_decode_lambda_revision_test() ->
    [Example | _] = ExampleLambdaRevisions = atm_test_utils:example_lambda_revisions(),
    encode_decode_test_base(ExampleLambdaRevisions),

    check_binary_sanitization(atm_lambda_revision, Example, <<"summary">>, ?SUMMARY_SIZE_LIMIT),
    check_binary_sanitization(atm_lambda_revision, Example, <<"description">>, ?DESCRIPTION_SIZE_LIMIT).


encode_decode_lambda_revision_registry_test() ->
    encode_decode_test_base(atm_test_utils:example_lambda_revision_registries()).


encode_decode_workflow_schema_revision_test() ->
    [Example | _] = ExampleWorkflowSchemaRevisions = atm_test_utils:example_workflow_schema_revisions(),
    encode_decode_test_base(ExampleWorkflowSchemaRevisions),

    check_binary_sanitization(atm_workflow_schema_revision, Example, <<"description">>, ?DESCRIPTION_SIZE_LIMIT).


encode_decode_workflow_schema_revision_registry_test() ->
    encode_decode_test_base(atm_test_utils:example_workflow_schema_revision_registries()).

%%%===================================================================
%%% Tests of upgraded models
%%%===================================================================

atm_task_schema_backward_compatibility_test() ->
    lists:foreach(fun(TaskSchema) ->
        check_backward_compatibility_of_newly_added_field(
            TaskSchema#atm_task_schema{resource_spec_override = undefined},
            <<"resourceSpecOverride">>
        ),
        check_backward_compatibility_of_newly_added_field(
            TaskSchema#atm_task_schema{lambda_revision_number = 1},
            <<"lambdaRevisionNumber">>
        )
    end, atm_test_utils:example_task_schemas()).


atm_store_iterator_spec_backward_compatibility_test() ->
    lists:foreach(fun(StoreIteratorSpec) ->
        check_backward_compatibility_of_newly_added_field(
            StoreIteratorSpec#atm_store_iterator_spec{max_batch_size = 100},
            <<"maxBatchSize">>
        )
    end, atm_test_utils:example_store_iterator_specs()).


atm_lane_schema_backward_compatibility_test() ->
    lists:foreach(fun(LaneSchema) ->
        check_backward_compatibility_of_newly_added_field(
            LaneSchema#atm_lane_schema{max_retries = 0},
            <<"maxRetries">>
        )
    end, atm_test_utils:example_lane_schemas()).


atm_lambda_revision_backward_compatibility_test() ->
    lists:foreach(fun(AtmLambdaRevision) ->
        check_backward_compatibility_of_newly_added_field(
            AtmLambdaRevision#atm_lambda_revision{
                preferred_batch_size = 100
            },
            <<"preferredBatchSize">>
        )
    end, atm_test_utils:example_lambda_revisions()).

%%%===================================================================
%%% Helpers
%%%===================================================================

% In case a new field is added with default value, no upgrader is obligatory.
% This procedure checks that the previous version of the record without the field is correctly parsed.
%% @private
check_backward_compatibility_of_newly_added_field(SubjectRecord, FieldName) ->
    RecordType = utils:record_type(SubjectRecord),

    EncodedPersistentRecord = persistent_record:encode(SubjectRecord, RecordType),
    PersistentRecordJson = json_utils:decode(EncodedPersistentRecord),
    EncodedPersistentRecordWithoutField = json_utils:encode(maps:remove(FieldName, PersistentRecordJson)),
    ?assertEqual(
        SubjectRecord,
        persistent_record:decode(EncodedPersistentRecordWithoutField, RecordType)
    ),

    JsonableRecord = jsonable_record:to_json(SubjectRecord, RecordType),
    JsonableRecordWithoutField = maps:remove(FieldName, JsonableRecord),
    ?assertEqual(
        SubjectRecord,
        jsonable_record:from_json(JsonableRecordWithoutField, RecordType)
    ).


%% @private
check_binary_sanitization(RecordType, Record, DataKey, SizeLimit) ->
    ExampleJson = jsonable_record:to_json(Record, RecordType),
    ?assert(is_record(jsonable_record:from_json(ExampleJson#{
        DataKey => str_utils:rand_hex(SizeLimit div 2)
    }, RecordType), RecordType)),
    ?assertThrow(?ERROR_BAD_VALUE_BINARY_TOO_LARGE(DataKey, SizeLimit), jsonable_record:from_json(ExampleJson#{
        DataKey => str_utils:rand_hex(SizeLimit div 2 + 1)
    }, RecordType)),
    ?assertThrow(?ERROR_BAD_VALUE_BINARY(DataKey), jsonable_record:from_json(ExampleJson#{
        DataKey => lists_utils:random_element([12345, atom, #{<<"a">> => <<"b">>}, [1, 2, 3]])
    }, RecordType)).


%% @private
encode_decode_test_base(Records) ->
    lists:foreach(fun(Record) ->
        ?assert(is_equal_after_json_encode_and_decode(Record)),
        ?assert(is_equal_after_db_encode_and_decode(Record))
    end, Records).


%% @private
is_equal_after_json_encode_and_decode(Record) ->
    is_equal_after_json_encode_and_decode(Record, utils:record_type(Record)).

is_equal_after_json_encode_and_decode(Record, RecordType) ->
    Record =:= jsonable_record:from_json(jsonable_record:to_json(Record, RecordType), RecordType).


%% @private
is_equal_after_db_encode_and_decode(Record) ->
    is_equal_after_db_encode_and_decode(Record, utils:record_type(Record)).

is_equal_after_db_encode_and_decode(Record, RecordType) ->
    Record =:= persistent_record:decode(persistent_record:encode(Record, RecordType), RecordType).

-endif.
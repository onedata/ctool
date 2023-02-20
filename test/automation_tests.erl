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
-include("test/test_utils.hrl").
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
    ?assert(eunit_utils:throws_error_during_decode_from_json(?ERROR_BAD_DATA(<<"atmResourceSpec">>), [
        Example#atm_resource_spec{cpu_requested = undefined},
        Example#atm_resource_spec{cpu_limit = <<"text">>},
        Example#atm_resource_spec{memory_requested = 17.8},
        Example#atm_resource_spec{memory_limit = #{<<"map">> => <<"val">>}},
        Example#atm_resource_spec{ephemeral_storage_requested = -4},
        Example#atm_resource_spec{ephemeral_storage_limit = 0}
    ])).


encode_decode_atm_data_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_data_specs()),

    ExpDuplicateNameMatchersError = ?ERROR_BAD_DATA(
        <<"valueConstraints.specs">>,
        <<"There cannot be two measurement specs with the same name matcher">>
    ),
    ?assert(eunit_utils:throws_error_during_decode_from_json(ExpDuplicateNameMatchersError, atm_data_spec, #atm_data_spec{
        type = atm_time_series_measurement_type,
        value_constraints = #{specs => [
            #atm_time_series_measurement_spec{name_matcher_type = exact, name_matcher = <<"latency">>, unit = milliseconds},
            #atm_time_series_measurement_spec{name_matcher_type = exact, name_matcher = <<"throughput">>, unit = bytes_per_sec},
            #atm_time_series_measurement_spec{name_matcher_type = has_prefix, name_matcher = <<"latency">>, unit = milliseconds}
        ]}
    })),

    ExpBadNumberConstraintsError = ?ERROR_BAD_DATA(
        <<"valueConstraints">>,
        <<"You must provide a list of numbers">>
    ),
    ?assert(eunit_utils:throws_error_during_decode_from_json(ExpBadNumberConstraintsError, atm_data_spec, #atm_data_spec{
        type = atm_number_type,
        value_constraints = #{integers_only => ?RAND_BOOL(), allowed_values => <<"string">>}
    })),
    ?assert(eunit_utils:throws_error_during_decode_from_json(ExpBadNumberConstraintsError, atm_data_spec, #atm_data_spec{
        type = atm_number_type,
        value_constraints = #{integers_only => ?RAND_BOOL(), allowed_values => [<<"list">>, <<"of">>, <<"strings">>]}
    })),

    ExpBadStringConstraintsError = ?ERROR_BAD_DATA(
        <<"valueConstraints">>,
        <<"You must provide a list of strings">>
    ),
    ?assert(eunit_utils:throws_error_during_decode_from_json(ExpBadStringConstraintsError, atm_data_spec, #atm_data_spec{
        type = atm_string_type,
        value_constraints = #{allowed_values => 234.7}
    })),
    ?assert(eunit_utils:throws_error_during_decode_from_json(ExpBadStringConstraintsError, atm_data_spec, #atm_data_spec{
        type = atm_string_type,
        value_constraints = #{allowed_values => #{<<"key">> => <<"value">>}}
    })).


encode_decode_operation_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_operation_specs()),

    ExpDisallowedOperationSpecError = ?ERROR_BAD_VALUE_NOT_ALLOWED(
        <<"operationSpec.engine">>,
        lists:map(
            fun atm_lambda_operation_spec:engine_to_json/1,
            atm_lambda_operation_spec:allowed_engines_for_custom_lambdas()
        )
    ),
    ?assert(eunit_utils:throws_error_during_decode_from_json(ExpDisallowedOperationSpecError, atm_lambda_operation_spec, [
        #atm_onedata_function_operation_spec{function_id = atm_test_utils:example_name()},
        #atm_workflow_operation_spec{atm_workflow_id = atm_test_utils:example_id()},
        #atm_user_form_operation_spec{user_form_id = atm_test_utils:example_id()}
    ])).


encode_decode_docker_execution_options_test() ->
    encode_decode_test_base(atm_test_utils:example_docker_execution_options()).


encode_decode_parameter_spec_test() ->
    [Example | _] = ExampleParameterSpecs = atm_test_utils:example_parameter_specs(),
    encode_decode_test_base(ExampleParameterSpecs),

    ?assert(eunit_utils:throws_error_during_decode_from_json(
        ?ERROR_BAD_VALUE_NAME(<<"parameterSpec.name">>),
        Example#atm_parameter_spec{name = <<"*@#$^!R!*!^$@!@(">>}
    )).


encode_decode_lambda_result_spec_test() ->
    [Example | _] = ExampleResultSpecs = atm_test_utils:example_result_specs(),
    encode_decode_test_base(ExampleResultSpecs),

    ?assert(eunit_utils:throws_error_during_decode_from_json(
        ?ERROR_BAD_VALUE_NAME(<<"resultSpec.name">>),
        Example#atm_lambda_result_spec{name = <<"><<>:.,{:<.',.;,'.;">>}
    )).


encode_decode_store_schema_test() ->
    encode_decode_test_base(atm_test_utils:example_store_schemas()),

    % these two stores do not allow initial content to be specified, the corresponding fields are implicitly adjusted
    lists:foreach(fun(StoreType) ->
        Example = atm_test_utils:example_store_schema(StoreType),
        ?assertEqual(
            Example#atm_store_schema{
                requires_initial_content = false,
                default_initial_content = undefined
            },
            jsonable_record:from_json(jsonable_record:to_json(Example#atm_store_schema{
                requires_initial_content = true,
                default_initial_content = atm_test_utils:example_predefined_value(atm_test_utils:example_data_spec())
            }, atm_store_schema), atm_store_schema)
        )
    end, [time_series, audit_log]).


encode_decode_store_config_test() ->
    encode_decode_test_base(atm_test_utils:example_store_configs()).


encode_decode_tree_forest_store_config_test() ->
    % tree forest store limits available data types to those compatible with tree models
    AllowedDataTypes = [atm_file_type, atm_dataset_type],
    ExpTreeForestCfgError = ?ERROR_BAD_VALUE_NOT_ALLOWED(
        <<"treeForestStoreConfig.dataSpec.type">>,
        [atm_data_type:type_to_json(T) || T <- AllowedDataTypes]
    ),
    lists:foreach(fun(DisallowedDataType) ->
        ?assert(eunit_utils:throws_error_during_decode_from_json(ExpTreeForestCfgError, #atm_tree_forest_store_config{
            item_data_spec = atm_test_utils:example_data_spec(DisallowedDataType)
        }))
    end, atm_data_type:all_data_types() -- AllowedDataTypes).


% dashboard specs are skipped during example generation
% (hence effectively never checked during the other tests),
% make sure they are properly encoded/decoded when used within a time series store
encode_decode_time_series_store_config_with_dashboard_spec_test() ->
    encode_decode_test_base(#atm_time_series_store_config{
        time_series_collection_schema = #time_series_collection_schema{
            time_series_schemas = ?RAND_SUBLIST(time_series_test_utils:example_time_series_schemas(), 1, all)
        },
        dashboard_spec = time_series_test_utils:example_dashboard_spec()
    }).


encode_decode_store_iterator_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_store_iterator_specs()).


encode_decode_store_content_update_options_test() ->
    encode_decode_test_base(atm_test_utils:example_store_content_update_options_records()).


encode_decode_time_series_dispatch_rule_test() ->
    encode_decode_test_base(atm_test_utils:example_time_series_dispatch_rules()).


encode_decode_task_argument_mapper_test() ->
    encode_decode_test_base(atm_test_utils:example_argument_mappers()).


encode_decode_task_result_mapper_test() ->
    encode_decode_test_base(atm_test_utils:example_result_mappers()).


encode_decode_task_schema_test() ->
    encode_decode_test_base(atm_test_utils:example_task_schemas()).


encode_decode_parallel_box_schema_test() ->
    encode_decode_test_base(atm_test_utils:example_parallel_box_schemas()).


encode_decode_lane_schema_test() ->
    [FirstExample | OtherExamples] = atm_test_utils:example_lane_schemas(),
    encode_decode_test_base(OtherExamples),
    % dashboard specs are skipped during example generation
    % (hence effectively never checked during the other tests),
    % make sure they are properly encoded/decoded when used within a lane schema
    encode_decode_test_base(FirstExample#atm_lane_schema{
        dashboard_spec = time_series_test_utils:example_dashboard_spec()
    }).


encode_decode_lambda_revision_test() ->
    [Example | _] = ExampleLambdaRevisions = atm_test_utils:example_lambda_revisions(),
    encode_decode_test_base(ExampleLambdaRevisions),

    check_binary_sanitization(atm_lambda_revision, Example, <<"summary">>, ?SUMMARY_SIZE_LIMIT),
    check_binary_sanitization(atm_lambda_revision, Example, <<"description">>, ?DESCRIPTION_SIZE_LIMIT).


encode_decode_lambda_revision_registry_test() ->
    encode_decode_test_base(atm_test_utils:example_lambda_revision_registries()).


encode_decode_workflow_schema_revision_test() ->
    [FirstExample | OtherExamples] = atm_test_utils:example_workflow_schema_revisions(),
    encode_decode_test_base(OtherExamples),

    % dashboard specs are skipped during example generation
    % (hence effectively never checked during the other tests),
    % make sure they are properly encoded/decoded when used within a workflow schema
    encode_decode_test_base(FirstExample#atm_workflow_schema_revision{
        dashboard_spec = time_series_test_utils:example_dashboard_spec()
    }),

    check_binary_sanitization(atm_workflow_schema_revision, FirstExample, <<"description">>, ?DESCRIPTION_SIZE_LIMIT).


encode_decode_workflow_schema_revision_registry_test() ->
    encode_decode_test_base(atm_test_utils:example_workflow_schema_revision_registries()).


encode_decode_time_series_measurement_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_time_series_measurement_specs()).

%%%===================================================================
%%% Helpers
%%%===================================================================

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
encode_decode_test_base(Record) when not is_list(Record) ->
    encode_decode_test_base([Record]);
encode_decode_test_base(Records) ->
    lists:foreach(fun(Record) ->
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(Record)),
        ?assert(eunit_utils:is_equal_after_db_encode_and_decode(Record))
    end, Records).


-endif.
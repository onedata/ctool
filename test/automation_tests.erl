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
    check_error_during_decode_from_json(?ERROR_BAD_DATA(<<"atmResourceSpec">>), [
        Example#atm_resource_spec{cpu_requested = undefined},
        Example#atm_resource_spec{cpu_limit = <<"text">>},
        Example#atm_resource_spec{memory_requested = 17.8},
        Example#atm_resource_spec{memory_limit = #{<<"map">> => <<"val">>}},
        Example#atm_resource_spec{ephemeral_storage_requested = -4},
        Example#atm_resource_spec{ephemeral_storage_limit = 0}
    ]).


encode_decode_atm_data_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_data_specs()).


encode_decode_operation_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_operation_specs()),

    ExpDisallowedOperationSpecError = ?ERROR_BAD_VALUE_NOT_ALLOWED(
        <<"operationSpec.engine">>,
        lists:map(
            fun atm_lambda_operation_spec:engine_to_json/1,
            atm_lambda_operation_spec:allowed_engines_for_custom_lambdas()
        )
    ),
    check_error_during_decode_from_json(ExpDisallowedOperationSpecError, atm_lambda_operation_spec, [
        #atm_onedata_function_operation_spec{function_id = atm_test_utils:example_name()},
        #atm_workflow_operation_spec{atm_workflow_id = atm_test_utils:example_id()},
        #atm_user_form_operation_spec{user_form_id = atm_test_utils:example_id()}
    ]).


encode_decode_docker_execution_options_test() ->
    encode_decode_test_base(atm_test_utils:example_docker_execution_options()).


encode_decode_lambda_argument_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_argument_specs()).


encode_decode_lambda_result_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_result_specs()).


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
    encode_decode_test_base(atm_test_utils:example_store_configs()),

    % tree forest store limits available data types to those compatible with tree models
    AllowedDataTypes = [atm_file_type, atm_dataset_type],
    ExpTreeForestCfgError = ?ERROR_BAD_VALUE_NOT_ALLOWED(
        <<"treeForestStoreConfig.dataSpec.type">>,
        [atm_data_type:type_to_json(T) || T <- AllowedDataTypes]
    ),
    lists:foreach(fun(DisallowedDataType) ->
        check_error_during_decode_from_json(ExpTreeForestCfgError, #atm_tree_forest_store_config{
            item_data_spec = atm_test_utils:example_data_spec(DisallowedDataType)
        })
    end, atm_data_type:all_data_types() -- AllowedDataTypes),

    % time series store cannot have conflicting name generators
    MakeSchema = fun(NameGeneratorType, NameGenerator) ->
        ExampleSchema = atm_test_utils:example_time_series_schema(),
        ExampleSchema#atm_time_series_schema{
            name_generator_type = NameGeneratorType,
            name_generator = NameGenerator
        }
    end,
    SchemasTestCases = [
        {conflict, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"count_">>),
            MakeSchema(exact, <<"throughput">>)
        ]},
        {correct, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"files_">>),
            MakeSchema(exact, <<"count_mp3">>)
        ]},
        {conflict, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(exact, <<"count_mp3">>),
            MakeSchema(add_prefix, <<"count_">>)
        ]},
        {correct, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(exact, <<"count/mp3">>),
            MakeSchema(add_prefix, <<"count_">>)
        ]},
        {conflict, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"files_">>),
            MakeSchema(add_prefix, <<"files_count_">>)
        ]},
        {correct, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"files_">>),
            MakeSchema(add_prefix, <<"file_count_">>)
        ]}
    ],
    ExpTimeSeriesCfgError = ?ERROR_BAD_DATA(<<"schemas">>, <<
        "Provided time series schemas have conflicting name generators; the generators "
        "cannot have the same values and no 'add_prefix' generator can be a prefix "
        "of any other generator."
    >>),
    lists:foreach(fun
        ({conflict, ConflictingSchemaSet}) ->
            check_error_during_decode_from_json(ExpTimeSeriesCfgError, #atm_time_series_store_config{
                schemas = lists_utils:shuffle(ConflictingSchemaSet)
            });
        ({correct, CorrectSchemaSet}) ->
            encode_decode_test_base(#atm_time_series_store_config{
                schemas = lists_utils:shuffle(CorrectSchemaSet)
            })
    end, SchemasTestCases).


encode_decode_store_iterator_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_store_iterator_specs()).


encode_decode_store_content_update_options_test() ->
    encode_decode_test_base(atm_test_utils:example_store_content_update_options_records()),

    ExampleDispatchRules = atm_test_utils:example_time_series_dispatch_rules(),

    ExpError = ?ERROR_BAD_DATA(
        <<"dispatchRules">>,
        <<"There cannot be two dispatch rules with the same name matcher">>
    ),
    check_error_during_decode_from_json(ExpError, #atm_time_series_content_update_options{
        dispatch_rules = [hd(ExampleDispatchRules) | ExampleDispatchRules]
    }).


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


encode_decode_time_series_measurements_spec_test() ->
    encode_decode_test_base(atm_test_utils:example_time_series_measurements_specs()).


encode_decode_time_series_metric_spec_test() ->
    [Example | _] = ExampleTimeSeriesMetricSpecs = atm_test_utils:example_metric_configs(),
    encode_decode_test_base(ExampleTimeSeriesMetricSpecs),

    check_error_during_decode_from_json(
        ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resolution">>, metric_config:allowed_resolutions()),
        Example#metric_config{resolution = 1337}
    ).


encode_decode_time_series_schema_test() ->
    [Example | _] = ExampleTimeSeriesSpecs = atm_test_utils:example_time_series_schemas(),
    encode_decode_test_base(ExampleTimeSeriesSpecs),

    check_error_during_decode_from_json(
        ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same label">>),
        Example#atm_time_series_schema{metrics = [
            #metric_config{label = <<"label1">>, resolution = 3600, retention = 12, aggregator = sum},
            #metric_config{label = <<"label1">>, resolution = 3600, retention = 12, aggregator = min}
        ]}
    ),
    check_error_during_decode_from_json(
        ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same resolution and aggregator">>),
        Example#atm_time_series_schema{metrics = [
            #metric_config{label = <<"label1">>, resolution = 3600, retention = 12, aggregator = sum},
            #metric_config{label = <<"label2">>, resolution = 3600, retention = 13, aggregator = sum}
        ]}
    ).

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


% validation is done during decoding from json, so it is possible to encode an invalid record
% to json, but the get an error when decoding it back from json
%% @private
check_error_during_decode_from_json(ExpError, Records) when is_list(Records) ->
    check_error_during_decode_from_json(ExpError, utils:record_type(hd(Records)), Records);
check_error_during_decode_from_json(ExpError, Record) ->
    check_error_during_decode_from_json(ExpError, utils:record_type(Record), Record).

check_error_during_decode_from_json(ExpError, RecordType, Records) when is_list(Records) ->
    lists:foreach(fun(Record) ->
        check_error_during_decode_from_json(ExpError, RecordType, Record)
    end, Records);
check_error_during_decode_from_json(ExpError, RecordType, Record) ->
    RecordJson = jsonable_record:to_json(Record, RecordType),
    ?assertThrow(ExpError, jsonable_record:from_json(RecordJson, RecordType)),
    % validation should be done only when decoding from json (not during db decoding)
    RecordDbEncoded = persistent_record:encode(Record, RecordType),
    ?assertEqual(Record, persistent_record:decode(RecordDbEncoded, RecordType)).


%% @private
encode_decode_test_base(Record) when not is_list(Record) ->
    encode_decode_test_base([Record]);
encode_decode_test_base(Records) ->
    lists:foreach(fun(Record) ->
        ?assert(is_equal_after_json_encode_and_decode(Record)),
        ?assert(is_equal_after_db_encode_and_decode(Record))
    end, Records).


%% @private
is_equal_after_json_encode_and_decode(Record) ->
    is_equal_after_json_encode_and_decode(Record, utils:record_type(Record)).

is_equal_after_json_encode_and_decode(Record, RecordType) ->
    case jsonable_record:from_json(jsonable_record:to_json(Record, RecordType), RecordType) of
        Record ->
            true;
        Other ->
            io:format(user, "Record different after json encode and decode!~nExpected: ~p~nGot:      ~p", [
                Record, Other
            ]),
            false
    end.


%% @private
is_equal_after_db_encode_and_decode(Record) ->
    is_equal_after_db_encode_and_decode(Record, utils:record_type(Record)).

is_equal_after_db_encode_and_decode(Record, RecordType) ->
    case persistent_record:decode(persistent_record:encode(Record, RecordType), RecordType) of
        Record ->
            true;
        Other ->
            io:format(user, "Record different after DB encode and decode!~nExpected: ~p~nGot:      ~p", [
                Record, Other
            ]),
            false
    end.

-endif.
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

-define(RAND_STR(), str_utils:rand_hex(16)).
-define(RAND_BOOL(), lists_utils:random_element([true, false])).
-define(RAND_INT(From, To), From + rand:uniform(To - From + 1) - 1).

encode_decode_atm_resource_spec_test() ->
    Example = example_resource_spec(),
    ExampleJson = atm_resource_spec:to_json(Example),
    ?assert(is_equal_after_json_encode_and_decode(Example)),
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
        <<"ephemeralStorageLimit">> => -16
    })).


encode_decode_atm_data_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ?assert(is_equal_after_json_encode_and_decode(DataSpec))
    end, example_data_specs()).


encode_decode_operation_spec_test() ->
    OperationSpecExamples = [
        #atm_onedata_function_operation_spec{
            function_id = ?RAND_STR()
        },
        #atm_openfaas_operation_spec{
            docker_image = ?RAND_STR(),
            docker_execution_options = #atm_docker_execution_options{
                readonly = ?RAND_BOOL(),
                mount_oneclient = ?RAND_BOOL(),
                oneclient_mount_point = <<"/a/b/c/d/", (?RAND_STR())/binary>>,
                oneclient_options = lists_utils:random_element([<<"">>, <<"--a --b">>])
            }
        },
        #atm_workflow_operation_spec{
            atm_workflow_id = ?RAND_STR()
        },
        #atm_user_form_operation_spec{
            user_form_id = ?RAND_STR()
        }
    ],
    lists:foreach(fun(OperationSpec) ->
        ?assert(is_equal_after_json_encode_and_decode(OperationSpec, atm_lambda_operation_spec)),
        ?assert(is_equal_after_db_encode_and_decode(OperationSpec, atm_lambda_operation_spec))
    end, OperationSpecExamples).


encode_decode_docker_execution_options_test() ->
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{}
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            readonly = true
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            readonly = false
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            mount_oneclient = ?RAND_BOOL()
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            oneclient_mount_point = <<"/a/b/c/d">>
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            mount_oneclient = true,
            oneclient_mount_point = <<"/a/b/c/d">>
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            mount_oneclient = true,
            oneclient_mount_point = <<"/a/b/c/d">>,
            oneclient_options = <<"--a --b">>
        }
    ).

encode_decode_docker_execution_options_test_base(ExecutionOptions) ->
    ?assert(is_equal_after_json_encode_and_decode(ExecutionOptions)),
    ?assert(is_equal_after_db_encode_and_decode(ExecutionOptions)).


encode_decode_lambda_argument_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ArgumentSpec = #atm_lambda_argument_spec{
            name = ?RAND_STR(),
            data_spec = DataSpec,
            is_batch = ?RAND_BOOL(),
            is_optional = ?RAND_BOOL(),
            default_value = gen_json_term()
        },
        ?assert(is_equal_after_json_encode_and_decode(ArgumentSpec)),
        ?assert(is_equal_after_db_encode_and_decode(ArgumentSpec))
    end, example_data_specs()).


encode_decode_lambda_result_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ResultSpec = #atm_lambda_result_spec{
            name = ?RAND_STR(),
            data_spec = DataSpec,
            is_batch = ?RAND_BOOL()
        },
        ?assert(is_equal_after_json_encode_and_decode(ResultSpec)),
        ?assert(is_equal_after_db_encode_and_decode(ResultSpec))
    end, example_data_specs()).


encode_decode_store_schema_test() ->
    lists:foreach(fun(DataSpec) ->
        StoreSchema = #atm_store_schema{
            id = ?RAND_STR(),
            name = ?RAND_STR(),
            description = ?RAND_STR(),
            type = lists_utils:random_element(automation:all_store_types()),
            data_spec = DataSpec,
            requires_initial_value = ?RAND_BOOL(),
            default_initial_value = lists_utils:random_element([undefined, gen_json_term()])
        },
        ?assert(is_equal_after_json_encode_and_decode(StoreSchema)),
        ?assert(is_equal_after_db_encode_and_decode(StoreSchema))
    end, example_data_specs()).


encode_decode_store_iterator_spec_test() ->
    lists:foreach(fun(StoreIteratorSpec) ->
        ?assert(is_equal_after_json_encode_and_decode(StoreIteratorSpec)),
        ?assert(is_equal_after_db_encode_and_decode(StoreIteratorSpec))
    end, example_store_iterator_specs()).


encode_decode_task_argument_mapper_test() ->
    lists:foreach(fun(TaskArgumentMapper) ->
        ?assert(is_equal_after_json_encode_and_decode(TaskArgumentMapper)),
        ?assert(is_equal_after_db_encode_and_decode(TaskArgumentMapper))
    end, example_argument_mappers()).


encode_decode_task_result_mapper_test() ->
    lists:foreach(fun(TaskResultMapper) ->
        ?assert(is_equal_after_json_encode_and_decode(TaskResultMapper)),
        ?assert(is_equal_after_db_encode_and_decode(TaskResultMapper))
    end, example_result_mappers()).


encode_decode_task_schema_test() ->
    lists:foreach(fun(TaskSchema) ->
        ?assert(is_equal_after_json_encode_and_decode(TaskSchema)),
        ?assert(is_equal_after_db_encode_and_decode(TaskSchema))
    end, example_task_schemas()).


encode_decode_parallel_box_schema_test() ->
    lists:foreach(fun(ParallelBoxSchema) ->
        ?assert(is_equal_after_json_encode_and_decode(ParallelBoxSchema)),
        ?assert(is_equal_after_db_encode_and_decode(ParallelBoxSchema))
    end, example_parallel_box_schemas()).


encode_decode_lane_schema_test() ->
    lists:foreach(fun(LaneSchema) ->
        ?assert(is_equal_after_json_encode_and_decode(LaneSchema)),
        ?assert(is_equal_after_db_encode_and_decode(LaneSchema))
    end, example_lane_schemas()).

%%%===================================================================
%%% Tests of upgraded models
%%%===================================================================

atm_lane_schema_backward_compatibility_test() ->
    lists:foreach(fun(LaneSchema) ->
        check_backward_compatibility_of_newly_added_field(
            LaneSchema#atm_lane_schema{max_retries = 0},
            <<"maxRetries">>
        )
    end, example_lane_schemas()).


atm_task_schema_backward_compatibility_test() ->
    lists:foreach(fun(TaskSchema) ->
        check_backward_compatibility_of_newly_added_field(
            TaskSchema#atm_task_schema{resource_spec_override = undefined},
            <<"resourceSpecOverride">>
        )
    end, example_task_schemas()).


% In case a new field is added with default value, no upgrader is obligatory.
% This procedure checks that the previous version of the record without the field is correctly parsed
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

%%%===================================================================
%%% Helper functions
%%%===================================================================

example_resource_spec() ->
    #atm_resource_spec{
        cpu_requested = rand:uniform() * 10,
        cpu_limit = lists_utils:random_element([undefined, rand:uniform() * 10]),

        memory_requested = ?RAND_INT(10000, 1000000000),
        memory_limit = lists_utils:random_element([undefined, ?RAND_INT(10000, 1000000000)]),

        ephemeral_storage_requested = ?RAND_INT(1000, 10000000000),
        ephemeral_storage_limit = lists_utils:random_element([undefined, ?RAND_INT(1000, 10000000000)])
    }.


example_data_specs() ->
    GenExampleValueConstraints = fun
        (atm_file_type) ->
            lists_utils:random_element([#{file_type => lists_utils:random_element(['REG', 'DIR', 'ANY'])}]);
        (atm_store_credentials_type) ->
            #{store_type => lists_utils:random_element(automation:all_store_types())};
        (_) ->
            #{}
    end,
    lists:map(fun(Type) ->
        #atm_data_spec{
            type = Type,
            value_constraints = GenExampleValueConstraints(Type)
        }
    end, atm_data_type:all_data_types()).


example_store_iterator_specs() ->
    [
        #atm_store_iterator_spec{
            store_schema_id = ?RAND_STR(),
            strategy = #atm_store_iterator_serial_strategy{}
        },
        #atm_store_iterator_spec{
            store_schema_id = ?RAND_STR(),
            strategy = #atm_store_iterator_batch_strategy{size = rand:uniform(1000)}
        }
    ].


example_argument_mappers() ->
    lists:map(fun(_) ->
        #atm_task_schema_argument_mapper{
            argument_name = ?RAND_STR(),
            value_builder = gen_example_argument_value_builder()
        }
    end, lists:seq(1, 10)).


example_result_mappers() ->
    lists:map(fun(DispatchFunction) ->
        #atm_task_schema_result_mapper{
            result_name = ?RAND_STR(),
            store_schema_id = ?RAND_STR(),
            dispatch_function = DispatchFunction
        }
    end, atm_task_schema_result_mapper:all_dispatch_functions()).


gen_example_argument_value_builder() ->
    case rand:uniform(6) of
        1 -> #atm_task_argument_value_builder{
            type = iterated_item, recipe = lists_utils:random_element([
                undefined,
                lists_utils:random_sublist(["key1", "key2", "key3", 0, 1, 2])
            ])
        };
        2 -> #atm_task_argument_value_builder{
            type = const, recipe = lists_utils:random_element([
                ?RAND_STR(), 0, 151, 27.8, #{<<"key">> => <<"val">>}, #{<<"key">> => #{<<"nested">> => [?RAND_STR(), 0, 27.8]}}
            ])
        };
        3 -> #atm_task_argument_value_builder{
            type = object, recipe = maps:from_list(lists:map(fun(_) ->
                {?RAND_STR(), gen_example_argument_value_builder()}
            end, lists:seq(1, rand:uniform(7))))
        };
        4 -> #atm_task_argument_value_builder{
            type = store_credentials, recipe = ?RAND_STR()
        };
        5 -> #atm_task_argument_value_builder{
            type = single_value_store_content, recipe = ?RAND_STR()
        };
        6 -> #atm_task_argument_value_builder{
            type = onedatafs_credentials, recipe = undefined
        }
    end.


example_task_schemas() ->
    lists:map(fun(_) ->
        #atm_task_schema{
            id = ?RAND_STR(),
            name = ?RAND_STR(),
            lambda_id = ?RAND_STR(),
            argument_mappings = lists_utils:random_sublist(example_argument_mappers()),
            result_mappings = lists_utils:random_sublist(example_result_mappers()),
            resource_spec_override = lists_utils:random_element([undefined, example_resource_spec()])
        }
    end, lists:seq(1, 10)).


example_parallel_box_schemas() ->
    lists:map(fun(_) ->
        #atm_parallel_box_schema{
            id = ?RAND_STR(),
            name = ?RAND_STR(),
            tasks = lists_utils:random_sublist(example_task_schemas())
        }
    end, lists:seq(1, 10)).


example_lane_schemas() ->
    lists:map(fun(_) ->
        #atm_lane_schema{
            id = ?RAND_STR(),
            name = ?RAND_STR(),
            parallel_boxes = lists_utils:random_sublist(example_parallel_box_schemas()),
            store_iterator_spec = lists_utils:random_element(example_store_iterator_specs()),
            max_retries = ?RAND_INT(0, 10)
        }
    end, lists:seq(1, 10)).


gen_json_term() ->
    lists_utils:random_element([
        true,
        false,
        4351,
        17.75,
        <<"text">>,
        #{<<"object">> => 134},
        #{<<"nested">> => #{<<"object">> => <<"text">>}}
    ]).


is_equal_after_json_encode_and_decode(Record) ->
    is_equal_after_json_encode_and_decode(Record, utils:record_type(Record)).

is_equal_after_json_encode_and_decode(Record, RecordType) ->
    Record =:= jsonable_record:from_json(jsonable_record:to_json(Record, RecordType), RecordType).


is_equal_after_db_encode_and_decode(Record) ->
    is_equal_after_db_encode_and_decode(Record, utils:record_type(Record)).

is_equal_after_db_encode_and_decode(Record, RecordType) ->
    Record =:= persistent_record:decode(persistent_record:encode(Record, RecordType), RecordType).

-endif.
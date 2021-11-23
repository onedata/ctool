%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper function for tests of automation related modules.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_test_utils).
-author("Lukasz Opiola").

-include("automation/automation.hrl").

-define(RAND_STR(), ?RAND_STR(16)).
-define(RAND_STR(Bytes), str_utils:rand_hex(Bytes)).
-define(RAND_BOOL(), lists_utils:random_element([true, false])).
-define(RAND_INT(From, To), From + rand:uniform(To - From + 1) - 1).

-export([example_id/0]).
-export([example_name/0]).
-export([example_summary/0]).
-export([example_description/0]).
-export([example_lifecycle_state/0]).
-export([example_store_type/0]).
-export([example_operation_spec/0, example_operation_specs/0]).
-export([example_docker_execution_options/0]).
-export([example_argument_spec/0, example_argument_spec/1, example_argument_spec/2, example_argument_specs/0]).
-export([example_result_spec/0, example_result_spec/1, example_result_specs/0]).
-export([example_resource_specs/0]).
-export([example_data_spec/0, example_data_spec/1, example_data_specs/0]).
-export([example_initial_value/1]).
-export([example_store_schema/0, example_store_schema/1, example_store_schema/3, example_store_schemas/0]).
-export([example_store_iterator_spec/1, example_store_iterator_specs/0]).
-export([example_argument_mappers/0, example_argument_mappers/2, example_argument_mappers_for_specs/2]).
-export([example_result_mappers/0, example_result_mappers/2, example_result_mappers_for_specs/2]).
-export([example_argument_value_builder/0, example_argument_value_builder/1]).
-export([example_task_schema/2, example_task_schemas/0, example_task_schemas/2]).
-export([example_parallel_box_schema_with_tasks/1, example_parallel_box_schemas/0]).
-export([example_lane_schema_with_parallel_boxes/2, example_lane_schemas/0]).
-export([example_lambda_revision/0, example_lambda_revisions/0]).
-export([example_lambda_revision_registry/0, example_lambda_revision_registry/1, example_lambda_revision_registries/0]).
-export([example_workflow_schema_revisions/0]).
-export([example_workflow_schema_revision_registries/0]).
-export([available_store_types_for_data_spec/1]).

-type lambda_registries() :: #{AtmLambdaId :: automation:id() => atm_lambda_revision_registry:record()}.
-export_type([lambda_registries/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec example_id() -> automation:id().
example_id() ->
    ?RAND_STR(16).

-spec example_name() -> automation:name().
example_name() ->
    ?RAND_STR(rand:uniform(10) + 10).


-spec example_summary() -> automation:summary().
example_summary() ->
    lists_utils:random_element([<<>>, ?RAND_STR(rand:uniform(50))]).


-spec example_description() -> automation:description().
example_description() ->
    lists_utils:random_element([<<>>, ?RAND_STR(rand:uniform(1000) + 50)]).


-spec example_lifecycle_state() -> json_utils:json_term().
example_lifecycle_state() ->
    lists_utils:random_element(automation:all_lifecycle_states()).


-spec example_store_type() -> automation:store_type().
example_store_type() ->
    lists_utils:random_element(automation:all_store_types()).


-spec example_operation_spec() -> atm_lambda_operation_spec:record().
example_operation_spec() ->
    lists_utils:random_element(example_operation_specs()).


-spec example_operation_specs() -> [atm_lambda_operation_spec:record()].
example_operation_specs() ->
    [
        #atm_openfaas_operation_spec{
            docker_image = ?RAND_STR(),
            docker_execution_options = #atm_docker_execution_options{
                readonly = ?RAND_BOOL(),
                mount_oneclient = ?RAND_BOOL(),
                oneclient_mount_point = <<"/a/b/c/d/", (?RAND_STR())/binary>>,
                oneclient_options = lists_utils:random_element([<<"">>, <<"--a --b">>])
            }
        }
        % @TODO VFS-8582 Implement automation engines other than OpenFaaS
%%        #atm_onedata_function_operation_spec{
%%            function_id = example_id()
%%        },
%%        #atm_workflow_operation_spec{
%%            atm_workflow_id = example_id()
%%        },
%%        #atm_user_form_operation_spec{
%%            user_form_id = example_id()
%%        }
    ].


-spec example_docker_execution_options() -> [atm_docker_execution_options:record()].
example_docker_execution_options() ->
    [
        #atm_docker_execution_options{},
        #atm_docker_execution_options{
            readonly = true
        },
        #atm_docker_execution_options{
            readonly = false
        },
        #atm_docker_execution_options{
            mount_oneclient = ?RAND_BOOL()
        },
        #atm_docker_execution_options{
            oneclient_mount_point = <<"/a/b/c/d">>
        },
        #atm_docker_execution_options{
            mount_oneclient = true,
            oneclient_mount_point = <<"/a/b/c/d">>
        },
        #atm_docker_execution_options{
            mount_oneclient = true,
            oneclient_mount_point = <<"/a/b/c/d">>,
            oneclient_options = <<"--a --b">>
        }
    ].


-spec example_argument_spec() -> atm_lambda_argument_spec:record().
example_argument_spec() ->
    example_argument_spec(example_data_spec()).

-spec example_argument_spec(atm_data_spec:record()) -> atm_lambda_argument_spec:record().
example_argument_spec(DataSpec) ->
    DefaultValue = lists_utils:random_element([undefined, example_initial_value(DataSpec#atm_data_spec.type)]),
    example_argument_spec(DataSpec, DefaultValue).

-spec example_argument_spec(atm_data_spec:record(), term()) -> atm_lambda_argument_spec:record().
example_argument_spec(DataSpec, DefaultValue) ->
    #atm_lambda_argument_spec{
        name = ?RAND_STR(),
        data_spec = DataSpec,
        is_optional = ?RAND_BOOL(),
        default_value = DefaultValue
    }.

-spec example_argument_specs() -> [atm_lambda_argument_spec:record()].
example_argument_specs() ->
    lists:map(fun example_argument_spec/1, example_data_specs()).


-spec example_result_spec() -> atm_lambda_result_spec:record().
example_result_spec() ->
    example_result_spec(example_data_spec()).

-spec example_result_spec(atm_data_spec:record()) -> atm_lambda_result_spec:record().
example_result_spec(DataSpec) ->
    #atm_lambda_result_spec{
        name = example_name(),
        data_spec = DataSpec
    }.

-spec example_result_specs() -> [atm_lambda_result_spec:record()].
example_result_specs() ->
    lists:map(fun example_result_spec/1, example_data_specs()).


-spec example_resource_spec() -> atm_resource_spec:record().
example_resource_spec() ->
    #atm_resource_spec{
        cpu_requested = rand:uniform() * 10,
        cpu_limit = lists_utils:random_element([undefined, rand:uniform() * 10]),

        memory_requested = ?RAND_INT(10000, 1000000000),
        memory_limit = lists_utils:random_element([undefined, ?RAND_INT(10000, 1000000000)]),

        ephemeral_storage_requested = ?RAND_INT(1000, 10000000000),
        ephemeral_storage_limit = lists_utils:random_element([undefined, ?RAND_INT(1000, 10000000000)])
    }.

-spec example_resource_specs() -> [atm_resource_spec:record()].
example_resource_specs() ->
    lists_utils:generate(fun example_resource_spec/0, 7).


-spec example_data_spec() -> atm_data_spec:record().
example_data_spec() ->
    example_data_spec(lists_utils:random_element(atm_data_type:all_data_types())).

-spec example_data_spec(atm_data_type:type()) -> atm_data_spec:record().
example_data_spec(atm_file_type) ->
    #atm_data_spec{
        type = atm_file_type,
        value_constraints = #{file_type => lists_utils:random_element(['REG', 'DIR', 'ANY'])}
    };
example_data_spec(atm_store_credentials_type) ->
    #atm_data_spec{
        type = atm_store_credentials_type,
        value_constraints = #{store_type => example_store_type()}
    };
example_data_spec(DataType) ->
    #atm_data_spec{
        type = DataType,
        value_constraints = #{}
    }.


-spec example_data_specs() -> [atm_data_spec:record()].
example_data_specs() ->
    lists:map(fun example_data_spec/1, atm_data_type:all_data_types()).


-spec example_initial_value(atm_data_type:type()) -> json_utils:json_term().
example_initial_value(atm_integer_type) ->
    ?RAND_INT(0, 1000);
example_initial_value(atm_string_type) ->
    ?RAND_STR(?RAND_INT(1, 25));
example_initial_value(atm_object_type) ->
    lists_utils:random_element([#{}, #{<<"key">> => 984.222}, #{<<"key">> => #{<<"nested">> => 984.222}}]);
%% @TODO VFS-7687 Implement all automation data types and validators
example_initial_value(atm_file_type) ->
    #{<<"file_id">> => ?RAND_STR()};
example_initial_value(atm_histogram_type) ->
    [1, 2, 3, 4];
example_initial_value(atm_dataset_type) ->
    #{<<"datasetId">> => ?RAND_STR()};
example_initial_value(atm_archive_type) ->
    #{<<"atm_archive_type">> => <<"value">>};
example_initial_value(atm_store_credentials_type) ->
    undefined;
example_initial_value(atm_onedatafs_credentials_type) ->
    undefined.


-spec example_store_schema() -> atm_store_schema:record().
example_store_schema() ->
    example_store_schema(example_data_spec()).

-spec example_store_schema(atm_data_spec:record()) -> atm_store_schema:record().
example_store_schema(DataSpec) ->
    StoreType = lists_utils:random_element(available_store_types_for_data_spec(DataSpec)),
    DefaultInitialValue = case StoreType of
        range ->
            #{<<"start">> => ?RAND_INT(0, 10), <<"end">> => ?RAND_INT(10, 20), <<"step">> => ?RAND_INT(0, 5)};
        list ->
            case ?RAND_BOOL() of
                true ->
                    undefined;
                false ->
                    lists_utils:random_sublist(lists:filtermap(fun(_) ->
                        case example_initial_value(DataSpec#atm_data_spec.type) of
                            undefined -> false;
                            Value -> {true, Value}
                        end
                    end, lists:seq(1, ?RAND_INT(0, 10))))
            end;
        _ ->
            lists_utils:random_element([undefined, example_initial_value(DataSpec#atm_data_spec.type)])
    end,
    example_store_schema(StoreType, DataSpec, DefaultInitialValue).

-spec example_store_schema(automation:store_type(), atm_data_spec:record(), term()) -> atm_store_schema:record().
example_store_schema(StoreType, DataSpec, DefaultInitialValue) ->
    #atm_store_schema{
        id = example_id(),
        name = example_name(),
        description = example_description(),
        type = StoreType,
        data_spec = DataSpec,
        requires_initial_value = ?RAND_BOOL(),
        default_initial_value = DefaultInitialValue
    }.

-spec example_store_schemas() -> [atm_store_schema:record()].
example_store_schemas() ->
    lists:map(fun example_store_schema/1, example_data_specs()).


-spec example_store_iterator_spec([automation:id()]) -> atm_store_iterator_spec:record().
example_store_iterator_spec(StoreSchemaIds) ->
    lists_utils:random_element([
        #atm_store_iterator_spec{
            store_schema_id = lists_utils:random_element(StoreSchemaIds),
            strategy = #atm_store_iterator_serial_strategy{}
        },
        #atm_store_iterator_spec{
            store_schema_id = lists_utils:random_element(StoreSchemaIds),
            strategy = #atm_store_iterator_batch_strategy{size = ?RAND_INT(1, 1000)}
        }
    ]).


-spec example_store_iterator_specs() -> [atm_store_iterator_spec:record()].
example_store_iterator_specs() ->
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 5),
    lists_utils:generate(fun() -> example_store_iterator_spec(StoreSchemaIds) end, 4).


-spec example_argument_mappers() -> [atm_task_schema_argument_mapper:record()].
example_argument_mappers() ->
    AtmLambdaRevision = example_lambda_revision(),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 5),
    example_argument_mappers(AtmLambdaRevision, StoreSchemaIds).

-spec example_argument_mappers(atm_lambda_revision:record(), [automation:id()]) ->
    [atm_task_schema_argument_mapper:record()].
example_argument_mappers(#atm_lambda_revision{argument_specs = ArgumentSpecs}, StoreSchemaIds) ->
    case ArgumentSpecs of
        [] ->
            [];
        _ ->
            {OptionalArgumentSpecs, RequiredArgumentSpecs} = lists:partition(fun(ArgumentSpec) ->
                ArgumentSpec#atm_lambda_argument_spec.is_optional
            end, ArgumentSpecs),
            % randomly select what arguments are mapped, but ensuring that all required arguments are
            ReferencedArgumentSpecs = RequiredArgumentSpecs ++ lists_utils:random_sublist(OptionalArgumentSpecs),
            example_argument_mappers_for_specs(ReferencedArgumentSpecs, StoreSchemaIds)
    end.

-spec example_argument_mappers_for_specs([atm_lambda_argument_spec:record()], [automation:id()]) ->
    [atm_task_schema_argument_mapper:record()].
example_argument_mappers_for_specs(ArgumentSpecs, StoreSchemaIds) ->
    lists:map(fun(ArgumentSpec) ->
        #atm_task_schema_argument_mapper{
            argument_name = ArgumentSpec#atm_lambda_argument_spec.name,
            value_builder = example_argument_value_builder(StoreSchemaIds)
        }
    end, lists_utils:shuffle(ArgumentSpecs)).


-spec example_result_mappers() -> [atm_task_schema_result_mapper:record()].
example_result_mappers() ->
    AtmLambdaRevision = example_lambda_revision(),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 5),
    example_result_mappers(AtmLambdaRevision, StoreSchemaIds).

-spec example_result_mappers(atm_lambda_revision:record(), [automation:id()]) ->
    [atm_task_schema_result_mapper:record()].
example_result_mappers(#atm_lambda_revision{result_specs = ResultSpecs}, StoreSchemaIds) ->
    case {ResultSpecs, StoreSchemaIds} of
        {[], _} ->
            [];
        {_, []} ->
            [];
        _ ->
            ReferencedResultSpecs = lists_utils:generate(fun() ->
                lists_utils:random_element(ResultSpecs)
            end, ?RAND_INT(0, 5)),
            example_result_mappers_for_specs(ReferencedResultSpecs, StoreSchemaIds)
    end.

-spec example_result_mappers_for_specs([atm_lambda_result_spec:record()], [automation:id()]) ->
    [atm_task_schema_result_mapper:record()].
example_result_mappers_for_specs(ResultSpecs, StoreSchemaIds) ->
    lists:map(fun(ResultSpec) ->
        #atm_task_schema_result_mapper{
            result_name = ResultSpec#atm_lambda_result_spec.name,
            store_schema_id = lists_utils:random_element([
                ?CURRENT_TASK_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID,
                ?WORKFLOW_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID
                | StoreSchemaIds
            ]),
            dispatch_function = lists_utils:random_element(atm_task_schema_result_mapper:all_dispatch_functions())
        }
    end, lists_utils:shuffle(ResultSpecs)).


-spec example_argument_value_builder() -> atm_task_argument_value_builder:record().
example_argument_value_builder() ->
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 5),
    example_argument_value_builder(StoreSchemaIds).

-spec example_argument_value_builder([automation:id()]) -> atm_task_argument_value_builder:record().
example_argument_value_builder(StoreSchemaIds) ->
    example_argument_value_builder(StoreSchemaIds, 1).

-spec example_argument_value_builder([automation:id()], pos_integer()) -> atm_task_argument_value_builder:record().
example_argument_value_builder(StoreSchemaIds, Depth) ->
    AvailableBuilders = lists:flatten([
        iterated_item,
        const,
        % the object value builder contains nested builders (including itself recursively);
        % cut off the nesting to avoid gigantic nested structures
        case Depth =< 3 of
            true -> object;
            false -> []
        end,
        case StoreSchemaIds of
            [] -> [];
            _ -> [store_credentials, single_value_store_content]
        end,
        onedatafs_credentials
    ]),

    case lists_utils:random_element(AvailableBuilders) of
        iterated_item -> #atm_task_argument_value_builder{
            type = iterated_item, recipe = lists_utils:random_element([
                undefined,
                lists_utils:random_sublist([<<"key1">>, <<"key2">>, <<"key3">>, 0, 1, 2])
            ])
        };
        const -> #atm_task_argument_value_builder{
            type = const, recipe = lists_utils:random_element([?RAND_STR(), 0, 151, 27.8])
        };
        object -> #atm_task_argument_value_builder{
            type = object, recipe = maps_utils:generate(fun() ->
                {?RAND_STR(), example_argument_value_builder(StoreSchemaIds, Depth + 1)}
            end, ?RAND_INT(1, 5))
        };
        store_credentials ->
            #atm_task_argument_value_builder{
                type = store_credentials, recipe = lists_utils:random_element(StoreSchemaIds)
            };
        single_value_store_content ->
            #atm_task_argument_value_builder{
                type = single_value_store_content, recipe = lists_utils:random_element(StoreSchemaIds)
            };
        onedatafs_credentials -> #atm_task_argument_value_builder{
            type = onedatafs_credentials, recipe = undefined
        }
    end.


-spec example_task_schema(lambda_registries(), [automation:id()]) -> atm_task_schema:record().
example_task_schema(AvailableLambdasWithRegistries, StoreSchemaIds) ->
    AtmLambdaId = lists_utils:random_element(maps:keys(AvailableLambdasWithRegistries)),
    RevisionRegistry = maps:get(AtmLambdaId, AvailableLambdasWithRegistries),
    RevisionNumber = case atm_lambda_revision_registry:get_all_revision_numbers(RevisionRegistry) of
        [] ->
            error(lambda_without_revisions);
        RevisionNumbers ->
            lists_utils:random_element(RevisionNumbers)
    end,
    AtmLambdaRevision = atm_lambda_revision_registry:get_revision(RevisionNumber, RevisionRegistry),
    #atm_task_schema{
        id = example_id(),
        name = example_name(),
        lambda_id = AtmLambdaId,
        lambda_revision_number = RevisionNumber,
        argument_mappings = example_argument_mappers(AtmLambdaRevision, StoreSchemaIds),
        result_mappings = example_result_mappers(AtmLambdaRevision, StoreSchemaIds)
    }.

-spec example_task_schemas() -> [atm_task_schema:record()].
example_task_schemas() ->
    AvailableLambdasWithRegistries = maps_utils:generate(fun() ->
        {example_id(), example_lambda_revision_registry()}
    end, 7),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 7),
    example_task_schemas(AvailableLambdasWithRegistries, StoreSchemaIds).

-spec example_task_schemas(lambda_registries(), [automation:id()]) -> [atm_task_schema:record()].
example_task_schemas(AvailableLambdasWithRegistries, StoreSchemaIds) ->
    lists_utils:generate(fun() ->
        example_task_schema(
            maps_utils:random_submap(AvailableLambdasWithRegistries, 1, all),
            lists_utils:random_sublist(StoreSchemaIds)
        )
    end, 7).


-spec example_parallel_box_schema_with_tasks([atm_task_schema:record()]) -> atm_parallel_box_schema:record().
example_parallel_box_schema_with_tasks(AtmTaskSchemas) ->
    #atm_parallel_box_schema{
        id = example_id(),
        name = example_name(),
        tasks = AtmTaskSchemas
    }.

-spec example_parallel_box_schemas() -> [atm_parallel_box_schema:record()].
example_parallel_box_schemas() ->
    ExampleTaskSchemas = example_task_schemas(),
    lists_utils:generate(fun() ->
        example_parallel_box_schema_with_tasks(lists_utils:random_sublist(ExampleTaskSchemas))
    end, 7).


-spec example_lane_schema_with_parallel_boxes([atm_parallel_box_schema:record()], [automation:id()]) ->
    atm_lane_schema:record().
example_lane_schema_with_parallel_boxes(ExampleParallelBoxSchemas, StoreSchemaIds) ->
    #atm_lane_schema{
        id = atm_test_utils:example_id(),
        name = atm_test_utils:example_name(),
        parallel_boxes = ExampleParallelBoxSchemas,
        store_iterator_spec = atm_test_utils:example_store_iterator_spec(StoreSchemaIds),
        max_retries = ?RAND_INT(0, 10)
    }.

-spec example_lane_schemas() -> [atm_lane_schema:record()].
example_lane_schemas() ->
    ExampleParallelBoxSchemas = example_parallel_box_schemas(),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 7),
    lists_utils:generate(fun() ->
        example_lane_schema_with_parallel_boxes(
            lists_utils:random_sublist(ExampleParallelBoxSchemas),
            lists_utils:random_sublist(StoreSchemaIds, 1, all)
        )
    end, 7).


-spec example_lambda_revision() -> atm_lambda_revision:record().
example_lambda_revision() ->
    RevisionWithoutChecksum = #atm_lambda_revision{
        name = example_name(),
        summary = example_summary(),
        description = example_description(),
        operation_spec = example_operation_spec(),
        batch_mode = ?RAND_BOOL(),
        argument_specs = lists_utils:random_sublist(example_argument_specs()),
        result_specs = lists_utils:random_sublist(example_result_specs()),
        resource_spec = example_resource_spec(),
        checksum = <<>>,
        state = example_lifecycle_state()
    },
    RevisionWithoutChecksum#atm_lambda_revision{
        checksum = atm_lambda_revision:calculate_checksum(RevisionWithoutChecksum)
    }.

-spec example_lambda_revisions() -> [atm_lambda_revision:record()].
example_lambda_revisions() ->
    lists_utils:generate(fun example_lambda_revision/0, 5).


-spec example_lambda_revision_registry() -> atm_lambda_revision_registry:record().
example_lambda_revision_registry() ->
    example_lambda_revision_registry(lists_utils:random_sublist(example_lambda_revisions(), 1, all)).

example_lambda_revision_registry(LambdaRevisions) ->
    #atm_lambda_revision_registry{
        registry = maps_utils:generate_from_list(fun(LambdaRevision) ->
            RevisionNumber = rand:uniform(100),
            {RevisionNumber, LambdaRevision}
        end, LambdaRevisions)
    }.

-spec example_lambda_revision_registries() -> [atm_lambda_revision_registry:record()].
example_lambda_revision_registries() ->
    ExampleLambdaRevisions = example_lambda_revisions(),
    lists_utils:generate(fun() -> example_lambda_revision_registry(ExampleLambdaRevisions) end, 5).


-spec example_workflow_schema_revisions() -> [atm_workflow_schema_revision:record()].
example_workflow_schema_revisions() ->
    ExampleStoreSchemas = example_store_schemas(),
    ExampleLaneSchemas = example_lane_schemas(),
    lists_utils:generate(fun() ->
        #atm_workflow_schema_revision{
            description = example_description(),
            stores = lists_utils:random_sublist(ExampleStoreSchemas),
            lanes = lists_utils:random_sublist(ExampleLaneSchemas),
            state = example_lifecycle_state()
        }
    end, 5).


-spec example_workflow_schema_revision_registries() -> [atm_workflow_schema_revision_registry:record()].
example_workflow_schema_revision_registries() ->
    ExampleWorkflowSchemaRevisions = example_workflow_schema_revisions(),
    lists_utils:generate(fun() ->
        #atm_workflow_schema_revision_registry{
            registry = maps_utils:generate_from_list(fun(WorkflowSchemaRevision) ->
                RevisionNumber = rand:uniform(100),
                {RevisionNumber, WorkflowSchemaRevision}
            end, lists_utils:random_sublist(ExampleWorkflowSchemaRevisions))
        }
    end, 5).


-spec available_store_types_for_data_spec(atm_data_spec:record()) -> [automation:store_type()].
available_store_types_for_data_spec(DataSpec) ->
    case DataSpec#atm_data_spec.type of
        atm_file_type -> automation:all_store_types() -- [range];
        atm_dataset_type -> automation:all_store_types() -- [range];
        atm_integer_type -> automation:all_store_types() -- [tree_forest];
        _ -> automation:all_store_types() -- [range, tree_forest]
    end.
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
-include("test/test_utils.hrl").
-include("onedata_file.hrl").

-export([example_id/0]).
-export([example_name/0]).
-export([example_summary/0]).
-export([example_description/0]).
-export([example_lifecycle_state/0]).
-export([example_store_type/0]).
-export([example_operation_spec/0, example_operation_specs/0]).
-export([example_docker_execution_options/0]).
-export([example_parameter_spec/0, example_parameter_spec/1, example_parameter_spec/2, example_parameter_specs/0]).
-export([example_result_spec/0, example_result_spec/1, example_result_specs/0]).
-export([example_resource_specs/0]).
-export([example_data_spec/0, example_data_spec/1, example_data_specs/0]).
-export([example_predefined_value/1]).
-export([example_store_schema/0, example_store_schema/1, example_store_schema/3, example_store_schemas/0]).
-export([example_store_config/1, example_store_configs/0]).
-export([example_store_iterator_spec/1, example_store_iterator_specs/0]).
-export([example_argument_mappers/0, example_argument_mappers/2, example_argument_mappers_for_specs/2]).
-export([example_result_mappers/0, example_result_mappers/2, example_result_mappers_for_specs/2]).
-export([example_store_content_update_options_records/0, example_time_series_dispatch_rules/0]).
-export([example_argument_value_builder/0, example_argument_value_builder/1]).
-export([example_task_schema/2, example_task_schemas/0, example_task_schemas/2]).
-export([example_parallel_box_schema_with_tasks/1, example_parallel_box_schemas/0]).
-export([example_lane_schema_with_parallel_boxes/2, example_lane_schemas/0]).
-export([example_lambda_revision/0, example_lambda_revisions/0]).
-export([example_lambda_revision_registry/0, example_lambda_revision_registry/1, example_lambda_revision_registries/0]).
-export([example_workflow_schema_revisions/0]).
-export([example_workflow_schema_revision_registries/0]).
-export([example_time_series_measurement_specs/0]).

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
    ?RAND_ELEMENT([<<>>, ?RAND_STR(rand:uniform(50))]).


-spec example_description() -> automation:description().
example_description() ->
    ?RAND_ELEMENT([<<>>, ?RAND_STR(rand:uniform(1000) + 50)]).


-spec example_lifecycle_state() -> json_utils:json_term().
example_lifecycle_state() ->
    ?RAND_ELEMENT(automation:all_lifecycle_states()).


-spec example_store_type() -> automation:store_type().
example_store_type() ->
    ?RAND_ELEMENT(automation:all_store_types()).


-spec example_operation_spec() -> atm_lambda_operation_spec:record().
example_operation_spec() ->
    ?RAND_ELEMENT(example_operation_specs()).


-spec example_operation_specs() -> [atm_lambda_operation_spec:record()].
example_operation_specs() ->
    [
        #atm_openfaas_operation_spec{
            docker_image = ?RAND_STR(),
            docker_execution_options = #atm_docker_execution_options{
                readonly = ?RAND_BOOL(),
                mount_oneclient = ?RAND_BOOL(),
                oneclient_mount_point = <<"/a/b/c/d/", (?RAND_STR())/binary>>,
                oneclient_options = ?RAND_ELEMENT([<<"">>, <<"--a --b">>])
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


-spec example_parameter_spec() -> atm_parameter_spec:record().
example_parameter_spec() ->
    DataSpec = example_data_spec_except([atm_time_series_measurement_type]),
    example_parameter_spec(ensure_data_spec_valid_for_input_parameters(DataSpec)).

-spec example_parameter_spec(atm_data_spec:record()) -> atm_parameter_spec:record().
example_parameter_spec(DataSpec) ->
    DefaultValue = ?RAND_ELEMENT([undefined, example_predefined_value(DataSpec)]),
    example_parameter_spec(DataSpec, DefaultValue).

-spec example_parameter_spec(atm_data_spec:record(), term()) -> atm_parameter_spec:record().
example_parameter_spec(DataSpec, DefaultValue) ->
    #atm_parameter_spec{
        name = ?RAND_STR(),
        data_spec = DataSpec,
        is_optional = ?RAND_BOOL(),
        default_value = DefaultValue
    }.

-spec example_parameter_specs() -> [atm_parameter_spec:record()].
example_parameter_specs() ->
    lists:map(fun(DataSpec) ->
        example_parameter_spec(ensure_data_spec_valid_for_input_parameters(DataSpec))
    end, example_data_specs_except([atm_time_series_measurement_type])).


-spec example_result_spec() -> atm_lambda_result_spec:record().
example_result_spec() ->
    example_result_spec(example_data_spec()).

-spec example_result_spec(atm_data_spec:record()) -> atm_lambda_result_spec:record().
example_result_spec(DataSpec) ->
    #atm_lambda_result_spec{
        name = example_name(),
        data_spec = DataSpec,
        relay_method = ?RAND_ELEMENT([return_value, file_pipe])
    }.

-spec example_result_specs() -> [atm_lambda_result_spec:record()].
example_result_specs() ->
    lists:map(fun example_result_spec/1, example_data_specs()).


-spec example_resource_spec() -> atm_resource_spec:record().
example_resource_spec() ->
    #atm_resource_spec{
        cpu_requested = rand:uniform() * 10,
        cpu_limit = ?RAND_ELEMENT([undefined, rand:uniform() * 10]),

        memory_requested = ?RAND_INT(10000, 1000000000),
        memory_limit = ?RAND_ELEMENT([undefined, ?RAND_INT(10000, 1000000000)]),

        ephemeral_storage_requested = ?RAND_INT(1000, 10000000000),
        ephemeral_storage_limit = ?RAND_ELEMENT([undefined, ?RAND_INT(1000, 10000000000)])
    }.

-spec example_resource_specs() -> [atm_resource_spec:record()].
example_resource_specs() ->
    lists_utils:generate(fun example_resource_spec/0, 4).


-spec example_data_spec() -> atm_data_spec:record().
example_data_spec() ->
    example_data_spec_except([]).

-spec example_data_spec_except([atm_data_type:type()]) -> atm_data_spec:record().
example_data_spec_except(DisallowedTypes) ->
    example_data_spec_except(
        ?RAND_ELEMENT(atm_data_type:all_data_types() -- DisallowedTypes),
        DisallowedTypes
    ).

-spec example_data_spec(atm_data_type:type()) -> atm_data_spec:record().
example_data_spec(DataType) ->
    example_data_spec_except(DataType, []).

-spec example_data_spec_except(atm_data_type:type(), [atm_data_type:type()]) -> atm_data_spec:record().
example_data_spec_except(atm_array_type, DisallowedTypes) ->
    #atm_array_data_spec{
        item_data_spec = example_data_spec_except(DisallowedTypes)
    };
example_data_spec_except(atm_boolean_type, _) ->
    #atm_boolean_data_spec{};
example_data_spec_except(atm_dataset_type, _) ->
    #atm_dataset_data_spec{};
example_data_spec_except(atm_file_type, _) ->
    #atm_file_data_spec{
        file_type = ?RAND_ELEMENT(atm_file_data_spec:allowed_file_type_specs()),
        %% @TODO VFS-12091 include all attrs after atm versioning is introduced
        attributes = ?RAND_CHOICE(undefined, lists:usort(?RAND_SUBLIST(?API_FILE_ATTRS -- [?attr_creation_time])))
    };
example_data_spec_except(atm_group_type, _) ->
    #atm_group_data_spec{
        attributes = ?RAND_CHOICE(undefined, lists:usort(?RAND_SUBLIST(atm_group_data_spec:allowed_group_attributes())))
    };
example_data_spec_except(atm_number_type, _) ->
    #atm_number_data_spec{
        integers_only = ?RAND_BOOL(),
        allowed_values = ?RAND_ELEMENT([undefined, ?RAND_SUBLIST([1, -17.8, 82734, 90.665])])
    };
example_data_spec_except(atm_object_type, _) ->
    #atm_object_data_spec{};
example_data_spec_except(atm_range_type, _) ->
    #atm_range_data_spec{};
example_data_spec_except(atm_string_type, _) ->
    #atm_string_data_spec{
        allowed_values = ?RAND_ELEMENT([undefined, ?RAND_SUBLIST([<<"a">>, <<"b">>, ?RAND_STR()])])
    };
example_data_spec_except(atm_time_series_measurement_type, _) ->
    #atm_time_series_measurement_data_spec{
        specs = ?RAND_SUBLIST(example_time_series_measurement_specs())
    }.

-spec example_data_specs() -> [atm_data_spec:record()].
example_data_specs() ->
    lists:map(fun example_data_spec/1, atm_data_type:all_data_types()).

-spec example_data_specs_except([atm_data_type:type()]) -> [atm_data_spec:record()].
example_data_specs_except(DisallowedTypes) ->
    lists:map(fun(DataType) ->
        example_data_spec_except(DataType, DisallowedTypes)
    end, atm_data_type:all_data_types() -- DisallowedTypes).


%% @TODO VFS-7687 Implement all automation data types and validators
-spec example_predefined_value(atm_data_spec:record()) -> json_utils:json_term().
example_predefined_value(#atm_array_data_spec{item_data_spec = ItemDataSpec}) ->
    case example_predefined_value(ItemDataSpec) of
        undefined ->
            [];
        Value ->
            lists:duplicate(?RAND_INT(0, 20), Value)
    end;
example_predefined_value(#atm_boolean_data_spec{}) ->
    ?RAND_BOOL();
example_predefined_value(#atm_dataset_data_spec{}) ->
    #{<<"datasetId">> => ?RAND_STR()};
example_predefined_value(#atm_file_data_spec{}) ->
    #{<<"fileId">> => ?RAND_STR()};
example_predefined_value(#atm_group_data_spec{}) ->
    #{<<"groupId">> => ?RAND_STR()};
example_predefined_value(#atm_number_data_spec{}) ->
    ?RAND_ELEMENT([?RAND_INT(0, 1000), rand:uniform() * ?RAND_INT(0, 1000)]);
example_predefined_value(#atm_object_data_spec{}) ->
    ?RAND_ELEMENT([#{}, #{<<"key">> => 984.222}, #{<<"key">> => #{<<"nested">> => 984.222}}]);
example_predefined_value(#atm_range_data_spec{}) ->
    #{<<"start">> => ?RAND_INT(0, 10), <<"end">> => ?RAND_INT(10, 20), <<"step">> => ?RAND_INT(0, 5)};
example_predefined_value(#atm_string_data_spec{}) ->
    ?RAND_STR(?RAND_INT(1, 25));
example_predefined_value(#atm_time_series_measurement_data_spec{}) ->
    undefined.


-spec example_store_schema() -> atm_store_schema:record().
example_store_schema() ->
    example_store_schema(example_store_type()).

-spec example_store_schema(automation:store_type()) -> atm_store_schema:record().
example_store_schema(StoreType) ->
    StoreConfig = example_store_config(StoreType),
    example_store_schema(StoreType, StoreConfig).

-spec example_store_schema(automation:store_type(), atm_store_config:record()) -> atm_store_schema:record().
example_store_schema(StoreType, StoreConfig) ->
    DefaultInitialContent = example_store_initial_content(StoreType, StoreConfig),
    example_store_schema(StoreType, StoreConfig, DefaultInitialContent).

-spec example_store_schema(automation:store_type(), atm_store_config:record(), term()) -> atm_store_schema:record().
example_store_schema(StoreType, StoreConfig, DefaultInitialContent) ->
    #atm_store_schema{
        id = example_id(),
        name = example_name(),
        description = example_description(),
        type = StoreType,
        config = StoreConfig,
        requires_initial_content = case StoreType of
            time_series -> false;
            audit_log -> false;
            _ -> ?RAND_BOOL()
        end,
        default_initial_content = DefaultInitialContent
    }.

-spec example_store_schemas() -> [atm_store_schema:record()].
example_store_schemas() ->
    lists:map(fun example_store_schema/1, automation:all_store_types()).


-spec example_store_config(automation:store_type()) -> atm_store_config:record().
example_store_config(single_value) ->
    #atm_single_value_store_config{item_data_spec = example_data_spec()};
example_store_config(list) ->
    #atm_list_store_config{item_data_spec = example_data_spec()};
example_store_config(tree_forest) ->
    #atm_tree_forest_store_config{item_data_spec = example_data_spec(?RAND_ELEMENT([atm_file_type, atm_dataset_type]))};
example_store_config(range) ->
    #atm_range_store_config{};
example_store_config(time_series) ->
    #atm_time_series_store_config{
        time_series_collection_schema = #time_series_collection_schema{
            time_series_schemas = ?RAND_SUBLIST(time_series_test_utils:example_time_series_schemas(), 1, all)
        },
        dashboard_spec = example_dashboard_spec()
    };
example_store_config(audit_log) ->
    #atm_audit_log_store_config{log_content_data_spec = example_data_spec()}.

-spec example_store_configs() -> [atm_store_config:record()].
example_store_configs() ->
    lists:map(fun example_store_config/1, automation:all_store_types()).


-spec example_store_initial_content(automation:store_type(), atm_store_config:record()) -> json_utils:json_term().
example_store_initial_content(single_value, #atm_single_value_store_config{item_data_spec = DataSpec}) ->
    example_predefined_value(DataSpec);
example_store_initial_content(list, #atm_list_store_config{item_data_spec = DataSpec}) ->
    case ?RAND_BOOL() of
        true ->
            undefined;
        false ->
            ?RAND_SUBLIST(lists:filtermap(fun(_) ->
                case example_predefined_value(DataSpec) of
                    undefined -> false;
                    Value -> {true, Value}
                end
            end, lists:seq(1, ?RAND_INT(0, 7))))
    end;
example_store_initial_content(tree_forest, #atm_tree_forest_store_config{item_data_spec = DataSpec}) ->
    example_store_initial_content(list, #atm_list_store_config{item_data_spec = DataSpec});
example_store_initial_content(range, #atm_range_store_config{}) ->
    example_predefined_value(#atm_range_data_spec{});
example_store_initial_content(time_series, #atm_time_series_store_config{}) ->
    undefined;
example_store_initial_content(audit_log, #atm_audit_log_store_config{}) ->
    undefined.


-spec example_store_iterator_spec([automation:id()]) -> atm_store_iterator_spec:record().
example_store_iterator_spec(StoreSchemaIds) ->
    #atm_store_iterator_spec{
        max_batch_size = ?RAND_INT(1, 1000),
        store_schema_id = ?RAND_ELEMENT(StoreSchemaIds)
    }.


-spec example_store_iterator_specs() -> [atm_store_iterator_spec:record()].
example_store_iterator_specs() ->
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 3),
    lists_utils:generate(fun() -> example_store_iterator_spec(StoreSchemaIds) end, 3).


-spec example_lambda_config(atm_lambda_revision:record()) ->
    #{atm_parameter_spec:name() => json_utils:json_term()}.
example_lambda_config(#atm_lambda_revision{config_parameter_specs = ParameterSpecs}) ->
    case ParameterSpecs of
        [] ->
            #{};
        _ ->
            {OptionalParameterSpecs, RequiredParameterSpecs} = lists:partition(fun(ParameterSpec) ->
                ParameterSpec#atm_parameter_spec.is_optional
            end, ParameterSpecs),
            % randomly select what parameters are mapped, but ensuring that all required parameters are
            ReferencedParameterSpecs = lists_utils:shuffle(RequiredParameterSpecs ++ ?RAND_SUBLIST(OptionalParameterSpecs)),
            maps_utils:generate_from_list(fun(#atm_parameter_spec{name = Name, data_spec = DataSpec}) ->
                ExampleValue = example_predefined_value(DataSpec),
                {Name, case is_map(ExampleValue) of
                    true -> maps_utils:undefined_to_null(ExampleValue);
                    _ -> utils:undefined_to_null(ExampleValue)
                end}
            end, ReferencedParameterSpecs)
    end.


-spec example_argument_mappers() -> [atm_task_schema_argument_mapper:record()].
example_argument_mappers() ->
    AtmLambdaRevision = example_lambda_revision(),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 3),
    example_argument_mappers(AtmLambdaRevision, StoreSchemaIds).

-spec example_argument_mappers(atm_lambda_revision:record(), [automation:id()]) ->
    [atm_task_schema_argument_mapper:record()].
example_argument_mappers(#atm_lambda_revision{argument_specs = ArgumentSpecs}, StoreSchemaIds) ->
    case ArgumentSpecs of
        [] ->
            [];
        _ ->
            {OptionalArgumentSpecs, RequiredArgumentSpecs} = lists:partition(fun(ArgumentSpec) ->
                ArgumentSpec#atm_parameter_spec.is_optional
            end, ArgumentSpecs),
            % randomly select what arguments are mapped, but ensuring that all required arguments are
            ReferencedArgumentSpecs = RequiredArgumentSpecs ++ ?RAND_SUBLIST(OptionalArgumentSpecs),
            example_argument_mappers_for_specs(ReferencedArgumentSpecs, StoreSchemaIds)
    end.

-spec example_argument_mappers_for_specs([atm_parameter_spec:record()], [automation:id()]) ->
    [atm_task_schema_argument_mapper:record()].
example_argument_mappers_for_specs(ArgumentSpecs, StoreSchemaIds) ->
    lists:map(fun(ArgumentSpec) ->
        #atm_task_schema_argument_mapper{
            argument_name = ArgumentSpec#atm_parameter_spec.name,
            value_builder = example_argument_value_builder(StoreSchemaIds)
        }
    end, lists_utils:shuffle(ArgumentSpecs)).


-spec example_result_mappers() -> [atm_task_schema_result_mapper:record()].
example_result_mappers() ->
    AtmLambdaRevision = example_lambda_revision(),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 3),
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
                ?RAND_ELEMENT(ResultSpecs)
            end, ?RAND_INT(0, 4)),
            example_result_mappers_for_specs(ReferencedResultSpecs, StoreSchemaIds)
    end.

-spec example_result_mappers_for_specs([atm_lambda_result_spec:record()], [automation:id()]) ->
    [atm_task_schema_result_mapper:record()].
example_result_mappers_for_specs(ResultSpecs, StoreSchemaIds) ->
    lists:map(fun(ResultSpec) ->
        #atm_task_schema_result_mapper{
            result_name = ResultSpec#atm_lambda_result_spec.name,
            store_schema_id = ?RAND_ELEMENT([
                ?CURRENT_TASK_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID,
                ?WORKFLOW_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID
                | StoreSchemaIds
            ]),
            store_content_update_options = ?RAND_ELEMENT(example_store_content_update_options_records())
        }
    end, lists_utils:shuffle(ResultSpecs)).


-spec example_store_content_update_options_records() -> [atm_store_content_update_options:record()].
example_store_content_update_options_records() -> [
    #atm_single_value_store_content_update_options{},
    #atm_list_store_content_update_options{function = ?RAND_ELEMENT([append, extend])},
    #atm_tree_forest_store_content_update_options{function = ?RAND_ELEMENT([append, extend])},
    #atm_range_store_content_update_options{},
    #atm_time_series_store_content_update_options{
        dispatch_rules = ?RAND_SUBLIST(example_time_series_dispatch_rules(), 1, 3)
    },
    #atm_audit_log_store_content_update_options{function = ?RAND_ELEMENT([append, extend])}
].


-spec example_time_series_dispatch_rules() -> [atm_time_series_dispatch_rule:record()].
example_time_series_dispatch_rules() ->
    lists_utils:generate(fun(Ordinal) ->
        #atm_time_series_dispatch_rule{
            measurement_ts_name_matcher_type = ?RAND_ELEMENT([exact, has_prefix]),
            measurement_ts_name_matcher = str_utils:format_bin("~B~ts", [Ordinal, example_name()]),
            target_ts_name_generator = example_name(),
            prefix_combiner = ?RAND_ELEMENT([concatenate, converge, overwrite])
        }
    end, 5).


-spec example_argument_value_builder() -> atm_task_argument_value_builder:record().
example_argument_value_builder() ->
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 3),
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
        end
    ]),

    case ?RAND_ELEMENT(AvailableBuilders) of
        iterated_item -> #atm_task_argument_value_builder{
            type = iterated_item, recipe = ?RAND_ELEMENT([
                undefined,
                ?RAND_SUBLIST([<<"key1">>, <<"key2">>, <<"key3">>, 0, 1, 2])
            ])
        };
        const -> #atm_task_argument_value_builder{
            type = const, recipe = ?RAND_ELEMENT([?RAND_STR(), 0, 151, 27.8])
        };
        object -> #atm_task_argument_value_builder{
            type = object, recipe = maps_utils:generate(fun() ->
                {?RAND_STR(), example_argument_value_builder(StoreSchemaIds, Depth + 1)}
            end, ?RAND_INT(1, 3))
        };
        store_credentials ->
            #atm_task_argument_value_builder{
                type = store_credentials, recipe = ?RAND_ELEMENT(StoreSchemaIds)
            };
        single_value_store_content ->
            #atm_task_argument_value_builder{
                type = single_value_store_content, recipe = ?RAND_ELEMENT(StoreSchemaIds)
            }
    end.


-spec example_task_schema(lambda_registries(), [automation:id()]) -> atm_task_schema:record().
example_task_schema(AvailableLambdasWithRegistries, StoreSchemaIds) ->
    AtmLambdaId = ?RAND_ELEMENT(maps:keys(AvailableLambdasWithRegistries)),
    RevisionRegistry = maps:get(AtmLambdaId, AvailableLambdasWithRegistries),
    RevisionNumber = case atm_lambda_revision_registry:get_all_revision_numbers(RevisionRegistry) of
        [] ->
            error(lambda_without_revisions);
        RevisionNumbers ->
            ?RAND_ELEMENT(RevisionNumbers)
    end,
    AtmLambdaRevision = atm_lambda_revision_registry:get_revision(RevisionNumber, RevisionRegistry),
    #atm_task_schema{
        id = example_id(),
        name = example_name(),
        lambda_id = AtmLambdaId,
        lambda_revision_number = RevisionNumber,
        lambda_config = example_lambda_config(AtmLambdaRevision),
        argument_mappings = example_argument_mappers(AtmLambdaRevision, StoreSchemaIds),
        result_mappings = example_result_mappers(AtmLambdaRevision, StoreSchemaIds),
        resource_spec_override = ?RAND_ELEMENT([undefined, example_resource_spec()]),
        time_series_store_config = ?RAND_ELEMENT([undefined, example_store_config(time_series)])
    }.

-spec example_task_schemas() -> [atm_task_schema:record()].
example_task_schemas() ->
    AvailableLambdasWithRegistries = maps_utils:generate(fun() ->
        {example_id(), example_lambda_revision_registry()}
    end, 5),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 4),
    example_task_schemas(AvailableLambdasWithRegistries, StoreSchemaIds).

-spec example_task_schemas(lambda_registries(), [automation:id()]) -> [atm_task_schema:record()].
example_task_schemas(AvailableLambdasWithRegistries, StoreSchemaIds) ->
    lists_utils:generate(fun() ->
        example_task_schema(
            maps_utils:random_submap(AvailableLambdasWithRegistries, 1, all),
            ?RAND_SUBLIST(StoreSchemaIds)
        )
    end, 4).


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
        example_parallel_box_schema_with_tasks(?RAND_SUBLIST(ExampleTaskSchemas))
    end, 4).


-spec example_lane_schema_with_parallel_boxes([atm_parallel_box_schema:record()], [automation:id()]) ->
    atm_lane_schema:record().
example_lane_schema_with_parallel_boxes(ExampleParallelBoxSchemas, StoreSchemaIds) ->
    #atm_lane_schema{
        id = example_id(),
        name = example_name(),
        parallel_boxes = ExampleParallelBoxSchemas,
        store_iterator_spec = example_store_iterator_spec(StoreSchemaIds),
        max_retries = ?RAND_INT(0, 10),
        instant_failure_exception_threshold = ?RAND_FLOAT(0.0, 1.0),
        dashboard_spec = example_dashboard_spec()
    }.

-spec example_lane_schemas() -> [atm_lane_schema:record()].
example_lane_schemas() ->
    ExampleParallelBoxSchemas = example_parallel_box_schemas(),
    StoreSchemaIds = lists_utils:generate(fun example_id/0, 5),
    lists_utils:generate(fun() ->
        example_lane_schema_with_parallel_boxes(
            ?RAND_SUBLIST(ExampleParallelBoxSchemas),
            ?RAND_SUBLIST(StoreSchemaIds, 1, all)
        )
    end, 4).


-spec example_lambda_revision() -> atm_lambda_revision:record().
example_lambda_revision() ->
    ExampleParameterSpecs = example_parameter_specs(),
    RevisionWithoutChecksum = #atm_lambda_revision{
        name = example_name(),
        summary = example_summary(),
        description = example_description(),
        operation_spec = example_operation_spec(),
        config_parameter_specs = ?RAND_SUBLIST(ExampleParameterSpecs, 0, 3),
        argument_specs = ?RAND_SUBLIST(ExampleParameterSpecs, 1, 4),
        result_specs = ?RAND_SUBLIST(example_result_specs(), 1, 4),
        preferred_batch_size = ?RAND_INT(1, 1000),
        resource_spec = example_resource_spec(),
        checksum = <<>>,
        state = example_lifecycle_state()
    },
    RevisionWithoutChecksum#atm_lambda_revision{
        checksum = atm_lambda_revision:calculate_checksum(RevisionWithoutChecksum)
    }.

-spec example_lambda_revisions() -> [atm_lambda_revision:record()].
example_lambda_revisions() ->
    lists_utils:generate(fun example_lambda_revision/0, 3).


-spec example_lambda_revision_registry() -> atm_lambda_revision_registry:record().
example_lambda_revision_registry() ->
    example_lambda_revision_registry(?RAND_SUBLIST(example_lambda_revisions(), 1, all)).

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
    lists_utils:generate(fun() -> example_lambda_revision_registry(ExampleLambdaRevisions) end, 3).


-spec example_workflow_schema_revisions() -> [atm_workflow_schema_revision:record()].
example_workflow_schema_revisions() ->
    ExampleStoreSchemas = example_store_schemas(),
    ExampleLaneSchemas = example_lane_schemas(),
    lists_utils:generate(fun() ->
        #atm_workflow_schema_revision{
            description = example_description(),
            state = example_lifecycle_state(),
            stores = ?RAND_SUBLIST(ExampleStoreSchemas),
            lanes = ?RAND_SUBLIST(ExampleLaneSchemas),
            dashboard_spec = example_dashboard_spec()
        }
    end, ?RAND_INT(1, 3)).


-spec example_workflow_schema_revision_registries() -> [atm_workflow_schema_revision_registry:record()].
example_workflow_schema_revision_registries() ->
    ExampleWorkflowSchemaRevisions = example_workflow_schema_revisions(),
    lists_utils:generate(fun() ->
        #atm_workflow_schema_revision_registry{
            registry = maps_utils:generate_from_list(fun(WorkflowSchemaRevision) ->
                RevisionNumber = rand:uniform(100),
                {RevisionNumber, WorkflowSchemaRevision}
            end, ?RAND_SUBLIST(ExampleWorkflowSchemaRevisions))
        }
    end, 4).


-spec example_time_series_measurement_spec(atm_time_series_names:measurement_ts_name_matcher()) ->
    atm_time_series_measurement_spec:record().
example_time_series_measurement_spec(NameMatcher) ->
    #atm_time_series_measurement_spec{
        name_matcher_type = ?RAND_ELEMENT([exact, has_prefix]),
        name_matcher = NameMatcher,
        unit = ?RAND_ELEMENT(time_series_test_utils:example_time_series_units())
    }.


-spec example_time_series_measurement_specs() -> [atm_time_series_measurement_spec:record()].
example_time_series_measurement_specs() ->
    lists_utils:generate(fun(Ordinal) ->
        example_time_series_measurement_spec(str_utils:format_bin("~B~ts", [Ordinal, example_name()]))
    end, 4).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec example_dashboard_spec() -> undefined.
example_dashboard_spec() ->
    % dashboard specs are not included in examples because they are very complicated and
    % their generation can cause horrendous slowdown of the automation example generators
    % (which are intensively used during tests)
    undefined.


% in the context of input parameters (config, arguments), data specs
% have additional restrictions - make sure they are valid
%% @private
-spec ensure_data_spec_valid_for_input_parameters(atm_data_spec:record()) -> atm_data_spec:record().
ensure_data_spec_valid_for_input_parameters(#atm_file_data_spec{attributes = [_ | _]} = DataSpec) ->
    DataSpec;
ensure_data_spec_valid_for_input_parameters(#atm_file_data_spec{attributes = _} = DataSpec) ->
    % the attributes field must be a non-empty list
    %% @TODO VFS-12091 include all attrs after atm versioning is introduced
    DataSpec#atm_file_data_spec{attributes = lists:sort(?RAND_SUBLIST(?API_FILE_ATTRS -- [?attr_creation_time], 1, all))};

ensure_data_spec_valid_for_input_parameters(#atm_group_data_spec{attributes = [_ | _]} = DataSpec) ->
    DataSpec;
ensure_data_spec_valid_for_input_parameters(#atm_group_data_spec{attributes = _} = DataSpec) ->
    % the attributes field must be a non-empty list
    DataSpec#atm_group_data_spec{
        attributes = lists:sort(?RAND_SUBLIST(atm_group_data_spec:allowed_group_attributes(), 1, all))
    };

ensure_data_spec_valid_for_input_parameters(#atm_array_data_spec{item_data_spec = ItemDataSpec}) ->
    #atm_array_data_spec{item_data_spec = ensure_data_spec_valid_for_input_parameters(ItemDataSpec)};

ensure_data_spec_valid_for_input_parameters(DataSpec) ->
    DataSpec.

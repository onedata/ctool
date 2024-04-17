%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and records related to automation functionalities.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AUTOMATION_HRL).
-define(AUTOMATION_HRL, 1).


-include("../time_series/common.hrl").


-define(DEFAULT_SUMMARY, <<"Missing summary">>).
-define(SUMMARY_SIZE_LIMIT, 200).
-define(DEFAULT_DESCRIPTION, <<"Missing description">>).
-define(DESCRIPTION_SIZE_LIMIT, 100000).


% Predefined store schemas that are implicitly defined in each workflow schema.
% Can be perceived as virtual store schemas - they do not appear in the
% workflow schema structure, but can be referenced in result mappers to apply
% operations on corresponding stores that are created during workflow execution.
% Each task gets its own audit log store (always) and time series store
% (if time series spec was specified for the task), and the whole workflow
% executions gets one global audit log store.
-define(CURRENT_TASK_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID, <<"CURRENT_TASK_SYSTEM_AUDIT_LOG">>).
-define(WORKFLOW_SYSTEM_AUDIT_LOG_STORE_SCHEMA_ID, <<"WORKFLOW_SYSTEM_AUDIT_LOG">>).
-define(CURRENT_TASK_TIME_SERIES_STORE_SCHEMA_ID, <<"CURRENT_TASK_TIME_SERIES">>).


-record(atm_resource_spec, {
    % CPU as number of cores (1.0 = 1 core)
    cpu_requested :: float(),
    cpu_limit :: undefined | float(),
    % memory in Bytes
    memory_requested :: integer(),
    memory_limit :: undefined | integer(),
    % storage in Bytes
    ephemeral_storage_requested :: integer(),
    ephemeral_storage_limit :: undefined | integer()
}).

-record(atm_array_data_spec, {
    item_data_spec :: atm_data_spec:record()
}).

-record(atm_boolean_data_spec, {}).

-record(atm_dataset_data_spec, {}).

-record(atm_file_data_spec, {
    file_type :: atm_file_data_spec:file_type_spec(),
    attributes :: undefined | [onedata_file:attr_name()]
}).

-record(atm_group_data_spec, {
    attributes :: [atm_group_data_spec:attribute_name()]
}).

-record(atm_number_data_spec, {
    integers_only :: boolean(),
    allowed_values :: undefined | [number()]
}).

-record(atm_object_data_spec, {}).

-record(atm_range_data_spec, {}).

-record(atm_string_data_spec, {
    allowed_values :: undefined | [binary()]
}).

-record(atm_time_series_measurement_data_spec, {
    specs :: [atm_time_series_measurement_spec:record()]
}).

-record(atm_docker_execution_options, {
    readonly = false :: boolean(),
    mount_oneclient = false :: boolean(),
    oneclient_mount_point = <<"/mnt/onedata">> :: binary(),
    oneclient_options = <<"">> :: binary()
}).

-record(atm_onedata_function_operation_spec, {
    function_id :: binary()
}).

-record(atm_openfaas_operation_spec, {
    docker_image :: binary(),
    docker_execution_options :: #atm_docker_execution_options{}
}).

-record(atm_workflow_operation_spec, {
    atm_workflow_id :: binary()
}).

-record(atm_user_form_operation_spec, {
    user_form_id :: binary()
}).

% Specification of a single input parameter, reused for example for specifying the
% names and format of lambda arguments or fields in a config object.
-record(atm_parameter_spec, {
    name :: atm_parameter_spec:name(),
    data_spec :: atm_data_spec:record(),
    % the is_optional flag is ignored if the default_value is specified
    % (default_value guarantees that a value for the argument will be given)
    is_optional :: boolean(),
    default_value = undefined :: undefined | json_utils:json_term()
}).

% Specification of a single returned value of a lambda function.
% Each function has a list of result specs.
-record(atm_lambda_result_spec, {
    name :: automation:name(),
    data_spec :: atm_data_spec:record(),
    relay_method :: atm_lambda_result_spec:relay_method()
}).

-record(atm_store_schema, {
    id :: automation:name(),
    name :: automation:name(),
    description :: automation:description(),
    type :: automation:store_type(),
    config :: atm_store_config:record(),
    requires_initial_content :: boolean(),
    default_initial_content :: undefined | json_utils:json_term()
}).

-record(atm_single_value_store_config, {
    item_data_spec :: atm_data_spec:record()
}).

-record(atm_list_store_config, {
    item_data_spec :: atm_data_spec:record()
}).

-record(atm_tree_forest_store_config, {
    item_data_spec :: atm_data_spec:record()
}).

-record(atm_range_store_config, {
}).

-record(atm_time_series_store_config, {
    time_series_collection_schema :: #time_series_collection_schema{},
    dashboard_spec :: undefined | ts_dashboard_spec:record()
}).

-record(atm_audit_log_store_config, {
    log_content_data_spec :: atm_data_spec:record()
}).

-record(atm_store_iterator_spec, {
    store_schema_id :: automation:id(),
    max_batch_size :: pos_integer()
}).

-record(atm_task_argument_value_builder, {
    type :: atm_task_argument_value_builder:type(),
    recipe :: atm_task_argument_value_builder:recipe()
}).

-record(atm_task_schema_argument_mapper, {
    argument_name :: automation:name(),
    value_builder :: atm_task_argument_value_builder:record()
}).

-record(atm_task_schema_result_mapper, {
    result_name :: automation:name(),
    store_schema_id :: automation:id(),
    store_content_update_options :: atm_store_content_update_options:record()
}).

-record(atm_single_value_store_content_update_options, {
}).

-record(atm_list_store_content_update_options, {
    function :: atm_list_store_content_update_options:update_function()
}).

-record(atm_tree_forest_store_content_update_options, {
    function :: atm_list_store_content_update_options:update_function()
}).

-record(atm_range_store_content_update_options, {
}).

-record(atm_time_series_store_content_update_options, {
    dispatch_rules :: [atm_time_series_dispatch_rule:record()]
}).

-record(atm_audit_log_store_content_update_options, {
    function :: atm_list_store_content_update_options:update_function()
}).

-record(atm_time_series_dispatch_rule, {
    measurement_ts_name_matcher_type :: atm_time_series_names:measurement_ts_name_matcher_type(),
    measurement_ts_name_matcher :: atm_time_series_names:measurement_ts_name_matcher(),
    target_ts_name_generator :: time_series_schema:name_generator(),
    prefix_combiner :: atm_time_series_names:prefix_combiner()
}).

-record(atm_task_schema, {
    id :: automation:id(),
    name :: automation:name(),
    lambda_id :: automation:id(),
    lambda_revision_number :: atm_lambda_revision:revision_number(),
    lambda_config :: #{atm_parameter_spec:name() => json_utils:json_term()},
    argument_mappings :: [atm_task_schema_argument_mapper:record()],
    result_mappings :: [atm_task_schema_result_mapper:record()],
    resource_spec_override :: undefined | atm_resource_spec:record(),
    % optional; if defined, a time series store assigned to this task will be automatically created
    time_series_store_config :: undefined | atm_time_series_store_config:record()
}).

-record(atm_parallel_box_schema, {
    id :: automation:id(),
    name :: automation:name(),
    tasks :: [atm_task_schema:record()]
}).

-record(atm_lane_schema, {
    id :: automation:id(),
    name :: automation:name(),
    parallel_boxes :: [atm_parallel_box_schema:record()],
    store_iterator_spec :: atm_store_iterator_spec:record(),
    % if the lane fails, it will be automatically retried (at most) this many times
    max_retries :: non_neg_integer(),
    % if the ratio of exceptions vs. total items taken for processing surpasses
    % the threshold (for any task), the lane (and hence, the workflow) fails instantly
    instant_failure_exception_threshold :: float(),  % between 0.0 .. 1.0
    dashboard_spec :: undefined | ts_dashboard_spec:record()
}).

-record(atm_lambda_revision, {
    name :: automation:name(),
    summary :: automation:summary(),
    description :: automation:description(),
    operation_spec :: atm_lambda_operation_spec:record(),
    config_parameter_specs :: [atm_parameter_spec:record()],
    argument_specs :: [atm_parameter_spec:record()],
    result_specs :: [atm_lambda_result_spec:record()],
    preferred_batch_size :: pos_integer(),
    resource_spec :: atm_resource_spec:record(),
    checksum :: atm_lambda_revision:checksum(),
    state :: automation:lifecycle_state()
}).

-record(atm_lambda_revision_registry, {
    registry = #{} :: #{atm_lambda_revision:revision_number() => atm_lambda_revision:record()}
}).

-record(atm_workflow_schema_revision, {
    description = <<>> :: automation:description(),
    state = draft :: automation:lifecycle_state(),
    stores = [] :: [atm_store_schema:record()],
    lanes = [] :: [atm_lane_schema:record()],
    dashboard_spec :: undefined | ts_dashboard_spec:record()
}).

-record(atm_workflow_schema_revision_registry, {
    registry = #{} :: #{atm_workflow_schema_revision:revision_number() => atm_workflow_schema_revision:record()}
}).

-record(atm_time_series_measurement_spec, {
    name_matcher_type :: atm_time_series_names:measurement_ts_name_matcher_type(),
    name_matcher :: atm_time_series_names:measurement_ts_name_matcher(),
    unit :: time_series:unit()
}).


-endif.

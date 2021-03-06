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


-define(DEFAULT_SUMMARY, <<"Missing summary">>).
-define(SUMMARY_SIZE_LIMIT, 200).
-define(DEFAULT_DESCRIPTION, <<"Missing description">>).
-define(DESCRIPTION_SIZE_LIMIT, 100000).


-record(atm_data_spec, {
    type :: atm_data_type:type(),
    value_constraints = #{} :: atm_data_type:value_constraints()
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

% Specification of a single input argument a lambda function.
% Each function has a list of argument specs.
-record(atm_lambda_argument_spec, {
    name :: automation:name(),
    data_spec :: atm_data_spec:record(),
    is_batch :: boolean(),
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
    is_batch :: boolean()
}).

-record(atm_store_schema, {
    id :: automation:name(),
    name :: automation:name(),
    description :: automation:description(),
    type :: automation:store_type(),
    data_spec :: atm_data_spec:record(),
    requires_initial_value :: boolean(),
    default_initial_value :: undefined | json_utils:json_term()
}).

-record(atm_store_iterator_serial_strategy, {}).
-record(atm_store_iterator_batch_strategy, {size :: pos_integer()}).

-record(atm_store_iterator_spec, {
    strategy :: atm_store_iterator_spec:strategy(),
    store_schema_id :: automation:id()
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
    dispatch_function :: atm_task_schema_result_mapper:dispatch_function()
}).

-record(atm_task_schema, {
    id :: automation:id(),
    name :: automation:name(),
    lambda_id :: automation:id(),
    argument_mappings :: [atm_task_schema_argument_mapper:record()],
    result_mappings :: [atm_task_schema_result_mapper:record()]
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
    store_iterator_spec :: atm_store_iterator_spec:record()
}).

-endif.

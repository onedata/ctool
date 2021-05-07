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

-record(atm_lambda_operation_spec, {
    spec :: atm_lambda_operation_spec:spec()
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

-endif.

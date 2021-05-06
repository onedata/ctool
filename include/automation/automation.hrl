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

-record(atm_lambda_engine_type, {
    type ::atm_lambda_engine_type:type()
}).

-record(atm_mount_space_options, {
    mount_oneclient = false :: boolean(),
    mount_point = <<"/mnt/onedata">> :: binary(),
    oneclient_options = <<"">> :: binary()
}).

-record(atm_lambda_execution_options, {
    readonly = false :: boolean(),
    mount_space_options = #atm_mount_space_options{} :: atm_mount_space_options:record()
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

%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Errors to be used across all Onedata products.
%%% @end
%%%-------------------------------------------------------------------
-module(errors).

-include("errors.hrl").
-include("http/codes.hrl").
-include("logging.hrl").

-type errno() :: od_error:errno().

-type error() ::
    od_error_unauthorized:t() |
    od_error_forbidden:t() |
    od_error_bad_basic_credentials:t() |
    od_error_user_blocked:t() |
    od_error_bad_token:t() |
    od_error_bad_service_token:t() |
    od_error_not_an_identity_token:t() |
    od_error_invite_token_target_id_invalid:t() |
    od_error_token_time_caveat_required:t() |
    od_error_token_service_forbidden:t() |
    od_error_token_too_large:t() |
    od_error_token_revoked:t() |
    od_error_token_invalid:t() |
    od_error_token_caveat_unverified:t() |
    od_error_token_subject_invalid:t() |
    od_error_invite_token_consumer_invalid:t() |
    od_error_bad_consumer_token:t() |
    od_error_token_caveat_unknown:t() |
    od_error_not_an_invite_token:t() |
    od_error_token_session_invalid:t() |
    od_error_bad_idp_access_token:t() |
    od_error_not_an_access_token:t() |
    od_error_invite_token_subject_not_authorized:t() |
    od_error_invite_token_usage_limit_reached:t() |
    od_error_no_connection_to_cluster_node:t() |
    od_error_no_connection_to_onezone:t() |
    od_error_no_connection_to_peer_oneprovider:t() |
    od_error_malformed_data:t() |
    od_error_missing_at_least_one_value:t() |
    od_error_illegal_support_stage_transition:t() |
    od_error_tsc_too_many_metrics:t() |
    od_error_invalid_qos_expression:t() |
    od_error_gui_package_unverified:t() |
    od_error_bad_gui_package:t() |
    od_error_bad_data:t() |
    od_error_missing_required_value:t() |
    od_error_tsc_missing_layout:t() |
    od_error_gui_package_too_large:t() |
    od_error_bad_value_integer:t() |
    od_error_bad_value_binary:t() |
    od_error_bad_value_ipv4_address:t() |
    od_error_bad_value_email:t() |
    od_error_bad_value_file_path:t() |
    od_error_bad_value_token_type:t() |
    od_error_bad_value_id_not_found:t() |
    od_error_bad_value_list_of_ipv4_addresses:t() |
    od_error_bad_value_text_too_large:t() |
    od_error_bad_value_too_high:t() |
    od_error_bad_value_name:t() |
    od_error_bad_value_qos_parameters:t() |
    od_error_bad_value_domain:t() |
    od_error_bad_value_not_allowed:t() |
    od_error_bad_value_subdomain:t() |
    od_error_bad_value_boolean:t() |
    od_error_bad_value_list_of_atoms:t() |
    od_error_bad_value_json:t() |
    od_error_bad_value_identifier_occupied:t() |
    od_error_bad_value_empty:t() |
    od_error_bad_value_identifier:t() |
    od_error_bad_value_password:t() |
    od_error_bad_value_not_in_range:t() |
    od_error_bad_value_atom:t() |
    od_error_bad_value_ambiguous_id:t() |
    od_error_bad_value_octal:t() |
    od_error_bad_value_list_of_binaries:t() |
    od_error_bad_value_full_name:t() |
    od_error_bad_value_list_not_allowed:t() |
    od_error_bad_value_username:t() |
    od_error_bad_value_invite_type:t() |
    od_error_bad_value_tsc_conflicting_metric_config:t() |
    od_error_bad_value_caveat:t() |
    od_error_bad_value_xml:t() |
    od_error_bad_value_token:t() |
    od_error_bad_value_too_low:t() |
    od_error_bad_value_float:t() |
    od_error_unregistered_oneprovider:t() |
    od_error_timeout:t() |
    od_error_temporary_failure:t() |
    od_error_external_service_operation_failed:t() |
    od_error_file_access:t() |
    od_error_service_unavailable:t() |
    od_error_bad_message:t() |
    od_error_not_supported:t() |
    od_error_already_exists:t() |
    od_error_not_implemented:t() |
    od_error_limit_reached:t() |
    od_error_not_found:t() |
    od_error_internal_server_error:t() |
    od_error_bad_version:t() |
    od_error_expected_handshake_message:t() |
    od_error_not_subscribable:t() |
    od_error_bad_gri:t() |
    od_error_rpc_undefined:t() |
    od_error_handshake_already_done:t() |
    od_error_node_not_compatible:t() |
    od_error_dns_servers_unreachable:t() |
    od_error_lets_encrypt_not_reachable:t() |
    od_error_node_already_in_cluster:t() |
    od_error_user_not_in_cluster:t() |
    od_error_file_allocation:t() |
    od_error_no_connection_to_new_node:t() |
    od_error_no_service_nodes:t() |
    od_error_lets_encrypt_response:t() |
    od_error_on_nodes:t() |
    od_error_forbidden_for_current_archive_state:t() |
    od_error_space_not_supported_by:t() |
    od_error_recall_target_conflict:t() |
    od_error_nested_archive_deletion_forbidden:t() |
    od_error_stat_operation_not_supported:t() |
    od_error_user_not_supported:t() |
    od_error_auto_cleaning_disabled:t() |
    od_error_quota_exceeded:t() |
    od_error_file_popularity_disabled:t() |
    od_error_atm_store_type_disallowed:t() |
    od_error_atm_store_frozen:t() |
    od_error_atm_unsupported_data_type:t() |
    od_error_atm_task_arg_mapping_failed:t() |
    od_error_atm_data_type_unverified:t() |
    od_error_atm_store_creation_failed:t() |
    od_error_atm_openfaas_not_configured:t() |
    od_error_atm_lane_execution_rerun_failed:t() |
    od_error_atm_task_result_missing:t() |
    od_error_atm_task_result_dispatch_failed:t() |
    od_error_atm_workflow_execution_stopped:t() |
    od_error_atm_openfaas_unhealthy:t() |
    od_error_atm_lane_execution_retry_failed:t() |
    od_error_atm_store_content_not_set:t() |
    od_error_atm_task_arg_mapper_for_required_lambda_arg_missing:t() |
    od_error_atm_job_batch_crashed:t() |
    od_error_atm_parallel_box_execution_creation_failed:t() |
    od_error_atm_task_arg_mapper_unsupported_value_builder:t() |
    od_error_atm_store_not_found:t() |
    od_error_atm_parallel_box_empty:t() |
    od_error_atm_store_missing_required_initial_content:t() |
    od_error_atm_workflow_execution_stopping:t() |
    od_error_atm_lane_empty:t() |
    od_error_atm_workflow_execution_ended:t() |
    od_error_atm_task_result_mapping_failed:t() |
    od_error_atm_lane_execution_creation_failed:t() |
    od_error_atm_workflow_empty:t() |
    od_error_atm_openfaas_function_registration_failed:t() |
    od_error_atm_workflow_execution_not_ended:t() |
    od_error_atm_parallel_box_execution_initiation_failed:t() |
    od_error_atm_task_execution_initiation_failed:t() |
    od_error_atm_job_batch_withdrawn:t() |
    od_error_atm_task_arg_mapper_for_nonexistent_lambda_arg:t() |
    od_error_atm_data_value_constraint_unverified:t() |
    od_error_atm_lambda_config_bad_value:t() |
    od_error_atm_invalid_status_transition:t() |
    od_error_atm_workflow_execution_not_resumable:t() |
    od_error_atm_task_execution_stopped:t() |
    od_error_atm_openfaas_unreachable:t() |
    od_error_atm_task_arg_mapper_iterated_item_query_failed:t() |
    od_error_atm_workflow_execution_not_stopped:t() |
    od_error_atm_openfaas_query_failed:t() |
    od_error_atm_lane_execution_initiation_failed:t() |
    od_error_atm_task_execution_creation_failed:t() |
    od_error_dir_stats_disabled_for_space:t() |
    od_error_dir_stats_not_ready:t() |
    od_error_requires_imported_storage:t() |
    od_error_requires_posix_compatible_storage:t() |
    od_error_storage_import_not_supported:t() |
    od_error_requires_auto_storage_import_mode:t() |
    od_error_auto_storage_import_not_supported:t() |
    od_error_requires_readonly_storage:t() |
    od_error_storage_test_failed:t() |
    od_error_storage_in_use:t() |
    od_error_not_a_local_storage_supporting_space:t() |
    od_error_requires_non_imported_storage:t() |
    od_error_transfer_already_ended:t() |
    od_error_transfer_not_ended:t() |
    od_error_view_query_failed:t() |
    od_error_view_not_exists_on:t() |
    od_error_atm_lambda_in_use:t() |
    od_error_relation_already_exists:t() |
    od_error_basic_auth_disabled:t() |
    od_error_cannot_add_relation_to_self:t() |
    od_error_cannot_delete_non_empty_handle_service:t() |
    od_error_cannot_delete_entity:t() |
    od_error_cannot_remove_last_owner:t() |
    od_error_protected_group:t() |
    od_error_basic_auth_not_supported:t() |
    od_error_relation_does_not_exist:t() |
    od_error_space_already_supported_with_imported_storage:t() |
    od_error_space_marketplace_disabled:t() |
    od_error_subdomain_delegation_disabled:t() |
    od_error_subdomain_delegation_not_supported:t() |
    od_error_posix:t() |
    % unrecognized is a special error and as such does not have its own module
    #od_error{type :: od_error_unrecognized_error}.

-export_type([errno/0, error/0]).

%% API
-export([
    is_known_error/1,
    is_posix_code/1,

    to_json/1,
    from_json/1,
    to_http_code/1
]).


-define(ERROR_ID_TO_TYPE_MAPPING, #{
    ?ERROR_UNAUTHORIZED_ID => ?ERROR_UNAUTHORIZED_TYPE,
    ?ERROR_FORBIDDEN_ID => ?ERROR_FORBIDDEN_TYPE,
    ?ERROR_BAD_BASIC_CREDENTIALS_ID => ?ERROR_BAD_BASIC_CREDENTIALS_TYPE,
    ?ERROR_USER_BLOCKED_ID => ?ERROR_USER_BLOCKED_TYPE,
    ?ERROR_BAD_TOKEN_ID => ?ERROR_BAD_TOKEN_TYPE,
    ?ERROR_BAD_SERVICE_TOKEN_ID => ?ERROR_BAD_SERVICE_TOKEN_TYPE,
    ?ERROR_NOT_AN_IDENTITY_TOKEN_ID => ?ERROR_NOT_AN_IDENTITY_TOKEN_TYPE,
    ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID_ID => ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE,
    ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED_ID => ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE,
    ?ERROR_TOKEN_SERVICE_FORBIDDEN_ID => ?ERROR_TOKEN_SERVICE_FORBIDDEN_TYPE,
    ?ERROR_TOKEN_TOO_LARGE_ID => ?ERROR_TOKEN_TOO_LARGE_TYPE,
    ?ERROR_TOKEN_REVOKED_ID => ?ERROR_TOKEN_REVOKED_TYPE,
    ?ERROR_TOKEN_INVALID_ID => ?ERROR_TOKEN_INVALID_TYPE,
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED_ID => ?ERROR_TOKEN_CAVEAT_UNVERIFIED_TYPE,
    ?ERROR_TOKEN_SUBJECT_INVALID_ID => ?ERROR_TOKEN_SUBJECT_INVALID_TYPE,
    ?ERROR_INVITE_TOKEN_CONSUMER_INVALID_ID => ?ERROR_INVITE_TOKEN_CONSUMER_INVALID_TYPE,
    ?ERROR_BAD_CONSUMER_TOKEN_ID => ?ERROR_BAD_CONSUMER_TOKEN_TYPE,
    ?ERROR_TOKEN_CAVEAT_UNKNOWN_ID => ?ERROR_TOKEN_CAVEAT_UNKNOWN_TYPE,
    ?ERROR_NOT_AN_INVITE_TOKEN_ID => ?ERROR_NOT_AN_INVITE_TOKEN_TYPE,
    ?ERROR_TOKEN_SESSION_INVALID_ID => ?ERROR_TOKEN_SESSION_INVALID_TYPE,
    ?ERROR_BAD_IDP_ACCESS_TOKEN_ID => ?ERROR_BAD_IDP_ACCESS_TOKEN_TYPE,
    ?ERROR_NOT_AN_ACCESS_TOKEN_ID => ?ERROR_NOT_AN_ACCESS_TOKEN_TYPE,
    ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_ID => ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE,
    ?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED_ID => ?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE,
    ?ERROR_NO_CONNECTION_TO_CLUSTER_NODE_ID => ?ERROR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE,
    ?ERROR_NO_CONNECTION_TO_ONEZONE_ID => ?ERROR_NO_CONNECTION_TO_ONEZONE_TYPE,
    ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER_ID => ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE,
    ?ERROR_MALFORMED_DATA_ID => ?ERROR_MALFORMED_DATA_TYPE,
    ?ERROR_MISSING_AT_LEAST_ONE_VALUE_ID => ?ERROR_MISSING_AT_LEAST_ONE_VALUE_TYPE,
    ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION_ID => ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE,
    ?ERROR_TSC_TOO_MANY_METRICS_ID => ?ERROR_TSC_TOO_MANY_METRICS_TYPE,
    ?ERROR_INVALID_QOS_EXPRESSION_ID => ?ERROR_INVALID_QOS_EXPRESSION_TYPE,
    ?ERROR_GUI_PACKAGE_UNVERIFIED_ID => ?ERROR_GUI_PACKAGE_UNVERIFIED_TYPE,
    ?ERROR_BAD_GUI_PACKAGE_ID => ?ERROR_BAD_GUI_PACKAGE_TYPE,
    ?ERROR_BAD_DATA_ID => ?ERROR_BAD_DATA_TYPE,
    ?ERROR_MISSING_REQUIRED_VALUE_ID => ?ERROR_MISSING_REQUIRED_VALUE_TYPE,
    ?ERROR_TSC_MISSING_LAYOUT_ID => ?ERROR_TSC_MISSING_LAYOUT_TYPE,
    ?ERROR_GUI_PACKAGE_TOO_LARGE_ID => ?ERROR_GUI_PACKAGE_TOO_LARGE_TYPE,
    ?ERROR_BAD_VALUE_INTEGER_ID => ?ERROR_BAD_VALUE_INTEGER_TYPE,
    ?ERROR_BAD_VALUE_BINARY_ID => ?ERROR_BAD_VALUE_BINARY_TYPE,
    ?ERROR_BAD_VALUE_IPV4_ADDRESS_ID => ?ERROR_BAD_VALUE_IPV4_ADDRESS_TYPE,
    ?ERROR_BAD_VALUE_EMAIL_ID => ?ERROR_BAD_VALUE_EMAIL_TYPE,
    ?ERROR_BAD_VALUE_FILE_PATH_ID => ?ERROR_BAD_VALUE_FILE_PATH_TYPE,
    ?ERROR_BAD_VALUE_TOKEN_TYPE_ID => ?ERROR_BAD_VALUE_TOKEN_TYPE_TYPE,
    ?ERROR_BAD_VALUE_ID_NOT_FOUND_ID => ?ERROR_BAD_VALUE_ID_NOT_FOUND_TYPE,
    ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_ID => ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE,
    ?ERROR_BAD_VALUE_TEXT_TOO_LARGE_ID => ?ERROR_BAD_VALUE_TEXT_TOO_LARGE_TYPE,
    ?ERROR_BAD_VALUE_TOO_HIGH_ID => ?ERROR_BAD_VALUE_TOO_HIGH_TYPE,
    ?ERROR_BAD_VALUE_NAME_ID => ?ERROR_BAD_VALUE_NAME_TYPE,
    ?ERROR_BAD_VALUE_QOS_PARAMETERS_ID => ?ERROR_BAD_VALUE_QOS_PARAMETERS_TYPE,
    ?ERROR_BAD_VALUE_DOMAIN_ID => ?ERROR_BAD_VALUE_DOMAIN_TYPE,
    ?ERROR_BAD_VALUE_NOT_ALLOWED_ID => ?ERROR_BAD_VALUE_NOT_ALLOWED_TYPE,
    ?ERROR_BAD_VALUE_SUBDOMAIN_ID => ?ERROR_BAD_VALUE_SUBDOMAIN_TYPE,
    ?ERROR_BAD_VALUE_BOOLEAN_ID => ?ERROR_BAD_VALUE_BOOLEAN_TYPE,
    ?ERROR_BAD_VALUE_LIST_OF_ATOMS_ID => ?ERROR_BAD_VALUE_LIST_OF_ATOMS_TYPE,
    ?ERROR_BAD_VALUE_JSON_ID => ?ERROR_BAD_VALUE_JSON_TYPE,
    ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED_ID => ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE,
    ?ERROR_BAD_VALUE_EMPTY_ID => ?ERROR_BAD_VALUE_EMPTY_TYPE,
    ?ERROR_BAD_VALUE_IDENTIFIER_ID => ?ERROR_BAD_VALUE_IDENTIFIER_TYPE,
    ?ERROR_BAD_VALUE_PASSWORD_ID => ?ERROR_BAD_VALUE_PASSWORD_TYPE,
    ?ERROR_BAD_VALUE_NOT_IN_RANGE_ID => ?ERROR_BAD_VALUE_NOT_IN_RANGE_TYPE,
    ?ERROR_BAD_VALUE_ATOM_ID => ?ERROR_BAD_VALUE_ATOM_TYPE,
    ?ERROR_BAD_VALUE_AMBIGUOUS_ID_ID => ?ERROR_BAD_VALUE_AMBIGUOUS_ID_TYPE,
    ?ERROR_BAD_VALUE_OCTAL_ID => ?ERROR_BAD_VALUE_OCTAL_TYPE,
    ?ERROR_BAD_VALUE_LIST_OF_BINARIES_ID => ?ERROR_BAD_VALUE_LIST_OF_BINARIES_TYPE,
    ?ERROR_BAD_VALUE_FULL_NAME_ID => ?ERROR_BAD_VALUE_FULL_NAME_TYPE,
    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED_ID => ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE,
    ?ERROR_BAD_VALUE_USERNAME_ID => ?ERROR_BAD_VALUE_USERNAME_TYPE,
    ?ERROR_BAD_VALUE_INVITE_TYPE_ID => ?ERROR_BAD_VALUE_INVITE_TYPE_TYPE,
    ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID => ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE,
    ?ERROR_BAD_VALUE_CAVEAT_ID => ?ERROR_BAD_VALUE_CAVEAT_TYPE,
    ?ERROR_BAD_VALUE_XML_ID => ?ERROR_BAD_VALUE_XML_TYPE,
    ?ERROR_BAD_VALUE_TOKEN_ID => ?ERROR_BAD_VALUE_TOKEN_TYPE,
    ?ERROR_BAD_VALUE_TOO_LOW_ID => ?ERROR_BAD_VALUE_TOO_LOW_TYPE,
    ?ERROR_BAD_VALUE_FLOAT_ID => ?ERROR_BAD_VALUE_FLOAT_TYPE,
    ?ERROR_UNREGISTERED_ONEPROVIDER_ID => ?ERROR_UNREGISTERED_ONEPROVIDER_TYPE,
    ?ERROR_TIMEOUT_ID => ?ERROR_TIMEOUT_TYPE,
    ?ERROR_TEMPORARY_FAILURE_ID => ?ERROR_TEMPORARY_FAILURE_TYPE,
    ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED_ID => ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE,
    ?ERROR_FILE_ACCESS_ID => ?ERROR_FILE_ACCESS_TYPE,
    ?ERROR_SERVICE_UNAVAILABLE_ID => ?ERROR_SERVICE_UNAVAILABLE_TYPE,
    ?ERROR_BAD_MESSAGE_ID => ?ERROR_BAD_MESSAGE_TYPE,
    ?ERROR_NOT_SUPPORTED_ID => ?ERROR_NOT_SUPPORTED_TYPE,
    ?ERROR_ALREADY_EXISTS_ID => ?ERROR_ALREADY_EXISTS_TYPE,
    ?ERROR_NOT_IMPLEMENTED_ID => ?ERROR_NOT_IMPLEMENTED_TYPE,
    ?ERROR_LIMIT_REACHED_ID => ?ERROR_LIMIT_REACHED_TYPE,
    ?ERROR_NOT_FOUND_ID => ?ERROR_NOT_FOUND_TYPE,
    ?ERROR_INTERNAL_SERVER_ERROR_ID => ?ERROR_INTERNAL_SERVER_ERROR_TYPE,
    ?ERROR_BAD_VERSION_ID => ?ERROR_BAD_VERSION_TYPE,
    ?ERROR_EXPECTED_HANDSHAKE_MESSAGE_ID => ?ERROR_EXPECTED_HANDSHAKE_MESSAGE_TYPE,
    ?ERROR_NOT_SUBSCRIBABLE_ID => ?ERROR_NOT_SUBSCRIBABLE_TYPE,
    ?ERROR_BAD_GRI_ID => ?ERROR_BAD_GRI_TYPE,
    ?ERROR_RPC_UNDEFINED_ID => ?ERROR_RPC_UNDEFINED_TYPE,
    ?ERROR_HANDSHAKE_ALREADY_DONE_ID => ?ERROR_HANDSHAKE_ALREADY_DONE_TYPE,
    ?ERROR_NODE_NOT_COMPATIBLE_ID => ?ERROR_NODE_NOT_COMPATIBLE_TYPE,
    ?ERROR_DNS_SERVERS_UNREACHABLE_ID => ?ERROR_DNS_SERVERS_UNREACHABLE_TYPE,
    ?ERROR_LETS_ENCRYPT_NOT_REACHABLE_ID => ?ERROR_LETS_ENCRYPT_NOT_REACHABLE_TYPE,
    ?ERROR_NODE_ALREADY_IN_CLUSTER_ID => ?ERROR_NODE_ALREADY_IN_CLUSTER_TYPE,
    ?ERROR_USER_NOT_IN_CLUSTER_ID => ?ERROR_USER_NOT_IN_CLUSTER_TYPE,
    ?ERROR_FILE_ALLOCATION_ID => ?ERROR_FILE_ALLOCATION_TYPE,
    ?ERROR_NO_CONNECTION_TO_NEW_NODE_ID => ?ERROR_NO_CONNECTION_TO_NEW_NODE_TYPE,
    ?ERROR_NO_SERVICE_NODES_ID => ?ERROR_NO_SERVICE_NODES_TYPE,
    ?ERROR_LETS_ENCRYPT_RESPONSE_ID => ?ERROR_LETS_ENCRYPT_RESPONSE_TYPE,
    ?ERROR_ON_NODES_ID => ?ERROR_ON_NODES_TYPE,
    ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_ID => ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE,
    ?ERROR_SPACE_NOT_SUPPORTED_BY_ID => ?ERROR_SPACE_NOT_SUPPORTED_BY_TYPE,
    ?ERROR_RECALL_TARGET_CONFLICT_ID => ?ERROR_RECALL_TARGET_CONFLICT_TYPE,
    ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN_ID => ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE,
    ?ERROR_STAT_OPERATION_NOT_SUPPORTED_ID => ?ERROR_STAT_OPERATION_NOT_SUPPORTED_TYPE,
    ?ERROR_USER_NOT_SUPPORTED_ID => ?ERROR_USER_NOT_SUPPORTED_TYPE,
    ?ERROR_AUTO_CLEANING_DISABLED_ID => ?ERROR_AUTO_CLEANING_DISABLED_TYPE,
    ?ERROR_QUOTA_EXCEEDED_ID => ?ERROR_QUOTA_EXCEEDED_TYPE,
    ?ERROR_FILE_POPULARITY_DISABLED_ID => ?ERROR_FILE_POPULARITY_DISABLED_TYPE,
    ?ERROR_ATM_STORE_TYPE_DISALLOWED_ID => ?ERROR_ATM_STORE_TYPE_DISALLOWED_TYPE,
    ?ERROR_ATM_STORE_FROZEN_ID => ?ERROR_ATM_STORE_FROZEN_TYPE,
    ?ERROR_ATM_UNSUPPORTED_DATA_TYPE_ID => ?ERROR_ATM_UNSUPPORTED_DATA_TYPE_TYPE,
    ?ERROR_ATM_TASK_ARG_MAPPING_FAILED_ID => ?ERROR_ATM_TASK_ARG_MAPPING_FAILED_TYPE,
    ?ERROR_ATM_DATA_TYPE_UNVERIFIED_ID => ?ERROR_ATM_DATA_TYPE_UNVERIFIED_TYPE,
    ?ERROR_ATM_STORE_CREATION_FAILED_ID => ?ERROR_ATM_STORE_CREATION_FAILED_TYPE,
    ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED_ID => ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE,
    ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED_ID => ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE,
    ?ERROR_ATM_TASK_RESULT_MISSING_ID => ?ERROR_ATM_TASK_RESULT_MISSING_TYPE,
    ?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED_ID => ?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE,
    ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED_ID => ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE,
    ?ERROR_ATM_OPENFAAS_UNHEALTHY_ID => ?ERROR_ATM_OPENFAAS_UNHEALTHY_TYPE,
    ?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED_ID => ?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE,
    ?ERROR_ATM_STORE_CONTENT_NOT_SET_ID => ?ERROR_ATM_STORE_CONTENT_NOT_SET_TYPE,
    ?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_ID => ?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE,
    ?ERROR_ATM_JOB_BATCH_CRASHED_ID => ?ERROR_ATM_JOB_BATCH_CRASHED_TYPE,
    ?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_ID => ?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE,
    ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID => ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE,
    ?ERROR_ATM_STORE_NOT_FOUND_ID => ?ERROR_ATM_STORE_NOT_FOUND_TYPE,
    ?ERROR_ATM_PARALLEL_BOX_EMPTY_ID => ?ERROR_ATM_PARALLEL_BOX_EMPTY_TYPE,
    ?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_ID => ?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE,
    ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING_ID => ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE,
    ?ERROR_ATM_LANE_EMPTY_ID => ?ERROR_ATM_LANE_EMPTY_TYPE,
    ?ERROR_ATM_WORKFLOW_EXECUTION_ENDED_ID => ?ERROR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE,
    ?ERROR_ATM_TASK_RESULT_MAPPING_FAILED_ID => ?ERROR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE,
    ?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED_ID => ?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE,
    ?ERROR_ATM_WORKFLOW_EMPTY_ID => ?ERROR_ATM_WORKFLOW_EMPTY_TYPE,
    ?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_ID => ?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE,
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_ID => ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE,
    ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_ID => ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE,
    ?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED_ID => ?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE,
    ?ERROR_ATM_JOB_BATCH_WITHDRAWN_ID => ?ERROR_ATM_JOB_BATCH_WITHDRAWN_TYPE,
    ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_ID => ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE,
    ?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_ID => ?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE,
    ?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE_ID => ?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE,
    ?ERROR_ATM_INVALID_STATUS_TRANSITION_ID => ?ERROR_ATM_INVALID_STATUS_TRANSITION_TYPE,
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_ID => ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE,
    ?ERROR_ATM_TASK_EXECUTION_STOPPED_ID => ?ERROR_ATM_TASK_EXECUTION_STOPPED_TYPE,
    ?ERROR_ATM_OPENFAAS_UNREACHABLE_ID => ?ERROR_ATM_OPENFAAS_UNREACHABLE_TYPE,
    ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID => ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE,
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_ID => ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE,
    ?ERROR_ATM_OPENFAAS_QUERY_FAILED_ID => ?ERROR_ATM_OPENFAAS_QUERY_FAILED_TYPE,
    ?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED_ID => ?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE,
    ?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED_ID => ?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE,
    ?ERROR_DIR_STATS_DISABLED_FOR_SPACE_ID => ?ERROR_DIR_STATS_DISABLED_FOR_SPACE_TYPE,
    ?ERROR_DIR_STATS_NOT_READY_ID => ?ERROR_DIR_STATS_NOT_READY_TYPE,
    ?ERROR_REQUIRES_IMPORTED_STORAGE_ID => ?ERROR_REQUIRES_IMPORTED_STORAGE_TYPE,
    ?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE_ID => ?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE,
    ?ERROR_STORAGE_IMPORT_NOT_SUPPORTED_ID => ?ERROR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE,
    ?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_ID => ?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE,
    ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID => ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE,
    ?ERROR_REQUIRES_READONLY_STORAGE_ID => ?ERROR_REQUIRES_READONLY_STORAGE_TYPE,
    ?ERROR_STORAGE_TEST_FAILED_ID => ?ERROR_STORAGE_TEST_FAILED_TYPE,
    ?ERROR_STORAGE_IN_USE_ID => ?ERROR_STORAGE_IN_USE_TYPE,
    ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_ID => ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE,
    ?ERROR_REQUIRES_NON_IMPORTED_STORAGE_ID => ?ERROR_REQUIRES_NON_IMPORTED_STORAGE_TYPE,
    ?ERROR_TRANSFER_ALREADY_ENDED_ID => ?ERROR_TRANSFER_ALREADY_ENDED_TYPE,
    ?ERROR_TRANSFER_NOT_ENDED_ID => ?ERROR_TRANSFER_NOT_ENDED_TYPE,
    ?ERROR_VIEW_QUERY_FAILED_ID => ?ERROR_VIEW_QUERY_FAILED_TYPE,
    ?ERROR_VIEW_NOT_EXISTS_ON_ID => ?ERROR_VIEW_NOT_EXISTS_ON_TYPE,
    ?ERROR_ATM_LAMBDA_IN_USE_ID => ?ERROR_ATM_LAMBDA_IN_USE_TYPE,
    ?ERROR_RELATION_ALREADY_EXISTS_ID => ?ERROR_RELATION_ALREADY_EXISTS_TYPE,
    ?ERROR_BASIC_AUTH_DISABLED_ID => ?ERROR_BASIC_AUTH_DISABLED_TYPE,
    ?ERROR_CANNOT_ADD_RELATION_TO_SELF_ID => ?ERROR_CANNOT_ADD_RELATION_TO_SELF_TYPE,
    ?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID => ?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE,
    ?ERROR_CANNOT_DELETE_ENTITY_ID => ?ERROR_CANNOT_DELETE_ENTITY_TYPE,
    ?ERROR_CANNOT_REMOVE_LAST_OWNER_ID => ?ERROR_CANNOT_REMOVE_LAST_OWNER_TYPE,
    ?ERROR_PROTECTED_GROUP_ID => ?ERROR_PROTECTED_GROUP_TYPE,
    ?ERROR_BASIC_AUTH_NOT_SUPPORTED_ID => ?ERROR_BASIC_AUTH_NOT_SUPPORTED_TYPE,
    ?ERROR_RELATION_DOES_NOT_EXIST_ID => ?ERROR_RELATION_DOES_NOT_EXIST_TYPE,
    ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID => ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE,
    ?ERROR_SPACE_MARKETPLACE_DISABLED_ID => ?ERROR_SPACE_MARKETPLACE_DISABLED_TYPE,
    ?ERROR_SUBDOMAIN_DELEGATION_DISABLED_ID => ?ERROR_SUBDOMAIN_DELEGATION_DISABLED_TYPE,
    ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_ID => ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE,
    ?ERROR_POSIX_ID => ?ERROR_POSIX_TYPE
}).


%%%===================================================================
%%% API
%%%===================================================================


-spec is_known_error(term()) -> boolean().
is_known_error(?ERROR) -> true;
is_known_error(_) -> false.


-spec is_posix_code(term()) -> boolean().
is_posix_code(ErrorCode) ->
    ordsets:is_element(ErrorCode, ?ERROR_CODES).


-spec to_json
    (undefined) -> null;
    (error()) -> json_utils:json_map().
to_json(undefined) ->
    null;

to_json(?ERROR_UNRECOGNIZED_ERROR(ErrorAsJson)) ->
    % Carries errors that have not been recognized upon decoding.
    case maps:is_key(<<"description">>, ErrorAsJson) of
        true ->
            ErrorAsJson;
        false ->
            ErrorAsJson#{<<"description">> => <<"No description (unknown error).">>}
    end;

to_json(#od_error{type = Type} = Error) ->
    Type:to_json(Error);

to_json(OtherError) ->
    % Wildcard to catch all errors that might be returned by the application logic, but does
    % not match any error defined in this module. Inability to translate is treated as an
    % unexpected exception (an ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef) is returned).
    ReturnedError = ?catch_exceptions(error({cannot_translate_error, OtherError})),
    to_json(ReturnedError).


-spec from_json
    (null) -> undefined;
    (json_utils:json_map()) -> error().
from_json(null) ->
    undefined;

from_json(ErrorJson) ->
    try
        ErrorId = maps:get(<<"id">>, ErrorJson),
        ErrorType = maps:get(ErrorId, ?ERROR_ID_TO_TYPE_MAPPING),
        ErrorType:from_json(ErrorJson)
    catch _:_ ->
        ?ERROR_UNRECOGNIZED_ERROR(ErrorJson)
    end.


-spec to_http_code(error()) -> 400 | 401 | 403 | 404 | 409 | 500 | 501 | 503.
to_http_code(?ERROR_UNRECOGNIZED_ERROR(_)) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR;

to_http_code(Error = #od_error{type = Type}) ->
    Type:to_http_code(Error).

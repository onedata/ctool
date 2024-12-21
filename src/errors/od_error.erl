%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines od_error record interface - implemented for onedata 
%%% errors that are to be returned via API calls. Each such error should have a
%%% dedicated module implementing the callbacks.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error).

-include("errors.hrl").

%% API
-export([
    build_ctx/2,
    ctx_to_json/1,
    ctx_from_json/1,

    format_description/2,
    format_csv/1,

    iso8601_now/0,
    version/0
]).


-type http_code() :: 400 | 401 | 403 | 404 | 409 | 500 | 501 | 503.

-type errno() :: 
    ?OK | ?E2BIG | ?EACCES | ?EADDRINUSE | ?EADDRNOTAVAIL |
    ?EAFNOSUPPORT | ?EAGAIN | ?EALREADY | ?EBADF | ?EBADMSG | ?EBUSY |
    ?ECANCELED | ?ECHILD | ?ECONNABORTED | ?ECONNREFUSED | ?ECONNRESET |
    ?EDEADLK | ?EDESTADDRREQ | ?EDOM | ?EEXIST | ?EFAULT | ?EFBIG |
    ?EHOSTUNREACH | ?EIDRM | ?EILSEQ | ?EINPROGRESS | ?EINTR | ?EINVAL | ?EIO |
    ?EISCONN | ?EISDIR | ?EKEYEXPIRED | ?ELOOP | ?EMFILE | ?EMLINK | ?EMSGSIZE |
    ?ENAMETOOLONG | ?ENETDOWN | ?ENETRESET | ?ENETUNREACH | ?ENFILE | ?ENOBUFS |
    ?ENODATA | ?ENODEV | ?ENOENT | ?ENOEXEC | ?ENOLCK | ?ENOLINK | ?ENOMEM |
    ?ENOMSG | ?ENOPROTOOPT | ?ENOSPC | ?ENOSR | ?ENOSTR | ?ENOSYS | ?ENOTCONN |
    ?ENOTDIR | ?ENOTEMPTY | ?ENOTRECOVERABLE | ?ENOTSOCK | ?ENOTSUP | ?ENOTTY |
    ?ENXIO | ?EOPNOTSUPP | ?EOVERFLOW | ?EOWNERDEAD | ?EPERM | ?EPIPE |
    ?EPROTO | ?EPROTONOSUPPORT | ?EPROTOTYPE | ?ERANGE | ?EROFS | ?ESPIPE |
    ?ESRCH | ?ETIME | ?ETIMEDOUT | ?ETXTBSY | ?EWOULDBLOCK | ?EXDEV.

-type ctx() :: #od_error_ctx{}.

-export_type([http_code/0, errno/0, ctx/0]).

-type auth_token_error() ::
    od_error_bad_consumer_token:t() |
    od_error_bad_idp_access_token:t() |
    od_error_bad_service_token:t() |
    od_error_bad_token:t() |
    od_error_invite_token_consumer_invalid:t() |
    od_error_invite_token_subject_not_authorized:t() |
    od_error_invite_token_target_id_invalid:t() |
    od_error_invite_token_usage_limit_reached:t() |
    od_error_not_an_access_token:t() |
    od_error_not_an_identity_token:t() |
    od_error_not_an_invite_token:t() |
    od_error_token_caveat_unknown:t() |
    od_error_token_caveat_unverified:t() |
    od_error_token_invalid:t() |
    od_error_token_revoked:t() |
    od_error_token_service_forbidden:t() |
    od_error_token_session_invalid:t() |
    od_error_token_subject_invalid:t() |
    od_error_token_time_caveat_required:t() |
    od_error_token_too_large:t().

-type auth_error() ::
    od_error_bad_basic_credentials:t() |
    od_error_forbidden:t() |
    od_error_forbidden_todo:t() |
    od_error_unauthorized:t() |
    od_error_user_blocked:t() |
    auth_token_error().

-type connection_error() ::
    od_error_no_connection_to_cluster_node:t() |
    od_error_no_connection_to_onezone:t() |
    od_error_no_connection_to_peer_oneprovider:t().

-type data_validation_value_error() ::
    od_error_bad_value_ambiguous_id:t() |
    od_error_bad_value_boolean:t() |
    od_error_bad_value_caveat:t() |
    od_error_bad_value_domain:t() |
    od_error_bad_value_email:t() |
    od_error_bad_value_empty:t() |
    od_error_bad_value_file_path:t() |
    od_error_bad_value_float:t() |
    od_error_bad_value_full_name:t() |
    od_error_bad_value_id_not_found:t() |
    od_error_bad_value_identifier:t() |
    od_error_bad_value_identifier_occupied:t() |
    od_error_bad_value_integer:t() |
    od_error_bad_value_invite_type:t() |
    od_error_bad_value_ipv4_address:t() |
    od_error_bad_value_json:t() |
    od_error_bad_value_list_not_allowed:t() |
    od_error_bad_value_list_of_ipv4_addresses:t() |
    od_error_bad_value_list_of_strings:t() |
    od_error_bad_value_name:t() |
    od_error_bad_value_not_allowed:t() |
    od_error_bad_value_not_in_range:t() |
    od_error_bad_value_octal:t() |
    od_error_bad_value_password:t() |
    od_error_bad_value_qos_parameters:t() |
    od_error_bad_value_string:t() |
    od_error_bad_value_subdomain:t() |
    od_error_bad_value_text_too_large:t() |
    od_error_bad_value_token:t() |
    od_error_bad_value_token_type:t() |
    od_error_bad_value_too_high:t() |
    od_error_bad_value_too_low:t() |
    od_error_bad_value_tsc_conflicting_metric_config:t() |
    od_error_bad_value_username:t() |
    od_error_bad_value_xml:t().

-type data_validation_error() ::
    od_error_bad_data:t() |
    od_error_bad_gui_package:t() |
    od_error_gui_package_too_large:t() |
    od_error_gui_package_unverified:t() |
    od_error_illegal_support_stage_transition:t() |
    od_error_invalid_qos_expression:t() |
    od_error_malformed_data:t() |
    od_error_missing_at_least_one_value:t() |
    od_error_missing_required_value:t() |
    od_error_tsc_missing_layout:t() |
    od_error_tsc_too_many_metrics:t() |
    data_validation_value_error().

-type general_error() ::
    od_error_already_exists:t() |
    od_error_bad_message:t() |
    od_error_external_service_operation_failed:t() |
    od_error_file_access:t() |
    od_error_internal_server_error:t() |
    od_error_limit_reached:t() |
    od_error_not_found:t() |
    od_error_not_implemented:t() |
    od_error_not_supported:t() |
    od_error_service_unavailable:t() |
    od_error_temporary_failure:t() |
    od_error_timeout:t() |
    od_error_unregistered_oneprovider:t().

-type graph_sync_error() ::
    od_error_bad_gri:t() |
    od_error_bad_version:t() |
    od_error_expected_handshake_message:t() |
    od_error_handshake_already_done:t() |
    od_error_not_subscribable:t() |
    od_error_rpc_undefined:t().

-type onepanel_error() ::
    od_error_dns_servers_unreachable:t() |
    od_error_lets_encrypt_not_reachable:t() |
    od_error_lets_encrypt_response:t() |
    od_error_no_connection_to_new_node:t() |
    od_error_no_service_nodes:t() |
    od_error_node_already_in_cluster:t() |
    od_error_node_not_compatible:t() |
    od_error_on_nodes:t() |
    od_error_user_not_in_cluster:t().

-type op_worker_atm_error() ::
    od_error_atm_data_type_unverified:t() |
    od_error_atm_data_value_constraint_unverified:t() |
    od_error_atm_invalid_status_transition:t() |
    od_error_atm_job_batch_crashed:t() |
    od_error_atm_job_batch_withdrawn:t() |
    od_error_atm_lambda_config_bad_value:t() |
    od_error_atm_lane_empty:t() |
    od_error_atm_lane_execution_creation_failed:t() |
    od_error_atm_lane_execution_initiation_failed:t() |
    od_error_atm_lane_execution_rerun_failed:t() |
    od_error_atm_lane_execution_retry_failed:t() |
    od_error_atm_openfaas_function_registration_failed:t() |
    od_error_atm_openfaas_not_configured:t() |
    od_error_atm_openfaas_query_failed:t() |
    od_error_atm_openfaas_unhealthy:t() |
    od_error_atm_openfaas_unreachable:t() |
    od_error_atm_parallel_box_empty:t() |
    od_error_atm_parallel_box_execution_creation_failed:t() |
    od_error_atm_parallel_box_execution_initiation_failed:t() |
    od_error_atm_store_content_not_set:t() |
    od_error_atm_store_creation_failed:t() |
    od_error_atm_store_frozen:t() |
    od_error_atm_store_missing_required_initial_content:t() |
    od_error_atm_store_not_found:t() |
    od_error_atm_store_type_disallowed:t() |
    od_error_atm_task_arg_mapper_for_nonexistent_lambda_arg:t() |
    od_error_atm_task_arg_mapper_for_required_lambda_arg_missing:t() |
    od_error_atm_task_arg_mapper_iterated_item_query_failed:t() |
    od_error_atm_task_arg_mapper_unsupported_value_builder:t() |
    od_error_atm_task_arg_mapping_failed:t() |
    od_error_atm_task_execution_creation_failed:t() |
    od_error_atm_task_execution_initiation_failed:t() |
    od_error_atm_task_execution_stopped:t() |
    od_error_atm_task_result_dispatch_failed:t() |
    od_error_atm_task_result_mapping_failed:t() |
    od_error_atm_task_result_missing:t() |
    od_error_atm_unsupported_data_type:t() |
    od_error_atm_workflow_empty:t() |
    od_error_atm_workflow_execution_ended:t() |
    od_error_atm_workflow_execution_not_ended:t() |
    od_error_atm_workflow_execution_not_resumable:t() |
    od_error_atm_workflow_execution_not_stopped:t() |
    od_error_atm_workflow_execution_stopped:t() |
    od_error_atm_workflow_execution_stopping:t().

-type op_worker_dir_stats_error() ::
    od_error_dir_stats_disabled_for_space:t() |
    od_error_dir_stats_not_ready:t().

-type op_worker_storage_error() ::
    od_error_auto_storage_import_not_supported:t() |
    od_error_not_a_local_storage_supporting_space:t() |
    od_error_requires_auto_storage_import_mode:t() |
    od_error_requires_imported_storage:t() |
    od_error_requires_non_imported_storage:t() |
    od_error_requires_posix_compatible_storage:t() |
    od_error_requires_readonly_storage:t() |
    od_error_storage_import_not_supported:t() |
    od_error_storage_in_use:t() |
    od_error_storage_test_failed:t().

-type op_worker_transfer_error() ::
    od_error_transfer_already_ended:t() |
    od_error_transfer_not_ended:t().

-type op_worker_view_error() ::
    od_error_view_not_exists_on:t() |
    od_error_view_query_failed:t().

-type op_worker_error() ::
    od_error_auto_cleaning_disabled:t() |
    od_error_file_popularity_disabled:t() |
    od_error_forbidden_for_current_archive_state:t() |
    od_error_nested_archive_deletion_forbidden:t() |
    od_error_quota_exceeded:t() |
    od_error_recall_target_conflict:t() |
    od_error_space_not_supported_by:t() |
    od_error_stat_operation_not_supported:t() |
    od_error_user_not_supported:t() |
    op_worker_atm_error() |
    op_worker_dir_stats_error() |
    op_worker_storage_error() |
    op_worker_transfer_error() |
    op_worker_view_error().

-type oz_worker_space_error() ::
    od_error_space_already_supported_with_imported_storage:t() |
    od_error_space_marketplace_disabled:t().

-type oz_worker_subdomain_error() ::
    od_error_subdomain_delegation_disabled:t() |
    od_error_subdomain_delegation_not_supported:t().

-type oz_worker_error() ::
    od_error_atm_lambda_in_use:t() |
    od_error_basic_auth_disabled:t() |
    od_error_basic_auth_not_supported:t() |
    od_error_cannot_add_relation_to_self:t() |
    od_error_cannot_delete_entity:t() |
    od_error_cannot_delete_non_empty_handle_service:t() |
    od_error_cannot_remove_last_owner:t() |
    od_error_protected_group:t() |
    od_error_relation_already_exists:t() |
    od_error_relation_does_not_exist:t() |
    oz_worker_space_error() |
    oz_worker_subdomain_error().

-type posix_error() ::
    od_error_posix:t().

-type error() ::
    auth_error() |
    connection_error() |
    data_validation_error() |
    general_error() |
    graph_sync_error() |
    onepanel_error() |
    op_worker_error() |
    oz_worker_error() |
    posix_error().

-export_type([
    auth_token_error/0,
    auth_error/0,
    connection_error/0,
    data_validation_value_error/0,
    data_validation_error/0,
    general_error/0,
    graph_sync_error/0,
    onepanel_error/0,
    op_worker_atm_error/0,
    op_worker_dir_stats_error/0,
    op_worker_storage_error/0,
    op_worker_transfer_error/0,
    op_worker_view_error/0,
    op_worker_error/0,
    oz_worker_space_error/0,
    oz_worker_subdomain_error/0,
    oz_worker_error/0,
    posix_error/0,
    error/0
]).


%%%===================================================================
%%% od_error behaviour definition
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Encodes an error JSON object.
%% @end
%%--------------------------------------------------------------------
-callback to_json(error()) -> json_utils:json_map().


%%--------------------------------------------------------------------
%% @doc
%% Decodes an error from a JSON object.
%% @end
%%--------------------------------------------------------------------
-callback from_json(json_utils:json_map()) -> error().


%%--------------------------------------------------------------------
%% @doc
%% Returns HTTP code to be returned in REST response.
%% @end
%%--------------------------------------------------------------------
-callback to_http_code(error()) -> http_code().


%%--------------------------------------------------------------------
%% @doc
%% Returns POSIX errno associated with the error.
%% @end
%%--------------------------------------------------------------------
-callback to_errno(error()) -> false | {true, errno()}.


%%%===================================================================
%%% API
%%%===================================================================


-spec build_ctx(module(), integer()) -> ctx().
build_ctx(Module, Line) ->
    #od_error_ctx{
        module = str_utils:to_binary(Module),
        line = Line,
        timestamp = iso8601_now(),
        version = version()
    }.


-spec ctx_to_json
    (undefined) -> null;
    (ctx()) -> json_utils:json_map().
ctx_to_json(undefined) ->
    null;
ctx_to_json(#od_error_ctx{module = Module, line = Line, timestamp = Timestamp, version = Version}) ->
    #{
        <<"module">> => utils:undefined_to_null(Module),
        <<"line">> => utils:undefined_to_null(Line),
        <<"timestamp">> => utils:undefined_to_null(Timestamp),
        <<"version">> => utils:undefined_to_null(Version)
    }.


-spec ctx_from_json
    (null) -> undefined;
    (json_utils:json_map()) -> ctx().
ctx_from_json(null) ->
    undefined;
ctx_from_json(Json) ->
    #od_error_ctx{
        module = utils:null_to_undefined(maps:get(<<"module">>, Json, null)),
        line = utils:null_to_undefined(maps:get(<<"line">>, Json, null)),
        timestamp = utils:null_to_undefined(maps:get(<<"timestamp">>, Json, null)),
        version = utils:null_to_undefined(maps:get(<<"version">>, Json, null))
    }.


-spec format_description(string(), [term()]) -> binary().
format_description(Format, Args) ->
    Desc = str_utils:format_bin(Format, Args),
    TrimmedDesc = string:trim(Desc, trailing, "."),
    <<TrimmedDesc/binary, ".">>.


-spec format_csv([term()]) -> binary().
format_csv(Values) ->
    str_utils:join_as_binaries(Values, <<", ">>).


%%--------------------------------------------------------------------
%% @doc
%% Returns current timestamp in ISO8601 format.
%% @end
%%--------------------------------------------------------------------
-spec iso8601_now() -> time:iso8601().
iso8601_now() ->
    Seconds = native_node_clock:system_time_millis() div 1000,
    DateTime = time:seconds_to_datetime(Seconds),
    time:datetime_to_iso8601(DateTime).


%%--------------------------------------------------------------------
%% @doc
%% Returns version of error definitions.
%% @end
%%--------------------------------------------------------------------
-spec version() -> binary().
version() ->
    <<"ceddd0b9">>.

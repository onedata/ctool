%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Error attributes to be used across all APIs in Onedata products.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERROR_ATTRS_HRL).
-define(ERROR_ATTRS_HRL, 1).


%%--------------------------------------------------------------------
%% deprecated errors
%%--------------------------------------------------------------------
-define(ERROR_ALREADY_EXISTS_ID, <<"alreadyExists">>).
-define(ERROR_ALREADY_EXISTS_TYPE, od_error_already_exists).

-define(ERROR_NOT_FOUND_ID, <<"notFound">>).
-define(ERROR_NOT_FOUND_TYPE, od_error_not_found).

-define(ERROR_NOT_SUPPORTED_ID, <<"notSupported">>).
-define(ERROR_NOT_SUPPORTED_TYPE, od_error_not_supported).

-define(ERROR_TIMEOUT_ID, <<"timeout">>).
-define(ERROR_TIMEOUT_TYPE, od_error_timeout).


%%--------------------------------------------------------------------
%% Unknown / unexpected error
%%--------------------------------------------------------------------
-define(ERR_UNRECOGNIZED_ERROR_TYPE, od_error_unrecognized_error).


%%--------------------------------------------------------------------
%% auth errors
%%--------------------------------------------------------------------
-define(ERR_BAD_BASIC_CREDENTIALS_ID, <<"badBasicCredentials">>).
-define(ERR_BAD_BASIC_CREDENTIALS_TYPE, od_error_bad_basic_credentials).

-define(ERR_FORBIDDEN_ID, <<"forbidden">>).
-define(ERR_FORBIDDEN_TYPE, od_error_forbidden).

-define(ERR_FORBIDDEN_WITH_HINT_ID, <<"forbiddenWithHint">>).
-define(ERR_FORBIDDEN_WITH_HINT_TYPE, od_error_forbidden_with_hint).

-define(ERR_UNAUTHORIZED_ID, <<"unauthorized">>).
-define(ERR_UNAUTHORIZED_TYPE, od_error_unauthorized).

-define(ERR_USER_BLOCKED_ID, <<"userBlocked">>).
-define(ERR_USER_BLOCKED_TYPE, od_error_user_blocked).


%%--------------------------------------------------------------------
%% auth/token errors
%%--------------------------------------------------------------------
-define(ERR_BAD_CONSUMER_TOKEN_ID, <<"badConsumerToken">>).
-define(ERR_BAD_CONSUMER_TOKEN_TYPE, od_error_bad_consumer_token).

-define(ERR_BAD_IDP_ACCESS_TOKEN_ID, <<"badIdpAccessToken">>).
-define(ERR_BAD_IDP_ACCESS_TOKEN_TYPE, od_error_bad_idp_access_token).

-define(ERR_BAD_SERVICE_TOKEN_ID, <<"badServiceToken">>).
-define(ERR_BAD_SERVICE_TOKEN_TYPE, od_error_bad_service_token).

-define(ERR_BAD_TOKEN_ID, <<"badToken">>).
-define(ERR_BAD_TOKEN_TYPE, od_error_bad_token).

-define(ERR_INVITE_TOKEN_CONSUMER_INVALID_ID, <<"inviteTokenConsumerInvalid">>).
-define(ERR_INVITE_TOKEN_CONSUMER_INVALID_TYPE, od_error_invite_token_consumer_invalid).

-define(ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_ID, <<"inviteTokenSubjectNotAuthorized">>).
-define(ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE, od_error_invite_token_subject_not_authorized).

-define(ERR_INVITE_TOKEN_TARGET_ID_INVALID_ID, <<"inviteTokenTargetIdInvalid">>).
-define(ERR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE, od_error_invite_token_target_id_invalid).

-define(ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_ID, <<"inviteTokenUsageLimitReached">>).
-define(ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE, od_error_invite_token_usage_limit_reached).

-define(ERR_NOT_AN_ACCESS_TOKEN_ID, <<"notAnAccessToken">>).
-define(ERR_NOT_AN_ACCESS_TOKEN_TYPE, od_error_not_an_access_token).

-define(ERR_NOT_AN_IDENTITY_TOKEN_ID, <<"notAnIdentityToken">>).
-define(ERR_NOT_AN_IDENTITY_TOKEN_TYPE, od_error_not_an_identity_token).

-define(ERR_NOT_AN_INVITE_TOKEN_ID, <<"notAnInviteToken">>).
-define(ERR_NOT_AN_INVITE_TOKEN_TYPE, od_error_not_an_invite_token).

-define(ERR_TOKEN_CAVEAT_UNKNOWN_ID, <<"tokenCaveatUnknown">>).
-define(ERR_TOKEN_CAVEAT_UNKNOWN_TYPE, od_error_token_caveat_unknown).

-define(ERR_TOKEN_CAVEAT_UNVERIFIED_ID, <<"tokenCaveatUnverified">>).
-define(ERR_TOKEN_CAVEAT_UNVERIFIED_TYPE, od_error_token_caveat_unverified).

-define(ERR_TOKEN_INVALID_ID, <<"tokenInvalid">>).
-define(ERR_TOKEN_INVALID_TYPE, od_error_token_invalid).

-define(ERR_TOKEN_REVOKED_ID, <<"tokenRevoked">>).
-define(ERR_TOKEN_REVOKED_TYPE, od_error_token_revoked).

-define(ERR_TOKEN_SERVICE_FORBIDDEN_ID, <<"tokenServiceForbidden">>).
-define(ERR_TOKEN_SERVICE_FORBIDDEN_TYPE, od_error_token_service_forbidden).

-define(ERR_TOKEN_SESSION_INVALID_ID, <<"tokenSessionInvalid">>).
-define(ERR_TOKEN_SESSION_INVALID_TYPE, od_error_token_session_invalid).

-define(ERR_TOKEN_SUBJECT_INVALID_ID, <<"tokenSubjectInvalid">>).
-define(ERR_TOKEN_SUBJECT_INVALID_TYPE, od_error_token_subject_invalid).

-define(ERR_TOKEN_TIME_CAVEAT_REQUIRED_ID, <<"tokenTimeCaveatRequired">>).
-define(ERR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE, od_error_token_time_caveat_required).

-define(ERR_TOKEN_TOO_LARGE_ID, <<"tokenTooLarge">>).
-define(ERR_TOKEN_TOO_LARGE_TYPE, od_error_token_too_large).


%%--------------------------------------------------------------------
%% connection errors
%%--------------------------------------------------------------------
-define(ERR_NO_CONNECTION_TO_CLUSTER_NODE_ID, <<"noConnectionToClusterNode">>).
-define(ERR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE, od_error_no_connection_to_cluster_node).

-define(ERR_NO_CONNECTION_TO_ONEZONE_ID, <<"noConnectionToOnezone">>).
-define(ERR_NO_CONNECTION_TO_ONEZONE_TYPE, od_error_no_connection_to_onezone).

-define(ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_ID, <<"noConnectionToPeerOneprovider">>).
-define(ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE, od_error_no_connection_to_peer_oneprovider).


%%--------------------------------------------------------------------
%% data_validation errors
%%--------------------------------------------------------------------
-define(ERR_BAD_DATA_ID, <<"badData">>).
-define(ERR_BAD_DATA_TYPE, od_error_bad_data).

-define(ERR_BAD_GUI_PACKAGE_ID, <<"badGuiPackage">>).
-define(ERR_BAD_GUI_PACKAGE_TYPE, od_error_bad_gui_package).

-define(ERR_GUI_PACKAGE_TOO_LARGE_ID, <<"guiPackageTooLarge">>).
-define(ERR_GUI_PACKAGE_TOO_LARGE_TYPE, od_error_gui_package_too_large).

-define(ERR_GUI_PACKAGE_UNVERIFIED_ID, <<"guiPackageUnverified">>).
-define(ERR_GUI_PACKAGE_UNVERIFIED_TYPE, od_error_gui_package_unverified).

-define(ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_ID, <<"illegalSupportStageTransition">>).
-define(ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE, od_error_illegal_support_stage_transition).

-define(ERR_INVALID_QOS_EXPRESSION_ID, <<"invalidQosExpression">>).
-define(ERR_INVALID_QOS_EXPRESSION_TYPE, od_error_invalid_qos_expression).

-define(ERR_MALFORMED_DATA_ID, <<"malformedData">>).
-define(ERR_MALFORMED_DATA_TYPE, od_error_malformed_data).

-define(ERR_MISSING_AT_LEAST_ONE_VALUE_ID, <<"missingAtLeastOneValue">>).
-define(ERR_MISSING_AT_LEAST_ONE_VALUE_TYPE, od_error_missing_at_least_one_value).

-define(ERR_MISSING_REQUIRED_VALUE_ID, <<"missingRequiredValue">>).
-define(ERR_MISSING_REQUIRED_VALUE_TYPE, od_error_missing_required_value).

-define(ERR_TSC_MISSING_LAYOUT_ID, <<"timeSeriesCollectionMissingLayout">>).
-define(ERR_TSC_MISSING_LAYOUT_TYPE, od_error_tsc_missing_layout).

-define(ERR_TSC_TOO_MANY_METRICS_ID, <<"timeSeriesCollectionTooManyMetrics">>).
-define(ERR_TSC_TOO_MANY_METRICS_TYPE, od_error_tsc_too_many_metrics).


%%--------------------------------------------------------------------
%% data_validation/value errors
%%--------------------------------------------------------------------
-define(ERR_BAD_VALUE_AMBIGUOUS_ID_ID, <<"badValueAmbiguousId">>).
-define(ERR_BAD_VALUE_AMBIGUOUS_ID_TYPE, od_error_bad_value_ambiguous_id).

-define(ERR_BAD_VALUE_BOOLEAN_ID, <<"badValueBoolean">>).
-define(ERR_BAD_VALUE_BOOLEAN_TYPE, od_error_bad_value_boolean).

-define(ERR_BAD_VALUE_CAVEAT_ID, <<"badValueCaveat">>).
-define(ERR_BAD_VALUE_CAVEAT_TYPE, od_error_bad_value_caveat).

-define(ERR_BAD_VALUE_DOMAIN_ID, <<"badValueDomain">>).
-define(ERR_BAD_VALUE_DOMAIN_TYPE, od_error_bad_value_domain).

-define(ERR_BAD_VALUE_EMAIL_ID, <<"badValueEmail">>).
-define(ERR_BAD_VALUE_EMAIL_TYPE, od_error_bad_value_email).

-define(ERR_BAD_VALUE_EMPTY_ID, <<"badValueEmpty">>).
-define(ERR_BAD_VALUE_EMPTY_TYPE, od_error_bad_value_empty).

-define(ERR_BAD_VALUE_FILE_PATH_ID, <<"badValueFilePath">>).
-define(ERR_BAD_VALUE_FILE_PATH_TYPE, od_error_bad_value_file_path).

-define(ERR_BAD_VALUE_FLOAT_ID, <<"badValueFloat">>).
-define(ERR_BAD_VALUE_FLOAT_TYPE, od_error_bad_value_float).

-define(ERR_BAD_VALUE_FULL_NAME_ID, <<"badValueFullName">>).
-define(ERR_BAD_VALUE_FULL_NAME_TYPE, od_error_bad_value_full_name).

-define(ERR_BAD_VALUE_ID_NOT_FOUND_ID, <<"badValueIdNotFound">>).
-define(ERR_BAD_VALUE_ID_NOT_FOUND_TYPE, od_error_bad_value_id_not_found).

-define(ERR_BAD_VALUE_IDENTIFIER_ID, <<"badValueIdentifier">>).
-define(ERR_BAD_VALUE_IDENTIFIER_TYPE, od_error_bad_value_identifier).

-define(ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_ID, <<"badValueIdentifierOccupied">>).
-define(ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE, od_error_bad_value_identifier_occupied).

-define(ERR_BAD_VALUE_INTEGER_ID, <<"badValueInteger">>).
-define(ERR_BAD_VALUE_INTEGER_TYPE, od_error_bad_value_integer).

-define(ERR_BAD_VALUE_INVITE_TYPE_ID, <<"badValueInviteType">>).
-define(ERR_BAD_VALUE_INVITE_TYPE_TYPE, od_error_bad_value_invite_type).

-define(ERR_BAD_VALUE_IPV4_ADDRESS_ID, <<"badValueIPv4Address">>).
-define(ERR_BAD_VALUE_IPV4_ADDRESS_TYPE, od_error_bad_value_ipv4_address).

-define(ERR_BAD_VALUE_JSON_ID, <<"badValueJSON">>).
-define(ERR_BAD_VALUE_JSON_TYPE, od_error_bad_value_json).

-define(ERR_BAD_VALUE_LIST_NOT_ALLOWED_ID, <<"badValueListNotAllowed">>).
-define(ERR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE, od_error_bad_value_list_not_allowed).

-define(ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_ID, <<"badValueListOfIPv4Addresses">>).
-define(ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE, od_error_bad_value_list_of_ipv4_addresses).

-define(ERR_BAD_VALUE_LIST_OF_STRINGS_ID, <<"badValueListOfStrings">>).
-define(ERR_BAD_VALUE_LIST_OF_STRINGS_TYPE, od_error_bad_value_list_of_strings).

-define(ERR_BAD_VALUE_NAME_ID, <<"badValueName">>).
-define(ERR_BAD_VALUE_NAME_TYPE, od_error_bad_value_name).

-define(ERR_BAD_VALUE_NOT_ALLOWED_ID, <<"badValueNotAllowed">>).
-define(ERR_BAD_VALUE_NOT_ALLOWED_TYPE, od_error_bad_value_not_allowed).

-define(ERR_BAD_VALUE_NOT_IN_RANGE_ID, <<"badValueNotInRange">>).
-define(ERR_BAD_VALUE_NOT_IN_RANGE_TYPE, od_error_bad_value_not_in_range).

-define(ERR_BAD_VALUE_OCTAL_ID, <<"badValueOctal">>).
-define(ERR_BAD_VALUE_OCTAL_TYPE, od_error_bad_value_octal).

-define(ERR_BAD_VALUE_PASSWORD_ID, <<"badValuePassword">>).
-define(ERR_BAD_VALUE_PASSWORD_TYPE, od_error_bad_value_password).

-define(ERR_BAD_VALUE_QOS_PARAMETERS_ID, <<"badValueQoSParameters">>).
-define(ERR_BAD_VALUE_QOS_PARAMETERS_TYPE, od_error_bad_value_qos_parameters).

-define(ERR_BAD_VALUE_STRING_ID, <<"badValueString">>).
-define(ERR_BAD_VALUE_STRING_TYPE, od_error_bad_value_string).

-define(ERR_BAD_VALUE_SUBDOMAIN_ID, <<"badValueSubdomain">>).
-define(ERR_BAD_VALUE_SUBDOMAIN_TYPE, od_error_bad_value_subdomain).

-define(ERR_BAD_VALUE_TEXT_TOO_LARGE_ID, <<"badValueTextTooLarge">>).
-define(ERR_BAD_VALUE_TEXT_TOO_LARGE_TYPE, od_error_bad_value_text_too_large).

-define(ERR_BAD_VALUE_TOKEN_ID, <<"badValueToken">>).
-define(ERR_BAD_VALUE_TOKEN_TYPE, od_error_bad_value_token).

-define(ERR_BAD_VALUE_TOKEN_TYPE_ID, <<"badValueTokenType">>).
-define(ERR_BAD_VALUE_TOKEN_TYPE_TYPE, od_error_bad_value_token_type).

-define(ERR_BAD_VALUE_TOO_HIGH_ID, <<"badValueTooHigh">>).
-define(ERR_BAD_VALUE_TOO_HIGH_TYPE, od_error_bad_value_too_high).

-define(ERR_BAD_VALUE_TOO_LOW_ID, <<"badValueTooLow">>).
-define(ERR_BAD_VALUE_TOO_LOW_TYPE, od_error_bad_value_too_low).

-define(ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID, <<"badValueTimeSeriesCollectionConflictingMetricConfig">>).
-define(ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE, od_error_bad_value_tsc_conflicting_metric_config).

-define(ERR_BAD_VALUE_USERNAME_ID, <<"badValueUsername">>).
-define(ERR_BAD_VALUE_USERNAME_TYPE, od_error_bad_value_username).

-define(ERR_BAD_VALUE_XML_ID, <<"badValueXML">>).
-define(ERR_BAD_VALUE_XML_TYPE, od_error_bad_value_xml).


%%--------------------------------------------------------------------
%% general errors
%%--------------------------------------------------------------------
-define(ERR_BAD_MESSAGE_ID, <<"badMessage">>).
-define(ERR_BAD_MESSAGE_TYPE, od_error_bad_message).

-define(ERR_EXTERNAL_SERVICE_OPERATION_FAILED_ID, <<"externalServiceOperationFailed">>).
-define(ERR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE, od_error_external_service_operation_failed).

-define(ERR_FILE_ACCESS_ID, <<"fileAccess">>).
-define(ERR_FILE_ACCESS_TYPE, od_error_file_access).

-define(ERR_INTERNAL_SERVER_ERROR_ID, <<"internalServerError">>).
-define(ERR_INTERNAL_SERVER_ERROR_TYPE, od_error_internal_server_error).

-define(ERR_LIMIT_REACHED_ID, <<"limitReached">>).
-define(ERR_LIMIT_REACHED_TYPE, od_error_limit_reached).

-define(ERR_NOT_IMPLEMENTED_ID, <<"notImplemented">>).
-define(ERR_NOT_IMPLEMENTED_TYPE, od_error_not_implemented).

-define(ERR_SERVICE_UNAVAILABLE_ID, <<"serviceUnavailable">>).
-define(ERR_SERVICE_UNAVAILABLE_TYPE, od_error_service_unavailable).

-define(ERR_TEMPORARY_FAILURE_ID, <<"temporaryFailure">>).
-define(ERR_TEMPORARY_FAILURE_TYPE, od_error_temporary_failure).

-define(ERR_UNREGISTERED_ONEPROVIDER_ID, <<"unregisteredOneprovider">>).
-define(ERR_UNREGISTERED_ONEPROVIDER_TYPE, od_error_unregistered_oneprovider).


%%--------------------------------------------------------------------
%% graph_sync errors
%%--------------------------------------------------------------------
-define(ERR_BAD_GRI_ID, <<"badGRI">>).
-define(ERR_BAD_GRI_TYPE, od_error_bad_gri).

-define(ERR_BAD_VERSION_ID, <<"badVersion">>).
-define(ERR_BAD_VERSION_TYPE, od_error_bad_version).

-define(ERR_EXPECTED_HANDSHAKE_MESSAGE_ID, <<"expectedHandshakeMessage">>).
-define(ERR_EXPECTED_HANDSHAKE_MESSAGE_TYPE, od_error_expected_handshake_message).

-define(ERR_HANDSHAKE_ALREADY_DONE_ID, <<"handshakeAlreadyDone">>).
-define(ERR_HANDSHAKE_ALREADY_DONE_TYPE, od_error_handshake_already_done).

-define(ERR_NOT_SUBSCRIBABLE_ID, <<"notSubscribable">>).
-define(ERR_NOT_SUBSCRIBABLE_TYPE, od_error_not_subscribable).

-define(ERR_RPC_UNDEFINED_ID, <<"rpcUndefined">>).
-define(ERR_RPC_UNDEFINED_TYPE, od_error_rpc_undefined).


%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
-define(ERR_DNS_SERVERS_UNREACHABLE_ID, <<"dnsServersUnreachable">>).
-define(ERR_DNS_SERVERS_UNREACHABLE_TYPE, od_error_dns_servers_unreachable).

-define(ERR_LETS_ENCRYPT_NOT_REACHABLE_ID, <<"letsEncryptNotReachable">>).
-define(ERR_LETS_ENCRYPT_NOT_REACHABLE_TYPE, od_error_lets_encrypt_not_reachable).

-define(ERR_LETS_ENCRYPT_RESPONSE_ID, <<"letsEncryptResponse">>).
-define(ERR_LETS_ENCRYPT_RESPONSE_TYPE, od_error_lets_encrypt_response).

-define(ERR_NO_CONNECTION_TO_NEW_NODE_ID, <<"noConnectionToNewNode">>).
-define(ERR_NO_CONNECTION_TO_NEW_NODE_TYPE, od_error_no_connection_to_new_node).

-define(ERR_NO_SERVICE_NODES_ID, <<"noServiceNodes">>).
-define(ERR_NO_SERVICE_NODES_TYPE, od_error_no_service_nodes).

-define(ERR_NODE_ALREADY_IN_CLUSTER_ID, <<"nodeAlreadyInCluster">>).
-define(ERR_NODE_ALREADY_IN_CLUSTER_TYPE, od_error_node_already_in_cluster).

-define(ERR_NODE_NOT_COMPATIBLE_ID, <<"nodeNotCompatible">>).
-define(ERR_NODE_NOT_COMPATIBLE_TYPE, od_error_node_not_compatible).

-define(ERR_ON_NODES_ID, <<"errorOnNodes">>).
-define(ERR_ON_NODES_TYPE, od_error_on_nodes).

-define(ERR_USER_NOT_IN_CLUSTER_ID, <<"userNotInCluster">>).
-define(ERR_USER_NOT_IN_CLUSTER_TYPE, od_error_user_not_in_cluster).


%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
-define(ERR_AUTO_CLEANING_DISABLED_ID, <<"autoCleaningDisabled">>).
-define(ERR_AUTO_CLEANING_DISABLED_TYPE, od_error_auto_cleaning_disabled).

-define(ERR_FILE_POPULARITY_DISABLED_ID, <<"filePopularityDisabled">>).
-define(ERR_FILE_POPULARITY_DISABLED_TYPE, od_error_file_popularity_disabled).

-define(ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_ID, <<"forbiddenForCurrentArchiveState">>).
-define(ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE, od_error_forbidden_for_current_archive_state).

-define(ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_ID, <<"nestedArchiveDeletionForbidden">>).
-define(ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE, od_error_nested_archive_deletion_forbidden).

-define(ERR_QUOTA_EXCEEDED_ID, <<"quotaExceeded">>).
-define(ERR_QUOTA_EXCEEDED_TYPE, od_error_quota_exceeded).

-define(ERR_RECALL_TARGET_CONFLICT_ID, <<"recallTargetConflict">>).
-define(ERR_RECALL_TARGET_CONFLICT_TYPE, od_error_recall_target_conflict).

-define(ERR_SPACE_NOT_SUPPORTED_BY_ID, <<"spaceNotSupportedBy">>).
-define(ERR_SPACE_NOT_SUPPORTED_BY_TYPE, od_error_space_not_supported_by).

-define(ERR_STAT_OPERATION_NOT_SUPPORTED_ID, <<"statOperationNotSupported">>).
-define(ERR_STAT_OPERATION_NOT_SUPPORTED_TYPE, od_error_stat_operation_not_supported).

-define(ERR_USER_NOT_SUPPORTED_ID, <<"userNotSupported">>).
-define(ERR_USER_NOT_SUPPORTED_TYPE, od_error_user_not_supported).


%%--------------------------------------------------------------------
%% op_worker/atm errors
%%--------------------------------------------------------------------
-define(ERR_ATM_DATA_TYPE_UNVERIFIED_ID, <<"atmDataTypeUnverified">>).
-define(ERR_ATM_DATA_TYPE_UNVERIFIED_TYPE, od_error_atm_data_type_unverified).

-define(ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_ID, <<"atmDataValueConstraintUnverified">>).
-define(ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE, od_error_atm_data_value_constraint_unverified).

-define(ERR_ATM_INVALID_STATUS_TRANSITION_ID, <<"atmInvalidStatusTransition">>).
-define(ERR_ATM_INVALID_STATUS_TRANSITION_TYPE, od_error_atm_invalid_status_transition).

-define(ERR_ATM_JOB_BATCH_CRASHED_ID, <<"atmJobBatchCrashed">>).
-define(ERR_ATM_JOB_BATCH_CRASHED_TYPE, od_error_atm_job_batch_crashed).

-define(ERR_ATM_JOB_BATCH_WITHDRAWN_ID, <<"atmJobBatchWithdrawn">>).
-define(ERR_ATM_JOB_BATCH_WITHDRAWN_TYPE, od_error_atm_job_batch_withdrawn).

-define(ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_ID, <<"atmLambdaConfigBadValue">>).
-define(ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE, od_error_atm_lambda_config_bad_value).

-define(ERR_ATM_LANE_EMPTY_ID, <<"atmLaneEmpty">>).
-define(ERR_ATM_LANE_EMPTY_TYPE, od_error_atm_lane_empty).

-define(ERR_ATM_LANE_EXECUTION_CREATION_FAILED_ID, <<"atmLaneExecutionCreationFailed">>).
-define(ERR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE, od_error_atm_lane_execution_creation_failed).

-define(ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_ID, <<"atmLaneExecutionInitiationFailed">>).
-define(ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE, od_error_atm_lane_execution_initiation_failed).

-define(ERR_ATM_LANE_EXECUTION_RERUN_FAILED_ID, <<"atmLaneExecutionRerunFailed">>).
-define(ERR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE, od_error_atm_lane_execution_rerun_failed).

-define(ERR_ATM_LANE_EXECUTION_RETRY_FAILED_ID, <<"atmLaneExecutionRetryFailed">>).
-define(ERR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE, od_error_atm_lane_execution_retry_failed).

-define(ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_ID, <<"atmOpenfaasFunctionRegistrationFailed">>).
-define(ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE, od_error_atm_openfaas_function_registration_failed).

-define(ERR_ATM_OPENFAAS_NOT_CONFIGURED_ID, <<"atmOpenfaasNotConfigured">>).
-define(ERR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE, od_error_atm_openfaas_not_configured).

-define(ERR_ATM_OPENFAAS_QUERY_FAILED_ID, <<"atmOpenfaasQueryFailed">>).
-define(ERR_ATM_OPENFAAS_QUERY_FAILED_TYPE, od_error_atm_openfaas_query_failed).

-define(ERR_ATM_OPENFAAS_UNHEALTHY_ID, <<"atmOpenfaasUnhealthy">>).
-define(ERR_ATM_OPENFAAS_UNHEALTHY_TYPE, od_error_atm_openfaas_unhealthy).

-define(ERR_ATM_OPENFAAS_UNREACHABLE_ID, <<"atmOpenfaasUnreachable">>).
-define(ERR_ATM_OPENFAAS_UNREACHABLE_TYPE, od_error_atm_openfaas_unreachable).

-define(ERR_ATM_PARALLEL_BOX_EMPTY_ID, <<"atmParallelBoxEmpty">>).
-define(ERR_ATM_PARALLEL_BOX_EMPTY_TYPE, od_error_atm_parallel_box_empty).

-define(ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_ID, <<"atmParallelBoxExecutionCreationFailed">>).
-define(ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE, od_error_atm_parallel_box_execution_creation_failed).

-define(ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_ID, <<"atmParallelBoxExecutionInitiationFailed">>).
-define(ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE, od_error_atm_parallel_box_execution_initiation_failed).

-define(ERR_ATM_STORE_CONTENT_NOT_SET_ID, <<"atmStoreContentNotSet">>).
-define(ERR_ATM_STORE_CONTENT_NOT_SET_TYPE, od_error_atm_store_content_not_set).

-define(ERR_ATM_STORE_CREATION_FAILED_ID, <<"atmStoreCreationFailed">>).
-define(ERR_ATM_STORE_CREATION_FAILED_TYPE, od_error_atm_store_creation_failed).

-define(ERR_ATM_STORE_FROZEN_ID, <<"atmStoreFrozen">>).
-define(ERR_ATM_STORE_FROZEN_TYPE, od_error_atm_store_frozen).

-define(ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_ID, <<"atmStoreMissingRequiredInitialContent">>).
-define(ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE, od_error_atm_store_missing_required_initial_content).

-define(ERR_ATM_STORE_NOT_FOUND_ID, <<"atmStoreNotFound">>).
-define(ERR_ATM_STORE_NOT_FOUND_TYPE, od_error_atm_store_not_found).

-define(ERR_ATM_STORE_TYPE_DISALLOWED_ID, <<"atmStoreTypeDisallowed">>).
-define(ERR_ATM_STORE_TYPE_DISALLOWED_TYPE, od_error_atm_store_type_disallowed).

-define(ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_ID, <<"atmTaskArgMapperForNonexistentLambdaArg">>).
-define(ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE, od_error_atm_task_arg_mapper_for_nonexistent_lambda_arg).

-define(ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_ID, <<"atmTaskArgMapperForRequiredLambdaArgMissing">>).
-define(ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE, od_error_atm_task_arg_mapper_for_required_lambda_arg_missing).

-define(ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID, <<"atmTaskArgMapperIteratedItemQueryFailed">>).
-define(ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE, od_error_atm_task_arg_mapper_iterated_item_query_failed).

-define(ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID, <<"atmTaskArgMapperUnsupportedValueBuilder">>).
-define(ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE, od_error_atm_task_arg_mapper_unsupported_value_builder).

-define(ERR_ATM_TASK_ARG_MAPPING_FAILED_ID, <<"atmTaskArgMappingFailed">>).
-define(ERR_ATM_TASK_ARG_MAPPING_FAILED_TYPE, od_error_atm_task_arg_mapping_failed).

-define(ERR_ATM_TASK_EXECUTION_CREATION_FAILED_ID, <<"atmTaskExecutionCreationFailed">>).
-define(ERR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE, od_error_atm_task_execution_creation_failed).

-define(ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_ID, <<"atmTaskExecutionInitiationFailed">>).
-define(ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE, od_error_atm_task_execution_initiation_failed).

-define(ERR_ATM_TASK_EXECUTION_STOPPED_ID, <<"atmTaskExecutionEnded">>).
-define(ERR_ATM_TASK_EXECUTION_STOPPED_TYPE, od_error_atm_task_execution_stopped).

-define(ERR_ATM_TASK_RESULT_DISPATCH_FAILED_ID, <<"atmTaskResultDispatchFailed">>).
-define(ERR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE, od_error_atm_task_result_dispatch_failed).

-define(ERR_ATM_TASK_RESULT_MAPPING_FAILED_ID, <<"atmTaskResultMappingFailed">>).
-define(ERR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE, od_error_atm_task_result_mapping_failed).

-define(ERR_ATM_TASK_RESULT_MISSING_ID, <<"atmTaskResultMissing">>).
-define(ERR_ATM_TASK_RESULT_MISSING_TYPE, od_error_atm_task_result_missing).

-define(ERR_ATM_UNSUPPORTED_DATA_TYPE_ID, <<"atmUnsupportedDataType">>).
-define(ERR_ATM_UNSUPPORTED_DATA_TYPE_TYPE, od_error_atm_unsupported_data_type).

-define(ERR_ATM_WORKFLOW_EMPTY_ID, <<"atmWorkflowEmpty">>).
-define(ERR_ATM_WORKFLOW_EMPTY_TYPE, od_error_atm_workflow_empty).

-define(ERR_ATM_WORKFLOW_EXECUTION_ENDED_ID, <<"atmWorkflowExecutionEnded">>).
-define(ERR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE, od_error_atm_workflow_execution_ended).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_ID, <<"atmWorkflowExecutionNotEnded">>).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE, od_error_atm_workflow_execution_not_ended).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_ID, <<"atmWorkflowExecutionNotResumable">>).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE, od_error_atm_workflow_execution_not_resumable).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_ID, <<"atmWorkflowExecutionNotStopped">>).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE, od_error_atm_workflow_execution_not_stopped).

-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPED_ID, <<"atmWorkflowExecutionStopped">>).
-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE, od_error_atm_workflow_execution_stopped).

-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPING_ID, <<"atmWorkflowExecutionStopping">>).
-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE, od_error_atm_workflow_execution_stopping).


%%--------------------------------------------------------------------
%% op_worker/dir_stats errors
%%--------------------------------------------------------------------
-define(ERR_DIR_STATS_DISABLED_FOR_SPACE_ID, <<"dirStatsDisabledForSpace">>).
-define(ERR_DIR_STATS_DISABLED_FOR_SPACE_TYPE, od_error_dir_stats_disabled_for_space).

-define(ERR_DIR_STATS_NOT_READY_ID, <<"dirStatsNotReady">>).
-define(ERR_DIR_STATS_NOT_READY_TYPE, od_error_dir_stats_not_ready).


%%--------------------------------------------------------------------
%% op_worker/storage errors
%%--------------------------------------------------------------------
-define(ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID, <<"autoStorageImportNotSupported">>).
-define(ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, od_error_auto_storage_import_not_supported).

-define(ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_ID, <<"notALocalStorageSupportingSpace">>).
-define(ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE, od_error_not_a_local_storage_supporting_space).

-define(ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_ID, <<"requiresAutoStorageImportMode">>).
-define(ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE, od_error_requires_auto_storage_import_mode).

-define(ERR_REQUIRES_IMPORTED_STORAGE_ID, <<"requiresImportedStorage">>).
-define(ERR_REQUIRES_IMPORTED_STORAGE_TYPE, od_error_requires_imported_storage).

-define(ERR_REQUIRES_NON_IMPORTED_STORAGE_ID, <<"requiresNonImportedStorage">>).
-define(ERR_REQUIRES_NON_IMPORTED_STORAGE_TYPE, od_error_requires_non_imported_storage).

-define(ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_ID, <<"requiresPosixCompatibleStorage">>).
-define(ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE, od_error_requires_posix_compatible_storage).

-define(ERR_REQUIRES_READONLY_STORAGE_ID, <<"requiresReadonlyStorage">>).
-define(ERR_REQUIRES_READONLY_STORAGE_TYPE, od_error_requires_readonly_storage).

-define(ERR_STORAGE_IMPORT_NOT_SUPPORTED_ID, <<"storageImportNotSupported">>).
-define(ERR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, od_error_storage_import_not_supported).

-define(ERR_STORAGE_IN_USE_ID, <<"storageInUse">>).
-define(ERR_STORAGE_IN_USE_TYPE, od_error_storage_in_use).

-define(ERR_STORAGE_TEST_FAILED_ID, <<"storageTestFailed">>).
-define(ERR_STORAGE_TEST_FAILED_TYPE, od_error_storage_test_failed).


%%--------------------------------------------------------------------
%% op_worker/transfer errors
%%--------------------------------------------------------------------
-define(ERR_TRANSFER_ALREADY_ENDED_ID, <<"transferAlreadyEnded">>).
-define(ERR_TRANSFER_ALREADY_ENDED_TYPE, od_error_transfer_already_ended).

-define(ERR_TRANSFER_NOT_ENDED_ID, <<"transferNotEnded">>).
-define(ERR_TRANSFER_NOT_ENDED_TYPE, od_error_transfer_not_ended).


%%--------------------------------------------------------------------
%% op_worker/view errors
%%--------------------------------------------------------------------
-define(ERR_VIEW_NOT_EXISTS_ON_ID, <<"viewNotExistsOn">>).
-define(ERR_VIEW_NOT_EXISTS_ON_TYPE, od_error_view_not_exists_on).

-define(ERR_VIEW_QUERY_FAILED_ID, <<"viewQueryFailed">>).
-define(ERR_VIEW_QUERY_FAILED_TYPE, od_error_view_query_failed).


%%--------------------------------------------------------------------
%% oz_worker errors
%%--------------------------------------------------------------------
-define(ERR_ATM_LAMBDA_IN_USE_ID, <<"atmLambdaInUse">>).
-define(ERR_ATM_LAMBDA_IN_USE_TYPE, od_error_atm_lambda_in_use).

-define(ERR_BASIC_AUTH_DISABLED_ID, <<"basicAuthDisabled">>).
-define(ERR_BASIC_AUTH_DISABLED_TYPE, od_error_basic_auth_disabled).

-define(ERR_BASIC_AUTH_NOT_SUPPORTED_ID, <<"basicAuthNotSupported">>).
-define(ERR_BASIC_AUTH_NOT_SUPPORTED_TYPE, od_error_basic_auth_not_supported).

-define(ERR_CANNOT_ADD_RELATION_TO_SELF_ID, <<"cannotAddRelationToSelf">>).
-define(ERR_CANNOT_ADD_RELATION_TO_SELF_TYPE, od_error_cannot_add_relation_to_self).

-define(ERR_CANNOT_DELETE_ENTITY_ID, <<"cannotDeleteEntity">>).
-define(ERR_CANNOT_DELETE_ENTITY_TYPE, od_error_cannot_delete_entity).

-define(ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID, <<"cannotDeleteNonEmptyHandleService">>).
-define(ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE, od_error_cannot_delete_non_empty_handle_service).

-define(ERR_CANNOT_REMOVE_LAST_OWNER_ID, <<"cannotRemoveLastOwner">>).
-define(ERR_CANNOT_REMOVE_LAST_OWNER_TYPE, od_error_cannot_remove_last_owner).

-define(ERR_PROTECTED_GROUP_ID, <<"protectedGroup">>).
-define(ERR_PROTECTED_GROUP_TYPE, od_error_protected_group).

-define(ERR_RELATION_ALREADY_EXISTS_ID, <<"relationAlreadyExists">>).
-define(ERR_RELATION_ALREADY_EXISTS_TYPE, od_error_relation_already_exists).

-define(ERR_RELATION_DOES_NOT_EXIST_ID, <<"relationDoesNotExist">>).
-define(ERR_RELATION_DOES_NOT_EXIST_TYPE, od_error_relation_does_not_exist).


%%--------------------------------------------------------------------
%% oz_worker/space errors
%%--------------------------------------------------------------------
-define(ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID, <<"spaceAlreadySupportedWithImportedStorage">>).
-define(ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE, od_error_space_already_supported_with_imported_storage).

-define(ERR_SPACE_MARKETPLACE_DISABLED_ID, <<"spaceMarketplaceDisabled">>).
-define(ERR_SPACE_MARKETPLACE_DISABLED_TYPE, od_error_space_marketplace_disabled).


%%--------------------------------------------------------------------
%% oz_worker/subdomain errors
%%--------------------------------------------------------------------
-define(ERR_SUBDOMAIN_DELEGATION_DISABLED_ID, <<"subdomainDelegationDisabled">>).
-define(ERR_SUBDOMAIN_DELEGATION_DISABLED_TYPE, od_error_subdomain_delegation_disabled).

-define(ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_ID, <<"subdomainDelegationNotSupported">>).
-define(ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE, od_error_subdomain_delegation_not_supported).


%%--------------------------------------------------------------------
%% posix errors
%%--------------------------------------------------------------------
-define(ERR_POSIX_ID, <<"posix">>).
-define(ERR_POSIX_TYPE, od_error_posix).


%%--------------------------------------------------------------------
%% Error ID to type mapping
%%--------------------------------------------------------------------
-define(ERROR_ID_TO_TYPE_MAPPING, #{
    ?ERROR_ALREADY_EXISTS_ID => ?ERROR_ALREADY_EXISTS_TYPE,
    ?ERROR_NOT_FOUND_ID => ?ERROR_NOT_FOUND_TYPE,
    ?ERROR_NOT_SUPPORTED_ID => ?ERROR_NOT_SUPPORTED_TYPE,
    ?ERROR_TIMEOUT_ID => ?ERROR_TIMEOUT_TYPE,
    ?ERR_BAD_BASIC_CREDENTIALS_ID => ?ERR_BAD_BASIC_CREDENTIALS_TYPE,
    ?ERR_FORBIDDEN_ID => ?ERR_FORBIDDEN_TYPE,
    ?ERR_FORBIDDEN_WITH_HINT_ID => ?ERR_FORBIDDEN_WITH_HINT_TYPE,
    ?ERR_UNAUTHORIZED_ID => ?ERR_UNAUTHORIZED_TYPE,
    ?ERR_USER_BLOCKED_ID => ?ERR_USER_BLOCKED_TYPE,
    ?ERR_BAD_CONSUMER_TOKEN_ID => ?ERR_BAD_CONSUMER_TOKEN_TYPE,
    ?ERR_BAD_IDP_ACCESS_TOKEN_ID => ?ERR_BAD_IDP_ACCESS_TOKEN_TYPE,
    ?ERR_BAD_SERVICE_TOKEN_ID => ?ERR_BAD_SERVICE_TOKEN_TYPE,
    ?ERR_BAD_TOKEN_ID => ?ERR_BAD_TOKEN_TYPE,
    ?ERR_INVITE_TOKEN_CONSUMER_INVALID_ID => ?ERR_INVITE_TOKEN_CONSUMER_INVALID_TYPE,
    ?ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_ID => ?ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE,
    ?ERR_INVITE_TOKEN_TARGET_ID_INVALID_ID => ?ERR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE,
    ?ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_ID => ?ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE,
    ?ERR_NOT_AN_ACCESS_TOKEN_ID => ?ERR_NOT_AN_ACCESS_TOKEN_TYPE,
    ?ERR_NOT_AN_IDENTITY_TOKEN_ID => ?ERR_NOT_AN_IDENTITY_TOKEN_TYPE,
    ?ERR_NOT_AN_INVITE_TOKEN_ID => ?ERR_NOT_AN_INVITE_TOKEN_TYPE,
    ?ERR_TOKEN_CAVEAT_UNKNOWN_ID => ?ERR_TOKEN_CAVEAT_UNKNOWN_TYPE,
    ?ERR_TOKEN_CAVEAT_UNVERIFIED_ID => ?ERR_TOKEN_CAVEAT_UNVERIFIED_TYPE,
    ?ERR_TOKEN_INVALID_ID => ?ERR_TOKEN_INVALID_TYPE,
    ?ERR_TOKEN_REVOKED_ID => ?ERR_TOKEN_REVOKED_TYPE,
    ?ERR_TOKEN_SERVICE_FORBIDDEN_ID => ?ERR_TOKEN_SERVICE_FORBIDDEN_TYPE,
    ?ERR_TOKEN_SESSION_INVALID_ID => ?ERR_TOKEN_SESSION_INVALID_TYPE,
    ?ERR_TOKEN_SUBJECT_INVALID_ID => ?ERR_TOKEN_SUBJECT_INVALID_TYPE,
    ?ERR_TOKEN_TIME_CAVEAT_REQUIRED_ID => ?ERR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE,
    ?ERR_TOKEN_TOO_LARGE_ID => ?ERR_TOKEN_TOO_LARGE_TYPE,
    ?ERR_NO_CONNECTION_TO_CLUSTER_NODE_ID => ?ERR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE,
    ?ERR_NO_CONNECTION_TO_ONEZONE_ID => ?ERR_NO_CONNECTION_TO_ONEZONE_TYPE,
    ?ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_ID => ?ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE,
    ?ERR_BAD_DATA_ID => ?ERR_BAD_DATA_TYPE,
    ?ERR_BAD_GUI_PACKAGE_ID => ?ERR_BAD_GUI_PACKAGE_TYPE,
    ?ERR_GUI_PACKAGE_TOO_LARGE_ID => ?ERR_GUI_PACKAGE_TOO_LARGE_TYPE,
    ?ERR_GUI_PACKAGE_UNVERIFIED_ID => ?ERR_GUI_PACKAGE_UNVERIFIED_TYPE,
    ?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_ID => ?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE,
    ?ERR_INVALID_QOS_EXPRESSION_ID => ?ERR_INVALID_QOS_EXPRESSION_TYPE,
    ?ERR_MALFORMED_DATA_ID => ?ERR_MALFORMED_DATA_TYPE,
    ?ERR_MISSING_AT_LEAST_ONE_VALUE_ID => ?ERR_MISSING_AT_LEAST_ONE_VALUE_TYPE,
    ?ERR_MISSING_REQUIRED_VALUE_ID => ?ERR_MISSING_REQUIRED_VALUE_TYPE,
    ?ERR_TSC_MISSING_LAYOUT_ID => ?ERR_TSC_MISSING_LAYOUT_TYPE,
    ?ERR_TSC_TOO_MANY_METRICS_ID => ?ERR_TSC_TOO_MANY_METRICS_TYPE,
    ?ERR_BAD_VALUE_AMBIGUOUS_ID_ID => ?ERR_BAD_VALUE_AMBIGUOUS_ID_TYPE,
    ?ERR_BAD_VALUE_BOOLEAN_ID => ?ERR_BAD_VALUE_BOOLEAN_TYPE,
    ?ERR_BAD_VALUE_CAVEAT_ID => ?ERR_BAD_VALUE_CAVEAT_TYPE,
    ?ERR_BAD_VALUE_DOMAIN_ID => ?ERR_BAD_VALUE_DOMAIN_TYPE,
    ?ERR_BAD_VALUE_EMAIL_ID => ?ERR_BAD_VALUE_EMAIL_TYPE,
    ?ERR_BAD_VALUE_EMPTY_ID => ?ERR_BAD_VALUE_EMPTY_TYPE,
    ?ERR_BAD_VALUE_FILE_PATH_ID => ?ERR_BAD_VALUE_FILE_PATH_TYPE,
    ?ERR_BAD_VALUE_FLOAT_ID => ?ERR_BAD_VALUE_FLOAT_TYPE,
    ?ERR_BAD_VALUE_FULL_NAME_ID => ?ERR_BAD_VALUE_FULL_NAME_TYPE,
    ?ERR_BAD_VALUE_ID_NOT_FOUND_ID => ?ERR_BAD_VALUE_ID_NOT_FOUND_TYPE,
    ?ERR_BAD_VALUE_IDENTIFIER_ID => ?ERR_BAD_VALUE_IDENTIFIER_TYPE,
    ?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_ID => ?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE,
    ?ERR_BAD_VALUE_INTEGER_ID => ?ERR_BAD_VALUE_INTEGER_TYPE,
    ?ERR_BAD_VALUE_INVITE_TYPE_ID => ?ERR_BAD_VALUE_INVITE_TYPE_TYPE,
    ?ERR_BAD_VALUE_IPV4_ADDRESS_ID => ?ERR_BAD_VALUE_IPV4_ADDRESS_TYPE,
    ?ERR_BAD_VALUE_JSON_ID => ?ERR_BAD_VALUE_JSON_TYPE,
    ?ERR_BAD_VALUE_LIST_NOT_ALLOWED_ID => ?ERR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE,
    ?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_ID => ?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE,
    ?ERR_BAD_VALUE_LIST_OF_STRINGS_ID => ?ERR_BAD_VALUE_LIST_OF_STRINGS_TYPE,
    ?ERR_BAD_VALUE_NAME_ID => ?ERR_BAD_VALUE_NAME_TYPE,
    ?ERR_BAD_VALUE_NOT_ALLOWED_ID => ?ERR_BAD_VALUE_NOT_ALLOWED_TYPE,
    ?ERR_BAD_VALUE_NOT_IN_RANGE_ID => ?ERR_BAD_VALUE_NOT_IN_RANGE_TYPE,
    ?ERR_BAD_VALUE_OCTAL_ID => ?ERR_BAD_VALUE_OCTAL_TYPE,
    ?ERR_BAD_VALUE_PASSWORD_ID => ?ERR_BAD_VALUE_PASSWORD_TYPE,
    ?ERR_BAD_VALUE_QOS_PARAMETERS_ID => ?ERR_BAD_VALUE_QOS_PARAMETERS_TYPE,
    ?ERR_BAD_VALUE_STRING_ID => ?ERR_BAD_VALUE_STRING_TYPE,
    ?ERR_BAD_VALUE_SUBDOMAIN_ID => ?ERR_BAD_VALUE_SUBDOMAIN_TYPE,
    ?ERR_BAD_VALUE_TEXT_TOO_LARGE_ID => ?ERR_BAD_VALUE_TEXT_TOO_LARGE_TYPE,
    ?ERR_BAD_VALUE_TOKEN_ID => ?ERR_BAD_VALUE_TOKEN_TYPE,
    ?ERR_BAD_VALUE_TOKEN_TYPE_ID => ?ERR_BAD_VALUE_TOKEN_TYPE_TYPE,
    ?ERR_BAD_VALUE_TOO_HIGH_ID => ?ERR_BAD_VALUE_TOO_HIGH_TYPE,
    ?ERR_BAD_VALUE_TOO_LOW_ID => ?ERR_BAD_VALUE_TOO_LOW_TYPE,
    ?ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID => ?ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE,
    ?ERR_BAD_VALUE_USERNAME_ID => ?ERR_BAD_VALUE_USERNAME_TYPE,
    ?ERR_BAD_VALUE_XML_ID => ?ERR_BAD_VALUE_XML_TYPE,
    ?ERR_BAD_MESSAGE_ID => ?ERR_BAD_MESSAGE_TYPE,
    ?ERR_EXTERNAL_SERVICE_OPERATION_FAILED_ID => ?ERR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE,
    ?ERR_FILE_ACCESS_ID => ?ERR_FILE_ACCESS_TYPE,
    ?ERR_INTERNAL_SERVER_ERROR_ID => ?ERR_INTERNAL_SERVER_ERROR_TYPE,
    ?ERR_LIMIT_REACHED_ID => ?ERR_LIMIT_REACHED_TYPE,
    ?ERR_NOT_IMPLEMENTED_ID => ?ERR_NOT_IMPLEMENTED_TYPE,
    ?ERR_SERVICE_UNAVAILABLE_ID => ?ERR_SERVICE_UNAVAILABLE_TYPE,
    ?ERR_TEMPORARY_FAILURE_ID => ?ERR_TEMPORARY_FAILURE_TYPE,
    ?ERR_UNREGISTERED_ONEPROVIDER_ID => ?ERR_UNREGISTERED_ONEPROVIDER_TYPE,
    ?ERR_BAD_GRI_ID => ?ERR_BAD_GRI_TYPE,
    ?ERR_BAD_VERSION_ID => ?ERR_BAD_VERSION_TYPE,
    ?ERR_EXPECTED_HANDSHAKE_MESSAGE_ID => ?ERR_EXPECTED_HANDSHAKE_MESSAGE_TYPE,
    ?ERR_HANDSHAKE_ALREADY_DONE_ID => ?ERR_HANDSHAKE_ALREADY_DONE_TYPE,
    ?ERR_NOT_SUBSCRIBABLE_ID => ?ERR_NOT_SUBSCRIBABLE_TYPE,
    ?ERR_RPC_UNDEFINED_ID => ?ERR_RPC_UNDEFINED_TYPE,
    ?ERR_DNS_SERVERS_UNREACHABLE_ID => ?ERR_DNS_SERVERS_UNREACHABLE_TYPE,
    ?ERR_LETS_ENCRYPT_NOT_REACHABLE_ID => ?ERR_LETS_ENCRYPT_NOT_REACHABLE_TYPE,
    ?ERR_LETS_ENCRYPT_RESPONSE_ID => ?ERR_LETS_ENCRYPT_RESPONSE_TYPE,
    ?ERR_NO_CONNECTION_TO_NEW_NODE_ID => ?ERR_NO_CONNECTION_TO_NEW_NODE_TYPE,
    ?ERR_NO_SERVICE_NODES_ID => ?ERR_NO_SERVICE_NODES_TYPE,
    ?ERR_NODE_ALREADY_IN_CLUSTER_ID => ?ERR_NODE_ALREADY_IN_CLUSTER_TYPE,
    ?ERR_NODE_NOT_COMPATIBLE_ID => ?ERR_NODE_NOT_COMPATIBLE_TYPE,
    ?ERR_ON_NODES_ID => ?ERR_ON_NODES_TYPE,
    ?ERR_USER_NOT_IN_CLUSTER_ID => ?ERR_USER_NOT_IN_CLUSTER_TYPE,
    ?ERR_AUTO_CLEANING_DISABLED_ID => ?ERR_AUTO_CLEANING_DISABLED_TYPE,
    ?ERR_FILE_POPULARITY_DISABLED_ID => ?ERR_FILE_POPULARITY_DISABLED_TYPE,
    ?ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_ID => ?ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE,
    ?ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_ID => ?ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE,
    ?ERR_QUOTA_EXCEEDED_ID => ?ERR_QUOTA_EXCEEDED_TYPE,
    ?ERR_RECALL_TARGET_CONFLICT_ID => ?ERR_RECALL_TARGET_CONFLICT_TYPE,
    ?ERR_SPACE_NOT_SUPPORTED_BY_ID => ?ERR_SPACE_NOT_SUPPORTED_BY_TYPE,
    ?ERR_STAT_OPERATION_NOT_SUPPORTED_ID => ?ERR_STAT_OPERATION_NOT_SUPPORTED_TYPE,
    ?ERR_USER_NOT_SUPPORTED_ID => ?ERR_USER_NOT_SUPPORTED_TYPE,
    ?ERR_ATM_DATA_TYPE_UNVERIFIED_ID => ?ERR_ATM_DATA_TYPE_UNVERIFIED_TYPE,
    ?ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_ID => ?ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE,
    ?ERR_ATM_INVALID_STATUS_TRANSITION_ID => ?ERR_ATM_INVALID_STATUS_TRANSITION_TYPE,
    ?ERR_ATM_JOB_BATCH_CRASHED_ID => ?ERR_ATM_JOB_BATCH_CRASHED_TYPE,
    ?ERR_ATM_JOB_BATCH_WITHDRAWN_ID => ?ERR_ATM_JOB_BATCH_WITHDRAWN_TYPE,
    ?ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_ID => ?ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE,
    ?ERR_ATM_LANE_EMPTY_ID => ?ERR_ATM_LANE_EMPTY_TYPE,
    ?ERR_ATM_LANE_EXECUTION_CREATION_FAILED_ID => ?ERR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE,
    ?ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_ID => ?ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE,
    ?ERR_ATM_LANE_EXECUTION_RERUN_FAILED_ID => ?ERR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE,
    ?ERR_ATM_LANE_EXECUTION_RETRY_FAILED_ID => ?ERR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE,
    ?ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_ID => ?ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE,
    ?ERR_ATM_OPENFAAS_NOT_CONFIGURED_ID => ?ERR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE,
    ?ERR_ATM_OPENFAAS_QUERY_FAILED_ID => ?ERR_ATM_OPENFAAS_QUERY_FAILED_TYPE,
    ?ERR_ATM_OPENFAAS_UNHEALTHY_ID => ?ERR_ATM_OPENFAAS_UNHEALTHY_TYPE,
    ?ERR_ATM_OPENFAAS_UNREACHABLE_ID => ?ERR_ATM_OPENFAAS_UNREACHABLE_TYPE,
    ?ERR_ATM_PARALLEL_BOX_EMPTY_ID => ?ERR_ATM_PARALLEL_BOX_EMPTY_TYPE,
    ?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_ID => ?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE,
    ?ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_ID => ?ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE,
    ?ERR_ATM_STORE_CONTENT_NOT_SET_ID => ?ERR_ATM_STORE_CONTENT_NOT_SET_TYPE,
    ?ERR_ATM_STORE_CREATION_FAILED_ID => ?ERR_ATM_STORE_CREATION_FAILED_TYPE,
    ?ERR_ATM_STORE_FROZEN_ID => ?ERR_ATM_STORE_FROZEN_TYPE,
    ?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_ID => ?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE,
    ?ERR_ATM_STORE_NOT_FOUND_ID => ?ERR_ATM_STORE_NOT_FOUND_TYPE,
    ?ERR_ATM_STORE_TYPE_DISALLOWED_ID => ?ERR_ATM_STORE_TYPE_DISALLOWED_TYPE,
    ?ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_ID => ?ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE,
    ?ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_ID => ?ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE,
    ?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID => ?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE,
    ?ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID => ?ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE,
    ?ERR_ATM_TASK_ARG_MAPPING_FAILED_ID => ?ERR_ATM_TASK_ARG_MAPPING_FAILED_TYPE,
    ?ERR_ATM_TASK_EXECUTION_CREATION_FAILED_ID => ?ERR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE,
    ?ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_ID => ?ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE,
    ?ERR_ATM_TASK_EXECUTION_STOPPED_ID => ?ERR_ATM_TASK_EXECUTION_STOPPED_TYPE,
    ?ERR_ATM_TASK_RESULT_DISPATCH_FAILED_ID => ?ERR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE,
    ?ERR_ATM_TASK_RESULT_MAPPING_FAILED_ID => ?ERR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE,
    ?ERR_ATM_TASK_RESULT_MISSING_ID => ?ERR_ATM_TASK_RESULT_MISSING_TYPE,
    ?ERR_ATM_UNSUPPORTED_DATA_TYPE_ID => ?ERR_ATM_UNSUPPORTED_DATA_TYPE_TYPE,
    ?ERR_ATM_WORKFLOW_EMPTY_ID => ?ERR_ATM_WORKFLOW_EMPTY_TYPE,
    ?ERR_ATM_WORKFLOW_EXECUTION_ENDED_ID => ?ERR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE,
    ?ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_ID => ?ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE,
    ?ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_ID => ?ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE,
    ?ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_ID => ?ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE,
    ?ERR_ATM_WORKFLOW_EXECUTION_STOPPED_ID => ?ERR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE,
    ?ERR_ATM_WORKFLOW_EXECUTION_STOPPING_ID => ?ERR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE,
    ?ERR_DIR_STATS_DISABLED_FOR_SPACE_ID => ?ERR_DIR_STATS_DISABLED_FOR_SPACE_TYPE,
    ?ERR_DIR_STATS_NOT_READY_ID => ?ERR_DIR_STATS_NOT_READY_TYPE,
    ?ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID => ?ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE,
    ?ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_ID => ?ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE,
    ?ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_ID => ?ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE,
    ?ERR_REQUIRES_IMPORTED_STORAGE_ID => ?ERR_REQUIRES_IMPORTED_STORAGE_TYPE,
    ?ERR_REQUIRES_NON_IMPORTED_STORAGE_ID => ?ERR_REQUIRES_NON_IMPORTED_STORAGE_TYPE,
    ?ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_ID => ?ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE,
    ?ERR_REQUIRES_READONLY_STORAGE_ID => ?ERR_REQUIRES_READONLY_STORAGE_TYPE,
    ?ERR_STORAGE_IMPORT_NOT_SUPPORTED_ID => ?ERR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE,
    ?ERR_STORAGE_IN_USE_ID => ?ERR_STORAGE_IN_USE_TYPE,
    ?ERR_STORAGE_TEST_FAILED_ID => ?ERR_STORAGE_TEST_FAILED_TYPE,
    ?ERR_TRANSFER_ALREADY_ENDED_ID => ?ERR_TRANSFER_ALREADY_ENDED_TYPE,
    ?ERR_TRANSFER_NOT_ENDED_ID => ?ERR_TRANSFER_NOT_ENDED_TYPE,
    ?ERR_VIEW_NOT_EXISTS_ON_ID => ?ERR_VIEW_NOT_EXISTS_ON_TYPE,
    ?ERR_VIEW_QUERY_FAILED_ID => ?ERR_VIEW_QUERY_FAILED_TYPE,
    ?ERR_ATM_LAMBDA_IN_USE_ID => ?ERR_ATM_LAMBDA_IN_USE_TYPE,
    ?ERR_BASIC_AUTH_DISABLED_ID => ?ERR_BASIC_AUTH_DISABLED_TYPE,
    ?ERR_BASIC_AUTH_NOT_SUPPORTED_ID => ?ERR_BASIC_AUTH_NOT_SUPPORTED_TYPE,
    ?ERR_CANNOT_ADD_RELATION_TO_SELF_ID => ?ERR_CANNOT_ADD_RELATION_TO_SELF_TYPE,
    ?ERR_CANNOT_DELETE_ENTITY_ID => ?ERR_CANNOT_DELETE_ENTITY_TYPE,
    ?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID => ?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE,
    ?ERR_CANNOT_REMOVE_LAST_OWNER_ID => ?ERR_CANNOT_REMOVE_LAST_OWNER_TYPE,
    ?ERR_PROTECTED_GROUP_ID => ?ERR_PROTECTED_GROUP_TYPE,
    ?ERR_RELATION_ALREADY_EXISTS_ID => ?ERR_RELATION_ALREADY_EXISTS_TYPE,
    ?ERR_RELATION_DOES_NOT_EXIST_ID => ?ERR_RELATION_DOES_NOT_EXIST_TYPE,
    ?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID => ?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE,
    ?ERR_SPACE_MARKETPLACE_DISABLED_ID => ?ERR_SPACE_MARKETPLACE_DISABLED_TYPE,
    ?ERR_SUBDOMAIN_DELEGATION_DISABLED_ID => ?ERR_SUBDOMAIN_DELEGATION_DISABLED_TYPE,
    ?ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_ID => ?ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE,
    ?ERR_POSIX_ID => ?ERR_POSIX_TYPE
}).


-endif.

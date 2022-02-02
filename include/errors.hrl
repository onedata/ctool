%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Error definitions to be used across all APIs in Onedata products.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERRORS_HRL).
-define(ERRORS_HRL, 1).

-include("posix/errno.hrl").

%%--------------------------------------------------------------------
%% General errors
%%--------------------------------------------------------------------
-define(ERROR_BAD_MESSAGE(MessageBinOrJson), {error, {bad_message, MessageBinOrJson}}).
-define(ERROR_NO_CONNECTION_TO_ONEZONE, {error, no_connection_to_onezone}).
-define(ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER, {error, no_connection_to_peer_oneprovider}).
-define(ERROR_NO_CONNECTION_TO_CLUSTER_NODE, {error, no_connection_to_cluster_node}).
-define(ERROR_UNREGISTERED_ONEPROVIDER, {error, unregistered_oneprovider}).
-define(ERROR_INTERNAL_SERVER_ERROR, {error, internal_server_error}).
% feature not implemented yet, to be expected in the future
-define(ERROR_NOT_IMPLEMENTED, {error, not_implemented}).
% feature does not exist
-define(ERROR_NOT_SUPPORTED, {error, not_supported}).
-define(ERROR_SERVICE_UNAVAILABLE, {error, service_unavailable}).
-define(ERROR_TIMEOUT, {error, timeout}).
-define(ERROR_TEMPORARY_FAILURE, {error, temporary_failure}).
-define(ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName), {error, {external_service_operation_failed, ServiceName}}).
-define(ERROR_UNAUTHORIZED(AuthError), {error, {unauthorized, AuthError}}).
-define(ERROR_UNAUTHORIZED, {error, unauthorized}).
-define(ERROR_FORBIDDEN, {error, forbidden}).
-define(ERROR_NOT_FOUND, {error, not_found}).
-define(ERROR_ALREADY_EXISTS, {error, already_exists}).
-define(ERROR_FILE_ACCESS(Path, Errno), {error, {file_access, Path, Errno}}).


%%--------------------------------------------------------------------
%% POSIX errors
%%--------------------------------------------------------------------
-define(ERROR_POSIX(Errno), {error, {posix, Errno}}).


%%--------------------------------------------------------------------
%% Auth errors
%%--------------------------------------------------------------------
-define(ERROR_USER_BLOCKED, {error, user_blocked}).
-define(ERROR_BAD_BASIC_CREDENTIALS, {error, bad_basic_credentials}).
-define(ERROR_BAD_IDP_ACCESS_TOKEN(IdP), {error, {bad_idp_access_token, IdP}}).
% The presented token cannot be understood by the server
-define(ERROR_BAD_TOKEN, {error, bad_token}).
% The presented service token is not valid due to TokenError
-define(ERROR_BAD_SERVICE_TOKEN(TokenError), {error, {bad_service_token, TokenError}}).
% The presented consumer token is not valid due to TokenError
-define(ERROR_BAD_CONSUMER_TOKEN(TokenError), {error, {bad_consumer_token, TokenError}}).
% The token is comprehensible, but not valid
-define(ERROR_TOKEN_INVALID, {error, token_invalid}).
-define(ERROR_TOKEN_REVOKED, {error, token_revoked}).
-define(ERROR_TOKEN_TOO_LARGE(SizeLimit), {error, {token_too_large, {max, SizeLimit}}}).
-define(ERROR_NOT_AN_ACCESS_TOKEN(ReceivedTokenType), {error, {not_an_access_token, ReceivedTokenType}}).
-define(ERROR_NOT_AN_IDENTITY_TOKEN(ReceivedTokenType), {error, {not_an_identity_token, ReceivedTokenType}}).
-define(ERROR_NOT_AN_INVITE_TOKEN(ExpInviteType, ReceivedTokenType), {error, {not_an_invite_token, ExpInviteType, ReceivedTokenType}}).
-define(ERROR_TOKEN_CAVEAT_UNKNOWN(CaveatBinary), {error, {token_caveat_unknown, CaveatBinary}}).
-define(ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat), {error, {token_caveat_unverified, Caveat}}).
-define(ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl), {error, {token_time_caveat_required, MaxTtl}}).
% Token cannot be created for requested subject as it is invalid
-define(ERROR_TOKEN_SUBJECT_INVALID, {error, token_subject_invalid}).
% Requested service is forbidden to use the token on behalf of the subject
% (e.g. subject user is not supported by the provider specified in service)
-define(ERROR_TOKEN_SERVICE_FORBIDDEN(ServiceSpec), {error, {token_service_forbidden, ServiceSpec}}).
-define(ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, {error, invite_token_subject_not_authorized}).
-define(ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED, {error, invite_token_usage_limit_exceeded}).
-define(ERROR_INVITE_TOKEN_CONSUMER_INVALID(Consumer), {error, {invite_token_consumer_invalid, Consumer}}).
-define(ERROR_INVITE_TOKEN_TARGET_ID_INVALID(Id), {error, {invite_token_target_id_invalid, Id}}).
-define(ERROR_TOKEN_SESSION_INVALID, {error, token_session_invalid}).


%%--------------------------------------------------------------------
%% Graph Sync errors
%%--------------------------------------------------------------------
-define(ERROR_EXPECTED_HANDSHAKE_MESSAGE, {error, expected_handshake_message}).
-define(ERROR_HANDSHAKE_ALREADY_DONE, {error, handshake_already_done}).
-define(ERROR_BAD_VERSION(SupportedVersions), {error, {bad_version, {supported, SupportedVersions}}}).
-define(ERROR_BAD_GRI, {error, bad_gri}).
-define(ERROR_RPC_UNDEFINED, {error, rpc_undefined}).
-define(ERROR_NOT_SUBSCRIBABLE, {error, not_subscribable}).


%%--------------------------------------------------------------------
%% Data validation errors
%%--------------------------------------------------------------------
-define(ERROR_MALFORMED_DATA, {error, malformed_data}).
-define(ERROR_MISSING_REQUIRED_VALUE(Key), {error, {missing_required_value, Key}}).
-define(ERROR_MISSING_AT_LEAST_ONE_VALUE(Keys), {error, {missing_at_least_one_value, Keys}}).
-define(ERROR_BAD_DATA(Key), {error, {bad_data, Key}}).
-define(ERROR_BAD_DATA(Key, SpecificErrorOrHumanReadableHint), {error, {bad_data, Key, SpecificErrorOrHumanReadableHint}}).
-define(ERROR_BAD_VALUE_EMPTY(Key), {error, {empty_value, Key}}).
-define(ERROR_BAD_VALUE_BOOLEAN(Key), {error, {bad_value_boolean, Key}}).
-define(ERROR_BAD_VALUE_ATOM(Key), {error, {bad_value_atom, Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_ATOMS(Key), {error, {bad_value_list_of_atoms, Key}}).
-define(ERROR_BAD_VALUE_BINARY(Key), {error, {bad_value_binary, Key}}).
-define(ERROR_BAD_VALUE_BINARY_TOO_LARGE(Key, SizeLimit), {error, {bad_value_binary_too_large, Key, {max, SizeLimit}}}).
-define(ERROR_BAD_VALUE_LIST_OF_BINARIES(Key), {error, {bad_value_list_of_binaries, Key}}).
-define(ERROR_BAD_VALUE_INTEGER(Key), {error, {bad_value_integer, Key}}).
-define(ERROR_BAD_VALUE_FLOAT(Key), {error, {bad_value_float, Key}}).
-define(ERROR_BAD_VALUE_JSON(Key), {error, {bad_value_json, Key}}).
-define(ERROR_BAD_VALUE_TOKEN(Key, TokenError), {error, {bad_value_token, Key, TokenError}}).
-define(ERROR_BAD_VALUE_TOKEN_TYPE(Key), {error, {bad_value_token_type, Key}}).
-define(ERROR_BAD_VALUE_INVITE_TYPE(Key), {error, {bad_value_invite_type, Key}}).
-define(ERROR_BAD_VALUE_IPV4_ADDRESS(Key), {error, {bad_value_ipv4_address, Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key), {error, {bad_value_list_of_ipv4_addresses, Key}}).
-define(ERROR_BAD_VALUE_TOO_LOW(Key, Threshold), {error, {value_too_low, Key, {min, Threshold}}}).
-define(ERROR_BAD_VALUE_TOO_HIGH(Key, Threshold), {error, {value_too_high, Key, {max, Threshold}}}).
-define(ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High), {error, {value_not_in_range, Key, {range, Low, High}}}).
-define(ERROR_BAD_VALUE_NOT_ALLOWED(Key, AllowedVals), {error, {value_not_allowed, Key, {allowed, AllowedVals}}}).
-define(ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, AllowedVals), {error, {values_not_allowed, Key, {allowed, AllowedVals}}}).
-define(ERROR_BAD_VALUE_ID_NOT_FOUND(Key), {error, {id_not_found, Key}}).
-define(ERROR_BAD_VALUE_AMBIGUOUS_ID(Key), {error, {ambiguous_id, Key}}).
-define(ERROR_BAD_VALUE_IDENTIFIER(Key), {error, {bad_identifier, Key}}).
-define(ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key), {error, {identifier_occupied, Key}}).
-define(ERROR_BAD_VALUE_OCTAL(Key), {error, {bad_value_octal, Key}}).
-define(ERROR_BAD_VALUE_FILE_PATH, {error, bad_file_path}).
-define(ERROR_BAD_VALUE_FULL_NAME, {error, bad_full_name}).
-define(ERROR_BAD_VALUE_USERNAME, {error, bad_username}).
-define(ERROR_BAD_VALUE_PASSWORD, {error, bad_password}).
-define(ERROR_BAD_VALUE_EMAIL, {error, bad_value_email}).
-define(ERROR_BAD_VALUE_NAME, {error, bad_name}).
-define(ERROR_BAD_VALUE_DOMAIN, {error, bad_value_domain}).
-define(ERROR_BAD_VALUE_SUBDOMAIN, {error, bad_value_subdomain}).
-define(ERROR_BAD_VALUE_CAVEAT(Caveat), {error, {bad_value_caveat, Caveat}}).
-define(ERROR_BAD_VALUE_QOS_PARAMETERS, {error, bad_qos_parameters}).
-define(ERROR_BAD_GUI_PACKAGE, {error, bad_gui_package}).
-define(ERROR_GUI_PACKAGE_TOO_LARGE, {error, gui_package_too_large}).
-define(ERROR_GUI_PACKAGE_UNVERIFIED(ShaSum), {error, {gui_package_unverified, ShaSum}}).
-define(ERROR_INVALID_QOS_EXPRESSION(Reason), {error, {invalid_qos_expression, Reason}}).
-define(ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ProviderStage, StorageStage), {error, {illegal_support_stage_transition, ProviderStage, StorageStage}}).


%%--------------------------------------------------------------------
%% oz_worker errors
%%--------------------------------------------------------------------
% Basic auth is not (currently) supported by this Onezone
-define(ERROR_BASIC_AUTH_NOT_SUPPORTED, {error, basic_auth_not_supported}).
% Basic auth is disabled for given user
-define(ERROR_BASIC_AUTH_DISABLED, {error, basic_auth_disabled}).
% Subdomain delegation is (currently) not supported by this Onezone
-define(ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED, {error, subdomain_delegation_not_supported}).
% Subdomain delegation is disabled for given Oneprovider
-define(ERROR_SUBDOMAIN_DELEGATION_DISABLED, {error, subdomain_delegation_disabled}).
-define(ERROR_PROTECTED_GROUP, {error, protected_group}).
-define(ERROR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas), {error, {atm_lambda_in_use, AtmWorkflowSchemas}}).
-define(ERROR_CANNOT_REMOVE_LAST_OWNER(EntityType, EntityId), {error, {cannot_remove_last_owner, EntityType, EntityId}}).
-define(ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId), {error, {cannot_delete_entity, EntityType, EntityId}}).
-define(ERROR_CANNOT_ADD_RELATION_TO_SELF, {error, cannot_add_relation_to_self}).
-define(ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId),
    {error, {relation_does_not_exist, ChType, ChId, ParType, ParId}}
).
-define(ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId),
    {error, {relation_already_exists, ChType, ChId, ParType, ParId}}
).
-define(ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId), 
    {error, {space_already_supported_with_imported_storage, SpaceId, StorageId}}
).


%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
-define(ERROR_USER_NOT_SUPPORTED, {error, user_not_supported}).
-define(ERROR_AUTO_CLEANING_DISABLED, {error, auto_cleaning_disabled}).
-define(ERROR_FILE_POPULARITY_DISABLED, {error, file_popularity_disabled}).
-define(ERROR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId), {error, {space_not_supported_by, SpaceId, ProviderId}}).
-define(ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId),
    {error, {not_a_local_storage_supporting_space, ProviderId, StorageId, SpaceId}}).
-define(ERROR_STORAGE_IN_USE, {error, storage_in_use}).
-define(ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE, {error, requires_auto_storage_import_mode}).
-define(ERROR_STORAGE_TEST_FAILED(Operation), {error, {storage_test_failed, Operation}}).
-define(ERROR_REQUIRES_NON_IMPORTED_STORAGE(StorageId), {error, {requires_non_imported_storage, StorageId}}).
-define(ERROR_REQUIRES_IMPORTED_STORAGE(StorageId), {error, {requires_imported_storage, StorageId}}).
-define(ERROR_REQUIRES_READONLY_STORAGE(StorageIdOrType), {error, {requires_readonly_storage, StorageIdOrType}}).
-define(ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(StorageId, PosixCompatibleStorages), {error, {requires_posix_compatible_storage, StorageId, PosixCompatibleStorages}}).

-define(ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages),
    {error, {auto_storage_import_not_supported, StorageId, SupportedStorages, SupportedObjectStorages}}).
-define(ERROR_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, ObjectStorages), {error, {storage_import_not_supported, StorageId, ObjectStorages}}).
-define(ERROR_STAT_OPERATION_NOT_SUPPORTED(StorageId), {error, {stat_operation_not_supported, StorageId}}).
-define(ERROR_TRANSFER_ALREADY_ENDED, {error, transfer_already_ended}).
-define(ERROR_TRANSFER_NOT_ENDED, {error, transfer_not_ended}).
-define(ERROR_VIEW_NOT_EXISTS_ON(ProviderId), {error, {view_not_exists_on, ProviderId}}).
-define(ERROR_VIEW_QUERY_FAILED(Category, Description), {error, {view_query_failed, Category, Description}}).
-define(ERROR_QUOTA_EXCEEDED, {error, quota_exceeded}).

%% TODO VFS-8272 move all internal atm errors (those that will never be returned via REST/gs) to op
-define(ERROR_ATM_UNSUPPORTED_DATA_TYPE(UnsupportedType, SupportedTypes),
    {error, {atm_unsupported_data_type, UnsupportedType, SupportedTypes}}
).
-define(ERROR_ATM_DATA_TYPE_UNVERIFIED(Value, ExpType),
    {error, {atm_data_type_unverified, Value, ExpType}}
).
-define(ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(Value, Type, ValueConstraintsJson),
    {error, {atm_data_value_constraint_unverified, Value, Type, ValueConstraintsJson}}
).

-define(ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_VALUE,
    {error, atm_store_missing_required_initial_value}
).
-define(ERROR_ATM_STORE_CREATION_FAILED(AtmStoreSchemaId, SpecificError),
    {error, {atm_store_creation_failed, AtmStoreSchemaId, SpecificError}}
).
-define(ERROR_ATM_STORE_FROZEN(AtmStoreSchemaId),
    {error, {atm_store_frozen, AtmStoreSchemaId}}
).
-define(ERROR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, AllowedTypes),
    {error, {atm_store_type_disallowed, AtmStoreSchemaId, AllowedTypes}}
).
-define(ERROR_ATM_STORE_EMPTY(AtmStoreSchemaId),
    {error, {atm_store_empty, AtmStoreSchemaId}}
).
-define(ERROR_ATM_STORE_NOT_FOUND(AtmStoreSchemaId),
    {error, {atm_store_not_found, AtmStoreSchemaId}}
).

-define(ERROR_ATM_WORKFLOW_EMPTY, {error, atm_workflow_empty}).
-define(ERROR_ATM_WORKFLOW_EXECUTION_ABORTING, {error, atm_workflow_execution_aborting}).
-define(ERROR_ATM_WORKFLOW_EXECUTION_ENDED, {error, atm_workflow_execution_ended}).
-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED, {error, atm_workflow_execution_not_ended}).

-define(ERROR_ATM_LANE_EMPTY(AtmLaneSchemaId),
    {error, {atm_lane_empty, AtmLaneSchemaId}}
).
-define(ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(AtmLaneSchemaId, SpecificError),
    {error, {atm_lane_execution_creation_failed, AtmLaneSchemaId, SpecificError}}
).
-define(ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(AtmLaneSchemaId, SpecificError),
    {error, {atm_lane_execution_initiation_failed, AtmLaneSchemaId, SpecificError}}
).

-define(ERROR_ATM_LANE_EXECUTION_RETRY_FAILED, {error, atm_lane_execution_retry_failed}).
-define(ERROR_ATM_LANE_EXECUTION_RERUN_FAILED, {error, atm_lane_execution_rerun_failed}).

-define(ERROR_ATM_PARALLEL_BOX_EMPTY(AtmParallelBoxSchemaId),
    {error, {atm_parallel_box_empty, AtmParallelBoxSchemaId}}
).
-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(AtmParallelBoxSchemaId, SpecificError),
    {error, {atm_parallel_box_execution_creation_failed, AtmParallelBoxSchemaId, SpecificError}}
).
-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(AtmParallelBoxSchemaId, SpecificError),
    {error, {atm_parallel_box_execution_initiation_failed, AtmParallelBoxSchemaId, SpecificError}}
).

-define(ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(AtmTaskSchemaId, SpecificError),
    {error, {atm_task_execution_creation_failed, AtmTaskSchemaId, SpecificError}}
).
-define(ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(AtmTaskSchemaId, SpecificError),
    {error, {atm_task_execution_initiation_failed, AtmTaskSchemaId, SpecificError}}
).

-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(ArgName),
    {error, {atm_task_arg_mapper_for_required_lambda_arg_missing, ArgName}}
).
-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(ArgName),
    {error, {atm_task_arg_mapper_for_nonexistent_lambda_arg, ArgName}}
).
-define(ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, SupportedTypes),
    {error, {atm_task_arg_mapper_unsupported_value_builder, Type, SupportedTypes}}
).
-define(ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(IteratedItem, Query),
    {error, {atm_task_arg_mapper_iterated_item_query_failed, IteratedItem, Query}}
).
-define(ERROR_ATM_TASK_ARG_MAPPING_FAILED(ArgName, SpecificError),
    {error, {atm_task_arg_mapping_failed, ArgName, SpecificError}}
).

-define(ERROR_ATM_TASK_RESULT_MISSING(ResultName),
    {error, {atm_task_result_missing, ResultName}}
).
-define(ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(AtmStoreSchemaId, SpecificError),
    {error, {atm_task_result_dispatch_failed, AtmStoreSchemaId, SpecificError}}
).
-define(ERROR_ATM_TASK_RESULT_MAPPING_FAILED(ResultName, SpecificError),
    {error, {atm_task_result_mapping_failed, ResultName, SpecificError}}
).

-define(ERROR_ATM_TASK_EXECUTION_ENDED, {error, atm_task_execution_ended}).

-define(ERROR_ATM_OPENFAAS_NOT_CONFIGURED, {error, atm_openfaas_not_configured}).
-define(ERROR_ATM_OPENFAAS_UNREACHABLE, {error, atm_openfaas_unreachable}).
-define(ERROR_ATM_OPENFAAS_QUERY_FAILED, {error, atm_openfaas_query_failed}).
-define(ERROR_ATM_OPENFAAS_QUERY_FAILED(Reason),
    {error, {atm_openfaas_query_failed, Reason}}
).
-define(ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED,
    {error, atm_openfaas_function_registration_failed}
).

-define(ERROR_ATM_INVALID_STATUS_TRANSITION(PrevStatus, NewStatus),
    {error, {atm_invalid_status_transition, PrevStatus, NewStatus}}
).

-define(ERROR_STATS_FOR_SPACE_DISABLED, {error, stats_for_space_disabled}).


%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
% error wrapper to indicate nodes where error occured
-define(ERROR_ON_NODES(Error, Hostnames), {error, {error_on_nodes, Error, Hostnames}}).
-define(ERROR_DNS_SERVERS_UNREACHABLE(UsedServers), {error, {dns_servers_unreachable, UsedServers}}).
-define(ERROR_FILE_ALLOCATION(ActualSize, TargetSize), {error, {file_allocation, ActualSize, TargetSize}}).
-define(ERROR_LETS_ENCRYPT_NOT_REACHABLE, {error, lets_encrypt_not_reachable}).
-define(ERROR_LETS_ENCRYPT_RESPONSE(ProblemDocument, ErrorMessage), {error, {lets_encrypt_response, ProblemDocument, ErrorMessage}}).
-define(ERROR_NODE_ALREADY_IN_CLUSTER(HostnameBin), {error, {node_already_in_cluster, HostnameBin}}).
-define(ERROR_NODE_NOT_COMPATIBLE(HostnameBin, ClusterType), {error, {node_not_compatible, HostnameBin, ClusterType}}).
-define(ERROR_NO_CONNECTION_TO_NEW_NODE(HostnameBin), {error, {no_connection_to_new_node, HostnameBin}}).
-define(ERROR_NO_SERVICE_NODES(Service), {error, {no_service_nodes, Service}}).
-define(ERROR_USER_NOT_IN_CLUSTER, {error, user_not_in_cluster}).


%%--------------------------------------------------------------------
%% Unknown / unexpected error
%%--------------------------------------------------------------------
% Reported when the system logic returns an internal error that is not recognized
% among the definitions in this module. In such case, a random ErrorRef will be
% generated and included in an automatic debug log. This way, the caller can
% examine the logs and find what error exactly has appeared.
-define(ERROR_UNEXPECTED_ERROR(ErrorRef), {error, {unexpected_error, ErrorRef}}).

% Reported when decoding an external error and its format is not known to the
% current software version (for example, a newer server responds with an error
% to an older client, which does not know the error signature). The original
% JSON representing the error is retained and returned to the caller.
-define(ERROR_UNKNOWN_ERROR(ErrorAsJson), {error, {unknown_error, ErrorAsJson}}).

-endif.


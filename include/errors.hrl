%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
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

-include("global_definitions.hrl").
-include("posix/errno.hrl").


% TODO mv to util modue? od_error_utils?
-define(fmt(Format, Args), str_utils:ensure_suffix(str_utils:format_bin(Format, Args), <<".">>)).
-define(fmt_csv(Values), str_utils:join_as_binaries(Values, <<", ">>)).


-record(od_error, {
    type :: module(),
    args = undefined :: term()
}).

-define(ERROR, #od_error{}).
-define(ERROR(Type), #od_error{type = Type}).
-define(ERROR(Type, Args), #od_error{type = Type, args = Args}).


%%--------------------------------------------------------------------
%% Unknown / unexpected error
%%--------------------------------------------------------------------
% Used to carry errors that have the proper JSON error format, but do not match
% any error specified in this software version. This can happen if a newer
% server responds with an error to an older client, which does not know the
% error Id. The original JSON representing the error is retained and returned
% upon encoding.
-define(ERROR_UNRECOGNIZED_ERROR_TYPE, od_error_unrecognized_error).
-define(ERROR_UNRECOGNIZED_ERROR(ErrorAsJson), ?ERROR(?ERROR_UNRECOGNIZED_ERROR_TYPE, {ErrorAsJson})).


%%--------------------------------------------------------------------
%% auth errors
%%--------------------------------------------------------------------
-define(ERROR_BAD_BASIC_CREDENTIALS_ID, <<"badBasicCredentials">>).
-define(ERROR_BAD_BASIC_CREDENTIALS_TYPE, od_error_bad_basic_credentials).
-define(ERROR_BAD_BASIC_CREDENTIALS, ?ERROR(?ERROR_BAD_BASIC_CREDENTIALS_TYPE)).

-define(ERROR_FORBIDDEN_ID, <<"forbidden">>).
-define(ERROR_FORBIDDEN_TYPE, od_error_forbidden).
-define(ERROR_FORBIDDEN(Hint), ?ERROR(?ERROR_FORBIDDEN_TYPE, {Hint})).

-define(ERROR_UNAUTHORIZED_ID, <<"unauthorized">>).
-define(ERROR_UNAUTHORIZED_TYPE, od_error_unauthorized).
-define(ERROR_UNAUTHORIZED(AuthError), ?ERROR(?ERROR_UNAUTHORIZED_TYPE, {AuthError})).

-define(ERROR_USER_BLOCKED_ID, <<"userBlocked">>).
-define(ERROR_USER_BLOCKED_TYPE, od_error_user_blocked).
-define(ERROR_USER_BLOCKED, ?ERROR(?ERROR_USER_BLOCKED_TYPE)).


%%--------------------------------------------------------------------
%% auth/token errors
%%--------------------------------------------------------------------
-define(ERROR_BAD_CONSUMER_TOKEN_ID, <<"badConsumerToken">>).
-define(ERROR_BAD_CONSUMER_TOKEN_TYPE, od_error_bad_consumer_token).
-define(ERROR_BAD_CONSUMER_TOKEN(TokenError), ?ERROR(?ERROR_BAD_CONSUMER_TOKEN_TYPE, {TokenError})).

-define(ERROR_BAD_IDP_ACCESS_TOKEN_ID, <<"badIdpAccessToken">>).
-define(ERROR_BAD_IDP_ACCESS_TOKEN_TYPE, od_error_bad_idp_access_token).
-define(ERROR_BAD_IDP_ACCESS_TOKEN(Idp), ?ERROR(?ERROR_BAD_IDP_ACCESS_TOKEN_TYPE, {Idp})).

-define(ERROR_BAD_SERVICE_TOKEN_ID, <<"badServiceToken">>).
-define(ERROR_BAD_SERVICE_TOKEN_TYPE, od_error_bad_service_token).
-define(ERROR_BAD_SERVICE_TOKEN(TokenError), ?ERROR(?ERROR_BAD_SERVICE_TOKEN_TYPE, {TokenError})).

-define(ERROR_BAD_TOKEN_ID, <<"badToken">>).
-define(ERROR_BAD_TOKEN_TYPE, od_error_bad_token).
-define(ERROR_BAD_TOKEN, ?ERROR(?ERROR_BAD_TOKEN_TYPE)).

-define(ERROR_INVITE_TOKEN_CONSUMER_INVALID_ID, <<"inviteTokenConsumerInvalid">>).
-define(ERROR_INVITE_TOKEN_CONSUMER_INVALID_TYPE, od_error_invite_token_consumer_invalid).
-define(ERROR_INVITE_TOKEN_CONSUMER_INVALID(Consumer), ?ERROR(?ERROR_INVITE_TOKEN_CONSUMER_INVALID_TYPE, {Consumer})).

-define(ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_ID, <<"inviteTokenSubjectNotAuthorized">>).
-define(ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE, od_error_invite_token_subject_not_authorized).
-define(ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, ?ERROR(?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE)).

-define(ERROR_INVITE_TOKEN_TARGET_ID_INVALID_ID, <<"inviteTokenTargetIdInvalid">>).
-define(ERROR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE, od_error_invite_token_target_id_invalid).
-define(ERROR_INVITE_TOKEN_TARGET_ID_INVALID(Id), ?ERROR(?ERROR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE, {Id})).

-define(ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED_ID, <<"inviteTokenUsageLimitReached">>).
-define(ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE, od_error_invite_token_usage_limit_reached).
-define(ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED, ?ERROR(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE)).

-define(ERROR_NOT_AN_ACCESS_TOKEN_ID, <<"notAnAccessToken">>).
-define(ERROR_NOT_AN_ACCESS_TOKEN_TYPE, od_error_not_an_access_token).
-define(ERROR_NOT_AN_ACCESS_TOKEN(Received), ?ERROR(?ERROR_NOT_AN_ACCESS_TOKEN_TYPE, {Received})).

-define(ERROR_NOT_AN_IDENTITY_TOKEN_ID, <<"notAnIdentityToken">>).
-define(ERROR_NOT_AN_IDENTITY_TOKEN_TYPE, od_error_not_an_identity_token).
-define(ERROR_NOT_AN_IDENTITY_TOKEN(Received), ?ERROR(?ERROR_NOT_AN_IDENTITY_TOKEN_TYPE, {Received})).

-define(ERROR_NOT_AN_INVITE_TOKEN_ID, <<"notAnInviteToken">>).
-define(ERROR_NOT_AN_INVITE_TOKEN_TYPE, od_error_not_an_invite_token).
-define(ERROR_NOT_AN_INVITE_TOKEN(ExpectedInviteType, Received), ?ERROR(?ERROR_NOT_AN_INVITE_TOKEN_TYPE, {ExpectedInviteType, Received})).

-define(ERROR_TOKEN_CAVEAT_UNKNOWN_ID, <<"tokenCaveatUnknown">>).
-define(ERROR_TOKEN_CAVEAT_UNKNOWN_TYPE, od_error_token_caveat_unknown).
-define(ERROR_TOKEN_CAVEAT_UNKNOWN(Caveat), ?ERROR(?ERROR_TOKEN_CAVEAT_UNKNOWN_TYPE, {Caveat})).

-define(ERROR_TOKEN_CAVEAT_UNVERIFIED_ID, <<"tokenCaveatUnverified">>).
-define(ERROR_TOKEN_CAVEAT_UNVERIFIED_TYPE, od_error_token_caveat_unverified).
-define(ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat), ?ERROR(?ERROR_TOKEN_CAVEAT_UNVERIFIED_TYPE, {Caveat})).

-define(ERROR_TOKEN_INVALID_ID, <<"tokenInvalid">>).
-define(ERROR_TOKEN_INVALID_TYPE, od_error_token_invalid).
-define(ERROR_TOKEN_INVALID, ?ERROR(?ERROR_TOKEN_INVALID_TYPE)).

-define(ERROR_TOKEN_REVOKED_ID, <<"tokenRevoked">>).
-define(ERROR_TOKEN_REVOKED_TYPE, od_error_token_revoked).
-define(ERROR_TOKEN_REVOKED, ?ERROR(?ERROR_TOKEN_REVOKED_TYPE)).

-define(ERROR_TOKEN_SERVICE_FORBIDDEN_ID, <<"tokenServiceForbidden">>).
-define(ERROR_TOKEN_SERVICE_FORBIDDEN_TYPE, od_error_token_service_forbidden).
-define(ERROR_TOKEN_SERVICE_FORBIDDEN(Service), ?ERROR(?ERROR_TOKEN_SERVICE_FORBIDDEN_TYPE, {Service})).

-define(ERROR_TOKEN_SESSION_INVALID_ID, <<"tokenSessionInvalid">>).
-define(ERROR_TOKEN_SESSION_INVALID_TYPE, od_error_token_session_invalid).
-define(ERROR_TOKEN_SESSION_INVALID, ?ERROR(?ERROR_TOKEN_SESSION_INVALID_TYPE)).

-define(ERROR_TOKEN_SUBJECT_INVALID_ID, <<"tokenSubjectInvalid">>).
-define(ERROR_TOKEN_SUBJECT_INVALID_TYPE, od_error_token_subject_invalid).
-define(ERROR_TOKEN_SUBJECT_INVALID, ?ERROR(?ERROR_TOKEN_SUBJECT_INVALID_TYPE)).

-define(ERROR_TOKEN_TIME_CAVEAT_REQUIRED_ID, <<"tokenTimeCaveatRequired">>).
-define(ERROR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE, od_error_token_time_caveat_required).
-define(ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl), ?ERROR(?ERROR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE, {MaxTtl})).

-define(ERROR_TOKEN_TOO_LARGE_ID, <<"tokenTooLarge">>).
-define(ERROR_TOKEN_TOO_LARGE_TYPE, od_error_token_too_large).
-define(ERROR_TOKEN_TOO_LARGE(Limit), ?ERROR(?ERROR_TOKEN_TOO_LARGE_TYPE, {Limit})).


%%--------------------------------------------------------------------
%% connection errors
%%--------------------------------------------------------------------
-define(ERROR_NO_CONNECTION_TO_CLUSTER_NODE_ID, <<"noConnectionToClusterNode">>).
-define(ERROR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE, od_error_no_connection_to_cluster_node).
-define(ERROR_NO_CONNECTION_TO_CLUSTER_NODE, ?ERROR(?ERROR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE)).

-define(ERROR_NO_CONNECTION_TO_ONEZONE_ID, <<"noConnectionToOnezone">>).
-define(ERROR_NO_CONNECTION_TO_ONEZONE_TYPE, od_error_no_connection_to_onezone).
-define(ERROR_NO_CONNECTION_TO_ONEZONE, ?ERROR(?ERROR_NO_CONNECTION_TO_ONEZONE_TYPE)).

-define(ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER_ID, <<"noConnectionToPeerOneprovider">>).
-define(ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE, od_error_no_connection_to_peer_oneprovider).
-define(ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER, ?ERROR(?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE)).


%%--------------------------------------------------------------------
%% data_validation errors
%%--------------------------------------------------------------------
-define(ERROR_BAD_DATA_ID, <<"badData">>).
-define(ERROR_BAD_DATA_TYPE, od_error_bad_data).
-define(ERROR_BAD_DATA(Key, SpecificErrorOrHint), ?ERROR(?ERROR_BAD_DATA_TYPE, {Key, SpecificErrorOrHint})).

-define(ERROR_BAD_GUI_PACKAGE_ID, <<"badGuiPackage">>).
-define(ERROR_BAD_GUI_PACKAGE_TYPE, od_error_bad_gui_package).
-define(ERROR_BAD_GUI_PACKAGE, ?ERROR(?ERROR_BAD_GUI_PACKAGE_TYPE)).

-define(ERROR_GUI_PACKAGE_TOO_LARGE_ID, <<"guiPackageTooLarge">>).
-define(ERROR_GUI_PACKAGE_TOO_LARGE_TYPE, od_error_gui_package_too_large).
-define(ERROR_GUI_PACKAGE_TOO_LARGE, ?ERROR(?ERROR_GUI_PACKAGE_TOO_LARGE_TYPE)).

-define(ERROR_GUI_PACKAGE_UNVERIFIED_ID, <<"guiPackageUnverified">>).
-define(ERROR_GUI_PACKAGE_UNVERIFIED_TYPE, od_error_gui_package_unverified).
-define(ERROR_GUI_PACKAGE_UNVERIFIED(ShaSum), ?ERROR(?ERROR_GUI_PACKAGE_UNVERIFIED_TYPE, {ShaSum})).

-define(ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION_ID, <<"illegalSupportStageTransition">>).
-define(ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE, od_error_illegal_support_stage_transition).
-define(ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(CurrentProviderStage, CurrentStorageStage), ?ERROR(?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE, {CurrentProviderStage, CurrentStorageStage})).

-define(ERROR_INVALID_QOS_EXPRESSION_ID, <<"invalidQosExpression">>).
-define(ERROR_INVALID_QOS_EXPRESSION_TYPE, od_error_invalid_qos_expression).
-define(ERROR_INVALID_QOS_EXPRESSION(Reason), ?ERROR(?ERROR_INVALID_QOS_EXPRESSION_TYPE, {Reason})).

-define(ERROR_MALFORMED_DATA_ID, <<"malformedData">>).
-define(ERROR_MALFORMED_DATA_TYPE, od_error_malformed_data).
-define(ERROR_MALFORMED_DATA, ?ERROR(?ERROR_MALFORMED_DATA_TYPE)).

-define(ERROR_MISSING_AT_LEAST_ONE_VALUE_ID, <<"missingAtLeastOneValue">>).
-define(ERROR_MISSING_AT_LEAST_ONE_VALUE_TYPE, od_error_missing_at_least_one_value).
-define(ERROR_MISSING_AT_LEAST_ONE_VALUE(Keys), ?ERROR(?ERROR_MISSING_AT_LEAST_ONE_VALUE_TYPE, {Keys})).

-define(ERROR_MISSING_REQUIRED_VALUE_ID, <<"missingRequiredValue">>).
-define(ERROR_MISSING_REQUIRED_VALUE_TYPE, od_error_missing_required_value).
-define(ERROR_MISSING_REQUIRED_VALUE(Key), ?ERROR(?ERROR_MISSING_REQUIRED_VALUE_TYPE, {Key})).

-define(ERROR_TSC_MISSING_LAYOUT_ID, <<"timeSeriesCollectionMissingLayout">>).
-define(ERROR_TSC_MISSING_LAYOUT_TYPE, od_error_tsc_missing_layout).
-define(ERROR_TSC_MISSING_LAYOUT(MissingLayout), ?ERROR(?ERROR_TSC_MISSING_LAYOUT_TYPE, {MissingLayout})).

-define(ERROR_TSC_TOO_MANY_METRICS_ID, <<"timeSeriesCollectionTooManyMetrics">>).
-define(ERROR_TSC_TOO_MANY_METRICS_TYPE, od_error_tsc_too_many_metrics).
-define(ERROR_TSC_TOO_MANY_METRICS(Limit), ?ERROR(?ERROR_TSC_TOO_MANY_METRICS_TYPE, {Limit})).


%%--------------------------------------------------------------------
%% data_validation/value errors
%%--------------------------------------------------------------------
-define(ERROR_BAD_VALUE_AMBIGUOUS_ID_ID, <<"badValueAmbiguousId">>).
-define(ERROR_BAD_VALUE_AMBIGUOUS_ID_TYPE, od_error_bad_value_ambiguous_id).
-define(ERROR_BAD_VALUE_AMBIGUOUS_ID(Key), ?ERROR(?ERROR_BAD_VALUE_AMBIGUOUS_ID_TYPE, {Key})).

-define(ERROR_BAD_VALUE_ATOM_ID, <<"badValueString">>).
-define(ERROR_BAD_VALUE_ATOM_TYPE, od_error_bad_value_atom).
-define(ERROR_BAD_VALUE_ATOM(Key), ?ERROR(?ERROR_BAD_VALUE_ATOM_TYPE, {Key})).

-define(ERROR_BAD_VALUE_BINARY_ID, <<"badValueString">>).
-define(ERROR_BAD_VALUE_BINARY_TYPE, od_error_bad_value_binary).
-define(ERROR_BAD_VALUE_BINARY(Key), ?ERROR(?ERROR_BAD_VALUE_BINARY_TYPE, {Key})).

-define(ERROR_BAD_VALUE_BOOLEAN_ID, <<"badValueBoolean">>).
-define(ERROR_BAD_VALUE_BOOLEAN_TYPE, od_error_bad_value_boolean).
-define(ERROR_BAD_VALUE_BOOLEAN(Key), ?ERROR(?ERROR_BAD_VALUE_BOOLEAN_TYPE, {Key})).

-define(ERROR_BAD_VALUE_CAVEAT_ID, <<"badValueCaveat">>).
-define(ERROR_BAD_VALUE_CAVEAT_TYPE, od_error_bad_value_caveat).
-define(ERROR_BAD_VALUE_CAVEAT(Caveat), ?ERROR(?ERROR_BAD_VALUE_CAVEAT_TYPE, {Caveat})).

-define(ERROR_BAD_VALUE_DOMAIN_ID, <<"badValueDomain">>).
-define(ERROR_BAD_VALUE_DOMAIN_TYPE, od_error_bad_value_domain).
-define(ERROR_BAD_VALUE_DOMAIN, ?ERROR(?ERROR_BAD_VALUE_DOMAIN_TYPE)).

-define(ERROR_BAD_VALUE_EMAIL_ID, <<"badValueEmail">>).
-define(ERROR_BAD_VALUE_EMAIL_TYPE, od_error_bad_value_email).
-define(ERROR_BAD_VALUE_EMAIL, ?ERROR(?ERROR_BAD_VALUE_EMAIL_TYPE)).

-define(ERROR_BAD_VALUE_EMPTY_ID, <<"badValueEmpty">>).
-define(ERROR_BAD_VALUE_EMPTY_TYPE, od_error_bad_value_empty).
-define(ERROR_BAD_VALUE_EMPTY(Key), ?ERROR(?ERROR_BAD_VALUE_EMPTY_TYPE, {Key})).

-define(ERROR_BAD_VALUE_FILE_PATH_ID, <<"badValueFilePath">>).
-define(ERROR_BAD_VALUE_FILE_PATH_TYPE, od_error_bad_value_file_path).
-define(ERROR_BAD_VALUE_FILE_PATH, ?ERROR(?ERROR_BAD_VALUE_FILE_PATH_TYPE)).

-define(ERROR_BAD_VALUE_FLOAT_ID, <<"badValueFloat">>).
-define(ERROR_BAD_VALUE_FLOAT_TYPE, od_error_bad_value_float).
-define(ERROR_BAD_VALUE_FLOAT(Key), ?ERROR(?ERROR_BAD_VALUE_FLOAT_TYPE, {Key})).

-define(ERROR_BAD_VALUE_FULL_NAME_ID, <<"badValueFullName">>).
-define(ERROR_BAD_VALUE_FULL_NAME_TYPE, od_error_bad_value_full_name).
-define(ERROR_BAD_VALUE_FULL_NAME, ?ERROR(?ERROR_BAD_VALUE_FULL_NAME_TYPE)).

-define(ERROR_BAD_VALUE_ID_NOT_FOUND_ID, <<"badValueIdNotFound">>).
-define(ERROR_BAD_VALUE_ID_NOT_FOUND_TYPE, od_error_bad_value_id_not_found).
-define(ERROR_BAD_VALUE_ID_NOT_FOUND(Key), ?ERROR(?ERROR_BAD_VALUE_ID_NOT_FOUND_TYPE, {Key})).

-define(ERROR_BAD_VALUE_IDENTIFIER_ID, <<"badValueIdentifier">>).
-define(ERROR_BAD_VALUE_IDENTIFIER_TYPE, od_error_bad_value_identifier).
-define(ERROR_BAD_VALUE_IDENTIFIER(Key), ?ERROR(?ERROR_BAD_VALUE_IDENTIFIER_TYPE, {Key})).

-define(ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED_ID, <<"badValueIdentifierOccupied">>).
-define(ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE, od_error_bad_value_identifier_occupied).
-define(ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key), ?ERROR(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE, {Key})).

-define(ERROR_BAD_VALUE_INTEGER_ID, <<"badValueInteger">>).
-define(ERROR_BAD_VALUE_INTEGER_TYPE, od_error_bad_value_integer).
-define(ERROR_BAD_VALUE_INTEGER(Key), ?ERROR(?ERROR_BAD_VALUE_INTEGER_TYPE, {Key})).

-define(ERROR_BAD_VALUE_INVITE_TYPE_ID, <<"badValueInviteType">>).
-define(ERROR_BAD_VALUE_INVITE_TYPE_TYPE, od_error_bad_value_invite_type).
-define(ERROR_BAD_VALUE_INVITE_TYPE(Key), ?ERROR(?ERROR_BAD_VALUE_INVITE_TYPE_TYPE, {Key})).

-define(ERROR_BAD_VALUE_IPV4_ADDRESS_ID, <<"badValueIPv4Address">>).
-define(ERROR_BAD_VALUE_IPV4_ADDRESS_TYPE, od_error_bad_value_ipv4_address).
-define(ERROR_BAD_VALUE_IPV4_ADDRESS(Key), ?ERROR(?ERROR_BAD_VALUE_IPV4_ADDRESS_TYPE, {Key})).

-define(ERROR_BAD_VALUE_JSON_ID, <<"badValueJSON">>).
-define(ERROR_BAD_VALUE_JSON_TYPE, od_error_bad_value_json).
-define(ERROR_BAD_VALUE_JSON(Key), ?ERROR(?ERROR_BAD_VALUE_JSON_TYPE, {Key})).

-define(ERROR_BAD_VALUE_LIST_NOT_ALLOWED_ID, <<"badValueListNotAllowed">>).
-define(ERROR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE, od_error_bad_value_list_not_allowed).
-define(ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, Allowed), ?ERROR(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE, {Key, Allowed})).

-define(ERROR_BAD_VALUE_LIST_OF_ATOMS_ID, <<"badValueListOfStrings">>).
-define(ERROR_BAD_VALUE_LIST_OF_ATOMS_TYPE, od_error_bad_value_list_of_atoms).
-define(ERROR_BAD_VALUE_LIST_OF_ATOMS(Key), ?ERROR(?ERROR_BAD_VALUE_LIST_OF_ATOMS_TYPE, {Key})).

-define(ERROR_BAD_VALUE_LIST_OF_BINARIES_ID, <<"badValueListOfStrings">>).
-define(ERROR_BAD_VALUE_LIST_OF_BINARIES_TYPE, od_error_bad_value_list_of_binaries).
-define(ERROR_BAD_VALUE_LIST_OF_BINARIES(Key), ?ERROR(?ERROR_BAD_VALUE_LIST_OF_BINARIES_TYPE, {Key})).

-define(ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_ID, <<"badValueListOfIPv4Addresses">>).
-define(ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE, od_error_bad_value_list_of_ipv4_addresses).
-define(ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key), ?ERROR(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE, {Key})).

-define(ERROR_BAD_VALUE_NAME_ID, <<"badValueName">>).
-define(ERROR_BAD_VALUE_NAME_TYPE, od_error_bad_value_name).
-define(ERROR_BAD_VALUE_NAME(Key), ?ERROR(?ERROR_BAD_VALUE_NAME_TYPE, {Key})).

-define(ERROR_BAD_VALUE_NOT_ALLOWED_ID, <<"badValueNotAllowed">>).
-define(ERROR_BAD_VALUE_NOT_ALLOWED_TYPE, od_error_bad_value_not_allowed).
-define(ERROR_BAD_VALUE_NOT_ALLOWED(Key, Allowed), ?ERROR(?ERROR_BAD_VALUE_NOT_ALLOWED_TYPE, {Key, Allowed})).

-define(ERROR_BAD_VALUE_NOT_IN_RANGE_ID, <<"badValueNotInRange">>).
-define(ERROR_BAD_VALUE_NOT_IN_RANGE_TYPE, od_error_bad_value_not_in_range).
-define(ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High), ?ERROR(?ERROR_BAD_VALUE_NOT_IN_RANGE_TYPE, {Key, Low, High})).

-define(ERROR_BAD_VALUE_OCTAL_ID, <<"badValueOctal">>).
-define(ERROR_BAD_VALUE_OCTAL_TYPE, od_error_bad_value_octal).
-define(ERROR_BAD_VALUE_OCTAL(Key), ?ERROR(?ERROR_BAD_VALUE_OCTAL_TYPE, {Key})).

-define(ERROR_BAD_VALUE_PASSWORD_ID, <<"badValuePassword">>).
-define(ERROR_BAD_VALUE_PASSWORD_TYPE, od_error_bad_value_password).
-define(ERROR_BAD_VALUE_PASSWORD, ?ERROR(?ERROR_BAD_VALUE_PASSWORD_TYPE)).

-define(ERROR_BAD_VALUE_QOS_PARAMETERS_ID, <<"badValueQoSParameters">>).
-define(ERROR_BAD_VALUE_QOS_PARAMETERS_TYPE, od_error_bad_value_qos_parameters).
-define(ERROR_BAD_VALUE_QOS_PARAMETERS, ?ERROR(?ERROR_BAD_VALUE_QOS_PARAMETERS_TYPE)).

-define(ERROR_BAD_VALUE_SUBDOMAIN_ID, <<"badValueSubdomain">>).
-define(ERROR_BAD_VALUE_SUBDOMAIN_TYPE, od_error_bad_value_subdomain).
-define(ERROR_BAD_VALUE_SUBDOMAIN, ?ERROR(?ERROR_BAD_VALUE_SUBDOMAIN_TYPE)).

-define(ERROR_BAD_VALUE_TEXT_TOO_LARGE_ID, <<"badValueTextTooLarge">>).
-define(ERROR_BAD_VALUE_TEXT_TOO_LARGE_TYPE, od_error_bad_value_text_too_large).
-define(ERROR_BAD_VALUE_TEXT_TOO_LARGE(Key, Limit), ?ERROR(?ERROR_BAD_VALUE_TEXT_TOO_LARGE_TYPE, {Key, Limit})).

-define(ERROR_BAD_VALUE_TOKEN_ID, <<"badValueToken">>).
-define(ERROR_BAD_VALUE_TOKEN_TYPE, od_error_bad_value_token).
-define(ERROR_BAD_VALUE_TOKEN(Key, TokenError), ?ERROR(?ERROR_BAD_VALUE_TOKEN_TYPE, {Key, TokenError})).

-define(ERROR_BAD_VALUE_TOKEN_TYPE_ID, <<"badValueTokenType">>).
-define(ERROR_BAD_VALUE_TOKEN_TYPE_TYPE, od_error_bad_value_token_type).
-define(ERROR_BAD_VALUE_TOKEN_TYPE(Key), ?ERROR(?ERROR_BAD_VALUE_TOKEN_TYPE_TYPE, {Key})).

-define(ERROR_BAD_VALUE_TOO_HIGH_ID, <<"badValueTooHigh">>).
-define(ERROR_BAD_VALUE_TOO_HIGH_TYPE, od_error_bad_value_too_high).
-define(ERROR_BAD_VALUE_TOO_HIGH(Key, Limit), ?ERROR(?ERROR_BAD_VALUE_TOO_HIGH_TYPE, {Key, Limit})).

-define(ERROR_BAD_VALUE_TOO_LOW_ID, <<"badValueTooLow">>).
-define(ERROR_BAD_VALUE_TOO_LOW_TYPE, od_error_bad_value_too_low).
-define(ERROR_BAD_VALUE_TOO_LOW(Key, Limit), ?ERROR(?ERROR_BAD_VALUE_TOO_LOW_TYPE, {Key, Limit})).

-define(ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_ID, <<"badValueTimeSeriesCollectionConflictingMetricConfig">>).
-define(ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE, od_error_bad_value_tsc_conflicting_metric_config).
-define(ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig), ?ERROR(?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE, {TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig})).

-define(ERROR_BAD_VALUE_USERNAME_ID, <<"badValueUsername">>).
-define(ERROR_BAD_VALUE_USERNAME_TYPE, od_error_bad_value_username).
-define(ERROR_BAD_VALUE_USERNAME, ?ERROR(?ERROR_BAD_VALUE_USERNAME_TYPE)).

-define(ERROR_BAD_VALUE_XML_ID, <<"badValueXML">>).
-define(ERROR_BAD_VALUE_XML_TYPE, od_error_bad_value_xml).
-define(ERROR_BAD_VALUE_XML(Key), ?ERROR(?ERROR_BAD_VALUE_XML_TYPE, {Key})).


%%--------------------------------------------------------------------
%% general errors
%%--------------------------------------------------------------------
-define(ERROR_ALREADY_EXISTS_ID, <<"alreadyExists">>).
-define(ERROR_ALREADY_EXISTS_TYPE, od_error_already_exists).
-define(ERROR_ALREADY_EXISTS, ?ERROR(?ERROR_ALREADY_EXISTS_TYPE)).

-define(ERROR_BAD_MESSAGE_ID, <<"badMessage">>).
-define(ERROR_BAD_MESSAGE_TYPE, od_error_bad_message).
-define(ERROR_BAD_MESSAGE(Message), ?ERROR(?ERROR_BAD_MESSAGE_TYPE, {Message})).

-define(ERROR_EXTERNAL_SERVICE_OPERATION_FAILED_ID, <<"externalServiceOperationFailed">>).
-define(ERROR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE, od_error_external_service_operation_failed).
-define(ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName), ?ERROR(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE, {ServiceName})).

-define(ERROR_FILE_ACCESS_ID, <<"fileAccess">>).
-define(ERROR_FILE_ACCESS_TYPE, od_error_file_access).
-define(ERROR_FILE_ACCESS(Path, Errno), ?ERROR(?ERROR_FILE_ACCESS_TYPE, {Path, Errno})).

-define(ERROR_INTERNAL_SERVER_ERROR_ID, <<"internalServerError">>).
-define(ERROR_INTERNAL_SERVER_ERROR_TYPE, od_error_internal_server_error).
-define(ERROR_INTERNAL_SERVER_ERROR(Reference), ?ERROR(?ERROR_INTERNAL_SERVER_ERROR_TYPE, {Reference})).

-define(ERROR_LIMIT_REACHED_ID, <<"limitReached">>).
-define(ERROR_LIMIT_REACHED_TYPE, od_error_limit_reached).
-define(ERROR_LIMIT_REACHED(Limit, ResourceDescription), ?ERROR(?ERROR_LIMIT_REACHED_TYPE, {Limit, ResourceDescription})).

-define(ERROR_NOT_FOUND_ID, <<"notFound">>).
-define(ERROR_NOT_FOUND_TYPE, od_error_not_found).
-define(ERROR_NOT_FOUND, ?ERROR(?ERROR_NOT_FOUND_TYPE)).

-define(ERROR_NOT_IMPLEMENTED_ID, <<"notImplemented">>).
-define(ERROR_NOT_IMPLEMENTED_TYPE, od_error_not_implemented).
-define(ERROR_NOT_IMPLEMENTED, ?ERROR(?ERROR_NOT_IMPLEMENTED_TYPE)).

-define(ERROR_NOT_SUPPORTED_ID, <<"notSupported">>).
-define(ERROR_NOT_SUPPORTED_TYPE, od_error_not_supported).
-define(ERROR_NOT_SUPPORTED, ?ERROR(?ERROR_NOT_SUPPORTED_TYPE)).

-define(ERROR_SERVICE_UNAVAILABLE_ID, <<"serviceUnavailable">>).
-define(ERROR_SERVICE_UNAVAILABLE_TYPE, od_error_service_unavailable).
-define(ERROR_SERVICE_UNAVAILABLE, ?ERROR(?ERROR_SERVICE_UNAVAILABLE_TYPE)).

-define(ERROR_TEMPORARY_FAILURE_ID, <<"temporaryFailure">>).
-define(ERROR_TEMPORARY_FAILURE_TYPE, od_error_temporary_failure).
-define(ERROR_TEMPORARY_FAILURE, ?ERROR(?ERROR_TEMPORARY_FAILURE_TYPE)).

-define(ERROR_TIMEOUT_ID, <<"timeout">>).
-define(ERROR_TIMEOUT_TYPE, od_error_timeout).
-define(ERROR_TIMEOUT, ?ERROR(?ERROR_TIMEOUT_TYPE)).

-define(ERROR_UNREGISTERED_ONEPROVIDER_ID, <<"unregisteredOneprovider">>).
-define(ERROR_UNREGISTERED_ONEPROVIDER_TYPE, od_error_unregistered_oneprovider).
-define(ERROR_UNREGISTERED_ONEPROVIDER, ?ERROR(?ERROR_UNREGISTERED_ONEPROVIDER_TYPE)).


%%--------------------------------------------------------------------
%% graph_sync errors
%%--------------------------------------------------------------------
-define(ERROR_BAD_GRI_ID, <<"badGRI">>).
-define(ERROR_BAD_GRI_TYPE, od_error_bad_gri).
-define(ERROR_BAD_GRI, ?ERROR(?ERROR_BAD_GRI_TYPE)).

-define(ERROR_BAD_VERSION_ID, <<"badVersion">>).
-define(ERROR_BAD_VERSION_TYPE, od_error_bad_version).
-define(ERROR_BAD_VERSION(SupportedVersions), ?ERROR(?ERROR_BAD_VERSION_TYPE, {SupportedVersions})).

-define(ERROR_EXPECTED_HANDSHAKE_MESSAGE_ID, <<"expectedHandshakeMessage">>).
-define(ERROR_EXPECTED_HANDSHAKE_MESSAGE_TYPE, od_error_expected_handshake_message).
-define(ERROR_EXPECTED_HANDSHAKE_MESSAGE, ?ERROR(?ERROR_EXPECTED_HANDSHAKE_MESSAGE_TYPE)).

-define(ERROR_HANDSHAKE_ALREADY_DONE_ID, <<"handshakeAlreadyDone">>).
-define(ERROR_HANDSHAKE_ALREADY_DONE_TYPE, od_error_handshake_already_done).
-define(ERROR_HANDSHAKE_ALREADY_DONE, ?ERROR(?ERROR_HANDSHAKE_ALREADY_DONE_TYPE)).

-define(ERROR_NOT_SUBSCRIBABLE_ID, <<"notSubscribable">>).
-define(ERROR_NOT_SUBSCRIBABLE_TYPE, od_error_not_subscribable).
-define(ERROR_NOT_SUBSCRIBABLE, ?ERROR(?ERROR_NOT_SUBSCRIBABLE_TYPE)).

-define(ERROR_RPC_UNDEFINED_ID, <<"rpcUndefined">>).
-define(ERROR_RPC_UNDEFINED_TYPE, od_error_rpc_undefined).
-define(ERROR_RPC_UNDEFINED, ?ERROR(?ERROR_RPC_UNDEFINED_TYPE)).


%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
-define(ERROR_DNS_SERVERS_UNREACHABLE_ID, <<"dnsServersUnreachable">>).
-define(ERROR_DNS_SERVERS_UNREACHABLE_TYPE, od_error_dns_servers_unreachable).
-define(ERROR_DNS_SERVERS_UNREACHABLE(Servers), ?ERROR(?ERROR_DNS_SERVERS_UNREACHABLE_TYPE, {Servers})).

-define(ERROR_FILE_ALLOCATION_ID, <<"fileAllocation">>).
-define(ERROR_FILE_ALLOCATION_TYPE, od_error_file_allocation).
-define(ERROR_FILE_ALLOCATION(ActualSize, TargetSize), ?ERROR(?ERROR_FILE_ALLOCATION_TYPE, {ActualSize, TargetSize})).

-define(ERROR_LETS_ENCRYPT_NOT_REACHABLE_ID, <<"letsEncryptNotReachable">>).
-define(ERROR_LETS_ENCRYPT_NOT_REACHABLE_TYPE, od_error_lets_encrypt_not_reachable).
-define(ERROR_LETS_ENCRYPT_NOT_REACHABLE, ?ERROR(?ERROR_LETS_ENCRYPT_NOT_REACHABLE_TYPE)).

-define(ERROR_LETS_ENCRYPT_RESPONSE_ID, <<"letsEncryptResponse">>).
-define(ERROR_LETS_ENCRYPT_RESPONSE_TYPE, od_error_lets_encrypt_response).
-define(ERROR_LETS_ENCRYPT_RESPONSE(ProblemDocument, ErrorMessage), ?ERROR(?ERROR_LETS_ENCRYPT_RESPONSE_TYPE, {ProblemDocument, ErrorMessage})).

-define(ERROR_NO_CONNECTION_TO_NEW_NODE_ID, <<"noConnectionToNewNode">>).
-define(ERROR_NO_CONNECTION_TO_NEW_NODE_TYPE, od_error_no_connection_to_new_node).
-define(ERROR_NO_CONNECTION_TO_NEW_NODE(Hostname), ?ERROR(?ERROR_NO_CONNECTION_TO_NEW_NODE_TYPE, {Hostname})).

-define(ERROR_NO_SERVICE_NODES_ID, <<"noServiceNodes">>).
-define(ERROR_NO_SERVICE_NODES_TYPE, od_error_no_service_nodes).
-define(ERROR_NO_SERVICE_NODES(Service), ?ERROR(?ERROR_NO_SERVICE_NODES_TYPE, {Service})).

-define(ERROR_NODE_ALREADY_IN_CLUSTER_ID, <<"nodeAlreadyInCluster">>).
-define(ERROR_NODE_ALREADY_IN_CLUSTER_TYPE, od_error_node_already_in_cluster).
-define(ERROR_NODE_ALREADY_IN_CLUSTER(Hostname), ?ERROR(?ERROR_NODE_ALREADY_IN_CLUSTER_TYPE, {Hostname})).

-define(ERROR_NODE_NOT_COMPATIBLE_ID, <<"nodeNotCompatible">>).
-define(ERROR_NODE_NOT_COMPATIBLE_TYPE, od_error_node_not_compatible).
-define(ERROR_NODE_NOT_COMPATIBLE(Hostname, ClusterType), ?ERROR(?ERROR_NODE_NOT_COMPATIBLE_TYPE, {Hostname, ClusterType})).

-define(ERROR_ON_NODES_ID, <<"errorOnNodes">>).
-define(ERROR_ON_NODES_TYPE, od_error_on_nodes).
-define(ERROR_ON_NODES(Error, Hostnames), ?ERROR(?ERROR_ON_NODES_TYPE, {Error, Hostnames})).

-define(ERROR_USER_NOT_IN_CLUSTER_ID, <<"userNotInCluster">>).
-define(ERROR_USER_NOT_IN_CLUSTER_TYPE, od_error_user_not_in_cluster).
-define(ERROR_USER_NOT_IN_CLUSTER, ?ERROR(?ERROR_USER_NOT_IN_CLUSTER_TYPE)).


%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
-define(ERROR_AUTO_CLEANING_DISABLED_ID, <<"autoCleaningDisabled">>).
-define(ERROR_AUTO_CLEANING_DISABLED_TYPE, od_error_auto_cleaning_disabled).
-define(ERROR_AUTO_CLEANING_DISABLED, ?ERROR(?ERROR_AUTO_CLEANING_DISABLED_TYPE)).

-define(ERROR_FILE_POPULARITY_DISABLED_ID, <<"filePopularityDisabled">>).
-define(ERROR_FILE_POPULARITY_DISABLED_TYPE, od_error_file_popularity_disabled).
-define(ERROR_FILE_POPULARITY_DISABLED, ?ERROR(?ERROR_FILE_POPULARITY_DISABLED_TYPE)).

-define(ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_ID, <<"forbiddenForCurrentArchiveState">>).
-define(ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE, od_error_forbidden_for_current_archive_state).
-define(ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(CurrentState, AllowedStates), ?ERROR(?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE, {CurrentState, AllowedStates})).

-define(ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN_ID, <<"nestedArchiveDeletionForbidden">>).
-define(ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE, od_error_nested_archive_deletion_forbidden).
-define(ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ParentArchiveId), ?ERROR(?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE, {ParentArchiveId})).

-define(ERROR_QUOTA_EXCEEDED_ID, <<"quotaExceeded">>).
-define(ERROR_QUOTA_EXCEEDED_TYPE, od_error_quota_exceeded).
-define(ERROR_QUOTA_EXCEEDED, ?ERROR(?ERROR_QUOTA_EXCEEDED_TYPE)).

-define(ERROR_RECALL_TARGET_CONFLICT_ID, <<"recallTargetConflict">>).
-define(ERROR_RECALL_TARGET_CONFLICT_TYPE, od_error_recall_target_conflict).
-define(ERROR_RECALL_TARGET_CONFLICT, ?ERROR(?ERROR_RECALL_TARGET_CONFLICT_TYPE)).

-define(ERROR_SPACE_NOT_SUPPORTED_BY_ID, <<"spaceNotSupportedBy">>).
-define(ERROR_SPACE_NOT_SUPPORTED_BY_TYPE, od_error_space_not_supported_by).
-define(ERROR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId), ?ERROR(?ERROR_SPACE_NOT_SUPPORTED_BY_TYPE, {SpaceId, ProviderId})).

-define(ERROR_STAT_OPERATION_NOT_SUPPORTED_ID, <<"statOperationNotSupported">>).
-define(ERROR_STAT_OPERATION_NOT_SUPPORTED_TYPE, od_error_stat_operation_not_supported).
-define(ERROR_STAT_OPERATION_NOT_SUPPORTED(StorageId), ?ERROR(?ERROR_STAT_OPERATION_NOT_SUPPORTED_TYPE, {StorageId})).

-define(ERROR_USER_NOT_SUPPORTED_ID, <<"userNotSupported">>).
-define(ERROR_USER_NOT_SUPPORTED_TYPE, od_error_user_not_supported).
-define(ERROR_USER_NOT_SUPPORTED, ?ERROR(?ERROR_USER_NOT_SUPPORTED_TYPE)).


%%--------------------------------------------------------------------
%% op_worker/atm errors
%%--------------------------------------------------------------------
-define(ERROR_ATM_DATA_TYPE_UNVERIFIED_ID, <<"atmDataTypeUnverified">>).
-define(ERROR_ATM_DATA_TYPE_UNVERIFIED_TYPE, od_error_atm_data_type_unverified).
-define(ERROR_ATM_DATA_TYPE_UNVERIFIED(Value, ExpType), ?ERROR(?ERROR_ATM_DATA_TYPE_UNVERIFIED_TYPE, {Value, ExpType})).

-define(ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_ID, <<"atmDataValueConstraintUnverified">>).
-define(ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE, od_error_atm_data_value_constraint_unverified).
-define(ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(Value, Type, ValueConstraints), ?ERROR(?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE, {Value, Type, ValueConstraints})).

-define(ERROR_ATM_INVALID_STATUS_TRANSITION_ID, <<"atmInvalidStatusTransition">>).
-define(ERROR_ATM_INVALID_STATUS_TRANSITION_TYPE, od_error_atm_invalid_status_transition).
-define(ERROR_ATM_INVALID_STATUS_TRANSITION(PrevStatus, NewStatus), ?ERROR(?ERROR_ATM_INVALID_STATUS_TRANSITION_TYPE, {PrevStatus, NewStatus})).

-define(ERROR_ATM_JOB_BATCH_CRASHED_ID, <<"atmJobBatchCrashed">>).
-define(ERROR_ATM_JOB_BATCH_CRASHED_TYPE, od_error_atm_job_batch_crashed).
-define(ERROR_ATM_JOB_BATCH_CRASHED(Reason), ?ERROR(?ERROR_ATM_JOB_BATCH_CRASHED_TYPE, {Reason})).

-define(ERROR_ATM_JOB_BATCH_WITHDRAWN_ID, <<"atmJobBatchWithdrawn">>).
-define(ERROR_ATM_JOB_BATCH_WITHDRAWN_TYPE, od_error_atm_job_batch_withdrawn).
-define(ERROR_ATM_JOB_BATCH_WITHDRAWN(Reason), ?ERROR(?ERROR_ATM_JOB_BATCH_WITHDRAWN_TYPE, {Reason})).

-define(ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE_ID, <<"atmLambdaConfigBadValue">>).
-define(ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE, od_error_atm_lambda_config_bad_value).
-define(ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(ParameterName, SpecificError), ?ERROR(?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE, {ParameterName, SpecificError})).

-define(ERROR_ATM_LANE_EMPTY_ID, <<"atmLaneEmpty">>).
-define(ERROR_ATM_LANE_EMPTY_TYPE, od_error_atm_lane_empty).
-define(ERROR_ATM_LANE_EMPTY(AtmLaneSchemaId), ?ERROR(?ERROR_ATM_LANE_EMPTY_TYPE, {AtmLaneSchemaId})).

-define(ERROR_ATM_LANE_EXECUTION_CREATION_FAILED_ID, <<"atmLaneExecutionCreationFailed">>).
-define(ERROR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE, od_error_atm_lane_execution_creation_failed).
-define(ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(AtmLaneSchemaId, SpecificError), ?ERROR(?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError})).

-define(ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED_ID, <<"atmLaneExecutionInitiationFailed">>).
-define(ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE, od_error_atm_lane_execution_initiation_failed).
-define(ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(AtmLaneSchemaId, SpecificError), ?ERROR(?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError})).

-define(ERROR_ATM_LANE_EXECUTION_RERUN_FAILED_ID, <<"atmLaneExecutionRerunFailed">>).
-define(ERROR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE, od_error_atm_lane_execution_rerun_failed).
-define(ERROR_ATM_LANE_EXECUTION_RERUN_FAILED, ?ERROR(?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE)).

-define(ERROR_ATM_LANE_EXECUTION_RETRY_FAILED_ID, <<"atmLaneExecutionRetryFailed">>).
-define(ERROR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE, od_error_atm_lane_execution_retry_failed).
-define(ERROR_ATM_LANE_EXECUTION_RETRY_FAILED, ?ERROR(?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE)).

-define(ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_ID, <<"atmOpenfaasFunctionRegistrationFailed">>).
-define(ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE, od_error_atm_openfaas_function_registration_failed).
-define(ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED, ?ERROR(?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE)).

-define(ERROR_ATM_OPENFAAS_NOT_CONFIGURED_ID, <<"atmOpenfaasNotConfigured">>).
-define(ERROR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE, od_error_atm_openfaas_not_configured).
-define(ERROR_ATM_OPENFAAS_NOT_CONFIGURED, ?ERROR(?ERROR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE)).

-define(ERROR_ATM_OPENFAAS_QUERY_FAILED_ID, <<"atmOpenfaasQueryFailed">>).
-define(ERROR_ATM_OPENFAAS_QUERY_FAILED_TYPE, od_error_atm_openfaas_query_failed).
-define(ERROR_ATM_OPENFAAS_QUERY_FAILED(Reason), ?ERROR(?ERROR_ATM_OPENFAAS_QUERY_FAILED_TYPE, {Reason})).

-define(ERROR_ATM_OPENFAAS_UNHEALTHY_ID, <<"atmOpenfaasUnhealthy">>).
-define(ERROR_ATM_OPENFAAS_UNHEALTHY_TYPE, od_error_atm_openfaas_unhealthy).
-define(ERROR_ATM_OPENFAAS_UNHEALTHY, ?ERROR(?ERROR_ATM_OPENFAAS_UNHEALTHY_TYPE)).

-define(ERROR_ATM_OPENFAAS_UNREACHABLE_ID, <<"atmOpenfaasUnreachable">>).
-define(ERROR_ATM_OPENFAAS_UNREACHABLE_TYPE, od_error_atm_openfaas_unreachable).
-define(ERROR_ATM_OPENFAAS_UNREACHABLE, ?ERROR(?ERROR_ATM_OPENFAAS_UNREACHABLE_TYPE)).

-define(ERROR_ATM_PARALLEL_BOX_EMPTY_ID, <<"atmParallelBoxEmpty">>).
-define(ERROR_ATM_PARALLEL_BOX_EMPTY_TYPE, od_error_atm_parallel_box_empty).
-define(ERROR_ATM_PARALLEL_BOX_EMPTY(AtmParallelBoxSchemaId), ?ERROR(?ERROR_ATM_PARALLEL_BOX_EMPTY_TYPE, {AtmParallelBoxSchemaId})).

-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_ID, <<"atmParallelBoxExecutionCreationFailed">>).
-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE, od_error_atm_parallel_box_execution_creation_failed).
-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(AtmParallelBoxSchemaId, SpecificError), ?ERROR(?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError})).

-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_ID, <<"atmParallelBoxExecutionInitiationFailed">>).
-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE, od_error_atm_parallel_box_execution_initiation_failed).
-define(ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(AtmParallelBoxSchemaId, SpecificError), ?ERROR(?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError})).

-define(ERROR_ATM_STORE_CONTENT_NOT_SET_ID, <<"atmStoreContentNotSet">>).
-define(ERROR_ATM_STORE_CONTENT_NOT_SET_TYPE, od_error_atm_store_content_not_set).
-define(ERROR_ATM_STORE_CONTENT_NOT_SET(AtmStoreSchemaId), ?ERROR(?ERROR_ATM_STORE_CONTENT_NOT_SET_TYPE, {AtmStoreSchemaId})).

-define(ERROR_ATM_STORE_CREATION_FAILED_ID, <<"atmStoreCreationFailed">>).
-define(ERROR_ATM_STORE_CREATION_FAILED_TYPE, od_error_atm_store_creation_failed).
-define(ERROR_ATM_STORE_CREATION_FAILED(AtmStoreSchemaId, SpecificError), ?ERROR(?ERROR_ATM_STORE_CREATION_FAILED_TYPE, {AtmStoreSchemaId, SpecificError})).

-define(ERROR_ATM_STORE_FROZEN_ID, <<"atmStoreFrozen">>).
-define(ERROR_ATM_STORE_FROZEN_TYPE, od_error_atm_store_frozen).
-define(ERROR_ATM_STORE_FROZEN(AtmStoreSchemaId), ?ERROR(?ERROR_ATM_STORE_FROZEN_TYPE, {AtmStoreSchemaId})).

-define(ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_ID, <<"atmStoreMissingRequiredInitialContent">>).
-define(ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE, od_error_atm_store_missing_required_initial_content).
-define(ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT, ?ERROR(?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE)).

-define(ERROR_ATM_STORE_NOT_FOUND_ID, <<"atmStoreNotFound">>).
-define(ERROR_ATM_STORE_NOT_FOUND_TYPE, od_error_atm_store_not_found).
-define(ERROR_ATM_STORE_NOT_FOUND(AtmStoreSchemaId), ?ERROR(?ERROR_ATM_STORE_NOT_FOUND_TYPE, {AtmStoreSchemaId})).

-define(ERROR_ATM_STORE_TYPE_DISALLOWED_ID, <<"atmStoreTypeDisallowed">>).
-define(ERROR_ATM_STORE_TYPE_DISALLOWED_TYPE, od_error_atm_store_type_disallowed).
-define(ERROR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, Allowed), ?ERROR(?ERROR_ATM_STORE_TYPE_DISALLOWED_TYPE, {AtmStoreSchemaId, Allowed})).

-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_ID, <<"atmTaskArgMapperForNonexistentLambdaArg">>).
-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE, od_error_atm_task_arg_mapper_for_nonexistent_lambda_arg).
-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(Argument), ?ERROR(?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE, {Argument})).

-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_ID, <<"atmTaskArgMapperForRequiredLambdaArgMissing">>).
-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE, od_error_atm_task_arg_mapper_for_required_lambda_arg_missing).
-define(ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(Argument), ?ERROR(?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE, {Argument})).

-define(ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID, <<"atmTaskArgMapperIteratedItemQueryFailed">>).
-define(ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE, od_error_atm_task_arg_mapper_iterated_item_query_failed).
-define(ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(Value, Query), ?ERROR(?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE, {Value, Query})).

-define(ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID, <<"atmTaskArgMapperUnsupportedValueBuilder">>).
-define(ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE, od_error_atm_task_arg_mapper_unsupported_value_builder).
-define(ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, Supported), ?ERROR(?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE, {Type, Supported})).

-define(ERROR_ATM_TASK_ARG_MAPPING_FAILED_ID, <<"atmTaskArgMappingFailed">>).
-define(ERROR_ATM_TASK_ARG_MAPPING_FAILED_TYPE, od_error_atm_task_arg_mapping_failed).
-define(ERROR_ATM_TASK_ARG_MAPPING_FAILED(Argument, SpecificError), ?ERROR(?ERROR_ATM_TASK_ARG_MAPPING_FAILED_TYPE, {Argument, SpecificError})).

-define(ERROR_ATM_TASK_EXECUTION_CREATION_FAILED_ID, <<"atmTaskExecutionCreationFailed">>).
-define(ERROR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE, od_error_atm_task_execution_creation_failed).
-define(ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(AtmTaskSchemaId, SpecificError), ?ERROR(?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError})).

-define(ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED_ID, <<"atmTaskExecutionInitiationFailed">>).
-define(ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE, od_error_atm_task_execution_initiation_failed).
-define(ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(AtmTaskSchemaId, SpecificError), ?ERROR(?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError})).

-define(ERROR_ATM_TASK_EXECUTION_STOPPED_ID, <<"atmTaskExecutionStopped">>).
-define(ERROR_ATM_TASK_EXECUTION_STOPPED_TYPE, od_error_atm_task_execution_stopped).
-define(ERROR_ATM_TASK_EXECUTION_STOPPED, ?ERROR(?ERROR_ATM_TASK_EXECUTION_STOPPED_TYPE)).

-define(ERROR_ATM_TASK_RESULT_DISPATCH_FAILED_ID, <<"atmTaskResultDispatchFailed">>).
-define(ERROR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE, od_error_atm_task_result_dispatch_failed).
-define(ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(AtmStoreSchemaId, SpecificError), ?ERROR(?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE, {AtmStoreSchemaId, SpecificError})).

-define(ERROR_ATM_TASK_RESULT_MAPPING_FAILED_ID, <<"atmTaskResultMappingFailed">>).
-define(ERROR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE, od_error_atm_task_result_mapping_failed).
-define(ERROR_ATM_TASK_RESULT_MAPPING_FAILED(Result, SpecificError), ?ERROR(?ERROR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE, {Result, SpecificError})).

-define(ERROR_ATM_TASK_RESULT_MISSING_ID, <<"atmTaskResultMissing">>).
-define(ERROR_ATM_TASK_RESULT_MISSING_TYPE, od_error_atm_task_result_missing).
-define(ERROR_ATM_TASK_RESULT_MISSING(MissingResultName, ReceivedResultNames), ?ERROR(?ERROR_ATM_TASK_RESULT_MISSING_TYPE, {MissingResultName, ReceivedResultNames})).

-define(ERROR_ATM_UNSUPPORTED_DATA_TYPE_ID, <<"atmUnsupportedDataType">>).
-define(ERROR_ATM_UNSUPPORTED_DATA_TYPE_TYPE, od_error_atm_unsupported_data_type).
-define(ERROR_ATM_UNSUPPORTED_DATA_TYPE(Type, Allowed), ?ERROR(?ERROR_ATM_UNSUPPORTED_DATA_TYPE_TYPE, {Type, Allowed})).

-define(ERROR_ATM_WORKFLOW_EMPTY_ID, <<"atmWorkflowEmpty">>).
-define(ERROR_ATM_WORKFLOW_EMPTY_TYPE, od_error_atm_workflow_empty).
-define(ERROR_ATM_WORKFLOW_EMPTY, ?ERROR(?ERROR_ATM_WORKFLOW_EMPTY_TYPE)).

-define(ERROR_ATM_WORKFLOW_EXECUTION_ENDED_ID, <<"atmWorkflowExecutionEnded">>).
-define(ERROR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE, od_error_atm_workflow_execution_ended).
-define(ERROR_ATM_WORKFLOW_EXECUTION_ENDED, ?ERROR(?ERROR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE)).

-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_ID, <<"atmWorkflowExecutionNotEnded">>).
-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE, od_error_atm_workflow_execution_not_ended).
-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED, ?ERROR(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE)).

-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_ID, <<"atmWorkflowExecutionNotResumable">>).
-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE, od_error_atm_workflow_execution_not_resumable).
-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE, ?ERROR(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE)).

-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_ID, <<"atmWorkflowExecutionNotStopped">>).
-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE, od_error_atm_workflow_execution_not_stopped).
-define(ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED, ?ERROR(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE)).

-define(ERROR_ATM_WORKFLOW_EXECUTION_STOPPED_ID, <<"atmWorkflowExecutionStopped">>).
-define(ERROR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE, od_error_atm_workflow_execution_stopped).
-define(ERROR_ATM_WORKFLOW_EXECUTION_STOPPED, ?ERROR(?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE)).

-define(ERROR_ATM_WORKFLOW_EXECUTION_STOPPING_ID, <<"atmWorkflowExecutionStopping">>).
-define(ERROR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE, od_error_atm_workflow_execution_stopping).
-define(ERROR_ATM_WORKFLOW_EXECUTION_STOPPING, ?ERROR(?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE)).


%%--------------------------------------------------------------------
%% op_worker/dir_stats errors
%%--------------------------------------------------------------------
-define(ERROR_DIR_STATS_DISABLED_FOR_SPACE_ID, <<"dirStatsDisabledForSpace">>).
-define(ERROR_DIR_STATS_DISABLED_FOR_SPACE_TYPE, od_error_dir_stats_disabled_for_space).
-define(ERROR_DIR_STATS_DISABLED_FOR_SPACE, ?ERROR(?ERROR_DIR_STATS_DISABLED_FOR_SPACE_TYPE)).

-define(ERROR_DIR_STATS_NOT_READY_ID, <<"dirStatsNotReady">>).
-define(ERROR_DIR_STATS_NOT_READY_TYPE, od_error_dir_stats_not_ready).
-define(ERROR_DIR_STATS_NOT_READY, ?ERROR(?ERROR_DIR_STATS_NOT_READY_TYPE)).


%%--------------------------------------------------------------------
%% op_worker/storage errors
%%--------------------------------------------------------------------
-define(ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID, <<"autoStorageImportNotSupported">>).
-define(ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, od_error_auto_storage_import_not_supported).
-define(ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages), ?ERROR(?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, SupportedStorages, SupportedObjectStorages})).

-define(ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_ID, <<"notALocalStorageSupportingSpace">>).
-define(ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE, od_error_not_a_local_storage_supporting_space).
-define(ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId), ?ERROR(?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE, {ProviderId, StorageId, SpaceId})).

-define(ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_ID, <<"requiresAutoStorageImportMode">>).
-define(ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE, od_error_requires_auto_storage_import_mode).
-define(ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE, ?ERROR(?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE)).

-define(ERROR_REQUIRES_IMPORTED_STORAGE_ID, <<"requiresImportedStorage">>).
-define(ERROR_REQUIRES_IMPORTED_STORAGE_TYPE, od_error_requires_imported_storage).
-define(ERROR_REQUIRES_IMPORTED_STORAGE(StorageId), ?ERROR(?ERROR_REQUIRES_IMPORTED_STORAGE_TYPE, {StorageId})).

-define(ERROR_REQUIRES_NON_IMPORTED_STORAGE_ID, <<"requiresNonImportedStorage">>).
-define(ERROR_REQUIRES_NON_IMPORTED_STORAGE_TYPE, od_error_requires_non_imported_storage).
-define(ERROR_REQUIRES_NON_IMPORTED_STORAGE(StorageId), ?ERROR(?ERROR_REQUIRES_NON_IMPORTED_STORAGE_TYPE, {StorageId})).

-define(ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE_ID, <<"requiresPosixCompatibleStorage">>).
-define(ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE, od_error_requires_posix_compatible_storage).
-define(ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(StorageId, PosixCompatibleStorages), ?ERROR(?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE, {StorageId, PosixCompatibleStorages})).

-define(ERROR_REQUIRES_READONLY_STORAGE_ID, <<"requiresReadonlyStorage">>).
-define(ERROR_REQUIRES_READONLY_STORAGE_TYPE, od_error_requires_readonly_storage).
-define(ERROR_REQUIRES_READONLY_STORAGE(StorageIdOrType), ?ERROR(?ERROR_REQUIRES_READONLY_STORAGE_TYPE, {StorageIdOrType})).

-define(ERROR_STORAGE_IMPORT_NOT_SUPPORTED_ID, <<"storageImportNotSupported">>).
-define(ERROR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, od_error_storage_import_not_supported).
-define(ERROR_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, ObjectStorages), ?ERROR(?ERROR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, ObjectStorages})).

-define(ERROR_STORAGE_IN_USE_ID, <<"storageInUse">>).
-define(ERROR_STORAGE_IN_USE_TYPE, od_error_storage_in_use).
-define(ERROR_STORAGE_IN_USE, ?ERROR(?ERROR_STORAGE_IN_USE_TYPE)).

-define(ERROR_STORAGE_TEST_FAILED_ID, <<"storageTestFailed">>).
-define(ERROR_STORAGE_TEST_FAILED_TYPE, od_error_storage_test_failed).
-define(ERROR_STORAGE_TEST_FAILED(Operation), ?ERROR(?ERROR_STORAGE_TEST_FAILED_TYPE, {Operation})).


%%--------------------------------------------------------------------
%% op_worker/transfer errors
%%--------------------------------------------------------------------
-define(ERROR_TRANSFER_ALREADY_ENDED_ID, <<"transferAlreadyEnded">>).
-define(ERROR_TRANSFER_ALREADY_ENDED_TYPE, od_error_transfer_already_ended).
-define(ERROR_TRANSFER_ALREADY_ENDED, ?ERROR(?ERROR_TRANSFER_ALREADY_ENDED_TYPE)).

-define(ERROR_TRANSFER_NOT_ENDED_ID, <<"transferNotEnded">>).
-define(ERROR_TRANSFER_NOT_ENDED_TYPE, od_error_transfer_not_ended).
-define(ERROR_TRANSFER_NOT_ENDED, ?ERROR(?ERROR_TRANSFER_NOT_ENDED_TYPE)).


%%--------------------------------------------------------------------
%% op_worker/view errors
%%--------------------------------------------------------------------
-define(ERROR_VIEW_NOT_EXISTS_ON_ID, <<"viewNotExistsOn">>).
-define(ERROR_VIEW_NOT_EXISTS_ON_TYPE, od_error_view_not_exists_on).
-define(ERROR_VIEW_NOT_EXISTS_ON(ProviderId), ?ERROR(?ERROR_VIEW_NOT_EXISTS_ON_TYPE, {ProviderId})).

-define(ERROR_VIEW_QUERY_FAILED_ID, <<"viewQueryFailed">>).
-define(ERROR_VIEW_QUERY_FAILED_TYPE, od_error_view_query_failed).
-define(ERROR_VIEW_QUERY_FAILED(Category, Description), ?ERROR(?ERROR_VIEW_QUERY_FAILED_TYPE, {Category, Description})).


%%--------------------------------------------------------------------
%% oz_worker errors
%%--------------------------------------------------------------------
-define(ERROR_ATM_LAMBDA_IN_USE_ID, <<"atmLambdaInUse">>).
-define(ERROR_ATM_LAMBDA_IN_USE_TYPE, od_error_atm_lambda_in_use).
-define(ERROR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas), ?ERROR(?ERROR_ATM_LAMBDA_IN_USE_TYPE, {AtmWorkflowSchemas})).

-define(ERROR_BASIC_AUTH_DISABLED_ID, <<"basicAuthDisabled">>).
-define(ERROR_BASIC_AUTH_DISABLED_TYPE, od_error_basic_auth_disabled).
-define(ERROR_BASIC_AUTH_DISABLED, ?ERROR(?ERROR_BASIC_AUTH_DISABLED_TYPE)).

-define(ERROR_BASIC_AUTH_NOT_SUPPORTED_ID, <<"basicAuthNotSupported">>).
-define(ERROR_BASIC_AUTH_NOT_SUPPORTED_TYPE, od_error_basic_auth_not_supported).
-define(ERROR_BASIC_AUTH_NOT_SUPPORTED, ?ERROR(?ERROR_BASIC_AUTH_NOT_SUPPORTED_TYPE)).

-define(ERROR_CANNOT_ADD_RELATION_TO_SELF_ID, <<"cannotAddRelationToSelf">>).
-define(ERROR_CANNOT_ADD_RELATION_TO_SELF_TYPE, od_error_cannot_add_relation_to_self).
-define(ERROR_CANNOT_ADD_RELATION_TO_SELF, ?ERROR(?ERROR_CANNOT_ADD_RELATION_TO_SELF_TYPE)).

-define(ERROR_CANNOT_DELETE_ENTITY_ID, <<"cannotDeleteEntity">>).
-define(ERROR_CANNOT_DELETE_ENTITY_TYPE, od_error_cannot_delete_entity).
-define(ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId), ?ERROR(?ERROR_CANNOT_DELETE_ENTITY_TYPE, {EntityType, EntityId})).

-define(ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_ID, <<"cannotDeleteNonEmptyHandleService">>).
-define(ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE, od_error_cannot_delete_non_empty_handle_service).
-define(ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE, ?ERROR(?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE)).

-define(ERROR_CANNOT_REMOVE_LAST_OWNER_ID, <<"cannotRemoveLastOwner">>).
-define(ERROR_CANNOT_REMOVE_LAST_OWNER_TYPE, od_error_cannot_remove_last_owner).
-define(ERROR_CANNOT_REMOVE_LAST_OWNER(EntityType, EntityId), ?ERROR(?ERROR_CANNOT_REMOVE_LAST_OWNER_TYPE, {EntityType, EntityId})).

-define(ERROR_PROTECTED_GROUP_ID, <<"protectedGroup">>).
-define(ERROR_PROTECTED_GROUP_TYPE, od_error_protected_group).
-define(ERROR_PROTECTED_GROUP, ?ERROR(?ERROR_PROTECTED_GROUP_TYPE)).

-define(ERROR_RELATION_ALREADY_EXISTS_ID, <<"relationAlreadyExists">>).
-define(ERROR_RELATION_ALREADY_EXISTS_TYPE, od_error_relation_already_exists).
-define(ERROR_RELATION_ALREADY_EXISTS(ChildType, ChildId, ParentType, ParentId), ?ERROR(?ERROR_RELATION_ALREADY_EXISTS_TYPE, {ChildType, ChildId, ParentType, ParentId})).

-define(ERROR_RELATION_DOES_NOT_EXIST_ID, <<"relationDoesNotExist">>).
-define(ERROR_RELATION_DOES_NOT_EXIST_TYPE, od_error_relation_does_not_exist).
-define(ERROR_RELATION_DOES_NOT_EXIST(ChildType, ChildId, ParentType, ParentId), ?ERROR(?ERROR_RELATION_DOES_NOT_EXIST_TYPE, {ChildType, ChildId, ParentType, ParentId})).


%%--------------------------------------------------------------------
%% oz_worker/space errors
%%--------------------------------------------------------------------
-define(ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID, <<"spaceAlreadySupportedWithImportedStorage">>).
-define(ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE, od_error_space_already_supported_with_imported_storage).
-define(ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId), ?ERROR(?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE, {SpaceId, StorageId})).

-define(ERROR_SPACE_MARKETPLACE_DISABLED_ID, <<"spaceMarketplaceDisabled">>).
-define(ERROR_SPACE_MARKETPLACE_DISABLED_TYPE, od_error_space_marketplace_disabled).
-define(ERROR_SPACE_MARKETPLACE_DISABLED, ?ERROR(?ERROR_SPACE_MARKETPLACE_DISABLED_TYPE)).


%%--------------------------------------------------------------------
%% oz_worker/subdomain errors
%%--------------------------------------------------------------------
-define(ERROR_SUBDOMAIN_DELEGATION_DISABLED_ID, <<"subdomainDelegationDisabled">>).
-define(ERROR_SUBDOMAIN_DELEGATION_DISABLED_TYPE, od_error_subdomain_delegation_disabled).
-define(ERROR_SUBDOMAIN_DELEGATION_DISABLED, ?ERROR(?ERROR_SUBDOMAIN_DELEGATION_DISABLED_TYPE)).

-define(ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_ID, <<"subdomainDelegationNotSupported">>).
-define(ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE, od_error_subdomain_delegation_not_supported).
-define(ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED, ?ERROR(?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE)).


%%--------------------------------------------------------------------
%% posix errors
%%--------------------------------------------------------------------
-define(ERROR_POSIX_ID, <<"posix">>).
-define(ERROR_POSIX_TYPE, od_error_posix).
-define(ERROR_POSIX(Errno), ?ERROR(?ERROR_POSIX_TYPE, {Errno})).



-endif.

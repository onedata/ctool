%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
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

-include("error_attrs.hrl").
-include("global_definitions.hrl").
-include("posix/errno.hrl").


-record(od_error_ctx, {
    onedata_errors_revision :: undefined | binary(),

    module :: undefined | binary(),
    line :: undefined | integer(),
    timestamp :: undefined | time:millis(),

    service :: undefined | onedata:service(),
    service_id :: undefined | onedata:service_id(),
    service_domain :: undefined | binary(),
    service_release_version :: undefined | onedata:release_version(),
    service_build_version :: undefined | binary(),

    unknown_fields = #{} :: json_utils:json_map()
}).

-record(od_error, {
    type :: module(),
    args = undefined :: term(),
    ctx :: od_error:ctx()
}).

-define(err_ctx(), od_error:build_ctx(?MODULE, ?LINE)).

-define(UNDEFINED_ERR_CTX,
    #od_error_ctx{
        onedata_errors_revision = undefined,
        module = undefined,
        line = undefined,
        timestamp = undefined,
        service = undefined,
        service_id = undefined,
        service_domain = undefined,
        service_release_version = undefined,
        service_build_version = undefined
    }
).

-define(ERR, {error, #od_error{}}).
-define(ERR(Type), {error, #od_error{type = Type}}).
-define(ERR(Type, Args), {error, #od_error{type = Type, args = Args}}).
-define(ERR(Type, Args, Ctx), {error, #od_error{type = Type, args = Args, ctx = Ctx}}).


% TODO VFS-12637 - remove below section after below errors are generated in new format
%%--------------------------------------------------------------------
%% deprecated errors
%%--------------------------------------------------------------------
-define(ERROR_ALREADY_EXISTS, {error, already_exists}).
-define(ERROR_NOT_FOUND, {error, not_found}).
-define(ERROR_TIMEOUT, {error, timeout}).
-define(ERROR_NOT_SUPPORTED, {error, not_supported}).


%%--------------------------------------------------------------------
%% Unknown / unexpected error
%%--------------------------------------------------------------------
% Used to carry errors that have the proper JSON error format, but do not match
% any error specified in this software version. This can happen if a newer
% server responds with an error to an older client, which does not know the
% error Id. The original JSON representing the error is retained and returned
% upon encoding.
-define(ERR_UNRECOGNIZED_ERROR(ErrorAsJson), 
    ?ERR(?ERR_UNRECOGNIZED_ERROR_TYPE, {ErrorAsJson})
).
-define(ERR_UNRECOGNIZED_ERROR(ErrorCtx, ErrorAsJson), 
    ?ERR(?ERR_UNRECOGNIZED_ERROR_TYPE, {ErrorAsJson}, ErrorCtx)
).


%%--------------------------------------------------------------------
%% auth errors
%%--------------------------------------------------------------------
-define(ERR_BAD_BASIC_CREDENTIALS, ?ERR(?ERR_BAD_BASIC_CREDENTIALS_TYPE)).
-define(ERR_BAD_BASIC_CREDENTIALS(ErrorCtx), ?ERR(?ERR_BAD_BASIC_CREDENTIALS_TYPE, undefined, ErrorCtx)).

-define(ERR_FORBIDDEN, ?ERR(?ERR_FORBIDDEN_TYPE)).
-define(ERR_FORBIDDEN(ErrorCtx), ?ERR(?ERR_FORBIDDEN_TYPE, undefined, ErrorCtx)).

-define(ERR_FORBIDDEN_WITH_HINT(Hint), ?ERR(?ERR_FORBIDDEN_WITH_HINT_TYPE, {Hint})).
-define(ERR_FORBIDDEN_WITH_HINT(ErrorCtx, Hint), ?ERR(?ERR_FORBIDDEN_WITH_HINT_TYPE, {Hint}, ErrorCtx)).

-define(ERR_UNAUTHORIZED(AuthError), ?ERR(?ERR_UNAUTHORIZED_TYPE, {AuthError})).
-define(ERR_UNAUTHORIZED(ErrorCtx, AuthError), ?ERR(?ERR_UNAUTHORIZED_TYPE, {AuthError}, ErrorCtx)).

-define(ERR_USER_BLOCKED, ?ERR(?ERR_USER_BLOCKED_TYPE)).
-define(ERR_USER_BLOCKED(ErrorCtx), ?ERR(?ERR_USER_BLOCKED_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% auth/token errors
%%--------------------------------------------------------------------
-define(ERR_BAD_CONSUMER_TOKEN(TokenError), ?ERR(?ERR_BAD_CONSUMER_TOKEN_TYPE, {TokenError})).
-define(ERR_BAD_CONSUMER_TOKEN(ErrorCtx, TokenError), ?ERR(?ERR_BAD_CONSUMER_TOKEN_TYPE, {TokenError}, ErrorCtx)).

-define(ERR_BAD_IDP_ACCESS_TOKEN(Idp), ?ERR(?ERR_BAD_IDP_ACCESS_TOKEN_TYPE, {Idp})).
-define(ERR_BAD_IDP_ACCESS_TOKEN(ErrorCtx, Idp), ?ERR(?ERR_BAD_IDP_ACCESS_TOKEN_TYPE, {Idp}, ErrorCtx)).

-define(ERR_BAD_SERVICE_TOKEN(TokenError), ?ERR(?ERR_BAD_SERVICE_TOKEN_TYPE, {TokenError})).
-define(ERR_BAD_SERVICE_TOKEN(ErrorCtx, TokenError), ?ERR(?ERR_BAD_SERVICE_TOKEN_TYPE, {TokenError}, ErrorCtx)).

-define(ERR_BAD_TOKEN, ?ERR(?ERR_BAD_TOKEN_TYPE)).
-define(ERR_BAD_TOKEN(ErrorCtx), ?ERR(?ERR_BAD_TOKEN_TYPE, undefined, ErrorCtx)).

-define(ERR_INVITE_TOKEN_CONSUMER_INVALID(Consumer), ?ERR(?ERR_INVITE_TOKEN_CONSUMER_INVALID_TYPE, {Consumer})).
-define(ERR_INVITE_TOKEN_CONSUMER_INVALID(ErrorCtx, Consumer), ?ERR(?ERR_INVITE_TOKEN_CONSUMER_INVALID_TYPE, {Consumer}, ErrorCtx)).

-define(ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, ?ERR(?ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE)).
-define(ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED(ErrorCtx), ?ERR(?ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE, undefined, ErrorCtx)).

-define(ERR_INVITE_TOKEN_TARGET_ID_INVALID(Id), ?ERR(?ERR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE, {Id})).
-define(ERR_INVITE_TOKEN_TARGET_ID_INVALID(ErrorCtx, Id), ?ERR(?ERR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE, {Id}, ErrorCtx)).

-define(ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED, ?ERR(?ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE)).
-define(ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED(ErrorCtx), ?ERR(?ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE, undefined, ErrorCtx)).

-define(ERR_NOT_AN_ACCESS_TOKEN(Received), ?ERR(?ERR_NOT_AN_ACCESS_TOKEN_TYPE, {Received})).
-define(ERR_NOT_AN_ACCESS_TOKEN(ErrorCtx, Received), ?ERR(?ERR_NOT_AN_ACCESS_TOKEN_TYPE, {Received}, ErrorCtx)).

-define(ERR_NOT_AN_IDENTITY_TOKEN(Received), ?ERR(?ERR_NOT_AN_IDENTITY_TOKEN_TYPE, {Received})).
-define(ERR_NOT_AN_IDENTITY_TOKEN(ErrorCtx, Received), ?ERR(?ERR_NOT_AN_IDENTITY_TOKEN_TYPE, {Received}, ErrorCtx)).

-define(ERR_NOT_AN_INVITE_TOKEN(ExpectedInviteType, Received), ?ERR(?ERR_NOT_AN_INVITE_TOKEN_TYPE, {ExpectedInviteType, Received})).
-define(ERR_NOT_AN_INVITE_TOKEN(ErrorCtx, ExpectedInviteType, Received), ?ERR(?ERR_NOT_AN_INVITE_TOKEN_TYPE, {ExpectedInviteType, Received}, ErrorCtx)).

-define(ERR_TOKEN_CAVEAT_UNKNOWN(Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNKNOWN_TYPE, {Caveat})).
-define(ERR_TOKEN_CAVEAT_UNKNOWN(ErrorCtx, Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNKNOWN_TYPE, {Caveat}, ErrorCtx)).

-define(ERR_TOKEN_CAVEAT_UNVERIFIED(Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNVERIFIED_TYPE, {Caveat})).
-define(ERR_TOKEN_CAVEAT_UNVERIFIED(ErrorCtx, Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNVERIFIED_TYPE, {Caveat}, ErrorCtx)).

-define(ERR_TOKEN_INVALID, ?ERR(?ERR_TOKEN_INVALID_TYPE)).
-define(ERR_TOKEN_INVALID(ErrorCtx), ?ERR(?ERR_TOKEN_INVALID_TYPE, undefined, ErrorCtx)).

-define(ERR_TOKEN_REVOKED, ?ERR(?ERR_TOKEN_REVOKED_TYPE)).
-define(ERR_TOKEN_REVOKED(ErrorCtx), ?ERR(?ERR_TOKEN_REVOKED_TYPE, undefined, ErrorCtx)).

-define(ERR_TOKEN_SERVICE_FORBIDDEN(Service), ?ERR(?ERR_TOKEN_SERVICE_FORBIDDEN_TYPE, {Service})).
-define(ERR_TOKEN_SERVICE_FORBIDDEN(ErrorCtx, Service), ?ERR(?ERR_TOKEN_SERVICE_FORBIDDEN_TYPE, {Service}, ErrorCtx)).

-define(ERR_TOKEN_SESSION_INVALID, ?ERR(?ERR_TOKEN_SESSION_INVALID_TYPE)).
-define(ERR_TOKEN_SESSION_INVALID(ErrorCtx), ?ERR(?ERR_TOKEN_SESSION_INVALID_TYPE, undefined, ErrorCtx)).

-define(ERR_TOKEN_SUBJECT_INVALID, ?ERR(?ERR_TOKEN_SUBJECT_INVALID_TYPE)).
-define(ERR_TOKEN_SUBJECT_INVALID(ErrorCtx), ?ERR(?ERR_TOKEN_SUBJECT_INVALID_TYPE, undefined, ErrorCtx)).

-define(ERR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl), ?ERR(?ERR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE, {MaxTtl})).
-define(ERR_TOKEN_TIME_CAVEAT_REQUIRED(ErrorCtx, MaxTtl), ?ERR(?ERR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE, {MaxTtl}, ErrorCtx)).

-define(ERR_TOKEN_TOO_LARGE(Limit), ?ERR(?ERR_TOKEN_TOO_LARGE_TYPE, {Limit})).
-define(ERR_TOKEN_TOO_LARGE(ErrorCtx, Limit), ?ERR(?ERR_TOKEN_TOO_LARGE_TYPE, {Limit}, ErrorCtx)).


%%--------------------------------------------------------------------
%% connection errors
%%--------------------------------------------------------------------
-define(ERR_NO_CONNECTION_TO_CLUSTER_NODE(NodeName), ?ERR(?ERR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE, {NodeName})).
-define(ERR_NO_CONNECTION_TO_CLUSTER_NODE(ErrorCtx, NodeName), ?ERR(?ERR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE, {NodeName}, ErrorCtx)).

-define(ERR_NO_CONNECTION_TO_ONEZONE(ZoneDomain), ?ERR(?ERR_NO_CONNECTION_TO_ONEZONE_TYPE, {ZoneDomain})).
-define(ERR_NO_CONNECTION_TO_ONEZONE(ErrorCtx, ZoneDomain), ?ERR(?ERR_NO_CONNECTION_TO_ONEZONE_TYPE, {ZoneDomain}, ErrorCtx)).

-define(ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER(ProviderId, ProviderDomain), ?ERR(?ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE, {ProviderId, ProviderDomain})).
-define(ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER(ErrorCtx, ProviderId, ProviderDomain), ?ERR(?ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE, {ProviderId, ProviderDomain}, ErrorCtx)).


%%--------------------------------------------------------------------
%% data_validation errors
%%--------------------------------------------------------------------
-define(ERR_BAD_DATA(Key, SpecificErrorOrHint), ?ERR(?ERR_BAD_DATA_TYPE, {Key, SpecificErrorOrHint})).
-define(ERR_BAD_DATA(ErrorCtx, Key, SpecificErrorOrHint), ?ERR(?ERR_BAD_DATA_TYPE, {Key, SpecificErrorOrHint}, ErrorCtx)).

-define(ERR_BAD_GUI_PACKAGE, ?ERR(?ERR_BAD_GUI_PACKAGE_TYPE)).
-define(ERR_BAD_GUI_PACKAGE(ErrorCtx), ?ERR(?ERR_BAD_GUI_PACKAGE_TYPE, undefined, ErrorCtx)).

-define(ERR_GUI_PACKAGE_TOO_LARGE, ?ERR(?ERR_GUI_PACKAGE_TOO_LARGE_TYPE)).
-define(ERR_GUI_PACKAGE_TOO_LARGE(ErrorCtx), ?ERR(?ERR_GUI_PACKAGE_TOO_LARGE_TYPE, undefined, ErrorCtx)).

-define(ERR_GUI_PACKAGE_UNVERIFIED(ShaSum), ?ERR(?ERR_GUI_PACKAGE_UNVERIFIED_TYPE, {ShaSum})).
-define(ERR_GUI_PACKAGE_UNVERIFIED(ErrorCtx, ShaSum), ?ERR(?ERR_GUI_PACKAGE_UNVERIFIED_TYPE, {ShaSum}, ErrorCtx)).

-define(ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(CurrentProviderStage, CurrentStorageStage), ?ERR(?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE, {CurrentProviderStage, CurrentStorageStage})).
-define(ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ErrorCtx, CurrentProviderStage, CurrentStorageStage), ?ERR(?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE, {CurrentProviderStage, CurrentStorageStage}, ErrorCtx)).

-define(ERR_INVALID_QOS_EXPRESSION(Reason), ?ERR(?ERR_INVALID_QOS_EXPRESSION_TYPE, {Reason})).
-define(ERR_INVALID_QOS_EXPRESSION(ErrorCtx, Reason), ?ERR(?ERR_INVALID_QOS_EXPRESSION_TYPE, {Reason}, ErrorCtx)).

-define(ERR_MALFORMED_DATA, ?ERR(?ERR_MALFORMED_DATA_TYPE)).
-define(ERR_MALFORMED_DATA(ErrorCtx), ?ERR(?ERR_MALFORMED_DATA_TYPE, undefined, ErrorCtx)).

-define(ERR_MISSING_AT_LEAST_ONE_VALUE(Keys), ?ERR(?ERR_MISSING_AT_LEAST_ONE_VALUE_TYPE, {Keys})).
-define(ERR_MISSING_AT_LEAST_ONE_VALUE(ErrorCtx, Keys), ?ERR(?ERR_MISSING_AT_LEAST_ONE_VALUE_TYPE, {Keys}, ErrorCtx)).

-define(ERR_MISSING_REQUIRED_VALUE(Key), ?ERR(?ERR_MISSING_REQUIRED_VALUE_TYPE, {Key})).
-define(ERR_MISSING_REQUIRED_VALUE(ErrorCtx, Key), ?ERR(?ERR_MISSING_REQUIRED_VALUE_TYPE, {Key}, ErrorCtx)).

-define(ERR_TSC_MISSING_LAYOUT(MissingLayout), ?ERR(?ERR_TSC_MISSING_LAYOUT_TYPE, {MissingLayout})).
-define(ERR_TSC_MISSING_LAYOUT(ErrorCtx, MissingLayout), ?ERR(?ERR_TSC_MISSING_LAYOUT_TYPE, {MissingLayout}, ErrorCtx)).

-define(ERR_TSC_TOO_MANY_METRICS(Limit), ?ERR(?ERR_TSC_TOO_MANY_METRICS_TYPE, {Limit})).
-define(ERR_TSC_TOO_MANY_METRICS(ErrorCtx, Limit), ?ERR(?ERR_TSC_TOO_MANY_METRICS_TYPE, {Limit}, ErrorCtx)).


%%--------------------------------------------------------------------
%% data_validation/value errors
%%--------------------------------------------------------------------
-define(ERR_BAD_VALUE_AMBIGUOUS_ID(Key), ?ERR(?ERR_BAD_VALUE_AMBIGUOUS_ID_TYPE, {Key})).
-define(ERR_BAD_VALUE_AMBIGUOUS_ID(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_AMBIGUOUS_ID_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_BOOLEAN(Key), ?ERR(?ERR_BAD_VALUE_BOOLEAN_TYPE, {Key})).
-define(ERR_BAD_VALUE_BOOLEAN(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_BOOLEAN_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_CAVEAT(Caveat), ?ERR(?ERR_BAD_VALUE_CAVEAT_TYPE, {Caveat})).
-define(ERR_BAD_VALUE_CAVEAT(ErrorCtx, Caveat), ?ERR(?ERR_BAD_VALUE_CAVEAT_TYPE, {Caveat}, ErrorCtx)).

-define(ERR_BAD_VALUE_DOMAIN, ?ERR(?ERR_BAD_VALUE_DOMAIN_TYPE)).
-define(ERR_BAD_VALUE_DOMAIN(ErrorCtx), ?ERR(?ERR_BAD_VALUE_DOMAIN_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_EMAIL, ?ERR(?ERR_BAD_VALUE_EMAIL_TYPE)).
-define(ERR_BAD_VALUE_EMAIL(ErrorCtx), ?ERR(?ERR_BAD_VALUE_EMAIL_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_EMPTY(Key), ?ERR(?ERR_BAD_VALUE_EMPTY_TYPE, {Key})).
-define(ERR_BAD_VALUE_EMPTY(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_EMPTY_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_FILE_PATH, ?ERR(?ERR_BAD_VALUE_FILE_PATH_TYPE)).
-define(ERR_BAD_VALUE_FILE_PATH(ErrorCtx), ?ERR(?ERR_BAD_VALUE_FILE_PATH_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_FLOAT(Key), ?ERR(?ERR_BAD_VALUE_FLOAT_TYPE, {Key})).
-define(ERR_BAD_VALUE_FLOAT(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_FLOAT_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_FULL_NAME, ?ERR(?ERR_BAD_VALUE_FULL_NAME_TYPE)).
-define(ERR_BAD_VALUE_FULL_NAME(ErrorCtx), ?ERR(?ERR_BAD_VALUE_FULL_NAME_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_ID_NOT_FOUND(Key), ?ERR(?ERR_BAD_VALUE_ID_NOT_FOUND_TYPE, {Key})).
-define(ERR_BAD_VALUE_ID_NOT_FOUND(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_ID_NOT_FOUND_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_IDENTIFIER(Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_TYPE, {Key})).
-define(ERR_BAD_VALUE_IDENTIFIER(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE, {Key})).
-define(ERR_BAD_VALUE_IDENTIFIER_OCCUPIED(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_INTEGER(Key), ?ERR(?ERR_BAD_VALUE_INTEGER_TYPE, {Key})).
-define(ERR_BAD_VALUE_INTEGER(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_INTEGER_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_INVITE_TYPE(Key), ?ERR(?ERR_BAD_VALUE_INVITE_TYPE_TYPE, {Key})).
-define(ERR_BAD_VALUE_INVITE_TYPE(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_INVITE_TYPE_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_IPV4_ADDRESS(Key), ?ERR(?ERR_BAD_VALUE_IPV4_ADDRESS_TYPE, {Key})).
-define(ERR_BAD_VALUE_IPV4_ADDRESS(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_IPV4_ADDRESS_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_JSON(Key), ?ERR(?ERR_BAD_VALUE_JSON_TYPE, {Key})).
-define(ERR_BAD_VALUE_JSON(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_JSON_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_LIST_NOT_ALLOWED(Key, Allowed), ?ERR(?ERR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE, {Key, Allowed})).
-define(ERR_BAD_VALUE_LIST_NOT_ALLOWED(ErrorCtx, Key, Allowed), ?ERR(?ERR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE, {Key, Allowed}, ErrorCtx)).

-define(ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE, {Key})).
-define(ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_LIST_OF_STRINGS(Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_STRINGS_TYPE, {Key})).
-define(ERR_BAD_VALUE_LIST_OF_STRINGS(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_STRINGS_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_NAME(Key), ?ERR(?ERR_BAD_VALUE_NAME_TYPE, {Key})).
-define(ERR_BAD_VALUE_NAME(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_NAME_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_NOT_ALLOWED(Key, Allowed), ?ERR(?ERR_BAD_VALUE_NOT_ALLOWED_TYPE, {Key, Allowed})).
-define(ERR_BAD_VALUE_NOT_ALLOWED(ErrorCtx, Key, Allowed), ?ERR(?ERR_BAD_VALUE_NOT_ALLOWED_TYPE, {Key, Allowed}, ErrorCtx)).

-define(ERR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High), ?ERR(?ERR_BAD_VALUE_NOT_IN_RANGE_TYPE, {Key, Low, High})).
-define(ERR_BAD_VALUE_NOT_IN_RANGE(ErrorCtx, Key, Low, High), ?ERR(?ERR_BAD_VALUE_NOT_IN_RANGE_TYPE, {Key, Low, High}, ErrorCtx)).

-define(ERR_BAD_VALUE_OCTAL(Key), ?ERR(?ERR_BAD_VALUE_OCTAL_TYPE, {Key})).
-define(ERR_BAD_VALUE_OCTAL(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_OCTAL_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_PASSWORD, ?ERR(?ERR_BAD_VALUE_PASSWORD_TYPE)).
-define(ERR_BAD_VALUE_PASSWORD(ErrorCtx), ?ERR(?ERR_BAD_VALUE_PASSWORD_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_QOS_PARAMETERS, ?ERR(?ERR_BAD_VALUE_QOS_PARAMETERS_TYPE)).
-define(ERR_BAD_VALUE_QOS_PARAMETERS(ErrorCtx), ?ERR(?ERR_BAD_VALUE_QOS_PARAMETERS_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_STRING(Key), ?ERR(?ERR_BAD_VALUE_STRING_TYPE, {Key})).
-define(ERR_BAD_VALUE_STRING(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_STRING_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_SUBDOMAIN, ?ERR(?ERR_BAD_VALUE_SUBDOMAIN_TYPE)).
-define(ERR_BAD_VALUE_SUBDOMAIN(ErrorCtx), ?ERR(?ERR_BAD_VALUE_SUBDOMAIN_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_TEXT_TOO_LARGE(Key, Limit), ?ERR(?ERR_BAD_VALUE_TEXT_TOO_LARGE_TYPE, {Key, Limit})).
-define(ERR_BAD_VALUE_TEXT_TOO_LARGE(ErrorCtx, Key, Limit), ?ERR(?ERR_BAD_VALUE_TEXT_TOO_LARGE_TYPE, {Key, Limit}, ErrorCtx)).

-define(ERR_BAD_VALUE_TOKEN(Key, TokenError), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE, {Key, TokenError})).
-define(ERR_BAD_VALUE_TOKEN(ErrorCtx, Key, TokenError), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE, {Key, TokenError}, ErrorCtx)).

-define(ERR_BAD_VALUE_TOKEN_TYPE(Key), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE_TYPE, {Key})).
-define(ERR_BAD_VALUE_TOKEN_TYPE(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE_TYPE, {Key}, ErrorCtx)).

-define(ERR_BAD_VALUE_TOO_HIGH(Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_HIGH_TYPE, {Key, Limit})).
-define(ERR_BAD_VALUE_TOO_HIGH(ErrorCtx, Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_HIGH_TYPE, {Key, Limit}, ErrorCtx)).

-define(ERR_BAD_VALUE_TOO_LOW(Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_LOW_TYPE, {Key, Limit})).
-define(ERR_BAD_VALUE_TOO_LOW(ErrorCtx, Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_LOW_TYPE, {Key, Limit}, ErrorCtx)).

-define(ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig), ?ERR(?ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE, {TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig})).
-define(ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(ErrorCtx, TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig), ?ERR(?ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE, {TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig}, ErrorCtx)).

-define(ERR_BAD_VALUE_USERNAME, ?ERR(?ERR_BAD_VALUE_USERNAME_TYPE)).
-define(ERR_BAD_VALUE_USERNAME(ErrorCtx), ?ERR(?ERR_BAD_VALUE_USERNAME_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VALUE_XML(Key), ?ERR(?ERR_BAD_VALUE_XML_TYPE, {Key})).
-define(ERR_BAD_VALUE_XML(ErrorCtx, Key), ?ERR(?ERR_BAD_VALUE_XML_TYPE, {Key}, ErrorCtx)).


%%--------------------------------------------------------------------
%% general errors
%%--------------------------------------------------------------------
-define(ERR_BAD_MESSAGE(Message), ?ERR(?ERR_BAD_MESSAGE_TYPE, {Message})).
-define(ERR_BAD_MESSAGE(ErrorCtx, Message), ?ERR(?ERR_BAD_MESSAGE_TYPE, {Message}, ErrorCtx)).

-define(ERR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName), ?ERR(?ERR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE, {ServiceName})).
-define(ERR_EXTERNAL_SERVICE_OPERATION_FAILED(ErrorCtx, ServiceName), ?ERR(?ERR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE, {ServiceName}, ErrorCtx)).

-define(ERR_FILE_ACCESS(Path, Errno), ?ERR(?ERR_FILE_ACCESS_TYPE, {Path, Errno})).
-define(ERR_FILE_ACCESS(ErrorCtx, Path, Errno), ?ERR(?ERR_FILE_ACCESS_TYPE, {Path, Errno}, ErrorCtx)).

-define(ERR_INTERNAL_SERVER_ERROR(Reference), ?ERR(?ERR_INTERNAL_SERVER_ERROR_TYPE, {Reference})).
-define(ERR_INTERNAL_SERVER_ERROR(ErrorCtx, Reference), ?ERR(?ERR_INTERNAL_SERVER_ERROR_TYPE, {Reference}, ErrorCtx)).

-define(ERR_LIMIT_REACHED(Limit, ResourceDescription), ?ERR(?ERR_LIMIT_REACHED_TYPE, {Limit, ResourceDescription})).
-define(ERR_LIMIT_REACHED(ErrorCtx, Limit, ResourceDescription), ?ERR(?ERR_LIMIT_REACHED_TYPE, {Limit, ResourceDescription}, ErrorCtx)).

-define(ERR_NOT_IMPLEMENTED, ?ERR(?ERR_NOT_IMPLEMENTED_TYPE)).
-define(ERR_NOT_IMPLEMENTED(ErrorCtx), ?ERR(?ERR_NOT_IMPLEMENTED_TYPE, undefined, ErrorCtx)).

-define(ERR_SERVICE_UNAVAILABLE, ?ERR(?ERR_SERVICE_UNAVAILABLE_TYPE)).
-define(ERR_SERVICE_UNAVAILABLE(ErrorCtx), ?ERR(?ERR_SERVICE_UNAVAILABLE_TYPE, undefined, ErrorCtx)).

-define(ERR_TEMPORARY_FAILURE, ?ERR(?ERR_TEMPORARY_FAILURE_TYPE)).
-define(ERR_TEMPORARY_FAILURE(ErrorCtx), ?ERR(?ERR_TEMPORARY_FAILURE_TYPE, undefined, ErrorCtx)).

-define(ERR_UNREGISTERED_ONEPROVIDER, ?ERR(?ERR_UNREGISTERED_ONEPROVIDER_TYPE)).
-define(ERR_UNREGISTERED_ONEPROVIDER(ErrorCtx), ?ERR(?ERR_UNREGISTERED_ONEPROVIDER_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% graph_sync errors
%%--------------------------------------------------------------------
-define(ERR_BAD_GRI, ?ERR(?ERR_BAD_GRI_TYPE)).
-define(ERR_BAD_GRI(ErrorCtx), ?ERR(?ERR_BAD_GRI_TYPE, undefined, ErrorCtx)).

-define(ERR_BAD_VERSION(SupportedVersions), ?ERR(?ERR_BAD_VERSION_TYPE, {SupportedVersions})).
-define(ERR_BAD_VERSION(ErrorCtx, SupportedVersions), ?ERR(?ERR_BAD_VERSION_TYPE, {SupportedVersions}, ErrorCtx)).

-define(ERR_EXPECTED_HANDSHAKE_MESSAGE, ?ERR(?ERR_EXPECTED_HANDSHAKE_MESSAGE_TYPE)).
-define(ERR_EXPECTED_HANDSHAKE_MESSAGE(ErrorCtx), ?ERR(?ERR_EXPECTED_HANDSHAKE_MESSAGE_TYPE, undefined, ErrorCtx)).

-define(ERR_HANDSHAKE_ALREADY_DONE, ?ERR(?ERR_HANDSHAKE_ALREADY_DONE_TYPE)).
-define(ERR_HANDSHAKE_ALREADY_DONE(ErrorCtx), ?ERR(?ERR_HANDSHAKE_ALREADY_DONE_TYPE, undefined, ErrorCtx)).

-define(ERR_NOT_SUBSCRIBABLE, ?ERR(?ERR_NOT_SUBSCRIBABLE_TYPE)).
-define(ERR_NOT_SUBSCRIBABLE(ErrorCtx), ?ERR(?ERR_NOT_SUBSCRIBABLE_TYPE, undefined, ErrorCtx)).

-define(ERR_RPC_UNDEFINED, ?ERR(?ERR_RPC_UNDEFINED_TYPE)).
-define(ERR_RPC_UNDEFINED(ErrorCtx), ?ERR(?ERR_RPC_UNDEFINED_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
-define(ERR_DNS_SERVERS_UNREACHABLE(Servers), ?ERR(?ERR_DNS_SERVERS_UNREACHABLE_TYPE, {Servers})).
-define(ERR_DNS_SERVERS_UNREACHABLE(ErrorCtx, Servers), ?ERR(?ERR_DNS_SERVERS_UNREACHABLE_TYPE, {Servers}, ErrorCtx)).

-define(ERR_LETS_ENCRYPT_NOT_REACHABLE, ?ERR(?ERR_LETS_ENCRYPT_NOT_REACHABLE_TYPE)).
-define(ERR_LETS_ENCRYPT_NOT_REACHABLE(ErrorCtx), ?ERR(?ERR_LETS_ENCRYPT_NOT_REACHABLE_TYPE, undefined, ErrorCtx)).

-define(ERR_LETS_ENCRYPT_RESPONSE(ProblemDocument, ErrorMessage), ?ERR(?ERR_LETS_ENCRYPT_RESPONSE_TYPE, {ProblemDocument, ErrorMessage})).
-define(ERR_LETS_ENCRYPT_RESPONSE(ErrorCtx, ProblemDocument, ErrorMessage), ?ERR(?ERR_LETS_ENCRYPT_RESPONSE_TYPE, {ProblemDocument, ErrorMessage}, ErrorCtx)).

-define(ERR_NO_CONNECTION_TO_NEW_NODE(Hostname), ?ERR(?ERR_NO_CONNECTION_TO_NEW_NODE_TYPE, {Hostname})).
-define(ERR_NO_CONNECTION_TO_NEW_NODE(ErrorCtx, Hostname), ?ERR(?ERR_NO_CONNECTION_TO_NEW_NODE_TYPE, {Hostname}, ErrorCtx)).

-define(ERR_NO_SERVICE_NODES(Service), ?ERR(?ERR_NO_SERVICE_NODES_TYPE, {Service})).
-define(ERR_NO_SERVICE_NODES(ErrorCtx, Service), ?ERR(?ERR_NO_SERVICE_NODES_TYPE, {Service}, ErrorCtx)).

-define(ERR_NODE_ALREADY_IN_CLUSTER(Hostname), ?ERR(?ERR_NODE_ALREADY_IN_CLUSTER_TYPE, {Hostname})).
-define(ERR_NODE_ALREADY_IN_CLUSTER(ErrorCtx, Hostname), ?ERR(?ERR_NODE_ALREADY_IN_CLUSTER_TYPE, {Hostname}, ErrorCtx)).

-define(ERR_NODE_NOT_COMPATIBLE(Hostname, ClusterType), ?ERR(?ERR_NODE_NOT_COMPATIBLE_TYPE, {Hostname, ClusterType})).
-define(ERR_NODE_NOT_COMPATIBLE(ErrorCtx, Hostname, ClusterType), ?ERR(?ERR_NODE_NOT_COMPATIBLE_TYPE, {Hostname, ClusterType}, ErrorCtx)).

-define(ERR_ON_NODES(Error, Hostnames), ?ERR(?ERR_ON_NODES_TYPE, {Error, Hostnames})).
-define(ERR_ON_NODES(ErrorCtx, Error, Hostnames), ?ERR(?ERR_ON_NODES_TYPE, {Error, Hostnames}, ErrorCtx)).

-define(ERR_USER_NOT_IN_CLUSTER, ?ERR(?ERR_USER_NOT_IN_CLUSTER_TYPE)).
-define(ERR_USER_NOT_IN_CLUSTER(ErrorCtx), ?ERR(?ERR_USER_NOT_IN_CLUSTER_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
-define(ERR_AUTO_CLEANING_DISABLED, ?ERR(?ERR_AUTO_CLEANING_DISABLED_TYPE)).
-define(ERR_AUTO_CLEANING_DISABLED(ErrorCtx), ?ERR(?ERR_AUTO_CLEANING_DISABLED_TYPE, undefined, ErrorCtx)).

-define(ERR_FILE_POPULARITY_DISABLED, ?ERR(?ERR_FILE_POPULARITY_DISABLED_TYPE)).
-define(ERR_FILE_POPULARITY_DISABLED(ErrorCtx), ?ERR(?ERR_FILE_POPULARITY_DISABLED_TYPE, undefined, ErrorCtx)).

-define(ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(CurrentState, AllowedStates), ?ERR(?ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE, {CurrentState, AllowedStates})).
-define(ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(ErrorCtx, CurrentState, AllowedStates), ?ERR(?ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE, {CurrentState, AllowedStates}, ErrorCtx)).

-define(ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ParentArchiveId), ?ERR(?ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE, {ParentArchiveId})).
-define(ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ErrorCtx, ParentArchiveId), ?ERR(?ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE, {ParentArchiveId}, ErrorCtx)).

-define(ERR_QUOTA_EXCEEDED, ?ERR(?ERR_QUOTA_EXCEEDED_TYPE)).
-define(ERR_QUOTA_EXCEEDED(ErrorCtx), ?ERR(?ERR_QUOTA_EXCEEDED_TYPE, undefined, ErrorCtx)).

-define(ERR_RECALL_TARGET_CONFLICT, ?ERR(?ERR_RECALL_TARGET_CONFLICT_TYPE)).
-define(ERR_RECALL_TARGET_CONFLICT(ErrorCtx), ?ERR(?ERR_RECALL_TARGET_CONFLICT_TYPE, undefined, ErrorCtx)).

-define(ERR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId), ?ERR(?ERR_SPACE_NOT_SUPPORTED_BY_TYPE, {SpaceId, ProviderId})).
-define(ERR_SPACE_NOT_SUPPORTED_BY(ErrorCtx, SpaceId, ProviderId), ?ERR(?ERR_SPACE_NOT_SUPPORTED_BY_TYPE, {SpaceId, ProviderId}, ErrorCtx)).

-define(ERR_STAT_OPERATION_NOT_SUPPORTED(StorageId), ?ERR(?ERR_STAT_OPERATION_NOT_SUPPORTED_TYPE, {StorageId})).
-define(ERR_STAT_OPERATION_NOT_SUPPORTED(ErrorCtx, StorageId), ?ERR(?ERR_STAT_OPERATION_NOT_SUPPORTED_TYPE, {StorageId}, ErrorCtx)).

-define(ERR_USER_NOT_SUPPORTED, ?ERR(?ERR_USER_NOT_SUPPORTED_TYPE)).
-define(ERR_USER_NOT_SUPPORTED(ErrorCtx), ?ERR(?ERR_USER_NOT_SUPPORTED_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% op_worker/atm errors
%%--------------------------------------------------------------------
-define(ERR_ATM_DATA_TYPE_UNVERIFIED(Value, ExpType), ?ERR(?ERR_ATM_DATA_TYPE_UNVERIFIED_TYPE, {Value, ExpType})).
-define(ERR_ATM_DATA_TYPE_UNVERIFIED(ErrorCtx, Value, ExpType), ?ERR(?ERR_ATM_DATA_TYPE_UNVERIFIED_TYPE, {Value, ExpType}, ErrorCtx)).

-define(ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(Value, Type, ValueConstraints), ?ERR(?ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE, {Value, Type, ValueConstraints})).
-define(ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(ErrorCtx, Value, Type, ValueConstraints), ?ERR(?ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE, {Value, Type, ValueConstraints}, ErrorCtx)).

-define(ERR_ATM_INVALID_STATUS_TRANSITION(PrevStatus, NewStatus), ?ERR(?ERR_ATM_INVALID_STATUS_TRANSITION_TYPE, {PrevStatus, NewStatus})).
-define(ERR_ATM_INVALID_STATUS_TRANSITION(ErrorCtx, PrevStatus, NewStatus), ?ERR(?ERR_ATM_INVALID_STATUS_TRANSITION_TYPE, {PrevStatus, NewStatus}, ErrorCtx)).

-define(ERR_ATM_JOB_BATCH_CRASHED(Reason), ?ERR(?ERR_ATM_JOB_BATCH_CRASHED_TYPE, {Reason})).
-define(ERR_ATM_JOB_BATCH_CRASHED(ErrorCtx, Reason), ?ERR(?ERR_ATM_JOB_BATCH_CRASHED_TYPE, {Reason}, ErrorCtx)).

-define(ERR_ATM_JOB_BATCH_WITHDRAWN(Reason), ?ERR(?ERR_ATM_JOB_BATCH_WITHDRAWN_TYPE, {Reason})).
-define(ERR_ATM_JOB_BATCH_WITHDRAWN(ErrorCtx, Reason), ?ERR(?ERR_ATM_JOB_BATCH_WITHDRAWN_TYPE, {Reason}, ErrorCtx)).

-define(ERR_ATM_LAMBDA_CONFIG_BAD_VALUE(ParameterName, SpecificError), ?ERR(?ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE, {ParameterName, SpecificError})).
-define(ERR_ATM_LAMBDA_CONFIG_BAD_VALUE(ErrorCtx, ParameterName, SpecificError), ?ERR(?ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE, {ParameterName, SpecificError}, ErrorCtx)).

-define(ERR_ATM_LANE_EMPTY(AtmLaneSchemaId), ?ERR(?ERR_ATM_LANE_EMPTY_TYPE, {AtmLaneSchemaId})).
-define(ERR_ATM_LANE_EMPTY(ErrorCtx, AtmLaneSchemaId), ?ERR(?ERR_ATM_LANE_EMPTY_TYPE, {AtmLaneSchemaId}, ErrorCtx)).

-define(ERR_ATM_LANE_EXECUTION_CREATION_FAILED(AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError})).
-define(ERR_ATM_LANE_EXECUTION_CREATION_FAILED(ErrorCtx, AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_LANE_EXECUTION_INITIATION_FAILED(AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError})).
-define(ERR_ATM_LANE_EXECUTION_INITIATION_FAILED(ErrorCtx, AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_LANE_EXECUTION_RERUN_FAILED, ?ERR(?ERR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE)).
-define(ERR_ATM_LANE_EXECUTION_RERUN_FAILED(ErrorCtx), ?ERR(?ERR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_LANE_EXECUTION_RETRY_FAILED, ?ERR(?ERR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE)).
-define(ERR_ATM_LANE_EXECUTION_RETRY_FAILED(ErrorCtx), ?ERR(?ERR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED, ?ERR(?ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE)).
-define(ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED(ErrorCtx), ?ERR(?ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_OPENFAAS_NOT_CONFIGURED, ?ERR(?ERR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE)).
-define(ERR_ATM_OPENFAAS_NOT_CONFIGURED(ErrorCtx), ?ERR(?ERR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_OPENFAAS_QUERY_FAILED(Reason), ?ERR(?ERR_ATM_OPENFAAS_QUERY_FAILED_TYPE, {Reason})).
-define(ERR_ATM_OPENFAAS_QUERY_FAILED(ErrorCtx, Reason), ?ERR(?ERR_ATM_OPENFAAS_QUERY_FAILED_TYPE, {Reason}, ErrorCtx)).

-define(ERR_ATM_OPENFAAS_UNHEALTHY, ?ERR(?ERR_ATM_OPENFAAS_UNHEALTHY_TYPE)).
-define(ERR_ATM_OPENFAAS_UNHEALTHY(ErrorCtx), ?ERR(?ERR_ATM_OPENFAAS_UNHEALTHY_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_OPENFAAS_UNREACHABLE, ?ERR(?ERR_ATM_OPENFAAS_UNREACHABLE_TYPE)).
-define(ERR_ATM_OPENFAAS_UNREACHABLE(ErrorCtx), ?ERR(?ERR_ATM_OPENFAAS_UNREACHABLE_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_PARALLEL_BOX_EMPTY(AtmParallelBoxSchemaId), ?ERR(?ERR_ATM_PARALLEL_BOX_EMPTY_TYPE, {AtmParallelBoxSchemaId})).
-define(ERR_ATM_PARALLEL_BOX_EMPTY(ErrorCtx, AtmParallelBoxSchemaId), ?ERR(?ERR_ATM_PARALLEL_BOX_EMPTY_TYPE, {AtmParallelBoxSchemaId}, ErrorCtx)).

-define(ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError})).
-define(ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(ErrorCtx, AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError})).
-define(ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(ErrorCtx, AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_STORE_CONTENT_NOT_SET(AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_CONTENT_NOT_SET_TYPE, {AtmStoreSchemaId})).
-define(ERR_ATM_STORE_CONTENT_NOT_SET(ErrorCtx, AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_CONTENT_NOT_SET_TYPE, {AtmStoreSchemaId}, ErrorCtx)).

-define(ERR_ATM_STORE_CREATION_FAILED(AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_STORE_CREATION_FAILED_TYPE, {AtmStoreSchemaId, SpecificError})).
-define(ERR_ATM_STORE_CREATION_FAILED(ErrorCtx, AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_STORE_CREATION_FAILED_TYPE, {AtmStoreSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_STORE_FROZEN(AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_FROZEN_TYPE, {AtmStoreSchemaId})).
-define(ERR_ATM_STORE_FROZEN(ErrorCtx, AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_FROZEN_TYPE, {AtmStoreSchemaId}, ErrorCtx)).

-define(ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT, ?ERR(?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE)).
-define(ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT(ErrorCtx), ?ERR(?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_STORE_NOT_FOUND(AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_NOT_FOUND_TYPE, {AtmStoreSchemaId})).
-define(ERR_ATM_STORE_NOT_FOUND(ErrorCtx, AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_NOT_FOUND_TYPE, {AtmStoreSchemaId}, ErrorCtx)).

-define(ERR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, Allowed), ?ERR(?ERR_ATM_STORE_TYPE_DISALLOWED_TYPE, {AtmStoreSchemaId, Allowed})).
-define(ERR_ATM_STORE_TYPE_DISALLOWED(ErrorCtx, AtmStoreSchemaId, Allowed), ?ERR(?ERR_ATM_STORE_TYPE_DISALLOWED_TYPE, {AtmStoreSchemaId, Allowed}, ErrorCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE, {Argument})).
-define(ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(ErrorCtx, Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE, {Argument}, ErrorCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE, {Argument})).
-define(ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(ErrorCtx, Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE, {Argument}, ErrorCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(Value, Query), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE, {Value, Query})).
-define(ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(ErrorCtx, Value, Query), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE, {Value, Query}, ErrorCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, Supported), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE, {Type, Supported})).
-define(ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(ErrorCtx, Type, Supported), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE, {Type, Supported}, ErrorCtx)).

-define(ERR_ATM_TASK_ARG_MAPPING_FAILED(Argument, SpecificError), ?ERR(?ERR_ATM_TASK_ARG_MAPPING_FAILED_TYPE, {Argument, SpecificError})).
-define(ERR_ATM_TASK_ARG_MAPPING_FAILED(ErrorCtx, Argument, SpecificError), ?ERR(?ERR_ATM_TASK_ARG_MAPPING_FAILED_TYPE, {Argument, SpecificError}, ErrorCtx)).

-define(ERR_ATM_TASK_EXECUTION_CREATION_FAILED(AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError})).
-define(ERR_ATM_TASK_EXECUTION_CREATION_FAILED(ErrorCtx, AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_TASK_EXECUTION_INITIATION_FAILED(AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError})).
-define(ERR_ATM_TASK_EXECUTION_INITIATION_FAILED(ErrorCtx, AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_TASK_EXECUTION_STOPPED, ?ERR(?ERR_ATM_TASK_EXECUTION_STOPPED_TYPE)).
-define(ERR_ATM_TASK_EXECUTION_STOPPED(ErrorCtx), ?ERR(?ERR_ATM_TASK_EXECUTION_STOPPED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_TASK_RESULT_DISPATCH_FAILED(AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE, {AtmStoreSchemaId, SpecificError})).
-define(ERR_ATM_TASK_RESULT_DISPATCH_FAILED(ErrorCtx, AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE, {AtmStoreSchemaId, SpecificError}, ErrorCtx)).

-define(ERR_ATM_TASK_RESULT_MAPPING_FAILED(Result, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE, {Result, SpecificError})).
-define(ERR_ATM_TASK_RESULT_MAPPING_FAILED(ErrorCtx, Result, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE, {Result, SpecificError}, ErrorCtx)).

-define(ERR_ATM_TASK_RESULT_MISSING(MissingResultName, ReceivedResultNames), ?ERR(?ERR_ATM_TASK_RESULT_MISSING_TYPE, {MissingResultName, ReceivedResultNames})).
-define(ERR_ATM_TASK_RESULT_MISSING(ErrorCtx, MissingResultName, ReceivedResultNames), ?ERR(?ERR_ATM_TASK_RESULT_MISSING_TYPE, {MissingResultName, ReceivedResultNames}, ErrorCtx)).

-define(ERR_ATM_UNSUPPORTED_DATA_TYPE(Type, Allowed), ?ERR(?ERR_ATM_UNSUPPORTED_DATA_TYPE_TYPE, {Type, Allowed})).
-define(ERR_ATM_UNSUPPORTED_DATA_TYPE(ErrorCtx, Type, Allowed), ?ERR(?ERR_ATM_UNSUPPORTED_DATA_TYPE_TYPE, {Type, Allowed}, ErrorCtx)).

-define(ERR_ATM_WORKFLOW_EMPTY, ?ERR(?ERR_ATM_WORKFLOW_EMPTY_TYPE)).
-define(ERR_ATM_WORKFLOW_EMPTY(ErrorCtx), ?ERR(?ERR_ATM_WORKFLOW_EMPTY_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_ENDED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_ENDED(ErrorCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED(ErrorCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE(ErrorCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED(ErrorCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPED(ErrorCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE, undefined, ErrorCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPING, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPING(ErrorCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% op_worker/dir_stats errors
%%--------------------------------------------------------------------
-define(ERR_DIR_STATS_DISABLED_FOR_SPACE, ?ERR(?ERR_DIR_STATS_DISABLED_FOR_SPACE_TYPE)).
-define(ERR_DIR_STATS_DISABLED_FOR_SPACE(ErrorCtx), ?ERR(?ERR_DIR_STATS_DISABLED_FOR_SPACE_TYPE, undefined, ErrorCtx)).

-define(ERR_DIR_STATS_NOT_READY, ?ERR(?ERR_DIR_STATS_NOT_READY_TYPE)).
-define(ERR_DIR_STATS_NOT_READY(ErrorCtx), ?ERR(?ERR_DIR_STATS_NOT_READY_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% op_worker/storage errors
%%--------------------------------------------------------------------
-define(ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages), ?ERR(?ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, SupportedStorages, SupportedObjectStorages})).
-define(ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(ErrorCtx, StorageId, SupportedStorages, SupportedObjectStorages), ?ERR(?ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, SupportedStorages, SupportedObjectStorages}, ErrorCtx)).

-define(ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId), ?ERR(?ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE, {ProviderId, StorageId, SpaceId})).
-define(ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ErrorCtx, ProviderId, StorageId, SpaceId), ?ERR(?ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE, {ProviderId, StorageId, SpaceId}, ErrorCtx)).

-define(ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE, ?ERR(?ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE)).
-define(ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE(ErrorCtx), ?ERR(?ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE, undefined, ErrorCtx)).

-define(ERR_REQUIRES_IMPORTED_STORAGE(StorageId), ?ERR(?ERR_REQUIRES_IMPORTED_STORAGE_TYPE, {StorageId})).
-define(ERR_REQUIRES_IMPORTED_STORAGE(ErrorCtx, StorageId), ?ERR(?ERR_REQUIRES_IMPORTED_STORAGE_TYPE, {StorageId}, ErrorCtx)).

-define(ERR_REQUIRES_NON_IMPORTED_STORAGE(StorageId), ?ERR(?ERR_REQUIRES_NON_IMPORTED_STORAGE_TYPE, {StorageId})).
-define(ERR_REQUIRES_NON_IMPORTED_STORAGE(ErrorCtx, StorageId), ?ERR(?ERR_REQUIRES_NON_IMPORTED_STORAGE_TYPE, {StorageId}, ErrorCtx)).

-define(ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE(StorageId, PosixCompatibleStorages), ?ERR(?ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE, {StorageId, PosixCompatibleStorages})).
-define(ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE(ErrorCtx, StorageId, PosixCompatibleStorages), ?ERR(?ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE, {StorageId, PosixCompatibleStorages}, ErrorCtx)).

-define(ERR_REQUIRES_READONLY_STORAGE(StorageIdOrType), ?ERR(?ERR_REQUIRES_READONLY_STORAGE_TYPE, {StorageIdOrType})).
-define(ERR_REQUIRES_READONLY_STORAGE(ErrorCtx, StorageIdOrType), ?ERR(?ERR_REQUIRES_READONLY_STORAGE_TYPE, {StorageIdOrType}, ErrorCtx)).

-define(ERR_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, ObjectStorages), ?ERR(?ERR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, ObjectStorages})).
-define(ERR_STORAGE_IMPORT_NOT_SUPPORTED(ErrorCtx, StorageId, ObjectStorages), ?ERR(?ERR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, ObjectStorages}, ErrorCtx)).

-define(ERR_STORAGE_IN_USE, ?ERR(?ERR_STORAGE_IN_USE_TYPE)).
-define(ERR_STORAGE_IN_USE(ErrorCtx), ?ERR(?ERR_STORAGE_IN_USE_TYPE, undefined, ErrorCtx)).

-define(ERR_STORAGE_TEST_FAILED(Operation), ?ERR(?ERR_STORAGE_TEST_FAILED_TYPE, {Operation})).
-define(ERR_STORAGE_TEST_FAILED(ErrorCtx, Operation), ?ERR(?ERR_STORAGE_TEST_FAILED_TYPE, {Operation}, ErrorCtx)).


%%--------------------------------------------------------------------
%% op_worker/transfer errors
%%--------------------------------------------------------------------
-define(ERR_TRANSFER_ALREADY_ENDED, ?ERR(?ERR_TRANSFER_ALREADY_ENDED_TYPE)).
-define(ERR_TRANSFER_ALREADY_ENDED(ErrorCtx), ?ERR(?ERR_TRANSFER_ALREADY_ENDED_TYPE, undefined, ErrorCtx)).

-define(ERR_TRANSFER_NOT_ENDED, ?ERR(?ERR_TRANSFER_NOT_ENDED_TYPE)).
-define(ERR_TRANSFER_NOT_ENDED(ErrorCtx), ?ERR(?ERR_TRANSFER_NOT_ENDED_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% op_worker/view errors
%%--------------------------------------------------------------------
-define(ERR_VIEW_NOT_EXISTS_ON(ProviderId), ?ERR(?ERR_VIEW_NOT_EXISTS_ON_TYPE, {ProviderId})).
-define(ERR_VIEW_NOT_EXISTS_ON(ErrorCtx, ProviderId), ?ERR(?ERR_VIEW_NOT_EXISTS_ON_TYPE, {ProviderId}, ErrorCtx)).

-define(ERR_VIEW_QUERY_FAILED(Category, Description), ?ERR(?ERR_VIEW_QUERY_FAILED_TYPE, {Category, Description})).
-define(ERR_VIEW_QUERY_FAILED(ErrorCtx, Category, Description), ?ERR(?ERR_VIEW_QUERY_FAILED_TYPE, {Category, Description}, ErrorCtx)).


%%--------------------------------------------------------------------
%% oz_worker errors
%%--------------------------------------------------------------------
-define(ERR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas), ?ERR(?ERR_ATM_LAMBDA_IN_USE_TYPE, {AtmWorkflowSchemas})).
-define(ERR_ATM_LAMBDA_IN_USE(ErrorCtx, AtmWorkflowSchemas), ?ERR(?ERR_ATM_LAMBDA_IN_USE_TYPE, {AtmWorkflowSchemas}, ErrorCtx)).

-define(ERR_BASIC_AUTH_DISABLED, ?ERR(?ERR_BASIC_AUTH_DISABLED_TYPE)).
-define(ERR_BASIC_AUTH_DISABLED(ErrorCtx), ?ERR(?ERR_BASIC_AUTH_DISABLED_TYPE, undefined, ErrorCtx)).

-define(ERR_BASIC_AUTH_NOT_SUPPORTED, ?ERR(?ERR_BASIC_AUTH_NOT_SUPPORTED_TYPE)).
-define(ERR_BASIC_AUTH_NOT_SUPPORTED(ErrorCtx), ?ERR(?ERR_BASIC_AUTH_NOT_SUPPORTED_TYPE, undefined, ErrorCtx)).

-define(ERR_CANNOT_ADD_RELATION_TO_SELF, ?ERR(?ERR_CANNOT_ADD_RELATION_TO_SELF_TYPE)).
-define(ERR_CANNOT_ADD_RELATION_TO_SELF(ErrorCtx), ?ERR(?ERR_CANNOT_ADD_RELATION_TO_SELF_TYPE, undefined, ErrorCtx)).

-define(ERR_CANNOT_DELETE_ENTITY(EntityType, EntityId), ?ERR(?ERR_CANNOT_DELETE_ENTITY_TYPE, {EntityType, EntityId})).
-define(ERR_CANNOT_DELETE_ENTITY(ErrorCtx, EntityType, EntityId), ?ERR(?ERR_CANNOT_DELETE_ENTITY_TYPE, {EntityType, EntityId}, ErrorCtx)).

-define(ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE, ?ERR(?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE)).
-define(ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE(ErrorCtx), ?ERR(?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE, undefined, ErrorCtx)).

-define(ERR_CANNOT_REMOVE_LAST_OWNER(EntityType, EntityId), ?ERR(?ERR_CANNOT_REMOVE_LAST_OWNER_TYPE, {EntityType, EntityId})).
-define(ERR_CANNOT_REMOVE_LAST_OWNER(ErrorCtx, EntityType, EntityId), ?ERR(?ERR_CANNOT_REMOVE_LAST_OWNER_TYPE, {EntityType, EntityId}, ErrorCtx)).

-define(ERR_PROTECTED_GROUP, ?ERR(?ERR_PROTECTED_GROUP_TYPE)).
-define(ERR_PROTECTED_GROUP(ErrorCtx), ?ERR(?ERR_PROTECTED_GROUP_TYPE, undefined, ErrorCtx)).

-define(ERR_RELATION_ALREADY_EXISTS(ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_ALREADY_EXISTS_TYPE, {ChildType, ChildId, ParentType, ParentId})).
-define(ERR_RELATION_ALREADY_EXISTS(ErrorCtx, ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_ALREADY_EXISTS_TYPE, {ChildType, ChildId, ParentType, ParentId}, ErrorCtx)).

-define(ERR_RELATION_DOES_NOT_EXIST(ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_DOES_NOT_EXIST_TYPE, {ChildType, ChildId, ParentType, ParentId})).
-define(ERR_RELATION_DOES_NOT_EXIST(ErrorCtx, ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_DOES_NOT_EXIST_TYPE, {ChildType, ChildId, ParentType, ParentId}, ErrorCtx)).


%%--------------------------------------------------------------------
%% oz_worker/space errors
%%--------------------------------------------------------------------
-define(ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId), ?ERR(?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE, {SpaceId, StorageId})).
-define(ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(ErrorCtx, SpaceId, StorageId), ?ERR(?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE, {SpaceId, StorageId}, ErrorCtx)).

-define(ERR_SPACE_MARKETPLACE_DISABLED, ?ERR(?ERR_SPACE_MARKETPLACE_DISABLED_TYPE)).
-define(ERR_SPACE_MARKETPLACE_DISABLED(ErrorCtx), ?ERR(?ERR_SPACE_MARKETPLACE_DISABLED_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% oz_worker/subdomain errors
%%--------------------------------------------------------------------
-define(ERR_SUBDOMAIN_DELEGATION_DISABLED, ?ERR(?ERR_SUBDOMAIN_DELEGATION_DISABLED_TYPE)).
-define(ERR_SUBDOMAIN_DELEGATION_DISABLED(ErrorCtx), ?ERR(?ERR_SUBDOMAIN_DELEGATION_DISABLED_TYPE, undefined, ErrorCtx)).

-define(ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED, ?ERR(?ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE)).
-define(ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED(ErrorCtx), ?ERR(?ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE, undefined, ErrorCtx)).


%%--------------------------------------------------------------------
%% posix errors
%%--------------------------------------------------------------------
-define(ERR_POSIX(Errno), ?ERR(?ERR_POSIX_TYPE, {Errno})).
-define(ERR_POSIX(ErrorCtx, Errno), ?ERR(?ERR_POSIX_TYPE, {Errno}, ErrorCtx)).


-endif.

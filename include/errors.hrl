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

-include("error_attrs.hrl").
-include("global_definitions.hrl").
-include("posix/errno.hrl").


-record(od_error_ctx, {
    file :: undefined | binary(),
    line :: undefined | integer(),
    timestamp :: undefined | time:iso8601(),
    version :: undefined | binary()
}).

-record(od_error, {
    type :: module(),
    args = undefined :: term(),
    ctx :: od_error:ctx()
}).

-define(err_ctx(),
    #od_error_ctx{
        file = <<?FILE>>,
        line = ?LINE,
        timestamp = od_error:iso8601_now(),
        version = od_error:version()
    }
).
-define(undefined_err_ctx,
    #od_error_ctx{
        file = undefined,
        line = undefined,
        timestamp = undefined,
        version = undefined
    }
).

-define(ERR, {error, #od_error{}}).
-define(ERR(Type), {error, #od_error{type = Type}}).
-define(ERR(Type, Args), {error, #od_error{type = Type, args = Args}}).
-define(ERR(Type, Args, Ctx), {error, #od_error{type = Type, args = Args, ctx = Ctx}}).


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
-define(ERR_UNRECOGNIZED_ERROR(ErrCtx, ErrorAsJson), 
    ?ERR(?ERR_UNRECOGNIZED_ERROR_TYPE, {ErrorAsJson}, ErrCtx)
).

%%--------------------------------------------------------------------
%% auth errors
%%--------------------------------------------------------------------
-define(ERR_BAD_BASIC_CREDENTIALS, ?ERR(?ERR_BAD_BASIC_CREDENTIALS_TYPE)).
-define(ERR_BAD_BASIC_CREDENTIALS(ErrCtx), ?ERR(?ERR_BAD_BASIC_CREDENTIALS_TYPE, undefined, ErrCtx)).

-define(ERR_FORBIDDEN(Hint), ?ERR(?ERR_FORBIDDEN_TYPE, {Hint})).
-define(ERR_FORBIDDEN(ErrCtx, Hint), ?ERR(?ERR_FORBIDDEN_TYPE, {Hint}, ErrCtx)).

-define(ERR_UNAUTHORIZED(AuthError), ?ERR(?ERR_UNAUTHORIZED_TYPE, {AuthError})).
-define(ERR_UNAUTHORIZED(ErrCtx, AuthError), ?ERR(?ERR_UNAUTHORIZED_TYPE, {AuthError}, ErrCtx)).

-define(ERR_USER_BLOCKED, ?ERR(?ERR_USER_BLOCKED_TYPE)).
-define(ERR_USER_BLOCKED(ErrCtx), ?ERR(?ERR_USER_BLOCKED_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% auth/token errors
%%--------------------------------------------------------------------
-define(ERR_BAD_CONSUMER_TOKEN(TokenError), ?ERR(?ERR_BAD_CONSUMER_TOKEN_TYPE, {TokenError})).
-define(ERR_BAD_CONSUMER_TOKEN(ErrCtx, TokenError), ?ERR(?ERR_BAD_CONSUMER_TOKEN_TYPE, {TokenError}, ErrCtx)).

-define(ERR_BAD_IDP_ACCESS_TOKEN(Idp), ?ERR(?ERR_BAD_IDP_ACCESS_TOKEN_TYPE, {Idp})).
-define(ERR_BAD_IDP_ACCESS_TOKEN(ErrCtx, Idp), ?ERR(?ERR_BAD_IDP_ACCESS_TOKEN_TYPE, {Idp}, ErrCtx)).

-define(ERR_BAD_SERVICE_TOKEN(TokenError), ?ERR(?ERR_BAD_SERVICE_TOKEN_TYPE, {TokenError})).
-define(ERR_BAD_SERVICE_TOKEN(ErrCtx, TokenError), ?ERR(?ERR_BAD_SERVICE_TOKEN_TYPE, {TokenError}, ErrCtx)).

-define(ERR_BAD_TOKEN, ?ERR(?ERR_BAD_TOKEN_TYPE)).
-define(ERR_BAD_TOKEN(ErrCtx), ?ERR(?ERR_BAD_TOKEN_TYPE, undefined, ErrCtx)).

-define(ERR_INVITE_TOKEN_CONSUMER_INVALID(Consumer), ?ERR(?ERR_INVITE_TOKEN_CONSUMER_INVALID_TYPE, {Consumer})).
-define(ERR_INVITE_TOKEN_CONSUMER_INVALID(ErrCtx, Consumer), ?ERR(?ERR_INVITE_TOKEN_CONSUMER_INVALID_TYPE, {Consumer}, ErrCtx)).

-define(ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, ?ERR(?ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE)).
-define(ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED(ErrCtx), ?ERR(?ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED_TYPE, undefined, ErrCtx)).

-define(ERR_INVITE_TOKEN_TARGET_ID_INVALID(Id), ?ERR(?ERR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE, {Id})).
-define(ERR_INVITE_TOKEN_TARGET_ID_INVALID(ErrCtx, Id), ?ERR(?ERR_INVITE_TOKEN_TARGET_ID_INVALID_TYPE, {Id}, ErrCtx)).

-define(ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED, ?ERR(?ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE)).
-define(ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED(ErrCtx), ?ERR(?ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED_TYPE, undefined, ErrCtx)).

-define(ERR_NOT_AN_ACCESS_TOKEN(Received), ?ERR(?ERR_NOT_AN_ACCESS_TOKEN_TYPE, {Received})).
-define(ERR_NOT_AN_ACCESS_TOKEN(ErrCtx, Received), ?ERR(?ERR_NOT_AN_ACCESS_TOKEN_TYPE, {Received}, ErrCtx)).

-define(ERR_NOT_AN_IDENTITY_TOKEN(Received), ?ERR(?ERR_NOT_AN_IDENTITY_TOKEN_TYPE, {Received})).
-define(ERR_NOT_AN_IDENTITY_TOKEN(ErrCtx, Received), ?ERR(?ERR_NOT_AN_IDENTITY_TOKEN_TYPE, {Received}, ErrCtx)).

-define(ERR_NOT_AN_INVITE_TOKEN(ExpectedInviteType, Received), ?ERR(?ERR_NOT_AN_INVITE_TOKEN_TYPE, {ExpectedInviteType, Received})).
-define(ERR_NOT_AN_INVITE_TOKEN(ErrCtx, ExpectedInviteType, Received), ?ERR(?ERR_NOT_AN_INVITE_TOKEN_TYPE, {ExpectedInviteType, Received}, ErrCtx)).

-define(ERR_TOKEN_CAVEAT_UNKNOWN(Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNKNOWN_TYPE, {Caveat})).
-define(ERR_TOKEN_CAVEAT_UNKNOWN(ErrCtx, Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNKNOWN_TYPE, {Caveat}, ErrCtx)).

-define(ERR_TOKEN_CAVEAT_UNVERIFIED(Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNVERIFIED_TYPE, {Caveat})).
-define(ERR_TOKEN_CAVEAT_UNVERIFIED(ErrCtx, Caveat), ?ERR(?ERR_TOKEN_CAVEAT_UNVERIFIED_TYPE, {Caveat}, ErrCtx)).

-define(ERR_TOKEN_INVALID, ?ERR(?ERR_TOKEN_INVALID_TYPE)).
-define(ERR_TOKEN_INVALID(ErrCtx), ?ERR(?ERR_TOKEN_INVALID_TYPE, undefined, ErrCtx)).

-define(ERR_TOKEN_REVOKED, ?ERR(?ERR_TOKEN_REVOKED_TYPE)).
-define(ERR_TOKEN_REVOKED(ErrCtx), ?ERR(?ERR_TOKEN_REVOKED_TYPE, undefined, ErrCtx)).

-define(ERR_TOKEN_SERVICE_FORBIDDEN(Service), ?ERR(?ERR_TOKEN_SERVICE_FORBIDDEN_TYPE, {Service})).
-define(ERR_TOKEN_SERVICE_FORBIDDEN(ErrCtx, Service), ?ERR(?ERR_TOKEN_SERVICE_FORBIDDEN_TYPE, {Service}, ErrCtx)).

-define(ERR_TOKEN_SESSION_INVALID, ?ERR(?ERR_TOKEN_SESSION_INVALID_TYPE)).
-define(ERR_TOKEN_SESSION_INVALID(ErrCtx), ?ERR(?ERR_TOKEN_SESSION_INVALID_TYPE, undefined, ErrCtx)).

-define(ERR_TOKEN_SUBJECT_INVALID, ?ERR(?ERR_TOKEN_SUBJECT_INVALID_TYPE)).
-define(ERR_TOKEN_SUBJECT_INVALID(ErrCtx), ?ERR(?ERR_TOKEN_SUBJECT_INVALID_TYPE, undefined, ErrCtx)).

-define(ERR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl), ?ERR(?ERR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE, {MaxTtl})).
-define(ERR_TOKEN_TIME_CAVEAT_REQUIRED(ErrCtx, MaxTtl), ?ERR(?ERR_TOKEN_TIME_CAVEAT_REQUIRED_TYPE, {MaxTtl}, ErrCtx)).

-define(ERR_TOKEN_TOO_LARGE(Limit), ?ERR(?ERR_TOKEN_TOO_LARGE_TYPE, {Limit})).
-define(ERR_TOKEN_TOO_LARGE(ErrCtx, Limit), ?ERR(?ERR_TOKEN_TOO_LARGE_TYPE, {Limit}, ErrCtx)).


%%--------------------------------------------------------------------
%% connection errors
%%--------------------------------------------------------------------
-define(ERR_NO_CONNECTION_TO_CLUSTER_NODE(NodeName), ?ERR(?ERR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE, {NodeName})).
-define(ERR_NO_CONNECTION_TO_CLUSTER_NODE(ErrCtx, NodeName), ?ERR(?ERR_NO_CONNECTION_TO_CLUSTER_NODE_TYPE, {NodeName}, ErrCtx)).

-define(ERR_NO_CONNECTION_TO_ONEZONE(ZoneDomain), ?ERR(?ERR_NO_CONNECTION_TO_ONEZONE_TYPE, {ZoneDomain})).
-define(ERR_NO_CONNECTION_TO_ONEZONE(ErrCtx, ZoneDomain), ?ERR(?ERR_NO_CONNECTION_TO_ONEZONE_TYPE, {ZoneDomain}, ErrCtx)).

-define(ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER(ProviderId, ProviderDomain), ?ERR(?ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE, {ProviderId, ProviderDomain})).
-define(ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER(ErrCtx, ProviderId, ProviderDomain), ?ERR(?ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER_TYPE, {ProviderId, ProviderDomain}, ErrCtx)).


%%--------------------------------------------------------------------
%% data_validation errors
%%--------------------------------------------------------------------
-define(ERR_BAD_DATA(Key, SpecificErrorOrHint), ?ERR(?ERR_BAD_DATA_TYPE, {Key, SpecificErrorOrHint})).
-define(ERR_BAD_DATA(ErrCtx, Key, SpecificErrorOrHint), ?ERR(?ERR_BAD_DATA_TYPE, {Key, SpecificErrorOrHint}, ErrCtx)).

-define(ERR_BAD_GUI_PACKAGE, ?ERR(?ERR_BAD_GUI_PACKAGE_TYPE)).
-define(ERR_BAD_GUI_PACKAGE(ErrCtx), ?ERR(?ERR_BAD_GUI_PACKAGE_TYPE, undefined, ErrCtx)).

-define(ERR_GUI_PACKAGE_TOO_LARGE, ?ERR(?ERR_GUI_PACKAGE_TOO_LARGE_TYPE)).
-define(ERR_GUI_PACKAGE_TOO_LARGE(ErrCtx), ?ERR(?ERR_GUI_PACKAGE_TOO_LARGE_TYPE, undefined, ErrCtx)).

-define(ERR_GUI_PACKAGE_UNVERIFIED(ShaSum), ?ERR(?ERR_GUI_PACKAGE_UNVERIFIED_TYPE, {ShaSum})).
-define(ERR_GUI_PACKAGE_UNVERIFIED(ErrCtx, ShaSum), ?ERR(?ERR_GUI_PACKAGE_UNVERIFIED_TYPE, {ShaSum}, ErrCtx)).

-define(ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(CurrentProviderStage, CurrentStorageStage), ?ERR(?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE, {CurrentProviderStage, CurrentStorageStage})).
-define(ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ErrCtx, CurrentProviderStage, CurrentStorageStage), ?ERR(?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION_TYPE, {CurrentProviderStage, CurrentStorageStage}, ErrCtx)).

-define(ERR_INVALID_QOS_EXPRESSION(Reason), ?ERR(?ERR_INVALID_QOS_EXPRESSION_TYPE, {Reason})).
-define(ERR_INVALID_QOS_EXPRESSION(ErrCtx, Reason), ?ERR(?ERR_INVALID_QOS_EXPRESSION_TYPE, {Reason}, ErrCtx)).

-define(ERR_MALFORMED_DATA, ?ERR(?ERR_MALFORMED_DATA_TYPE)).
-define(ERR_MALFORMED_DATA(ErrCtx), ?ERR(?ERR_MALFORMED_DATA_TYPE, undefined, ErrCtx)).

-define(ERR_MISSING_AT_LEAST_ONE_VALUE(Keys), ?ERR(?ERR_MISSING_AT_LEAST_ONE_VALUE_TYPE, {Keys})).
-define(ERR_MISSING_AT_LEAST_ONE_VALUE(ErrCtx, Keys), ?ERR(?ERR_MISSING_AT_LEAST_ONE_VALUE_TYPE, {Keys}, ErrCtx)).

-define(ERR_MISSING_REQUIRED_VALUE(Key), ?ERR(?ERR_MISSING_REQUIRED_VALUE_TYPE, {Key})).
-define(ERR_MISSING_REQUIRED_VALUE(ErrCtx, Key), ?ERR(?ERR_MISSING_REQUIRED_VALUE_TYPE, {Key}, ErrCtx)).

-define(ERR_TSC_MISSING_LAYOUT(MissingLayout), ?ERR(?ERR_TSC_MISSING_LAYOUT_TYPE, {MissingLayout})).
-define(ERR_TSC_MISSING_LAYOUT(ErrCtx, MissingLayout), ?ERR(?ERR_TSC_MISSING_LAYOUT_TYPE, {MissingLayout}, ErrCtx)).

-define(ERR_TSC_TOO_MANY_METRICS(Limit), ?ERR(?ERR_TSC_TOO_MANY_METRICS_TYPE, {Limit})).
-define(ERR_TSC_TOO_MANY_METRICS(ErrCtx, Limit), ?ERR(?ERR_TSC_TOO_MANY_METRICS_TYPE, {Limit}, ErrCtx)).


%%--------------------------------------------------------------------
%% data_validation/value errors
%%--------------------------------------------------------------------
-define(ERR_BAD_VALUE_AMBIGUOUS_ID(Key), ?ERR(?ERR_BAD_VALUE_AMBIGUOUS_ID_TYPE, {Key})).
-define(ERR_BAD_VALUE_AMBIGUOUS_ID(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_AMBIGUOUS_ID_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_BOOLEAN(Key), ?ERR(?ERR_BAD_VALUE_BOOLEAN_TYPE, {Key})).
-define(ERR_BAD_VALUE_BOOLEAN(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_BOOLEAN_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_CAVEAT(Caveat), ?ERR(?ERR_BAD_VALUE_CAVEAT_TYPE, {Caveat})).
-define(ERR_BAD_VALUE_CAVEAT(ErrCtx, Caveat), ?ERR(?ERR_BAD_VALUE_CAVEAT_TYPE, {Caveat}, ErrCtx)).

-define(ERR_BAD_VALUE_DOMAIN, ?ERR(?ERR_BAD_VALUE_DOMAIN_TYPE)).
-define(ERR_BAD_VALUE_DOMAIN(ErrCtx), ?ERR(?ERR_BAD_VALUE_DOMAIN_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_EMAIL, ?ERR(?ERR_BAD_VALUE_EMAIL_TYPE)).
-define(ERR_BAD_VALUE_EMAIL(ErrCtx), ?ERR(?ERR_BAD_VALUE_EMAIL_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_EMPTY(Key), ?ERR(?ERR_BAD_VALUE_EMPTY_TYPE, {Key})).
-define(ERR_BAD_VALUE_EMPTY(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_EMPTY_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_FILE_PATH, ?ERR(?ERR_BAD_VALUE_FILE_PATH_TYPE)).
-define(ERR_BAD_VALUE_FILE_PATH(ErrCtx), ?ERR(?ERR_BAD_VALUE_FILE_PATH_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_FLOAT(Key), ?ERR(?ERR_BAD_VALUE_FLOAT_TYPE, {Key})).
-define(ERR_BAD_VALUE_FLOAT(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_FLOAT_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_FULL_NAME, ?ERR(?ERR_BAD_VALUE_FULL_NAME_TYPE)).
-define(ERR_BAD_VALUE_FULL_NAME(ErrCtx), ?ERR(?ERR_BAD_VALUE_FULL_NAME_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_ID_NOT_FOUND(Key), ?ERR(?ERR_BAD_VALUE_ID_NOT_FOUND_TYPE, {Key})).
-define(ERR_BAD_VALUE_ID_NOT_FOUND(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_ID_NOT_FOUND_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_IDENTIFIER(Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_TYPE, {Key})).
-define(ERR_BAD_VALUE_IDENTIFIER(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE, {Key})).
-define(ERR_BAD_VALUE_IDENTIFIER_OCCUPIED(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_INTEGER(Key), ?ERR(?ERR_BAD_VALUE_INTEGER_TYPE, {Key})).
-define(ERR_BAD_VALUE_INTEGER(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_INTEGER_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_INVITE_TYPE(Key), ?ERR(?ERR_BAD_VALUE_INVITE_TYPE_TYPE, {Key})).
-define(ERR_BAD_VALUE_INVITE_TYPE(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_INVITE_TYPE_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_IPV4_ADDRESS(Key), ?ERR(?ERR_BAD_VALUE_IPV4_ADDRESS_TYPE, {Key})).
-define(ERR_BAD_VALUE_IPV4_ADDRESS(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_IPV4_ADDRESS_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_JSON(Key), ?ERR(?ERR_BAD_VALUE_JSON_TYPE, {Key})).
-define(ERR_BAD_VALUE_JSON(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_JSON_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_LIST_NOT_ALLOWED(Key, Allowed), ?ERR(?ERR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE, {Key, Allowed})).
-define(ERR_BAD_VALUE_LIST_NOT_ALLOWED(ErrCtx, Key, Allowed), ?ERR(?ERR_BAD_VALUE_LIST_NOT_ALLOWED_TYPE, {Key, Allowed}, ErrCtx)).

-define(ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE, {Key})).
-define(ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_LIST_OF_STRINGS(Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_STRINGS_TYPE, {Key})).
-define(ERR_BAD_VALUE_LIST_OF_STRINGS(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_LIST_OF_STRINGS_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_NAME(Key), ?ERR(?ERR_BAD_VALUE_NAME_TYPE, {Key})).
-define(ERR_BAD_VALUE_NAME(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_NAME_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_NOT_ALLOWED(Key, Allowed), ?ERR(?ERR_BAD_VALUE_NOT_ALLOWED_TYPE, {Key, Allowed})).
-define(ERR_BAD_VALUE_NOT_ALLOWED(ErrCtx, Key, Allowed), ?ERR(?ERR_BAD_VALUE_NOT_ALLOWED_TYPE, {Key, Allowed}, ErrCtx)).

-define(ERR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High), ?ERR(?ERR_BAD_VALUE_NOT_IN_RANGE_TYPE, {Key, Low, High})).
-define(ERR_BAD_VALUE_NOT_IN_RANGE(ErrCtx, Key, Low, High), ?ERR(?ERR_BAD_VALUE_NOT_IN_RANGE_TYPE, {Key, Low, High}, ErrCtx)).

-define(ERR_BAD_VALUE_OCTAL(Key), ?ERR(?ERR_BAD_VALUE_OCTAL_TYPE, {Key})).
-define(ERR_BAD_VALUE_OCTAL(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_OCTAL_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_PASSWORD, ?ERR(?ERR_BAD_VALUE_PASSWORD_TYPE)).
-define(ERR_BAD_VALUE_PASSWORD(ErrCtx), ?ERR(?ERR_BAD_VALUE_PASSWORD_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_QOS_PARAMETERS, ?ERR(?ERR_BAD_VALUE_QOS_PARAMETERS_TYPE)).
-define(ERR_BAD_VALUE_QOS_PARAMETERS(ErrCtx), ?ERR(?ERR_BAD_VALUE_QOS_PARAMETERS_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_STRING(Key), ?ERR(?ERR_BAD_VALUE_STRING_TYPE, {Key})).
-define(ERR_BAD_VALUE_STRING(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_STRING_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_SUBDOMAIN, ?ERR(?ERR_BAD_VALUE_SUBDOMAIN_TYPE)).
-define(ERR_BAD_VALUE_SUBDOMAIN(ErrCtx), ?ERR(?ERR_BAD_VALUE_SUBDOMAIN_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_TEXT_TOO_LARGE(Key, Limit), ?ERR(?ERR_BAD_VALUE_TEXT_TOO_LARGE_TYPE, {Key, Limit})).
-define(ERR_BAD_VALUE_TEXT_TOO_LARGE(ErrCtx, Key, Limit), ?ERR(?ERR_BAD_VALUE_TEXT_TOO_LARGE_TYPE, {Key, Limit}, ErrCtx)).

-define(ERR_BAD_VALUE_TOKEN(Key, TokenError), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE, {Key, TokenError})).
-define(ERR_BAD_VALUE_TOKEN(ErrCtx, Key, TokenError), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE, {Key, TokenError}, ErrCtx)).

-define(ERR_BAD_VALUE_TOKEN_TYPE(Key), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE_TYPE, {Key})).
-define(ERR_BAD_VALUE_TOKEN_TYPE(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_TOKEN_TYPE_TYPE, {Key}, ErrCtx)).

-define(ERR_BAD_VALUE_TOO_HIGH(Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_HIGH_TYPE, {Key, Limit})).
-define(ERR_BAD_VALUE_TOO_HIGH(ErrCtx, Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_HIGH_TYPE, {Key, Limit}, ErrCtx)).

-define(ERR_BAD_VALUE_TOO_LOW(Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_LOW_TYPE, {Key, Limit})).
-define(ERR_BAD_VALUE_TOO_LOW(ErrCtx, Key, Limit), ?ERR(?ERR_BAD_VALUE_TOO_LOW_TYPE, {Key, Limit}, ErrCtx)).

-define(ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig), ?ERR(?ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE, {TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig})).
-define(ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(ErrCtx, TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig), ?ERR(?ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG_TYPE, {TimeSeriesName, MetricName, ExistingMetricConfig, ConflictingMetricConfig}, ErrCtx)).

-define(ERR_BAD_VALUE_USERNAME, ?ERR(?ERR_BAD_VALUE_USERNAME_TYPE)).
-define(ERR_BAD_VALUE_USERNAME(ErrCtx), ?ERR(?ERR_BAD_VALUE_USERNAME_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VALUE_XML(Key), ?ERR(?ERR_BAD_VALUE_XML_TYPE, {Key})).
-define(ERR_BAD_VALUE_XML(ErrCtx, Key), ?ERR(?ERR_BAD_VALUE_XML_TYPE, {Key}, ErrCtx)).


%%--------------------------------------------------------------------
%% general errors
%%--------------------------------------------------------------------
-define(ERR_ALREADY_EXISTS, ?ERR(?ERR_ALREADY_EXISTS_TYPE)).
-define(ERR_ALREADY_EXISTS(ErrCtx), ?ERR(?ERR_ALREADY_EXISTS_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_MESSAGE(Message), ?ERR(?ERR_BAD_MESSAGE_TYPE, {Message})).
-define(ERR_BAD_MESSAGE(ErrCtx, Message), ?ERR(?ERR_BAD_MESSAGE_TYPE, {Message}, ErrCtx)).

-define(ERR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName), ?ERR(?ERR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE, {ServiceName})).
-define(ERR_EXTERNAL_SERVICE_OPERATION_FAILED(ErrCtx, ServiceName), ?ERR(?ERR_EXTERNAL_SERVICE_OPERATION_FAILED_TYPE, {ServiceName}, ErrCtx)).

-define(ERR_FILE_ACCESS(Path, Errno), ?ERR(?ERR_FILE_ACCESS_TYPE, {Path, Errno})).
-define(ERR_FILE_ACCESS(ErrCtx, Path, Errno), ?ERR(?ERR_FILE_ACCESS_TYPE, {Path, Errno}, ErrCtx)).

-define(ERR_INTERNAL_SERVER_ERROR(Reference), ?ERR(?ERR_INTERNAL_SERVER_ERROR_TYPE, {Reference})).
-define(ERR_INTERNAL_SERVER_ERROR(ErrCtx, Reference), ?ERR(?ERR_INTERNAL_SERVER_ERROR_TYPE, {Reference}, ErrCtx)).

-define(ERR_LIMIT_REACHED(Limit, ResourceDescription), ?ERR(?ERR_LIMIT_REACHED_TYPE, {Limit, ResourceDescription})).
-define(ERR_LIMIT_REACHED(ErrCtx, Limit, ResourceDescription), ?ERR(?ERR_LIMIT_REACHED_TYPE, {Limit, ResourceDescription}, ErrCtx)).

-define(ERR_NOT_FOUND, ?ERR(?ERR_NOT_FOUND_TYPE)).
-define(ERR_NOT_FOUND(ErrCtx), ?ERR(?ERR_NOT_FOUND_TYPE, undefined, ErrCtx)).

-define(ERR_NOT_IMPLEMENTED, ?ERR(?ERR_NOT_IMPLEMENTED_TYPE)).
-define(ERR_NOT_IMPLEMENTED(ErrCtx), ?ERR(?ERR_NOT_IMPLEMENTED_TYPE, undefined, ErrCtx)).

-define(ERR_NOT_SUPPORTED, ?ERR(?ERR_NOT_SUPPORTED_TYPE)).
-define(ERR_NOT_SUPPORTED(ErrCtx), ?ERR(?ERR_NOT_SUPPORTED_TYPE, undefined, ErrCtx)).

-define(ERR_SERVICE_UNAVAILABLE, ?ERR(?ERR_SERVICE_UNAVAILABLE_TYPE)).
-define(ERR_SERVICE_UNAVAILABLE(ErrCtx), ?ERR(?ERR_SERVICE_UNAVAILABLE_TYPE, undefined, ErrCtx)).

-define(ERR_TEMPORARY_FAILURE, ?ERR(?ERR_TEMPORARY_FAILURE_TYPE)).
-define(ERR_TEMPORARY_FAILURE(ErrCtx), ?ERR(?ERR_TEMPORARY_FAILURE_TYPE, undefined, ErrCtx)).

-define(ERR_TIMEOUT, ?ERR(?ERR_TIMEOUT_TYPE)).
-define(ERR_TIMEOUT(ErrCtx), ?ERR(?ERR_TIMEOUT_TYPE, undefined, ErrCtx)).

-define(ERR_UNREGISTERED_ONEPROVIDER, ?ERR(?ERR_UNREGISTERED_ONEPROVIDER_TYPE)).
-define(ERR_UNREGISTERED_ONEPROVIDER(ErrCtx), ?ERR(?ERR_UNREGISTERED_ONEPROVIDER_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% graph_sync errors
%%--------------------------------------------------------------------
-define(ERR_BAD_GRI, ?ERR(?ERR_BAD_GRI_TYPE)).
-define(ERR_BAD_GRI(ErrCtx), ?ERR(?ERR_BAD_GRI_TYPE, undefined, ErrCtx)).

-define(ERR_BAD_VERSION(SupportedVersions), ?ERR(?ERR_BAD_VERSION_TYPE, {SupportedVersions})).
-define(ERR_BAD_VERSION(ErrCtx, SupportedVersions), ?ERR(?ERR_BAD_VERSION_TYPE, {SupportedVersions}, ErrCtx)).

-define(ERR_EXPECTED_HANDSHAKE_MESSAGE, ?ERR(?ERR_EXPECTED_HANDSHAKE_MESSAGE_TYPE)).
-define(ERR_EXPECTED_HANDSHAKE_MESSAGE(ErrCtx), ?ERR(?ERR_EXPECTED_HANDSHAKE_MESSAGE_TYPE, undefined, ErrCtx)).

-define(ERR_HANDSHAKE_ALREADY_DONE, ?ERR(?ERR_HANDSHAKE_ALREADY_DONE_TYPE)).
-define(ERR_HANDSHAKE_ALREADY_DONE(ErrCtx), ?ERR(?ERR_HANDSHAKE_ALREADY_DONE_TYPE, undefined, ErrCtx)).

-define(ERR_NOT_SUBSCRIBABLE, ?ERR(?ERR_NOT_SUBSCRIBABLE_TYPE)).
-define(ERR_NOT_SUBSCRIBABLE(ErrCtx), ?ERR(?ERR_NOT_SUBSCRIBABLE_TYPE, undefined, ErrCtx)).

-define(ERR_RPC_UNDEFINED, ?ERR(?ERR_RPC_UNDEFINED_TYPE)).
-define(ERR_RPC_UNDEFINED(ErrCtx), ?ERR(?ERR_RPC_UNDEFINED_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
-define(ERR_DNS_SERVERS_UNREACHABLE(Servers), ?ERR(?ERR_DNS_SERVERS_UNREACHABLE_TYPE, {Servers})).
-define(ERR_DNS_SERVERS_UNREACHABLE(ErrCtx, Servers), ?ERR(?ERR_DNS_SERVERS_UNREACHABLE_TYPE, {Servers}, ErrCtx)).

-define(ERR_LETS_ENCRYPT_NOT_REACHABLE, ?ERR(?ERR_LETS_ENCRYPT_NOT_REACHABLE_TYPE)).
-define(ERR_LETS_ENCRYPT_NOT_REACHABLE(ErrCtx), ?ERR(?ERR_LETS_ENCRYPT_NOT_REACHABLE_TYPE, undefined, ErrCtx)).

-define(ERR_LETS_ENCRYPT_RESPONSE(ProblemDocument, ErrorMessage), ?ERR(?ERR_LETS_ENCRYPT_RESPONSE_TYPE, {ProblemDocument, ErrorMessage})).
-define(ERR_LETS_ENCRYPT_RESPONSE(ErrCtx, ProblemDocument, ErrorMessage), ?ERR(?ERR_LETS_ENCRYPT_RESPONSE_TYPE, {ProblemDocument, ErrorMessage}, ErrCtx)).

-define(ERR_NO_CONNECTION_TO_NEW_NODE(Hostname), ?ERR(?ERR_NO_CONNECTION_TO_NEW_NODE_TYPE, {Hostname})).
-define(ERR_NO_CONNECTION_TO_NEW_NODE(ErrCtx, Hostname), ?ERR(?ERR_NO_CONNECTION_TO_NEW_NODE_TYPE, {Hostname}, ErrCtx)).

-define(ERR_NO_SERVICE_NODES(Service), ?ERR(?ERR_NO_SERVICE_NODES_TYPE, {Service})).
-define(ERR_NO_SERVICE_NODES(ErrCtx, Service), ?ERR(?ERR_NO_SERVICE_NODES_TYPE, {Service}, ErrCtx)).

-define(ERR_NODE_ALREADY_IN_CLUSTER(Hostname), ?ERR(?ERR_NODE_ALREADY_IN_CLUSTER_TYPE, {Hostname})).
-define(ERR_NODE_ALREADY_IN_CLUSTER(ErrCtx, Hostname), ?ERR(?ERR_NODE_ALREADY_IN_CLUSTER_TYPE, {Hostname}, ErrCtx)).

-define(ERR_NODE_NOT_COMPATIBLE(Hostname, ClusterType), ?ERR(?ERR_NODE_NOT_COMPATIBLE_TYPE, {Hostname, ClusterType})).
-define(ERR_NODE_NOT_COMPATIBLE(ErrCtx, Hostname, ClusterType), ?ERR(?ERR_NODE_NOT_COMPATIBLE_TYPE, {Hostname, ClusterType}, ErrCtx)).

-define(ERR_ON_NODES(Error, Hostnames), ?ERR(?ERR_ON_NODES_TYPE, {Error, Hostnames})).
-define(ERR_ON_NODES(ErrCtx, Error, Hostnames), ?ERR(?ERR_ON_NODES_TYPE, {Error, Hostnames}, ErrCtx)).

-define(ERR_USER_NOT_IN_CLUSTER, ?ERR(?ERR_USER_NOT_IN_CLUSTER_TYPE)).
-define(ERR_USER_NOT_IN_CLUSTER(ErrCtx), ?ERR(?ERR_USER_NOT_IN_CLUSTER_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
-define(ERR_AUTO_CLEANING_DISABLED, ?ERR(?ERR_AUTO_CLEANING_DISABLED_TYPE)).
-define(ERR_AUTO_CLEANING_DISABLED(ErrCtx), ?ERR(?ERR_AUTO_CLEANING_DISABLED_TYPE, undefined, ErrCtx)).

-define(ERR_FILE_POPULARITY_DISABLED, ?ERR(?ERR_FILE_POPULARITY_DISABLED_TYPE)).
-define(ERR_FILE_POPULARITY_DISABLED(ErrCtx), ?ERR(?ERR_FILE_POPULARITY_DISABLED_TYPE, undefined, ErrCtx)).

-define(ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(CurrentState, AllowedStates), ?ERR(?ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE, {CurrentState, AllowedStates})).
-define(ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(ErrCtx, CurrentState, AllowedStates), ?ERR(?ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_TYPE, {CurrentState, AllowedStates}, ErrCtx)).

-define(ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ParentArchiveId), ?ERR(?ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE, {ParentArchiveId})).
-define(ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ErrCtx, ParentArchiveId), ?ERR(?ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN_TYPE, {ParentArchiveId}, ErrCtx)).

-define(ERR_QUOTA_EXCEEDED, ?ERR(?ERR_QUOTA_EXCEEDED_TYPE)).
-define(ERR_QUOTA_EXCEEDED(ErrCtx), ?ERR(?ERR_QUOTA_EXCEEDED_TYPE, undefined, ErrCtx)).

-define(ERR_RECALL_TARGET_CONFLICT, ?ERR(?ERR_RECALL_TARGET_CONFLICT_TYPE)).
-define(ERR_RECALL_TARGET_CONFLICT(ErrCtx), ?ERR(?ERR_RECALL_TARGET_CONFLICT_TYPE, undefined, ErrCtx)).

-define(ERR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId), ?ERR(?ERR_SPACE_NOT_SUPPORTED_BY_TYPE, {SpaceId, ProviderId})).
-define(ERR_SPACE_NOT_SUPPORTED_BY(ErrCtx, SpaceId, ProviderId), ?ERR(?ERR_SPACE_NOT_SUPPORTED_BY_TYPE, {SpaceId, ProviderId}, ErrCtx)).

-define(ERR_STAT_OPERATION_NOT_SUPPORTED(StorageId), ?ERR(?ERR_STAT_OPERATION_NOT_SUPPORTED_TYPE, {StorageId})).
-define(ERR_STAT_OPERATION_NOT_SUPPORTED(ErrCtx, StorageId), ?ERR(?ERR_STAT_OPERATION_NOT_SUPPORTED_TYPE, {StorageId}, ErrCtx)).

-define(ERR_USER_NOT_SUPPORTED, ?ERR(?ERR_USER_NOT_SUPPORTED_TYPE)).
-define(ERR_USER_NOT_SUPPORTED(ErrCtx), ?ERR(?ERR_USER_NOT_SUPPORTED_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% op_worker/atm errors
%%--------------------------------------------------------------------
-define(ERR_ATM_DATA_TYPE_UNVERIFIED(Value, ExpType), ?ERR(?ERR_ATM_DATA_TYPE_UNVERIFIED_TYPE, {Value, ExpType})).
-define(ERR_ATM_DATA_TYPE_UNVERIFIED(ErrCtx, Value, ExpType), ?ERR(?ERR_ATM_DATA_TYPE_UNVERIFIED_TYPE, {Value, ExpType}, ErrCtx)).

-define(ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(Value, Type, ValueConstraints), ?ERR(?ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE, {Value, Type, ValueConstraints})).
-define(ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(ErrCtx, Value, Type, ValueConstraints), ?ERR(?ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED_TYPE, {Value, Type, ValueConstraints}, ErrCtx)).

-define(ERR_ATM_INVALID_STATUS_TRANSITION(PrevStatus, NewStatus), ?ERR(?ERR_ATM_INVALID_STATUS_TRANSITION_TYPE, {PrevStatus, NewStatus})).
-define(ERR_ATM_INVALID_STATUS_TRANSITION(ErrCtx, PrevStatus, NewStatus), ?ERR(?ERR_ATM_INVALID_STATUS_TRANSITION_TYPE, {PrevStatus, NewStatus}, ErrCtx)).

-define(ERR_ATM_JOB_BATCH_CRASHED(Reason), ?ERR(?ERR_ATM_JOB_BATCH_CRASHED_TYPE, {Reason})).
-define(ERR_ATM_JOB_BATCH_CRASHED(ErrCtx, Reason), ?ERR(?ERR_ATM_JOB_BATCH_CRASHED_TYPE, {Reason}, ErrCtx)).

-define(ERR_ATM_JOB_BATCH_WITHDRAWN(Reason), ?ERR(?ERR_ATM_JOB_BATCH_WITHDRAWN_TYPE, {Reason})).
-define(ERR_ATM_JOB_BATCH_WITHDRAWN(ErrCtx, Reason), ?ERR(?ERR_ATM_JOB_BATCH_WITHDRAWN_TYPE, {Reason}, ErrCtx)).

-define(ERR_ATM_LAMBDA_CONFIG_BAD_VALUE(ParameterName, SpecificError), ?ERR(?ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE, {ParameterName, SpecificError})).
-define(ERR_ATM_LAMBDA_CONFIG_BAD_VALUE(ErrCtx, ParameterName, SpecificError), ?ERR(?ERR_ATM_LAMBDA_CONFIG_BAD_VALUE_TYPE, {ParameterName, SpecificError}, ErrCtx)).

-define(ERR_ATM_LANE_EMPTY(AtmLaneSchemaId), ?ERR(?ERR_ATM_LANE_EMPTY_TYPE, {AtmLaneSchemaId})).
-define(ERR_ATM_LANE_EMPTY(ErrCtx, AtmLaneSchemaId), ?ERR(?ERR_ATM_LANE_EMPTY_TYPE, {AtmLaneSchemaId}, ErrCtx)).

-define(ERR_ATM_LANE_EXECUTION_CREATION_FAILED(AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError})).
-define(ERR_ATM_LANE_EXECUTION_CREATION_FAILED(ErrCtx, AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_CREATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_LANE_EXECUTION_INITIATION_FAILED(AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError})).
-define(ERR_ATM_LANE_EXECUTION_INITIATION_FAILED(ErrCtx, AtmLaneSchemaId, SpecificError), ?ERR(?ERR_ATM_LANE_EXECUTION_INITIATION_FAILED_TYPE, {AtmLaneSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_LANE_EXECUTION_RERUN_FAILED, ?ERR(?ERR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE)).
-define(ERR_ATM_LANE_EXECUTION_RERUN_FAILED(ErrCtx), ?ERR(?ERR_ATM_LANE_EXECUTION_RERUN_FAILED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_LANE_EXECUTION_RETRY_FAILED, ?ERR(?ERR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE)).
-define(ERR_ATM_LANE_EXECUTION_RETRY_FAILED(ErrCtx), ?ERR(?ERR_ATM_LANE_EXECUTION_RETRY_FAILED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED, ?ERR(?ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE)).
-define(ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED(ErrCtx), ?ERR(?ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_OPENFAAS_NOT_CONFIGURED, ?ERR(?ERR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE)).
-define(ERR_ATM_OPENFAAS_NOT_CONFIGURED(ErrCtx), ?ERR(?ERR_ATM_OPENFAAS_NOT_CONFIGURED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_OPENFAAS_QUERY_FAILED(Reason), ?ERR(?ERR_ATM_OPENFAAS_QUERY_FAILED_TYPE, {Reason})).
-define(ERR_ATM_OPENFAAS_QUERY_FAILED(ErrCtx, Reason), ?ERR(?ERR_ATM_OPENFAAS_QUERY_FAILED_TYPE, {Reason}, ErrCtx)).

-define(ERR_ATM_OPENFAAS_UNHEALTHY, ?ERR(?ERR_ATM_OPENFAAS_UNHEALTHY_TYPE)).
-define(ERR_ATM_OPENFAAS_UNHEALTHY(ErrCtx), ?ERR(?ERR_ATM_OPENFAAS_UNHEALTHY_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_OPENFAAS_UNREACHABLE, ?ERR(?ERR_ATM_OPENFAAS_UNREACHABLE_TYPE)).
-define(ERR_ATM_OPENFAAS_UNREACHABLE(ErrCtx), ?ERR(?ERR_ATM_OPENFAAS_UNREACHABLE_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_PARALLEL_BOX_EMPTY(AtmParallelBoxSchemaId), ?ERR(?ERR_ATM_PARALLEL_BOX_EMPTY_TYPE, {AtmParallelBoxSchemaId})).
-define(ERR_ATM_PARALLEL_BOX_EMPTY(ErrCtx, AtmParallelBoxSchemaId), ?ERR(?ERR_ATM_PARALLEL_BOX_EMPTY_TYPE, {AtmParallelBoxSchemaId}, ErrCtx)).

-define(ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError})).
-define(ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(ErrCtx, AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError})).
-define(ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(ErrCtx, AtmParallelBoxSchemaId, SpecificError), ?ERR(?ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED_TYPE, {AtmParallelBoxSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_STORE_CONTENT_NOT_SET(AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_CONTENT_NOT_SET_TYPE, {AtmStoreSchemaId})).
-define(ERR_ATM_STORE_CONTENT_NOT_SET(ErrCtx, AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_CONTENT_NOT_SET_TYPE, {AtmStoreSchemaId}, ErrCtx)).

-define(ERR_ATM_STORE_CREATION_FAILED(AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_STORE_CREATION_FAILED_TYPE, {AtmStoreSchemaId, SpecificError})).
-define(ERR_ATM_STORE_CREATION_FAILED(ErrCtx, AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_STORE_CREATION_FAILED_TYPE, {AtmStoreSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_STORE_FROZEN(AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_FROZEN_TYPE, {AtmStoreSchemaId})).
-define(ERR_ATM_STORE_FROZEN(ErrCtx, AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_FROZEN_TYPE, {AtmStoreSchemaId}, ErrCtx)).

-define(ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT, ?ERR(?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE)).
-define(ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT(ErrCtx), ?ERR(?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_STORE_NOT_FOUND(AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_NOT_FOUND_TYPE, {AtmStoreSchemaId})).
-define(ERR_ATM_STORE_NOT_FOUND(ErrCtx, AtmStoreSchemaId), ?ERR(?ERR_ATM_STORE_NOT_FOUND_TYPE, {AtmStoreSchemaId}, ErrCtx)).

-define(ERR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, Allowed), ?ERR(?ERR_ATM_STORE_TYPE_DISALLOWED_TYPE, {AtmStoreSchemaId, Allowed})).
-define(ERR_ATM_STORE_TYPE_DISALLOWED(ErrCtx, AtmStoreSchemaId, Allowed), ?ERR(?ERR_ATM_STORE_TYPE_DISALLOWED_TYPE, {AtmStoreSchemaId, Allowed}, ErrCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE, {Argument})).
-define(ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(ErrCtx, Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_TYPE, {Argument}, ErrCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE, {Argument})).
-define(ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(ErrCtx, Argument), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING_TYPE, {Argument}, ErrCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(Value, Query), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE, {Value, Query})).
-define(ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(ErrCtx, Value, Query), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_TYPE, {Value, Query}, ErrCtx)).

-define(ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, Supported), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE, {Type, Supported})).
-define(ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(ErrCtx, Type, Supported), ?ERR(?ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_TYPE, {Type, Supported}, ErrCtx)).

-define(ERR_ATM_TASK_ARG_MAPPING_FAILED(Argument, SpecificError), ?ERR(?ERR_ATM_TASK_ARG_MAPPING_FAILED_TYPE, {Argument, SpecificError})).
-define(ERR_ATM_TASK_ARG_MAPPING_FAILED(ErrCtx, Argument, SpecificError), ?ERR(?ERR_ATM_TASK_ARG_MAPPING_FAILED_TYPE, {Argument, SpecificError}, ErrCtx)).

-define(ERR_ATM_TASK_EXECUTION_CREATION_FAILED(AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError})).
-define(ERR_ATM_TASK_EXECUTION_CREATION_FAILED(ErrCtx, AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_CREATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_TASK_EXECUTION_INITIATION_FAILED(AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError})).
-define(ERR_ATM_TASK_EXECUTION_INITIATION_FAILED(ErrCtx, AtmTaskSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_EXECUTION_INITIATION_FAILED_TYPE, {AtmTaskSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_TASK_EXECUTION_STOPPED, ?ERR(?ERR_ATM_TASK_EXECUTION_STOPPED_TYPE)).
-define(ERR_ATM_TASK_EXECUTION_STOPPED(ErrCtx), ?ERR(?ERR_ATM_TASK_EXECUTION_STOPPED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_TASK_RESULT_DISPATCH_FAILED(AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE, {AtmStoreSchemaId, SpecificError})).
-define(ERR_ATM_TASK_RESULT_DISPATCH_FAILED(ErrCtx, AtmStoreSchemaId, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_DISPATCH_FAILED_TYPE, {AtmStoreSchemaId, SpecificError}, ErrCtx)).

-define(ERR_ATM_TASK_RESULT_MAPPING_FAILED(Result, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE, {Result, SpecificError})).
-define(ERR_ATM_TASK_RESULT_MAPPING_FAILED(ErrCtx, Result, SpecificError), ?ERR(?ERR_ATM_TASK_RESULT_MAPPING_FAILED_TYPE, {Result, SpecificError}, ErrCtx)).

-define(ERR_ATM_TASK_RESULT_MISSING(MissingResultName, ReceivedResultNames), ?ERR(?ERR_ATM_TASK_RESULT_MISSING_TYPE, {MissingResultName, ReceivedResultNames})).
-define(ERR_ATM_TASK_RESULT_MISSING(ErrCtx, MissingResultName, ReceivedResultNames), ?ERR(?ERR_ATM_TASK_RESULT_MISSING_TYPE, {MissingResultName, ReceivedResultNames}, ErrCtx)).

-define(ERR_ATM_UNSUPPORTED_DATA_TYPE(Type, Allowed), ?ERR(?ERR_ATM_UNSUPPORTED_DATA_TYPE_TYPE, {Type, Allowed})).
-define(ERR_ATM_UNSUPPORTED_DATA_TYPE(ErrCtx, Type, Allowed), ?ERR(?ERR_ATM_UNSUPPORTED_DATA_TYPE_TYPE, {Type, Allowed}, ErrCtx)).

-define(ERR_ATM_WORKFLOW_EMPTY, ?ERR(?ERR_ATM_WORKFLOW_EMPTY_TYPE)).
-define(ERR_ATM_WORKFLOW_EMPTY(ErrCtx), ?ERR(?ERR_ATM_WORKFLOW_EMPTY_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_ENDED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_ENDED(ErrCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_ENDED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED(ErrCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE(ErrCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED(ErrCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPED, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPED(ErrCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPED_TYPE, undefined, ErrCtx)).

-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPING, ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE)).
-define(ERR_ATM_WORKFLOW_EXECUTION_STOPPING(ErrCtx), ?ERR(?ERR_ATM_WORKFLOW_EXECUTION_STOPPING_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% op_worker/dir_stats errors
%%--------------------------------------------------------------------
-define(ERR_DIR_STATS_DISABLED_FOR_SPACE, ?ERR(?ERR_DIR_STATS_DISABLED_FOR_SPACE_TYPE)).
-define(ERR_DIR_STATS_DISABLED_FOR_SPACE(ErrCtx), ?ERR(?ERR_DIR_STATS_DISABLED_FOR_SPACE_TYPE, undefined, ErrCtx)).

-define(ERR_DIR_STATS_NOT_READY, ?ERR(?ERR_DIR_STATS_NOT_READY_TYPE)).
-define(ERR_DIR_STATS_NOT_READY(ErrCtx), ?ERR(?ERR_DIR_STATS_NOT_READY_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% op_worker/storage errors
%%--------------------------------------------------------------------
-define(ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages), ?ERR(?ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, SupportedStorages, SupportedObjectStorages})).
-define(ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(ErrCtx, StorageId, SupportedStorages, SupportedObjectStorages), ?ERR(?ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, SupportedStorages, SupportedObjectStorages}, ErrCtx)).

-define(ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId), ?ERR(?ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE, {ProviderId, StorageId, SpaceId})).
-define(ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ErrCtx, ProviderId, StorageId, SpaceId), ?ERR(?ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE_TYPE, {ProviderId, StorageId, SpaceId}, ErrCtx)).

-define(ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE, ?ERR(?ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE)).
-define(ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE(ErrCtx), ?ERR(?ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE_TYPE, undefined, ErrCtx)).

-define(ERR_REQUIRES_IMPORTED_STORAGE(StorageId), ?ERR(?ERR_REQUIRES_IMPORTED_STORAGE_TYPE, {StorageId})).
-define(ERR_REQUIRES_IMPORTED_STORAGE(ErrCtx, StorageId), ?ERR(?ERR_REQUIRES_IMPORTED_STORAGE_TYPE, {StorageId}, ErrCtx)).

-define(ERR_REQUIRES_NON_IMPORTED_STORAGE(StorageId), ?ERR(?ERR_REQUIRES_NON_IMPORTED_STORAGE_TYPE, {StorageId})).
-define(ERR_REQUIRES_NON_IMPORTED_STORAGE(ErrCtx, StorageId), ?ERR(?ERR_REQUIRES_NON_IMPORTED_STORAGE_TYPE, {StorageId}, ErrCtx)).

-define(ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE(StorageId, PosixCompatibleStorages), ?ERR(?ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE, {StorageId, PosixCompatibleStorages})).
-define(ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE(ErrCtx, StorageId, PosixCompatibleStorages), ?ERR(?ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE_TYPE, {StorageId, PosixCompatibleStorages}, ErrCtx)).

-define(ERR_REQUIRES_READONLY_STORAGE(StorageIdOrType), ?ERR(?ERR_REQUIRES_READONLY_STORAGE_TYPE, {StorageIdOrType})).
-define(ERR_REQUIRES_READONLY_STORAGE(ErrCtx, StorageIdOrType), ?ERR(?ERR_REQUIRES_READONLY_STORAGE_TYPE, {StorageIdOrType}, ErrCtx)).

-define(ERR_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, ObjectStorages), ?ERR(?ERR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, ObjectStorages})).
-define(ERR_STORAGE_IMPORT_NOT_SUPPORTED(ErrCtx, StorageId, ObjectStorages), ?ERR(?ERR_STORAGE_IMPORT_NOT_SUPPORTED_TYPE, {StorageId, ObjectStorages}, ErrCtx)).

-define(ERR_STORAGE_IN_USE, ?ERR(?ERR_STORAGE_IN_USE_TYPE)).
-define(ERR_STORAGE_IN_USE(ErrCtx), ?ERR(?ERR_STORAGE_IN_USE_TYPE, undefined, ErrCtx)).

-define(ERR_STORAGE_TEST_FAILED(Operation), ?ERR(?ERR_STORAGE_TEST_FAILED_TYPE, {Operation})).
-define(ERR_STORAGE_TEST_FAILED(ErrCtx, Operation), ?ERR(?ERR_STORAGE_TEST_FAILED_TYPE, {Operation}, ErrCtx)).


%%--------------------------------------------------------------------
%% op_worker/transfer errors
%%--------------------------------------------------------------------
-define(ERR_TRANSFER_ALREADY_ENDED, ?ERR(?ERR_TRANSFER_ALREADY_ENDED_TYPE)).
-define(ERR_TRANSFER_ALREADY_ENDED(ErrCtx), ?ERR(?ERR_TRANSFER_ALREADY_ENDED_TYPE, undefined, ErrCtx)).

-define(ERR_TRANSFER_NOT_ENDED, ?ERR(?ERR_TRANSFER_NOT_ENDED_TYPE)).
-define(ERR_TRANSFER_NOT_ENDED(ErrCtx), ?ERR(?ERR_TRANSFER_NOT_ENDED_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% op_worker/view errors
%%--------------------------------------------------------------------
-define(ERR_VIEW_NOT_EXISTS_ON(ProviderId), ?ERR(?ERR_VIEW_NOT_EXISTS_ON_TYPE, {ProviderId})).
-define(ERR_VIEW_NOT_EXISTS_ON(ErrCtx, ProviderId), ?ERR(?ERR_VIEW_NOT_EXISTS_ON_TYPE, {ProviderId}, ErrCtx)).

-define(ERR_VIEW_QUERY_FAILED(Category, Description), ?ERR(?ERR_VIEW_QUERY_FAILED_TYPE, {Category, Description})).
-define(ERR_VIEW_QUERY_FAILED(ErrCtx, Category, Description), ?ERR(?ERR_VIEW_QUERY_FAILED_TYPE, {Category, Description}, ErrCtx)).


%%--------------------------------------------------------------------
%% oz_worker errors
%%--------------------------------------------------------------------
-define(ERR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas), ?ERR(?ERR_ATM_LAMBDA_IN_USE_TYPE, {AtmWorkflowSchemas})).
-define(ERR_ATM_LAMBDA_IN_USE(ErrCtx, AtmWorkflowSchemas), ?ERR(?ERR_ATM_LAMBDA_IN_USE_TYPE, {AtmWorkflowSchemas}, ErrCtx)).

-define(ERR_BASIC_AUTH_DISABLED, ?ERR(?ERR_BASIC_AUTH_DISABLED_TYPE)).
-define(ERR_BASIC_AUTH_DISABLED(ErrCtx), ?ERR(?ERR_BASIC_AUTH_DISABLED_TYPE, undefined, ErrCtx)).

-define(ERR_BASIC_AUTH_NOT_SUPPORTED, ?ERR(?ERR_BASIC_AUTH_NOT_SUPPORTED_TYPE)).
-define(ERR_BASIC_AUTH_NOT_SUPPORTED(ErrCtx), ?ERR(?ERR_BASIC_AUTH_NOT_SUPPORTED_TYPE, undefined, ErrCtx)).

-define(ERR_CANNOT_ADD_RELATION_TO_SELF, ?ERR(?ERR_CANNOT_ADD_RELATION_TO_SELF_TYPE)).
-define(ERR_CANNOT_ADD_RELATION_TO_SELF(ErrCtx), ?ERR(?ERR_CANNOT_ADD_RELATION_TO_SELF_TYPE, undefined, ErrCtx)).

-define(ERR_CANNOT_DELETE_ENTITY(EntityType, EntityId), ?ERR(?ERR_CANNOT_DELETE_ENTITY_TYPE, {EntityType, EntityId})).
-define(ERR_CANNOT_DELETE_ENTITY(ErrCtx, EntityType, EntityId), ?ERR(?ERR_CANNOT_DELETE_ENTITY_TYPE, {EntityType, EntityId}, ErrCtx)).

-define(ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE, ?ERR(?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE)).
-define(ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE(ErrCtx), ?ERR(?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE_TYPE, undefined, ErrCtx)).

-define(ERR_CANNOT_REMOVE_LAST_OWNER(EntityType, EntityId), ?ERR(?ERR_CANNOT_REMOVE_LAST_OWNER_TYPE, {EntityType, EntityId})).
-define(ERR_CANNOT_REMOVE_LAST_OWNER(ErrCtx, EntityType, EntityId), ?ERR(?ERR_CANNOT_REMOVE_LAST_OWNER_TYPE, {EntityType, EntityId}, ErrCtx)).

-define(ERR_PROTECTED_GROUP, ?ERR(?ERR_PROTECTED_GROUP_TYPE)).
-define(ERR_PROTECTED_GROUP(ErrCtx), ?ERR(?ERR_PROTECTED_GROUP_TYPE, undefined, ErrCtx)).

-define(ERR_RELATION_ALREADY_EXISTS(ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_ALREADY_EXISTS_TYPE, {ChildType, ChildId, ParentType, ParentId})).
-define(ERR_RELATION_ALREADY_EXISTS(ErrCtx, ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_ALREADY_EXISTS_TYPE, {ChildType, ChildId, ParentType, ParentId}, ErrCtx)).

-define(ERR_RELATION_DOES_NOT_EXIST(ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_DOES_NOT_EXIST_TYPE, {ChildType, ChildId, ParentType, ParentId})).
-define(ERR_RELATION_DOES_NOT_EXIST(ErrCtx, ChildType, ChildId, ParentType, ParentId), ?ERR(?ERR_RELATION_DOES_NOT_EXIST_TYPE, {ChildType, ChildId, ParentType, ParentId}, ErrCtx)).


%%--------------------------------------------------------------------
%% oz_worker/space errors
%%--------------------------------------------------------------------
-define(ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId), ?ERR(?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE, {SpaceId, StorageId})).
-define(ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(ErrCtx, SpaceId, StorageId), ?ERR(?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_TYPE, {SpaceId, StorageId}, ErrCtx)).

-define(ERR_SPACE_MARKETPLACE_DISABLED, ?ERR(?ERR_SPACE_MARKETPLACE_DISABLED_TYPE)).
-define(ERR_SPACE_MARKETPLACE_DISABLED(ErrCtx), ?ERR(?ERR_SPACE_MARKETPLACE_DISABLED_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% oz_worker/subdomain errors
%%--------------------------------------------------------------------
-define(ERR_SUBDOMAIN_DELEGATION_DISABLED, ?ERR(?ERR_SUBDOMAIN_DELEGATION_DISABLED_TYPE)).
-define(ERR_SUBDOMAIN_DELEGATION_DISABLED(ErrCtx), ?ERR(?ERR_SUBDOMAIN_DELEGATION_DISABLED_TYPE, undefined, ErrCtx)).

-define(ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED, ?ERR(?ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE)).
-define(ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED(ErrCtx), ?ERR(?ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED_TYPE, undefined, ErrCtx)).


%%--------------------------------------------------------------------
%% posix errors
%%--------------------------------------------------------------------
-define(ERR_POSIX(Errno), ?ERR(?ERR_POSIX_TYPE, {Errno})).
-define(ERR_POSIX(ErrCtx, Errno), ?ERR(?ERR_POSIX_TYPE, {Errno}, ErrCtx)).


-endif.

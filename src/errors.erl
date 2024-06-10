%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Error encoding / decoding functions to be used across all Onedata products.
%%% NOTE: when modifying this file, please make sure to adjust all 3 functions
%%% (to_json/1, from_json/1, http_code/1), as well as the below type
%%% specifications and macros in errors.hrl.
%%% @end
%%%-------------------------------------------------------------------
-module(errors).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("errors.hrl").
-include("http/codes.hrl").
-include("logging.hrl").
-include("onedata.hrl").
-include("validation.hrl").

% The *unauthorized* error is emphasized as a separate type because it should be
% used universally for all errors related to authentication, understood as in
% the HTTP 401 code (https://httpstatuses.com/401):
%   > The request has not been applied because it lacks
%   > valid authentication credentials for the target resource.
% The unauthorized error has a variant for when a more detailed error that
% caused authentication failure is known.
-type unauthorized() :: unauthorized | {unauthorized, error()}.
-type unauthorized_error() :: {error, unauthorized()}.
-export_type([unauthorized_error/0]).

-type general() :: {bad_message, json_utils:json_term()} | no_connection_to_oz
| no_connection_to_peer_provider | no_connection_to_cluster_node
| unregistered_provider | internal_server_error | {internal_server_error, ErrorRef :: binary()}
| not_implemented | not_supported | service_unavailable |  timeout
| temporary_failure | {external_service_operation_failed, ServiceName :: binary()}
| unauthorized() | forbidden | {forbidden, HumanReadableHint :: binary()} | not_found | already_exists
| {file_access, Path :: file:name_all(), errno()} | {error, {limit_reached, number(), binary()}}.

-type auth() :: user_blocked | bad_basic_credentials | {bad_idp_access_token, IdsP :: atom()}
| bad_token | {bad_service_token, auth()} | {bad_consumer_token, auth()}
| token_invalid | token_revoked | not_an_access_token | not_an_identity_token
| {not_an_invite_token, ExpectedInviteType :: any | token_type:invite_type(), Received :: tokens:type()}
| {token_caveat_unverified, caveats:caveat()}
| {token_time_caveat_required, time:seconds()}
| token_subject_invalid | {token_service_forbidden, aai:service_spec()}
| invite_token_subject_not_authorized | invite_token_usage_limit_exceeded
| {invite_token_consumer_invalid, aai:consumer_spec()}
| {invite_token_target_id_invalid, Id :: binary()} | token_session_invalid.

-type graph_sync() :: expected_handshake_message | handshake_already_done
| {bad_version, {supported, [Version :: integer()]}} | bad_gri
| rpc_undefined | not_subscribable.

% Name of the key in data structure that caused the error, e.g. <<"name">>.
-type key() :: binary().
-type data_validation() :: malformed_data | {missing_required_value, key()}
| {missing_at_least_one_value, [key()]} | {bad_data, key()}
| {bad_data, key(), SpecificErrorOrHumanReadableHint :: error() | binary()}
| {empty_value, key()} | {bad_value_atom, key()}
| {bad_value_list_of_atoms, key()} | {bad_value_boolean, key()}
| {bad_value_binary, key()} | {bad_value_text_too_large, key(), {max, integer()}}
| {bad_value_list_of_binaries, key()}
| {bad_value_integer, key()} | {bad_value_float, key()}
| {bad_value_json, key()}
| {bad_value_xml, key()}
| {bad_value_token, key(), auth()}
| {bad_value_token_type, key()} | {bad_value_invite_type, key()}
| {bad_value_ipv4_address, key()} | {bad_value_list_of_ipv4_addresses, key()}
| {value_too_low, key(), {min, integer()}}
| {value_too_high, key(), {max, integer()}}
| {value_not_in_range, key(), {range, Low :: integer(), High :: integer()}}
| {value_not_allowed, key(), {allowed, [term()]}}
| {values_not_allowed, key(), {allowed, [term()]}} | {id_not_found, key()}
| {ambiguous_id, key()} | {bad_identifier, key()} | {identifier_occupied, key()}
| {bad_value_octal, Key :: key()}
| bad_file_path
| bad_full_name | bad_username | bad_password | bad_value_email
| bad_value_name | {bad_value_name, key()}
| bad_value_domain | bad_value_subdomain
| {bad_value_caveat, Caveat :: binary() | json_utils:json_map()} | bad_value_qos_parameter
| {tsc_missing_layout, map()} | {tsc_too_many_metrics, pos_integer()}
| {bad_value_tsc_conflicting_metric_configs, binary(), binary(), metric_config:record(), metric_config:record()}
| bad_gui_package | gui_package_too_large | {gui_package_unverified, onedata:gui_hash()}
| {invalid_qos_expression, Reason :: binary()}
| {illegal_support_stage_transition, support_stage:provider_support_stage(), support_stage:storage_support_stage()}.

-type oz_worker() :: basic_auth_not_supported | basic_auth_disabled
| subdomain_delegation_not_supported | subdomain_delegation_disabled
| space_marketplace_disabled | protected_group
| {atm_lambda_in_use, AtmWorkflowIds :: [gri:entity_id()]}
| {cannot_remove_last_owner, gri:entity_type(), gri:entity_id()}
| {cannot_delete_entity, gri:entity_type(), gri:entity_id()}
| cannot_add_relation_to_self
| {relation_does_not_exist,
    ChType :: gri:entity_type(),
    ChId :: gri:entity_id(),
    ParType :: gri:entity_type(),
    ParId :: gri:entity_id()
} | {relation_already_exists,
    ChType :: gri:entity_type(),
    ChId :: gri:entity_id(),
    ParType :: gri:entity_type(),
    ParId :: gri:entity_id()
} | {space_already_supported_with_imported_storage, SpaceId :: binary(), StorageId :: binary()}
| cannot_delete_non_empty_handle_service.

-type op_worker() :: user_not_supported | auto_cleaning_disabled
| file_popularity_disabled
| {space_not_supported_by, SpaceId :: binary(), ProviderId :: binary()}
| {not_a_local_storage_supporting_space, ProviderId :: binary(), StorageId :: binary(), SpaceId :: binary()}
| storage_in_use
| requires_auto_storage_import_mode
| transfer_already_ended | transfer_not_ended
| {storage_test_failed, read | write | remove}
| {requires_non_imported_storage, StorageId :: binary()}
| {requires_imported_storage, StorageId :: binary()}
| {requires_readonly_storage, StorageIdOrType :: binary()}
| {requires_posix_compatible_storage, StorageId :: binary(), PosixCompatibleStorages :: [binary()]}
| {storage_import_not_supported, StorageId :: binary(), ObjectStorages :: [binary()]}
| {auto_storage_import_not_supported, StorageId :: binary(), SupportedStorages :: [binary()], SupportedObjectStorages :: [binary()]}
| {stat_operation_not_supported, StorageId :: binary}
| {view_not_exists_on, ProviderId :: binary()}
| {view_query_failed, Category :: binary(), Description :: binary()}
| quota_exceeded
| {atm_unsupported_data_type, UnsupportedType :: atm_data_type:type(), SupportedTypes :: [atm_data_type:type()]}
| {atm_data_type_unverified, Value :: json_utils:json_term(), ExpType :: atm_data_type:type()}
| {atm_data_value_constraint_unverified,
    Value :: json_utils:json_term(),
    Type :: atm_data_type:type(),
    ValueConstraintsJson :: json_utils:json_map()}
| atm_store_missing_required_initial_content
| {atm_store_creation_failed, AtmStoreSchemaId :: binary(), SpecificError :: error()}
| {atm_store_frozen, AtmStoreSchemaId :: binary()}
| {atm_store_type_disallowed, AtmStoreSchemaId :: binary(), AllowedTypes :: [automation:store_type()]}
| {atm_store_content_not_set, AtmStoreSchemaId :: binary()}
| {atm_store_not_found, AtmStoreSchemaId :: binary()}
| atm_workflow_empty
| atm_workflow_execution_stopping
| atm_workflow_execution_stopped
| atm_workflow_execution_not_stopped
| atm_workflow_execution_ended
| atm_workflow_execution_not_ended
| atm_workflow_execution_not_resumable
| {atm_lane_empty, AtmLaneSchemaId :: binary()}
| {atm_lane_execution_creation_failed, AtmLaneSchemaId :: binary(), SpecificError :: error()}
| {atm_lane_execution_initiation_failed, AtmLaneSchemaId :: binary(), SpecificError :: error()}
| atm_lane_execution_retry_failed
| atm_lane_execution_rerun_failed
| {atm_parallel_box_empty, AtmParallelBoxSchemaId :: binary()}
| {atm_parallel_box_execution_creation_failed, AtmParallelBoxSchemaId :: binary(), SpecificError :: error()}
| {atm_parallel_box_execution_initiation_failed, AtmParallelBoxSchemaId :: binary(), SpecificError :: error()}
| {atm_task_execution_creation_failed, AtmTaskSchemaId :: binary(), SpecificError :: error()}
| {atm_task_execution_initiation_failed, AtmTaskSchemaId :: binary(), SpecificError :: error()}
| {atm_lambda_config_bad_value, ParameterName :: binary(), SpecificError :: error()}
| {atm_task_arg_mapper_for_required_lambda_arg_missing, ArgName :: binary()}
| {atm_task_arg_mapper_for_nonexistent_lambda_arg, ArgName :: binary()}
| {atm_task_arg_mapper_unsupported_value_builder,
    Type :: atm_task_argument_value_builder:type(),
    SupportedTypes :: [atm_task_argument_value_builder:type()]}
| {atm_task_arg_mapper_iterated_item_query_failed,
    IteratedItem :: json_utils:json_term(),
    Query :: json_utils:query()}
| {atm_task_arg_mapping_failed, ArgName :: binary(), SpecificError :: error()}
| {atm_task_result_missing, ResultName :: binary()}
| {atm_task_result_dispatch_failed, AtmStoreSchemaId :: binary(), SpecificError :: error()}
| {atm_task_result_mapping_failed, ResultName :: binary(), SpecificError :: error()}
| atm_task_execution_stopped
| {atm_job_batch_withdrawn, json_utils:json_term()}
| {atm_job_batch_crashed, json_utils:json_term()}
| atm_openfaas_not_configured
| atm_openfaas_unreachable
| atm_openfaas_unhealthy
| atm_openfaas_query_failed
| {atm_openfaas_query_failed, Reason :: binary()}
| atm_openfaas_function_registration_failed
| {atm_invalid_status_transition, PrevStatus :: atom(), NewStatus :: atom()}
| dir_stats_disabled_for_space
| dir_stats_not_ready
| {forbidden_for_current_archive_state, CurrentState :: atom(), AllowedStates :: [atom()]}
| {nested_archive_deletion_forbidden, ParentArchiveId :: binary()}
| recall_target_conflict.

-type onepanel() :: {error_on_nodes, error(), Hostnames :: [binary()]}
| {dns_servers_unreachable, [ip_utils:ip() | default]}
| {file_allocation, ActualSize :: number(), TargetSize :: number()}
| lets_encrypt_not_supported | lets_encrypt_not_reachable
| {lets_encrypt_response, problem_document() | undefined, binary()}
| {node_already_in_cluster, Hostname :: binary()}
| {node_not_compatible, Hostname :: binary(), onedata:cluster_type()}
| {no_connection_to_new_node, Hostname :: binary()}
| {no_service_nodes, Service :: atom() | binary()}
| user_not_in_cluster.

-type errno() :: ?OK | ?E2BIG | ?EACCES | ?EADDRINUSE | ?EADDRNOTAVAIL
| ?EAFNOSUPPORT | ?EAGAIN | ?EALREADY | ?EBADF | ?EBADMSG | ?EBUSY
| ?ECANCELED | ?ECHILD | ?ECONNABORTED | ?ECONNREFUSED | ?ECONNRESET
| ?EDEADLK | ?EDESTADDRREQ | ?EDOM | ?EEXIST | ?EFAULT | ?EFBIG
| ?EHOSTUNREACH | ?EIDRM | ?EILSEQ | ?EINPROGRESS | ?EINTR | ?EINVAL | ?EIO
| ?EISCONN | ?EISDIR | ?EKEYEXPIRED | ?ELOOP | ?EMFILE | ?EMLINK | ?EMSGSIZE
| ?ENAMETOOLONG | ?ENETDOWN | ?ENETRESET | ?ENETUNREACH | ?ENFILE | ?ENOBUFS
| ?ENODATA | ?ENODEV | ?ENOENT | ?ENOEXEC | ?ENOLCK | ?ENOLINK | ?ENOMEM
| ?ENOMSG | ?ENOPROTOOPT | ?ENOSPC | ?ENOSR | ?ENOSTR | ?ENOSYS | ?ENOTCONN
| ?ENOTDIR | ?ENOTEMPTY | ?ENOTRECOVERABLE | ?ENOTSOCK | ?ENOTSUP | ?ENOTTY
| ?ENXIO | ?EOPNOTSUPP | ?EOVERFLOW | ?EOWNERDEAD | ?EPERM | ?EPIPE
| ?EPROTO | ?EPROTONOSUPPORT | ?EPROTOTYPE | ?ERANGE | ?EROFS | ?ESPIPE
| ?ESRCH | ?ETIME | ?ETIMEDOUT | ?ETXTBSY | ?EWOULDBLOCK | ?EXDEV.

-type posix() :: {posix, errno()}.

% Defined in RFC7807; used by Let's Encrypt
% #{<<"type">> := <<"errorId">>, <<"detail">> => <<"error description">>, _ => _}
-type problem_document() :: json_utils:json_map().

-type unrecognized() :: {unrecognized_error, as_json()}.

-type reason() :: general() | auth() | graph_sync() | data_validation()
| oz_worker() | posix() | op_worker() | onepanel() | unrecognized().
-type error() :: {error, reason()}.

-type as_json() :: json_utils:json_map().
-export_type([error/0, reason/0, as_json/0]).

%% API
-export([is_known_error/1, is_posix_code/1]).
-export([to_json/1, from_json/1, to_http_code/1]).

-define(FMT(Format, Args), str_utils:format_bin(Format, Args)).
-define(DNS_DEFAULTS, <<"system defaults">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec is_known_error(term()) -> boolean().
is_known_error(Error) ->
    try
        errors:to_http_code(Error),
        true
    catch _:_ ->
        false
    end.


-spec is_posix_code(term()) -> boolean().
is_posix_code(ErrorCode) ->
    ordsets:is_element(ErrorCode, ?ERROR_CODES).


-spec to_json(undefined | error()) -> as_json().
to_json(undefined) ->
    null;

%% -----------------------------------------------------------------------------
%% General errors
%% -----------------------------------------------------------------------------
to_json(?ERROR_BAD_MESSAGE(MessageBinOrJson)) -> #{
    <<"id">> => <<"badMessage">>,
    <<"details">> => #{
        <<"message">> => MessageBinOrJson
    },
    <<"description">> => <<"This message could not be understood by the server.">>
};
to_json(?ERROR_NO_CONNECTION_TO_ONEZONE) -> #{
    <<"id">> => <<"noConnectionToOnezone">>,
    <<"description">> => <<"No connection to Onezone.">>
};
to_json(?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER) -> #{
    <<"id">> => <<"noConnectionToPeerOneprovider">>,
    <<"description">> => <<"No connection to peer Oneprovider.">>
};
to_json(?ERROR_NO_CONNECTION_TO_CLUSTER_NODE) -> #{
    <<"id">> => <<"noConnectionToClusterNode">>,
    <<"description">> => <<"No connection to cluster node.">>
};
to_json(?ERROR_UNREGISTERED_ONEPROVIDER) -> #{
    <<"id">> => <<"unregisteredOneprovider">>,
    <<"description">> => <<"This Oneprovider is not registered.">>
};
to_json(?ERROR_INTERNAL_SERVER_ERROR(ErrorRef)) -> #{
    <<"id">> => <<"internalServerError">>,
    <<"details">> => #{
        <<"reference">> => ErrorRef
    },
    <<"description">> => ?FMT(
        "The server has encountered an error while processing this request. "
        "If the problem persists, please contact the site's administrators, citing the following reference: ~s.", [ErrorRef]
    )
};
to_json(?ERROR_INTERNAL_SERVER_ERROR) -> #{
    <<"id">> => <<"internalServerError">>,
    <<"description">> => <<"The server has encountered an error while processing this request.">>
};
to_json(?ERROR_NOT_IMPLEMENTED) -> #{
    <<"id">> => <<"notImplemented">>,
    <<"description">> => <<"This operation is not implemented.">>
};
to_json(?ERROR_NOT_SUPPORTED) -> #{
    <<"id">> => <<"notSupported">>,
    <<"description">> => <<"This operation is not supported.">>
};
to_json(?ERROR_SERVICE_UNAVAILABLE) -> #{
    <<"id">> => <<"serviceUnavailable">>,
    <<"description">> => <<"Service required for this operation is offline.">>};
to_json(?ERROR_TIMEOUT) -> #{
    <<"id">> => <<"timeout">>,
    <<"description">> => <<"Operation timed out.">>
};
to_json(?ERROR_TEMPORARY_FAILURE) -> #{
    <<"id">> => <<"temporaryFailure">>,
    <<"description">> => <<"Temporary failure - please try again later.">>
};
to_json(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName)) -> #{
    <<"id">> => <<"externalServiceOperationFailed">>,
    <<"details">> => #{
        <<"serviceName">> => ServiceName
    },
    <<"description">> => ?FMT(
        "Your request could not be fulfilled due to problems with the external "
        "service '~ts'. This might be a temporary problem or a misconfiguration. "
        "Please try again later or contact the site administrators if the problem persists.",
        [ServiceName]
    )
};
to_json(?ERROR_UNAUTHORIZED(AuthError)) -> #{
    <<"id">> => <<"unauthorized">>,
    <<"details">> => #{
        <<"authError">> => to_json(AuthError)
    },
    <<"description">> => <<"Provided authentication is not valid (see details).">>
};
to_json(?ERROR_UNAUTHORIZED) -> #{
    <<"id">> => <<"unauthorized">>,
    <<"description">> => <<"You must authenticate yourself to perform this operation.">>
};
to_json(?ERROR_FORBIDDEN(HumanReadableHint)) -> #{
    <<"id">> => <<"forbidden">>,
    <<"details">> => #{
        <<"hint">> => HumanReadableHint
    },
    <<"description">> => ?FMT("You are not authorized to perform this operation: ~s", [
        str_utils:ensure_suffix(HumanReadableHint, <<".">>)
    ])
};
to_json(?ERROR_FORBIDDEN) -> #{
    <<"id">> => <<"forbidden">>,
    <<"description">> => <<"You are not authorized to perform this operation.">>
};
to_json(?ERROR_NOT_FOUND) -> #{
    <<"id">> => <<"notFound">>,
    <<"description">> => <<"The resource could not be found.">>
};
to_json(?ERROR_ALREADY_EXISTS) -> #{
    <<"id">> => <<"alreadyExists">>,
    <<"description">> => <<"The resource already exists.">>
};
to_json(?ERROR_FILE_ACCESS(Path, Errno)) ->
    PathBin = str_utils:to_binary(filename:flatten(Path)),
    #{
        <<"id">> => <<"fileAccess">>,
        <<"details">> => #{<<"path">> => PathBin, <<"errno">> => Errno},
        <<"description">> => ?FMT("Cannot access file \"~ts\": ~p.", [PathBin, Errno])
    };
to_json(?ERROR_LIMIT_REACHED(Limit, ResourceDescription)) ->
    #{
        <<"id">> => <<"limitReached">>,
        <<"details">> => #{
            <<"limit">> => Limit,
            <<"resourceDescription">> => ResourceDescription
        },
        <<"description">> => ?FMT("The limit for ~s has been reached: ~p.", [ResourceDescription, Limit])
    };

%% -----------------------------------------------------------------------------
%% POSIX errors
%% -----------------------------------------------------------------------------
to_json(?ERROR_POSIX(Errno)) -> #{
    <<"id">> => <<"posix">>,
    <<"details">> => #{
        <<"errno">> => atom_to_binary(Errno, utf8)
    },
    <<"description">> => ?FMT("Operation failed with POSIX error: ~s.", [Errno])
};

%% -----------------------------------------------------------------------------
%% Auth errors
%% -----------------------------------------------------------------------------
to_json(?ERROR_USER_BLOCKED) -> #{
    <<"id">> => <<"userBlocked">>,
    <<"description">> => <<
        "This user account has been blocked by the administrator and "
        "cannot be used unless it is unblocked again."
    >>
};
to_json(?ERROR_BAD_BASIC_CREDENTIALS) -> #{
    <<"id">> => <<"badBasicCredentials">>,
    <<"description">> => <<"Invalid username or password.">>
};
to_json(?ERROR_BAD_IDP_ACCESS_TOKEN(IdP)) -> #{
    <<"id">> => <<"badIdpAccessToken">>,
    <<"details">> => #{
        <<"idp">> => IdP
    },
    <<"description">> => ?FMT("Provided access token for \"~p\" is not valid.", [IdP])
};
to_json(?ERROR_BAD_TOKEN) -> #{
    <<"id">> => <<"badToken">>,
    <<"description">> => <<"Provided token could not be understood by the server.">>
};
to_json(?ERROR_BAD_SERVICE_TOKEN(TokenError)) -> #{
    <<"id">> => <<"badServiceToken">>,
    <<"details">> => #{
        <<"tokenError">> => to_json(TokenError)
    },
    <<"description">> => <<"Provided service token is not valid (see details).">>
};
to_json(?ERROR_BAD_CONSUMER_TOKEN(TokenError)) -> #{
    <<"id">> => <<"badConsumerToken">>,
    <<"details">> => #{
        <<"tokenError">> => to_json(TokenError)
    },
    <<"description">> => <<"Provided consumer token is not valid (see details).">>
};
to_json(?ERROR_TOKEN_INVALID) -> #{
    <<"id">> => <<"tokenInvalid">>,
    <<"description">> => <<"Provided token is not valid.">>
};
to_json(?ERROR_TOKEN_REVOKED) -> #{
    <<"id">> => <<"tokenRevoked">>,
    <<"description">> => <<"Provided token has been revoked.">>
};
to_json(?ERROR_TOKEN_TOO_LARGE(SizeLimit)) -> #{
    <<"id">> => <<"tokenTooLarge">>,
    <<"details">> => #{
        <<"limit">> => SizeLimit
    },
    <<"description">> => ?FMT("Provided token exceeds the allowed size (~B characters).", [SizeLimit])
};
to_json(?ERROR_NOT_AN_ACCESS_TOKEN(ReceivedTokenType)) -> #{
    <<"id">> => <<"notAnAccessToken">>,
    <<"details">> => #{
        <<"received">> => token_type:to_json(ReceivedTokenType)
    },
    <<"description">> => ?FMT("Expected an access token, but received a(n) ~s.", [
        token_type:to_printable(ReceivedTokenType)
    ])
};
to_json(?ERROR_NOT_AN_IDENTITY_TOKEN(ReceivedTokenType)) -> #{
    <<"id">> => <<"notAnIdentityToken">>,
    <<"details">> => #{
        <<"received">> => token_type:to_json(ReceivedTokenType)
    },
    <<"description">> => ?FMT("Expected an identity token, but received a(n) ~s.", [
        token_type:to_printable(ReceivedTokenType)
    ])
};
to_json(?ERROR_NOT_AN_INVITE_TOKEN(ExpectedInviteType, ReceivedTokenType)) -> #{
    <<"id">> => <<"notAnInviteToken">>,
    <<"details">> => #{
        <<"expectedInviteType">> => case ExpectedInviteType of
            any -> <<"any">>;
            _ -> token_type:invite_type_to_str(ExpectedInviteType)
        end,
        <<"received">> => token_type:to_json(ReceivedTokenType)
    },
    <<"description">> => ?FMT("Expected an invitation token of type '~s', but received a(n) ~s.", [
        ExpectedInviteType,
        token_type:to_printable(ReceivedTokenType)
    ])
};
to_json(?ERROR_TOKEN_CAVEAT_UNKNOWN(CaveatBinary)) -> #{
    <<"id">> => <<"tokenCaveatUnknown">>,
    <<"details">> => #{
        <<"caveat">> => CaveatBinary
    },
    <<"description">> => ?FMT("Unknown caveat - '~ts'.", [CaveatBinary])
};
to_json(?ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat)) -> #{
    <<"id">> => <<"tokenCaveatUnverified">>,
    <<"details">> => #{
        <<"caveat">> => caveats:to_json(Caveat)
    },
    <<"description">> => ?FMT("Provided token is not valid - ~ts.", [caveats:unverified_description(Caveat)])
};
to_json(?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)) -> #{
    <<"id">> => <<"tokenTimeCaveatRequired">>,
    <<"details">> => #{
        <<"maxTtl">> => MaxTtl
    },
    <<"description">> => ?FMT("You must specify a time caveat with maximum TTL of ~B seconds.", [MaxTtl])
};
to_json(?ERROR_TOKEN_SUBJECT_INVALID) -> #{
    <<"id">> => <<"tokenSubjectInvalid">>,
    <<"description">> => <<"The token subject is invalid (does not exist or is different than expected).">>
};
to_json(?ERROR_TOKEN_SERVICE_FORBIDDEN(Service)) -> #{
    <<"id">> => <<"tokenServiceForbidden">>,
    <<"details">> => #{
        <<"service">> => aai:service_to_json(Service)
    },
    <<"description">> => ?FMT("The service ~s is forbidden for this subject.", [aai:service_to_printable(Service)])
};
to_json(?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED) -> #{
    <<"id">> => <<"inviteTokenSubjectNotAuthorized">>,
    <<"description">> => <<"The subject of this token is not (or no longer) authorized to issue such invitations.">>
};
to_json(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED) -> #{
    <<"id">> => <<"inviteTokenUsageLimitReached">>,
    <<"description">> => <<"The usage limit of this invite token has been reached.">>
};
to_json(?ERROR_INVITE_TOKEN_CONSUMER_INVALID(Consumer)) -> #{
    <<"id">> => <<"inviteTokenConsumerInvalid">>,
    <<"details">> => #{
        <<"consumer">> => aai:subject_to_json(Consumer)
    },
    <<"description">> => ?FMT("The consumer '~s' is invalid for this type of invite token.", [
        aai:subject_to_printable(Consumer)
    ])
};
to_json(?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(Id)) -> #{
    <<"id">> => <<"inviteTokenTargetIdInvalid">>,
    <<"details">> => #{
        <<"id">> => Id
    },
    <<"description">> => ?FMT("The target id '~s' is invalid for this type of invite token.", [Id])
};
to_json(?ERROR_TOKEN_SESSION_INVALID) -> #{
    <<"id">> => <<"tokenSessionInvalid">>,
    <<"description">> => <<"This token is bound to a session different than presented by the client or no longer existent.">>
};

%% -----------------------------------------------------------------------------
%% Graph Sync errors
%% -----------------------------------------------------------------------------
to_json(?ERROR_EXPECTED_HANDSHAKE_MESSAGE) -> #{
    <<"id">> => <<"expectedHandshakeMessage">>,
    <<"description">> => <<"Handshake must be performed prior to any requests.">>
};
to_json(?ERROR_HANDSHAKE_ALREADY_DONE) -> #{
    <<"id">> => <<"handshakeAlreadyDone">>,
    <<"description">> => <<"Handshake has already been done.">>
};
to_json(?ERROR_BAD_VERSION(SupportedVersions)) -> #{
    <<"id">> => <<"badVersion">>,
    <<"details">> => #{
        <<"supportedVersions">> => SupportedVersions
    },
    <<"description">> => ?FMT("Bad version - supported versions: ~p.", [SupportedVersions])
};
to_json(?ERROR_BAD_GRI) -> #{
    <<"id">> => <<"badGRI">>,
    <<"description">> => <<"Provided GRI (Graph Resource Identifier) is invalid.">>
};
to_json(?ERROR_RPC_UNDEFINED) -> #{
    <<"id">> => <<"rpcUndefined">>,
    <<"description">> => <<"Requested RPC operation is not defined.">>
};
to_json(?ERROR_NOT_SUBSCRIBABLE) -> #{
    <<"id">> => <<"notSubscribable">>,
    <<"description">> => <<"Requested resource is not subscribable.">>
};

%% -----------------------------------------------------------------------------
%% Data validation errors
%% -----------------------------------------------------------------------------
to_json(?ERROR_MALFORMED_DATA) -> #{
    <<"id">> => <<"malformedData">>,
    <<"description">> => <<"Provided data could not be understood by the server.">>
};
to_json(?ERROR_MISSING_REQUIRED_VALUE(Key)) -> #{
    <<"id">> => <<"missingRequiredValue">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Missing required value: ~s.", [Key])
};
to_json(?ERROR_MISSING_AT_LEAST_ONE_VALUE(Keys)) -> #{
    <<"id">> => <<"missingAtLeastOneValue">>,
    <<"details">> => #{
        <<"keys">> => Keys
    },
    <<"description">> => ?FMT("Missing data, you must provide at least one of: ~p.", [Keys])
};
to_json(?ERROR_BAD_DATA(Key, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"badData">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT("Bad value provided for \"~s\" (see details).", [Key])
};
to_json(?ERROR_BAD_DATA(Key, HumanReadableHint)) -> #{
    <<"id">> => <<"badData">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"hint">> => HumanReadableHint
    },
    <<"description">> => ?FMT("Bad value provided for \"~s\": ~ts.", [Key, HumanReadableHint])
};
to_json(?ERROR_BAD_DATA(Key)) -> #{
    <<"id">> => <<"badData">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT(
        "Bad value: provided \"~s\" has an invalid format or is incomprehensible "
        "in the context of this operation.",
        [Key]
    )
};
to_json(?ERROR_BAD_VALUE_EMPTY(Key)) -> #{
    <<"id">> => <<"badValueEmpty">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must not be empty.", [Key])
};
to_json(?ERROR_BAD_VALUE_BOOLEAN(Key)) -> #{
    <<"id">> => <<"badValueBoolean">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a boolean.", [Key])
};
% We do not differentiate between atoms and binaries in JSON, so they are
% treated as the same.
to_json(?ERROR_BAD_VALUE_ATOM(Key)) -> #{
    <<"id">> => <<"badValueString">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a string.", [Key])
};
to_json(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key)) -> #{
    <<"id">> => <<"badValueListOfStrings">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a list of strings.", [Key])
};
to_json(?ERROR_BAD_VALUE_BINARY(Key)) -> #{
    <<"id">> => <<"badValueString">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a string.", [Key])
};
to_json(?ERROR_BAD_VALUE_TEXT_TOO_LARGE(Key, SizeLimit)) -> #{
    <<"id">> => <<"badValueTextTooLarge">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"limit">> => SizeLimit
    },
    <<"description">> => ?FMT("Bad value: the text provided in \"~s\" cannot be larger than ~B characters.", [
        Key, SizeLimit
    ])
};
to_json(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key)) -> #{
    <<"id">> => <<"badValueListOfStrings">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a list of strings.", [Key])
};
to_json(?ERROR_BAD_VALUE_INTEGER(Key)) -> #{
    <<"id">> => <<"badValueInteger">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be an integer.", [Key])
};
to_json(?ERROR_BAD_VALUE_FLOAT(Key)) -> #{
    <<"id">> => <<"badValueFloat">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a floating point number.", [Key])
};
to_json(?ERROR_BAD_VALUE_JSON(Key)) -> #{
    <<"id">> => <<"badValueJSON">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a valid JSON.", [Key])
};
to_json(?ERROR_BAD_VALUE_XML(Key)) -> #{
    <<"id">> => <<"badValueXML">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be a valid XML.", [Key])
};
to_json(?ERROR_BAD_VALUE_TOKEN(Key, TokenError)) -> #{
    <<"id">> => <<"badValueToken">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"tokenError">> => to_json(TokenError)
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid token (see details).", [Key])
};
to_json(?ERROR_BAD_VALUE_TOKEN_TYPE(Key)) -> #{
    <<"id">> => <<"badValueTokenType">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid token type.", [Key])
};
to_json(?ERROR_BAD_VALUE_INVITE_TYPE(Key)) -> #{
    <<"id">> => <<"badValueInviteType">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid invite type.", [Key])
};
to_json(?ERROR_BAD_VALUE_IPV4_ADDRESS(Key)) -> #{
    <<"id">> => <<"badValueIPv4Address">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid IPv4 address.", [Key])
};
to_json(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key)) -> #{
    <<"id">> => <<"badValueListOfIPv4Addresses">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid list of IPv4 addresses.", [Key])
};
to_json(?ERROR_BAD_VALUE_TOO_LOW(Key, Threshold)) -> #{
    <<"id">> => <<"badValueTooLow">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"limit">> => Threshold
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be at least ~B.", [Key, Threshold])
};
to_json(?ERROR_BAD_VALUE_TOO_HIGH(Key, Threshold)) -> #{
    <<"id">> => <<"badValueTooHigh">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"limit">> => Threshold
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must not exceed ~B.", [Key, Threshold])
};
to_json(?ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High)) -> #{
    <<"id">> => <<"badValueNotInRange">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"low">> => Low,
        <<"high">> => High
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" must be between <~B, ~B>.", [Key, Low, High])
};
to_json(?ERROR_BAD_VALUE_NOT_ALLOWED(Key, AllowedValues)) -> #{
    <<"id">> => <<"badValueNotAllowed">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"allowed">> => AllowedValues
    },
    <<"description">> => ?FMT(
        "Bad value: provided \"~s\" must be one of: ~ts.",
        [Key, join_values_with_commas(AllowedValues)]
    )
};
to_json(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, AllowedValues)) -> #{
    <<"id">> => <<"badValueListNotAllowed">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"allowed">> => AllowedValues
    },
    <<"description">> => ?FMT(
        "Bad value: provided \"~s\" must be a list containing zero or more following values: ~ts.",
        [Key, join_values_with_commas(AllowedValues)]
    )
};
to_json(?ERROR_BAD_VALUE_ID_NOT_FOUND(Key)) -> #{
    <<"id">> => <<"badValueIdNotFound">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided ID (\"~s\") does not exist.", [Key])
};
to_json(?ERROR_BAD_VALUE_AMBIGUOUS_ID(Key)) -> #{
    <<"id">> => <<"badValueAmbiguousId">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided ID (\"~s\") is ambiguous.", [Key])
};
to_json(?ERROR_BAD_VALUE_IDENTIFIER(Key)) -> #{
    <<"id">> => <<"badValueIdentifier">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid identifier.", [Key])
};
to_json(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key)) -> #{
    <<"id">> => <<"badValueIdentifierOccupied">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided identifier (\"~s\") is already occupied.", [Key])
};
to_json(?ERROR_BAD_VALUE_OCTAL(Key)) -> #{
    <<"id">> => <<"badValueOctal">>,
    <<"details">> => #{<<"key">> => Key},
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid octal number.", [Key])
};
to_json(?ERROR_BAD_VALUE_FILE_PATH) -> #{
    <<"id">> => <<"badValueFilePath">>,
    <<"description">> => <<"Bad value: provided file path is invalid.">>
};
to_json(?ERROR_BAD_VALUE_FULL_NAME) -> #{
    <<"id">> => <<"badValueFullName">>,
    <<"description">> => <<"Bad value: ", (?FULL_NAME_REQUIREMENTS_DESCRIPTION)/binary>>
};
to_json(?ERROR_BAD_VALUE_USERNAME) -> #{
    <<"id">> => <<"badValueUsername">>,
    <<"description">> => <<"Bad value: ", (?USERNAME_REQUIREMENTS_DESCRIPTION)/binary>>
};
to_json(?ERROR_BAD_VALUE_PASSWORD) -> #{
    <<"id">> => <<"badValuePassword">>,
    <<"description">> => <<"Bad value: ", (?PASSWORD_REQUIREMENTS_DESCRIPTION)/binary>>
};
to_json(?ERROR_BAD_VALUE_EMAIL) -> #{
    <<"id">> => <<"badValueEmail">>,
    <<"description">> => <<"Bad value: provided e-mail is not valid.">>
};
to_json(?ERROR_BAD_VALUE_NAME) -> #{
    <<"id">> => <<"badValueName">>,
    <<"description">> => <<"Bad value: ", (?NAME_REQUIREMENTS_DESCRIPTION)/binary>>
};
to_json(?ERROR_BAD_VALUE_NAME(Key)) -> #{
    <<"id">> => <<"badValueName">>,
    <<"details">> => #{<<"key">> => Key},
    <<"description">> => ?FMT("Bad value provided for \"~ts\": ~ts", [Key, ?NAME_REQUIREMENTS_DESCRIPTION])
};
to_json(?ERROR_BAD_VALUE_DOMAIN) -> #{
    <<"id">> => <<"badValueDomain">>,
    <<"description">> => <<"Bad value: provided domain is not valid.">>
};
to_json(?ERROR_BAD_VALUE_SUBDOMAIN) -> #{
    <<"id">> => <<"badValueSubdomain">>,
    <<"description">> => <<"Bad value: provided subdomain is not valid.">>
};
to_json(?ERROR_BAD_VALUE_CAVEAT(CaveatJson)) -> #{
    <<"id">> => <<"badValueCaveat">>,
    <<"details">> => #{
        <<"caveat">> => CaveatJson
    },
    <<"description">> => ?FMT("Provided caveat is invalid: '~s'.", [json_utils:encode(CaveatJson)])
};
to_json(?ERROR_BAD_VALUE_QOS_PARAMETERS) -> #{
    <<"id">> => <<"badValueQoSParameters">>,
    <<"description">> => <<"Provided QoS parameters are invalid.">>
};
to_json(?ERROR_TSC_MISSING_LAYOUT(MissingLayout)) -> #{
    <<"id">> => <<"timeSeriesCollectionMissingLayout">>,
    <<"details">> => #{
        <<"missingLayout">> => MissingLayout
    },
    <<"description">> => ?FMT(
        "The request refers to a layout that is not reflected in the time series collection; "
        "the following part of the layout is missing (time series name -> metric names): ~s.", [
            join_values_with_commas(maps:fold(fun(TimeSeriesName, MetricNames, Acc) ->
                Acc ++ [?FMT("~s -> [~s]", [TimeSeriesName, join_values_with_commas(MetricNames)])]
            end, [], MissingLayout))
        ])
};
to_json(?ERROR_TSC_TOO_MANY_METRICS(Limit)) -> #{
    <<"id">> => <<"timeSeriesCollectionTooManyMetrics">>,
    <<"details">> => #{
        <<"limit">> => Limit
    },
    <<"description">> => ?FMT("The time series collection cannot have more than ~B metrics.", [Limit])
};
to_json(?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(TSName, MetricName, ExistingMConfig, ConflictingMConfig)) -> #{
    <<"id">> => <<"badValueTimeSeriesCollectionConflictingMetricConfig">>,
    <<"details">> => #{
        <<"timeSeriesName">> => TSName,
        <<"metricName">> => MetricName,
        <<"existingMetricConfig">> => jsonable_record:to_json(ExistingMConfig, metric_config),
        <<"conflictingMetricConfig">> => jsonable_record:to_json(ConflictingMConfig, metric_config)
    },
    <<"description">> => ?FMT(
        "Provided metric config for 'time series' ~s and metric '~s' conflicts with existing metric config (see details).", [
            TSName, MetricName
        ])
};
to_json(?ERROR_BAD_GUI_PACKAGE) -> #{
    <<"id">> => <<"badGuiPackage">>,
    <<"description">> => <<"Provider GUI package could not be understood by the server.">>
};
to_json(?ERROR_GUI_PACKAGE_TOO_LARGE) -> #{
    <<"id">> => <<"guiPackageTooLarge">>,
    <<"description">> => <<"Provider GUI package is too large.">>
};
to_json(?ERROR_GUI_PACKAGE_UNVERIFIED(ShaSum)) -> #{
    <<"id">> => <<"guiPackageUnverified">>,
    <<"details">> => #{
        <<"shaSum">> => ShaSum
    },
    <<"description">> => ?FMT("Provided GUI package could not be verified - unknown SHA sum '~s'.", [ShaSum])
};
to_json(?ERROR_INVALID_QOS_EXPRESSION(Reason)) -> #{
    <<"id">> => <<"invalidQosExpression">>,
    <<"details">> => #{
        <<"reason">> => Reason
    },
    <<"description">> => ?FMT("Invalid QoS expression: ~ts.", [Reason])
};
to_json(?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ProviderStage, StorageStage)) -> #{
    <<"id">> => <<"illegalSupportStageTransition">>,
    <<"details">> => #{
        <<"currentProviderStage">> => support_stage:serialize(provider, ProviderStage),
        <<"currentStorageStage">> => support_stage:serialize(storage, StorageStage)
    },
    <<"description">> => ?FMT(
        "Illegal support stage transition: this operation cannot be performed while "
        "the storage is in stage '~w' and provider is in stage '~w'.",
        [StorageStage, ProviderStage]
    )
};

%%--------------------------------------------------------------------
%% oz_worker error
%%--------------------------------------------------------------------
to_json(?ERROR_BASIC_AUTH_NOT_SUPPORTED) -> #{
    <<"id">> => <<"basicAuthNotSupported">>,
    <<"description">> => <<"Basic auth is not supported by this Onezone.">>
};
to_json(?ERROR_BASIC_AUTH_DISABLED) -> #{
    <<"id">> => <<"basicAuthDisabled">>,
    <<"description">> => <<"Basic auth is disabled for this user.">>
};
to_json(?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED) -> #{
    <<"id">> => <<"subdomainDelegationNotSupported">>,
    <<"description">> => <<"Subdomain delegation is not supported by this Onezone.">>
};
to_json(?ERROR_SUBDOMAIN_DELEGATION_DISABLED) -> #{
    <<"id">> => <<"subdomainDelegationDisabled">>,
    <<"description">> => <<"Subdomain delegation is disabled for this Oneprovider.">>
};
to_json(?ERROR_SPACE_MARKETPLACE_DISABLED) -> #{
    <<"id">> => <<"spaceMarketplaceDisabled">>,
    <<"description">> => <<"Space marketplace is disabled for this Onezone.">>
};
to_json(?ERROR_PROTECTED_GROUP) -> #{
    <<"id">> => <<"protectedGroup">>,
    <<"description">> => <<"Specified group is protected and cannot be deleted.">>
};
to_json(?ERROR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas)) -> #{
    <<"id">> => <<"atmLambdaInUse">>,
    <<"details">> => #{
        <<"atmWorkflowSchemas">> => AtmWorkflowSchemas
    },
    <<"description">> => ?FMT(
        "This lambda cannot be removed because it is used by the following workflow schemas: ~s.",
        [join_values_with_commas(AtmWorkflowSchemas)]
    )
};
to_json(?ERROR_CANNOT_REMOVE_LAST_OWNER(EntityType, EntityId)) -> #{
    <<"id">> => <<"cannotRemoveLastOwner">>,
    <<"details">> => #{
        <<"entityType">> => EntityType,
        <<"entityId">> => EntityId
    },
    <<"description">> => <<
        "Cannot remove the last owner - another owner must be assigned first. "
        "Ownership can be granted to any direct or effective member."
    >>
};
to_json(?ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId)) -> #{
    <<"id">> => <<"cannotDeleteEntity">>,
    <<"details">> => #{
        <<"entityType">> => EntityType,
        <<"entityId">> => EntityId
    },
    <<"description">> => ?FMT("Cannot delete ~s:~s; failed to delete some dependent relations.", [
        gri:serialize_type(EntityType), EntityId
    ])
};
to_json(?ERROR_CANNOT_ADD_RELATION_TO_SELF) -> #{
    <<"id">> => <<"cannotAddRelationToSelf">>,
    <<"description">> => <<"Cannot add relation to self.">>
};
to_json(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is not supported by">>;
        {_, _} -> <<"is not a member of">>
    end,
    #{
        <<"id">> => <<"relationDoesNotExist">>,
        <<"details">> => #{
            <<"childType">> => ChType,
            <<"childId">> => ChId,
            <<"parentType">> => ParType,
            <<"parentId">> => ParId
        },
        <<"description">> => ?FMT("Bad value: ~s:~s ~s ~s:~s.", [
            gri:serialize_type(ChType), ChId,
            RelationToString,
            gri:serialize_type(ParType), ParId
        ])
    };
to_json(?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is already supported by">>;
        {_, _} -> <<"is already a member of">>
    end,
    #{
        <<"id">> => <<"relationAlreadyExists">>,
        <<"details">> => #{
            <<"childType">> => gri:serialize_type(ChType),
            <<"childId">> => ChId,
            <<"parentType">> => gri:serialize_type(ParType),
            <<"parentId">> => ParId
        },
        <<"description">> => ?FMT("Bad value: ~s:~s ~s ~s:~s.", [
            gri:serialize_type(ChType), ChId,
            RelationToString,
            gri:serialize_type(ParType), ParId
        ])
    };
to_json(?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId)) -> #{
    <<"id">> => <<"spaceAlreadySupportedWithImportedStorage">>,
    <<"details">> => #{
        <<"spaceId">> => SpaceId,
        <<"storageId">> => StorageId
    },
    <<"description">> => ?FMT("Space ~s is already supported with an imported storage ~s.", [SpaceId, StorageId])
};
to_json(?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE) -> #{
    <<"id">> => <<"cannotDeleteNonEmptyHandleService">>,
    <<"description">> => <<
        "This handle service cannot be deleted as it still has some handles registered. "
        "All the handles would have to be deleted first, but proceed with caution as Open Data "
        "records should be persistent."
    >>
};

%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
to_json(?ERROR_USER_NOT_SUPPORTED) -> #{
    <<"id">> => <<"userNotSupported">>,
    <<"description">> => <<
        "Authenticated user is not supported by this Oneprovider "
        "(none of the user's spaces is supported by the Oneprovider)."
    >>
};
to_json(?ERROR_AUTO_CLEANING_DISABLED) -> #{
    <<"id">> => <<"autoCleaningDisabled">>,
    <<"description">> => <<"Auto-cleaning is disabled.">>
};
to_json(?ERROR_FILE_POPULARITY_DISABLED) -> #{
    <<"id">> => <<"filePopularityDisabled">>,
    <<"description">> => <<"File popularity is disabled.">>
};
to_json(?ERROR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId)) -> #{
    <<"id">> => <<"spaceNotSupportedBy">>,
    <<"details">> => #{
        <<"spaceId">> => SpaceId,
        <<"providerId">> => ProviderId
    },
    <<"description">> => ?FMT("Specified space: ~s is not supported by provider ~s.", [SpaceId, ProviderId])
};
to_json(?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId)) -> #{
    <<"id">> => <<"notALocalStorageSupportingSpace">>,
    <<"details">> => #{
        <<"providerId">> => ProviderId,
        <<"storageId">> => StorageId,
        <<"spaceId">> => SpaceId
    },
    <<"description">> => ?FMT(
        "Storage ~s does not belong to this Oneprovider (~s) and/or does not support the space ~s.",
        [StorageId, ProviderId, SpaceId]
    )
};
to_json(?ERROR_STORAGE_IN_USE) -> #{
    <<"id">> => <<"storageInUse">>,
    <<"description">> => <<"Specified storage supports a space.">>
};
to_json(?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE) -> #{
    <<"id">> => <<"requiresAutoStorageImportMode">>,
    <<"description">> => <<"Operation requires space with auto storage import mode.">>
};
to_json(?ERROR_STORAGE_TEST_FAILED(Operation)) -> #{
    <<"id">> => <<"storageTestFailed">>,
    <<"details">> => #{<<"operation">> => str_utils:to_binary(Operation)},
    <<"description">> => ?FMT("Failed to ~ts test file on storage.", [Operation])
};
to_json(?ERROR_REQUIRES_NON_IMPORTED_STORAGE(StorageId)) -> #{
    <<"id">> => <<"requiresNonImportedStorage">>,
    <<"details">> => #{<<"storageId">> => StorageId},
    <<"description">> => ?FMT(
        "Cannot apply for storage ~s - this operation requires a non-imported storage.",
        [StorageId]
    )
};
to_json(?ERROR_REQUIRES_IMPORTED_STORAGE(StorageId)) -> #{
    <<"id">> => <<"requiresImportedStorage">>,
    <<"details">> => #{<<"storageId">> => StorageId},
    <<"description">> => ?FMT(
        "Cannot apply for storage ~s - this operation requires an imported storage.",
        [StorageId]
    )
};
to_json(?ERROR_REQUIRES_READONLY_STORAGE(StorageIdOrType)) -> #{
    <<"id">> => <<"requiresReadonlyStorage">>,
    <<"details">> => #{<<"storageIdOrType">> => StorageIdOrType},
    <<"description">> => ?FMT(
        "Cannot apply for storage ~s - this operation requires a readonly storage.",
        [StorageIdOrType]
    )
};
to_json(?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(StorageId, PosixCompatibleStorages)) -> #{
    <<"id">> => <<"requiresPosixCompatibleStorage">>,
    <<"details">> => #{<<"storageId">> => StorageId, <<"posixCompatibleStorages">> => PosixCompatibleStorages},
    <<"description">> => ?FMT(
        "Cannot apply for storage ~s - this operation requires a POSIX-compatible storage "
        "(any of: ~s).",
        [StorageId, join_values_with_commas(PosixCompatibleStorages)]
    )
};
to_json(?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages)) -> #{
    <<"id">> => <<"autoStorageImportNotSupported">>,
    <<"details">> => #{
        <<"storageId">> => StorageId,
        <<"supportedStorages">> => SupportedStorages,
        <<"supportedObjectStorages">> => SupportedObjectStorages
    },
    <<"description">> => ?FMT(
        "Cannot configure auto storage import on storage ~s - this operation requires any of: ~s storage with canonical path type and on "
        "object storages (any of: ~s) it requires blockSize = 0.",
        [StorageId, join_values_with_commas(SupportedStorages), join_values_with_commas(SupportedObjectStorages)]
    )
};
to_json(?ERROR_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, ObjectStorages)) -> #{
    <<"id">> => <<"storageImportNotSupported">>,
    <<"details">> => #{<<"storageId">> => StorageId, <<"objectStorages">> => ObjectStorages},
    <<"description">> => ?FMT(
        "Cannot configure storage import on storage ~s - this operation requires storage with canonical path type and on "
        "object storages (any of: ~s) it requires blockSize = 0.",
        [StorageId, join_values_with_commas(ObjectStorages)]
    )
};
to_json(?ERROR_STAT_OPERATION_NOT_SUPPORTED(StorageId)) -> #{
    <<"id">> => <<"statOperationNotSupported">>,
    <<"details">> => #{<<"storageId">> => StorageId},
    <<"description">> => ?FMT(
        "Storage ~s does not support the `stat` operation or equivalent used for acquiring files metadata.",
        [StorageId]
    )
};
to_json(?ERROR_TRANSFER_ALREADY_ENDED) -> #{
    <<"id">> => <<"transferAlreadyEnded">>,
    <<"description">> => <<"Specified transfer has already ended.">>
};
to_json(?ERROR_TRANSFER_NOT_ENDED) -> #{
    <<"id">> => <<"transferNotEnded">>,
    <<"description">> => <<"Specified transfer has not ended yet.">>
};
to_json(?ERROR_VIEW_NOT_EXISTS_ON(ProviderId)) -> #{
    <<"id">> => <<"viewNotExistsOn">>,
    <<"details">> => #{
        <<"providerId">> => ProviderId
    },
    <<"description">> => ?FMT("Specified view does not exist on provider ~s.", [ProviderId])
};
to_json(?ERROR_VIEW_QUERY_FAILED(Category, Description)) -> #{
    <<"id">> => <<"viewQueryFailed">>,
    <<"details">> => #{
        <<"category">> => Category,
        <<"description">> => Description
    },
    <<"description">> => ?FMT("Query on view failed. Error category: ~s. Description: ~s.", [Category, Description])
};
to_json(?ERROR_QUOTA_EXCEEDED) -> #{
    <<"id">> => <<"quotaExceeded">>,
    <<"description">> => <<"Space's storage quota has been exceeded.">>
};

to_json(?ERROR_ATM_UNSUPPORTED_DATA_TYPE(Type, SupportedTypes)) ->
    TypeJson = atm_data_type:type_to_json(Type),
    SupportedTypesJson = lists:map(fun atm_data_type:type_to_json/1, SupportedTypes),

    #{
        <<"id">> => <<"atmUnsupportedDataType">>,
        <<"details">> => #{
            <<"type">> => TypeJson,
            <<"allowed">> => SupportedTypesJson
        },
        <<"description">> => ?FMT(
            "Bad automation data type: provided \"~s\" is not one of: ~ts.",
            [TypeJson, join_values_with_commas(SupportedTypesJson)]
        )
    };
to_json(?ERROR_ATM_DATA_TYPE_UNVERIFIED(Value, ExpType)) -> #{
    <<"id">> => <<"atmDataTypeUnverified">>,
    <<"details">> => #{
        <<"value">> => Value,
        <<"expType">> => atm_data_type:type_to_json(ExpType)
    },
    <<"description">> => <<"Provided value is not of expected type (see details).">>
};
to_json(?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(Value, Type, ValueConstraintJson)) -> #{
    <<"id">> => <<"atmDataValueConstraintUnverified">>,
    <<"details">> => #{
        <<"value">> => Value,
        <<"type">> => atm_data_type:type_to_json(Type),
        <<"valueConstraints">> => ValueConstraintJson
    },
    <<"description">> => <<"Provided value doesn't meet the constraints (see details).">>
};

to_json(?ERROR_ATM_STORE_CREATION_FAILED(AtmStoreSchemaId, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmStoreCreationFailed">>,
    <<"details">> => #{
        <<"atmStoreSchemaId">> => AtmStoreSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to create automation store (schema id: \"~s\") (see details).",
        [AtmStoreSchemaId]
    )
};
to_json(?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT) -> #{
    <<"id">> => <<"atmStoreMissingRequiredInitialContent">>,
    <<"description">> => <<"Missing initial content required to create automation store.">>
};
to_json(?ERROR_ATM_STORE_FROZEN(AtmStoreSchemaId)) -> #{
    <<"id">> => <<"atmStoreFrozen">>,
    <<"details">> => #{
        <<"atmStoreSchemaId">> => AtmStoreSchemaId
    },
    <<"description">> => ?FMT(
        "Failed to perform operation on automation store (schema id: \"~s\") as any modification is forbidden.",
        [AtmStoreSchemaId]
    )
};
to_json(?ERROR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, AllowedTypes)) ->
    AllowedTypesJson = lists:map(fun automation:store_type_to_json/1, AllowedTypes),

    #{
        <<"id">> => <<"atmStoreTypeDisallowed">>,
        <<"details">> => #{
            <<"atmStoreSchemaId">> => AtmStoreSchemaId,
            <<"allowed">> => AllowedTypesJson
        },
        <<"description">> => ?FMT(
            "Bad automation store: the type of store (schema id: \"~s\") must be one of: ~ts.",
            [AtmStoreSchemaId, join_values_with_commas(AllowedTypesJson)]
        )
    };
to_json(?ERROR_ATM_STORE_CONTENT_NOT_SET(AtmStoreSchemaId)) -> #{
    <<"id">> => <<"atmStoreContentNotSet">>,
    <<"details">> => #{
        <<"atmStoreSchemaId">> => AtmStoreSchemaId
    },
    <<"description">> => ?FMT(
        "No content has been set for this store (schema id: \"~s\").",
        [AtmStoreSchemaId]
    )
};
to_json(?ERROR_ATM_STORE_NOT_FOUND(AtmStoreSchemaId)) -> #{
    <<"id">> => <<"atmStoreNotFound">>,
    <<"details">> => #{
        <<"atmStoreSchemaId">> => AtmStoreSchemaId
    },
    <<"description">> => ?FMT(
        "Bad automation store: store (schema id: \"~s\") does not exist.",
        [AtmStoreSchemaId]
    )
};

to_json(?ERROR_ATM_WORKFLOW_EMPTY) -> #{
    <<"id">> => <<"atmWorkflowEmpty">>,
    <<"description">> => <<"Bad automation workflow: no lanes defined.">>
};

to_json(?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING) -> #{
    <<"id">> => <<"atmWorkflowExecutionStopping">>,
    <<"description">> => <<"Specified automation workflow execution is already stopping.">>
};

to_json(?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED) -> #{
    <<"id">> => <<"atmWorkflowExecutionStopped">>,
    <<"description">> => <<"Specified automation workflow execution has already stopped.">>
};

to_json(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED) -> #{
    <<"id">> => <<"atmWorkflowExecutionNotStopped">>,
    <<"description">> => <<"Specified automation workflow execution has not stopped yet.">>
};

to_json(?ERROR_ATM_WORKFLOW_EXECUTION_ENDED) -> #{
    <<"id">> => <<"atmWorkflowExecutionEnded">>,
    <<"description">> => <<"Specified automation workflow execution has already ended.">>
};

to_json(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED) -> #{
    <<"id">> => <<"atmWorkflowExecutionNotEnded">>,
    <<"description">> => <<"Specified automation workflow execution has not ended yet.">>
};

to_json(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE) -> #{
    <<"id">> => <<"atmWorkflowExecutionNotResumable">>,
    <<"description">> => <<"Specified automation workflow execution cannot be resumed.">>
};

to_json(?ERROR_ATM_LANE_EMPTY(AtmLaneSchemaId)) -> #{
    <<"id">> => <<"atmLaneEmpty">>,
    <<"details">> => #{
        <<"atmLaneSchemaId">> => AtmLaneSchemaId
    },
    <<"description">> => ?FMT(
        "Bad automation lane: lane (id: \"~s\") must not be empty.",
        [AtmLaneSchemaId]
    )
};
to_json(?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(AtmLaneSchemaId, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmLaneExecutionCreationFailed">>,
    <<"details">> => #{
        <<"atmLaneSchemaId">> => AtmLaneSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to create automation lane execution (id: \"~s\") (see details).",
        [AtmLaneSchemaId]
    )
};
to_json(?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(AtmLaneSchemaId, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmLaneExecutionInitiationFailed">>,
    <<"details">> => #{
        <<"atmLaneSchemaId">> => AtmLaneSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to initiate automation lane execution (id: \"~s\") (see details).",
        [AtmLaneSchemaId]
    )
};
to_json(?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED) -> #{
    <<"id">> => <<"atmLaneExecutionRetryFailed">>,
    <<"description">> => <<
        "Failed to retry specified lane execution. Lane execution can be retried
        only if all items have been processed but some of them failed."
    >>
};
to_json(?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED) -> #{
    <<"id">> => <<"atmLaneExecutionRerunFailed">>,
    <<"description">> => <<
        "Failed to rerun specified lane execution. Lane execution can be rerun only if
        it is in one of the following states: 'finished', 'failed', 'cancelled'."
    >>
};

to_json(?ERROR_ATM_PARALLEL_BOX_EMPTY(AtmParallelBoxSchemaId)) -> #{
    <<"id">> => <<"atmParallelBoxEmpty">>,
    <<"details">> => #{
        <<"atmParallelBoxSchemaId">> => AtmParallelBoxSchemaId
    },
    <<"description">> => ?FMT(
        "Bad automation parallel box: parallel box (id: \"~s\") must not be empty.",
        [AtmParallelBoxSchemaId]
    )
};
to_json(?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(
    AtmParallelBoxSchemaId,
    {error, _} = SpecificError
)) -> #{
    <<"id">> => <<"atmParallelBoxExecutionCreationFailed">>,
    <<"details">> => #{
        <<"atmParallelBoxSchemaId">> => AtmParallelBoxSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to create automation parallel box execution (id: \"~s\") (see details).",
        [AtmParallelBoxSchemaId]
    )
};
to_json(?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(
    AtmParallelBoxSchemaId,
    {error, _} = SpecificError
)) -> #{
    <<"id">> => <<"atmParallelBoxExecutionInitiationFailed">>,
    <<"details">> => #{
        <<"atmParallelBoxSchemaId">> => AtmParallelBoxSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to initiate automation parallel box execution (id: \"~s\") (see details).",
        [AtmParallelBoxSchemaId]
    )
};

to_json(?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(AtmTaskSchemaId, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmTaskExecutionCreationFailed">>,
    <<"details">> => #{
        <<"atmTaskSchemaId">> => AtmTaskSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to create automation task execution (id: \"~s\") (see details).",
        [AtmTaskSchemaId]
    )
};
to_json(?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(AtmTaskSchemaId, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmTaskExecutionInitiationFailed">>,
    <<"details">> => #{
        <<"atmTaskSchemaId">> => AtmTaskSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to initiate automation task execution (id: \"~s\") (see details).",
        [AtmTaskSchemaId]
    )
};
to_json(?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(ParameterName, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmLambdaConfigBadValue">>,
    <<"details">> => #{
        <<"parameterName">> => ParameterName,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Bad value provided for parameter \"~s\" of lambda config (see details).",
        [ParameterName]
    )
};
to_json(?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(ArgName)) -> #{
    <<"id">> => <<"atmTaskArgMapperForRequiredLambdaArgMissing">>,
    <<"details">> => #{
        <<"argument">> => ArgName
    },
    <<"description">> => ?FMT(
        "Missing argument mapper for required lambda argument: ~s.",
        [ArgName]
    )
};
to_json(?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(ArgName)) -> #{
    <<"id">> => <<"atmTaskArgMapperForNonexistentLambdaArg">>,
    <<"details">> => #{
        <<"argument">> => ArgName
    },
    <<"description">> => ?FMT(
        "Found excessive argument mapper for nonexistent lambda argument: ~s.",
        [ArgName]
    )
};
to_json(?ERROR_ATM_TASK_ARG_MAPPING_FAILED(ArgName, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmTaskArgMappingFailed">>,
    <<"details">> => #{
        <<"argument">> => ArgName,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to map automation task execution argument \"~s\" (see details).",
        [ArgName]
    )
};
to_json(?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, SupportedTypes)) ->
    TypeJson = atm_task_argument_value_builder:type_to_json(Type),
    SupportedTypesJson = lists:map(fun atm_task_argument_value_builder:type_to_json/1, SupportedTypes),

    #{
        <<"id">> => <<"atmTaskArgMapperUnsupportedValueBuilder">>,
        <<"details">> => #{
            <<"type">> => TypeJson,
            <<"supported">> => SupportedTypesJson
        },
        <<"description">> => ?FMT(
            "Bad automation task argument value builder: type \"~s\" not supported - must be one of: ~ts.",
            [TypeJson, join_values_with_commas(SupportedTypesJson)]
        )
    };
to_json(?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(Value, Query)) -> #{
    <<"id">> => <<"atmTaskArgMapperIteratedItemQueryFailed">>,
    <<"details">> => #{
        <<"value">> => Value,
        <<"query">> => Query
    },
    <<"description">> => <<"Failed to perform query on iterated item (see details).">>
};
to_json(?ERROR_ATM_TASK_RESULT_MISSING(MissingResultName, ReceivedResultNames)) -> #{
    <<"id">> => <<"atmTaskResultMissing">>,
    <<"details">> => #{
        <<"missingResultName">> => MissingResultName,
        <<"receivedResultNames">> => ReceivedResultNames
    },
    <<"description">> => ?FMT(
        "Missing required value for result '~s' in the lambda output. Received values for result names: ~s.",
        [MissingResultName, join_values_with_commas(ReceivedResultNames)]
    )
};
to_json(?ERROR_ATM_TASK_RESULT_MAPPING_FAILED(ResultName, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmTaskResultMappingFailed">>,
    <<"details">> => #{
        <<"result">> => ResultName,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to map automation task execution result \"~s\" (see details).",
        [ResultName]
    )
};
to_json(?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(AtmStoreSchemaId, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"atmTaskResultDispatchFailed">>,
    <<"details">> => #{
        <<"atmStoreSchemaId">> => AtmStoreSchemaId,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?FMT(
        "Failed to dispatch automation task execution result to automation store \"~s\" (see details).",
        [AtmStoreSchemaId]
    )
};
to_json(?ERROR_ATM_TASK_EXECUTION_STOPPED) -> #{
    <<"id">> => <<"atmTaskExecutionEnded">>,
    <<"description">> => <<"Specified automation task execution has already stopped.">>
};

to_json(?ERROR_ATM_JOB_BATCH_WITHDRAWN(Reason)) -> #{
    <<"id">> => <<"atmJobBatchWithdrawn">>,
    <<"details">> => #{
        <<"reason">> => Reason
    },
    <<"description">> => <<"Previosuly scheduled job batch has been withdrawn.">>
};
to_json(?ERROR_ATM_JOB_BATCH_CRASHED(Reason)) -> #{
    <<"id">> => <<"atmJobBatchCrashed">>,
    <<"details">> => #{
        <<"reason">> => Reason
    },
    <<"description">> => <<"Job batch execution has crashed.">>
};

to_json(?ERROR_ATM_OPENFAAS_NOT_CONFIGURED) -> #{
    <<"id">> => <<"atmOpenfaasNotConfigured">>,
    <<"description">> => <<"OpenFaaS service is not configured.">>
};
to_json(?ERROR_ATM_OPENFAAS_UNREACHABLE) -> #{
    <<"id">> => <<"atmOpenfaasUnreachable">>,
    <<"description">> => <<"Cannot connect to OpenFaaS service.">>
};
to_json(?ERROR_ATM_OPENFAAS_UNHEALTHY) -> #{
    <<"id">> => <<"atmOpenfaasUnhealthy">>,
    <<"description">> => <<"OpenFaaS service is unhealthy.">>
};
to_json(?ERROR_ATM_OPENFAAS_QUERY_FAILED) -> #{
    <<"id">> => <<"atmOpenfaasQueryFailed">>,
    <<"description">> => <<"Failed to query OpenFaaS service.">>
};
to_json(?ERROR_ATM_OPENFAAS_QUERY_FAILED(Reason)) -> #{
    <<"id">> => <<"atmOpenfaasQueryFailed">>,
    <<"details">> => #{
        <<"reason">> => Reason
    },
    <<"description">> => <<"Failed to query OpenFaaS service (see details).">>
};
to_json(?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED) -> #{
    <<"id">> => <<"atmOpenfaasFunctionRegistrationFailed">>,
    <<"description">> => <<"Failed to register function in OpenFaaS service.">>
};

to_json(?ERROR_ATM_INVALID_STATUS_TRANSITION(PrevStatus, NewStatus)) -> #{
    <<"id">> => <<"atmInvalidStatusTransition">>,
    <<"details">> => #{
        <<"prevStatus">> => atom_to_binary(PrevStatus, utf8),
        <<"newStatus">> => atom_to_binary(NewStatus, utf8)
    },
    <<"description">> => <<"Invalid status transition (see details).">>
};

to_json(?ERROR_DIR_STATS_DISABLED_FOR_SPACE) -> #{
    <<"id">> => <<"dirStatsDisabledForSpace">>,
    <<"description">> => <<"Directory statistics collection is disabled for this space.">>
};

to_json(?ERROR_DIR_STATS_NOT_READY) -> #{
    <<"id">> => <<"dirStatsNotReady">>,
    <<"description">> => <<"Requested directory statistics are not ready yet - calculation is in progress.">>
};

to_json(?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(CurrentState, AllowedStates)) -> #{
    <<"id">> => <<"forbiddenForCurrentArchiveState">>,
    <<"description">> => ?FMT(
        "This operation is forbidden while the archive state is ~s. Allowed states are: ~s.",
        [CurrentState, join_values_with_commas(AllowedStates)]
    ),
    <<"details">> => #{
        <<"allowedStates">> => json_utils:encode(AllowedStates),
        <<"currentState">> => json_utils:encode(CurrentState)
    }
};

to_json(?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ParentArchiveId)) -> #{
    <<"id">> =>
    <<"nestedArchiveDeletionForbidden">>,
    <<"description">> =>
    <<"This archive cannot be deleted since it is nested in another archive.">>,
    <<"details">> => #{
        <<"parentArchiveId">> => ParentArchiveId
    }
};

to_json(?ERROR_RECALL_TARGET_CONFLICT) -> #{
    <<"id">> => <<"recallTargetConflict">>,
    <<"description">> => <<"Conflict - recall target cannot be within a directory that is being recalled.">>
};

%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
to_json(?ERROR_ON_NODES(Error, Hostnames)) ->
    #{<<"description">> := Description} = InnerError = to_json(Error),
    #{
        <<"id">> => <<"errorOnNodes">>,
        <<"details">> => #{
            <<"error">> => InnerError,
            <<"hostnames">> => Hostnames
        },
        <<"description">> => ?FMT("Error on nodes ~ts: ~ts",
            [join_values_with_commas(Hostnames), Description])
    };
to_json(?ERROR_DNS_SERVERS_UNREACHABLE(UsedServers)) ->
    Servers = lists:map(fun
        (default) -> ?DNS_DEFAULTS;
        (IP) -> element(2, {ok, _} = ip_utils:to_binary(IP))
    end, UsedServers),
    #{
        <<"id">> => <<"dnsServersUnreachable">>,
        <<"details">> => #{
            <<"servers">> => Servers
        },
        <<"description">> => ?FMT("Error fetching DNS records. Used servers: ~ts.",
            [join_values_with_commas(Servers)])
    };
to_json(?ERROR_FILE_ALLOCATION(ActualSize, TargetSize)) -> #{
    <<"id">> => <<"fileAllocation">>,
    <<"description">> => ?FMT("File allocation error. Allocated ~s out of ~s.",
        [str_utils:format_byte_size(ActualSize), str_utils:format_byte_size(TargetSize)]),
    <<"details">> => #{
        <<"actualSize">> => ActualSize,
        <<"targetSize">> => TargetSize
    }
};
to_json(?ERROR_LETS_ENCRYPT_NOT_REACHABLE) -> #{
    <<"id">> => <<"letsEncryptNotReachable">>,
    <<"description">> => <<"Connection to Let's Encrypt server failed.">>
};
to_json(?ERROR_LETS_ENCRYPT_RESPONSE(ProblemDocument, ErrorMessage)) -> #{
    <<"id">> => <<"letsEncryptResponse">>,
    <<"description">> => ?FMT("Bad Let's Encrypt response: ~ts.", [ErrorMessage]),
    <<"details">> => #{
        <<"problemDocument">> => utils:undefined_to_null(ProblemDocument),
        <<"errorMessage">> => ErrorMessage
    }
};
to_json(?ERROR_NODE_ALREADY_IN_CLUSTER(Hostname)) -> #{
    <<"id">> => <<"nodeAlreadyInCluster">>,
    <<"details">> => #{<<"hostname">> => Hostname},
    <<"description">> => ?FMT("Cannot add \"~ts\", it is already part of a cluster.", [Hostname])
};
to_json(?ERROR_NODE_NOT_COMPATIBLE(Hostname, NodeClusterType)) when
    NodeClusterType == ?ONEPROVIDER orelse NodeClusterType == ?ONEZONE -> #{
    <<"id">> => <<"nodeNotCompatible">>,
    <<"details">> => #{
        <<"hostname">> => Hostname, <<"clusterType">> => NodeClusterType},
    <<"description">> => ?FMT("Cannot add \"~ts\", it is a ~ts node.",
        [Hostname, NodeClusterType])
};
to_json(?ERROR_NO_CONNECTION_TO_NEW_NODE(Hostname)) -> #{
    <<"id">> => <<"noConnectionToNewNode">>,
    <<"details">> => #{<<"hostname">> => Hostname},
    <<"description">> => ?FMT("Cannot add node \"~ts\", connection failed.", [Hostname])
};
to_json(?ERROR_NO_SERVICE_NODES(Service)) -> #{
    <<"id">> => <<"noServiceNodes">>,
    <<"description">> => ?FMT("Service ~s is not deployed on any node.", [Service]),
    <<"details">> => #{
        <<"service">> => Service
    }
};
to_json(?ERROR_USER_NOT_IN_CLUSTER) -> #{
    <<"id">> => <<"userNotInCluster">>,
    <<"description">> => <<"Authenticated user is not a member of this cluster.">>};

%%--------------------------------------------------------------------
%% Unknown error
%%--------------------------------------------------------------------
to_json(?ERROR_UNRECOGNIZED_ERROR(ErrorAsJson)) ->
    % Carries errors that have not been recognized upon decoding.
    case maps:is_key(<<"description">>, ErrorAsJson) of
        true ->
            ErrorAsJson;
        false ->
            ErrorAsJson#{<<"description">> => <<"No description (unknown error).">>}
    end;
to_json(OtherError) ->
    % Wildcard to catch all errors that might be returned by the application logic, but does
    % not match any error defined in this module. Inability to translate is treated as an
    % unexpected exception (an ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef) is returned).
    ReturnedError = ?catch_exceptions(error({cannot_translate_error, OtherError})),
    to_json(ReturnedError).


-spec from_json(as_json()) -> undefined | error().
from_json(null) ->
    undefined;

%% -----------------------------------------------------------------------------
%% General errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"badMessage">>, <<"details">> := #{<<"message">> := Msg}}) ->
    ?ERROR_BAD_MESSAGE(Msg);

from_json(#{<<"id">> := <<"noConnectionToOnezone">>}) ->
    ?ERROR_NO_CONNECTION_TO_ONEZONE;

from_json(#{<<"id">> := <<"noConnectionToPeerOneprovider">>}) ->
    ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER;

from_json(#{<<"id">> := <<"noConnectionToClusterNode">>}) ->
    ?ERROR_NO_CONNECTION_TO_CLUSTER_NODE;

from_json(#{<<"id">> := <<"unregisteredOneprovider">>}) ->
    ?ERROR_UNREGISTERED_ONEPROVIDER;

from_json(#{<<"id">> := <<"internalServerError">>, <<"details">> := #{<<"reference">> := ErrorRef}}) ->
    ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef);

from_json(#{<<"id">> := <<"internalServerError">>}) ->
    ?ERROR_INTERNAL_SERVER_ERROR;

from_json(#{<<"id">> := <<"notImplemented">>}) ->
    ?ERROR_NOT_IMPLEMENTED;

from_json(#{<<"id">> := <<"notSupported">>}) ->
    ?ERROR_NOT_SUPPORTED;

from_json(#{<<"id">> := <<"serviceUnavailable">>}) ->
    ?ERROR_SERVICE_UNAVAILABLE;

from_json(#{<<"id">> := <<"timeout">>}) ->
    ?ERROR_TIMEOUT;

from_json(#{<<"id">> := <<"temporaryFailure">>}) ->
    ?ERROR_TEMPORARY_FAILURE;

from_json(#{<<"id">> := <<"externalServiceOperationFailed">>, <<"details">> := #{<<"serviceName">> := ServiceName}}) ->
    ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(ServiceName);

from_json(#{<<"id">> := <<"unauthorized">>, <<"details">> := #{<<"authError">> := AuthError}}) ->
    ?ERROR_UNAUTHORIZED(from_json(AuthError));

from_json(#{<<"id">> := <<"unauthorized">>}) ->
    ?ERROR_UNAUTHORIZED;

from_json(#{<<"id">> := <<"forbidden">>, <<"details">> := #{<<"hint">> := HumanReadableHint}}) ->
    ?ERROR_FORBIDDEN(HumanReadableHint);

from_json(#{<<"id">> := <<"forbidden">>}) ->
    ?ERROR_FORBIDDEN;

from_json(#{<<"id">> := <<"notFound">>}) ->
    ?ERROR_NOT_FOUND;

from_json(#{<<"id">> := <<"alreadyExists">>}) ->
    ?ERROR_ALREADY_EXISTS;

from_json(#{<<"id">> := <<"fileAccess">>, <<"details">> := #{<<"path">> := Path, <<"errno">> := Errno}}) ->
    ?ERROR_FILE_ACCESS(Path, binary_to_existing_atom(Errno, utf8));

from_json(#{<<"id">> := <<"limitReached">>, <<"details">> := #{
    <<"limit">> := Limit, <<"resourceDescription">> := ResourceDescription
}}) ->
    ?ERROR_LIMIT_REACHED(Limit, ResourceDescription);

%% -----------------------------------------------------------------------------
%% POSIX errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"posix">>, <<"details">> := #{<<"errno">> := Errno}}) ->
    ?ERROR_POSIX(binary_to_existing_atom(Errno, utf8));

%% -----------------------------------------------------------------------------
%% Auth errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"userBlocked">>}) ->
    ?ERROR_USER_BLOCKED;

from_json(#{<<"id">> := <<"badBasicCredentials">>}) ->
    ?ERROR_BAD_BASIC_CREDENTIALS;

from_json(#{<<"id">> := <<"badIdpAccessToken">>, <<"details">> := #{<<"idp">> := IdP}}) ->
    ?ERROR_BAD_IDP_ACCESS_TOKEN(IdP);

from_json(#{<<"id">> := <<"badToken">>}) ->
    ?ERROR_BAD_TOKEN;

from_json(#{<<"id">> := <<"badServiceToken">>, <<"details">> := #{<<"tokenError">> := TokenError}}) ->
    ?ERROR_BAD_SERVICE_TOKEN(from_json(TokenError));

from_json(#{<<"id">> := <<"badConsumerToken">>, <<"details">> := #{<<"tokenError">> := TokenError}}) ->
    ?ERROR_BAD_CONSUMER_TOKEN(from_json(TokenError));

from_json(#{<<"id">> := <<"tokenInvalid">>}) ->
    ?ERROR_TOKEN_INVALID;

from_json(#{<<"id">> := <<"tokenRevoked">>}) ->
    ?ERROR_TOKEN_REVOKED;

from_json(#{<<"id">> := <<"tokenTooLarge">>, <<"details">> := #{<<"limit">> := SizeLimit}}) ->
    ?ERROR_TOKEN_TOO_LARGE(SizeLimit);

from_json(#{<<"id">> := <<"notAnAccessToken">>, <<"details">> := #{<<"received">> := ReceivedTokenType}}) ->
    ?ERROR_NOT_AN_ACCESS_TOKEN(token_type:from_json(ReceivedTokenType));

from_json(#{<<"id">> := <<"notAnIdentityToken">>, <<"details">> := #{<<"received">> := ReceivedTokenType}}) ->
    ?ERROR_NOT_AN_IDENTITY_TOKEN(token_type:from_json(ReceivedTokenType));

from_json(#{<<"id">> := <<"notAnInviteToken">>, <<"details">> := Details}) ->
    #{<<"expectedInviteType">> := ExpectedInviteTypeStr, <<"received">> := RecvType} = Details,
    ExpectedInviteType = case ExpectedInviteTypeStr of
        <<"any">> -> any;
        _ -> token_type:invite_type_from_str(ExpectedInviteTypeStr)
    end,
    ?ERROR_NOT_AN_INVITE_TOKEN(ExpectedInviteType, token_type:from_json(RecvType));

from_json(#{<<"id">> := <<"tokenCaveatUnknown">>, <<"details">> := #{<<"caveat">> := CaveatBinary}}) ->
    ?ERROR_TOKEN_CAVEAT_UNKNOWN(CaveatBinary);

from_json(#{<<"id">> := <<"tokenCaveatUnverified">>, <<"details">> := #{<<"caveat">> := Caveat}}) ->
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:from_json(Caveat));

from_json(#{<<"id">> := <<"tokenTimeCaveatRequired">>, <<"details">> := #{<<"maxTtl">> := MaxTtl}}) ->
    ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl);

from_json(#{<<"id">> := <<"tokenSubjectInvalid">>}) ->
    ?ERROR_TOKEN_SUBJECT_INVALID;

from_json(#{<<"id">> := <<"tokenServiceForbidden">>, <<"details">> := #{<<"service">> := Service}}) ->
    ?ERROR_TOKEN_SERVICE_FORBIDDEN(aai:service_from_json(Service));

from_json(#{<<"id">> := <<"inviteTokenSubjectNotAuthorized">>}) ->
    ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED;

from_json(#{<<"id">> := <<"inviteTokenUsageLimitReached">>}) ->
    ?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED;

from_json(#{<<"id">> := <<"inviteTokenConsumerInvalid">>, <<"details">> := #{<<"consumer">> := Consumer}}) ->
    ?ERROR_INVITE_TOKEN_CONSUMER_INVALID(aai:subject_from_json(Consumer));

from_json(#{<<"id">> := <<"inviteTokenTargetIdInvalid">>, <<"details">> := #{<<"id">> := Id}}) ->
    ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(Id);

from_json(#{<<"id">> := <<"tokenSessionInvalid">>}) ->
    ?ERROR_TOKEN_SESSION_INVALID;

%% -----------------------------------------------------------------------------
%% Graph Sync errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"expectedHandshakeMessage">>}) ->
    ?ERROR_EXPECTED_HANDSHAKE_MESSAGE;

from_json(#{<<"id">> := <<"handshakeAlreadyDone">>}) ->
    ?ERROR_HANDSHAKE_ALREADY_DONE;

from_json(#{<<"id">> := <<"badVersion">>, <<"details">> := #{<<"supportedVersions">> := SupportedVersions}}) ->
    ?ERROR_BAD_VERSION(SupportedVersions);

from_json(#{<<"id">> := <<"badGRI">>}) ->
    ?ERROR_BAD_GRI;

from_json(#{<<"id">> := <<"rpcUndefined">>}) ->
    ?ERROR_RPC_UNDEFINED;

from_json(#{<<"id">> := <<"notSubscribable">>}) ->
    ?ERROR_NOT_SUBSCRIBABLE;

%% -----------------------------------------------------------------------------
%% Data validation errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"malformedData">>}) ->
    ?ERROR_MALFORMED_DATA;

from_json(#{<<"id">> := <<"missingRequiredValue">>,
    <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_MISSING_REQUIRED_VALUE(Key);

from_json(#{<<"id">> := <<"missingAtLeastOneValue">>, <<"details">> := #{<<"keys">> := Keys}}) ->
    ?ERROR_MISSING_AT_LEAST_ONE_VALUE(Keys);

from_json(#{<<"id">> := <<"badData">>, <<"details">> := #{<<"key">> := Key, <<"specificError">> := SpecificError}}) ->
    ?ERROR_BAD_DATA(Key, from_json(SpecificError));

from_json(#{<<"id">> := <<"badData">>, <<"details">> := #{<<"key">> := Key, <<"hint">> := HumanReadableHint}}) ->
    ?ERROR_BAD_DATA(Key, HumanReadableHint);

from_json(#{<<"id">> := <<"badData">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_DATA(Key);

from_json(#{<<"id">> := <<"badValueEmpty">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_EMPTY(Key);

from_json(#{<<"id">> := <<"badValueBoolean">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_BOOLEAN(Key);

from_json(#{<<"id">> := <<"badValueString">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_BINARY(Key);

from_json(#{<<"id">> := <<"badValueTextTooLarge">>, <<"details">> := #{<<"key">> := Key, <<"limit">> := SizeLimit}}) ->
    ?ERROR_BAD_VALUE_TEXT_TOO_LARGE(Key, SizeLimit);

from_json(#{<<"id">> := <<"badValueListOfStrings">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key);

from_json(#{<<"id">> := <<"badValueInteger">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_INTEGER(Key);

from_json(#{<<"id">> := <<"badValueFloat">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_FLOAT(Key);

from_json(#{<<"id">> := <<"badValueJSON">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_JSON(Key);

from_json(#{<<"id">> := <<"badValueXML">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_XML(Key);

from_json(#{<<"id">> := <<"badValueToken">>, <<"details">> := #{<<"key">> := Key, <<"tokenError">> := TokenError}}) ->
    ?ERROR_BAD_VALUE_TOKEN(Key, from_json(TokenError));

from_json(#{<<"id">> := <<"badValueTokenType">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_TOKEN_TYPE(Key);

from_json(#{<<"id">> := <<"badValueInviteType">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_INVITE_TYPE(Key);

from_json(#{<<"id">> := <<"badValueIPv4Address">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_IPV4_ADDRESS(Key);

from_json(#{<<"id">> := <<"badValueListOfIPv4Addresses">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key);

from_json(#{<<"id">> := <<"badValueTooLow">>, <<"details">> := #{<<"key">> := Key, <<"limit">> := Limit}}) ->
    ?ERROR_BAD_VALUE_TOO_LOW(Key, Limit);

from_json(#{<<"id">> := <<"badValueTooHigh">>, <<"details">> := #{<<"key">> := Key, <<"limit">> := Limit}}) ->
    ?ERROR_BAD_VALUE_TOO_HIGH(Key, Limit);

from_json(#{<<"id">> := <<"badValueNotInRange">>, <<"details">> := #{<<"key">> := Key, <<"low">> := Low, <<"high">> := High}}) ->
    ?ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High);

from_json(#{<<"id">> := <<"badValueNotAllowed">>, <<"details">> := #{<<"key">> := Key, <<"allowed">> := Allowed}}) ->
    ?ERROR_BAD_VALUE_NOT_ALLOWED(Key, Allowed);

from_json(#{<<"id">> := <<"badValueListNotAllowed">>, <<"details">> := #{<<"key">> := Key, <<"allowed">> := Allowed}}) ->
    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, Allowed);

from_json(#{<<"id">> := <<"badValueIdNotFound">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_ID_NOT_FOUND(Key);

from_json(#{<<"id">> := <<"badValueAmbiguousId">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_AMBIGUOUS_ID(Key);

from_json(#{<<"id">> := <<"badValueIdentifier">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_IDENTIFIER(Key);

from_json(#{<<"id">> := <<"badValueIdentifierOccupied">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key);

from_json(#{<<"id">> := <<"badValueOctal">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_OCTAL(Key);

from_json(#{<<"id">> := <<"badValueFilePath">>}) ->
    ?ERROR_BAD_VALUE_FILE_PATH;

from_json(#{<<"id">> := <<"badValueFullName">>}) ->
    ?ERROR_BAD_VALUE_FULL_NAME;

from_json(#{<<"id">> := <<"badValueUsername">>}) ->
    ?ERROR_BAD_VALUE_USERNAME;

from_json(#{<<"id">> := <<"badValuePassword">>}) ->
    ?ERROR_BAD_VALUE_PASSWORD;

from_json(#{<<"id">> := <<"badValueEmail">>}) ->
    ?ERROR_BAD_VALUE_EMAIL;

from_json(#{<<"id">> := <<"badValueName">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_NAME(Key);

from_json(#{<<"id">> := <<"badValueName">>}) ->
    ?ERROR_BAD_VALUE_NAME;

from_json(#{<<"id">> := <<"badValueDomain">>}) ->
    ?ERROR_BAD_VALUE_DOMAIN;

from_json(#{<<"id">> := <<"badValueSubdomain">>}) ->
    ?ERROR_BAD_VALUE_SUBDOMAIN;

from_json(#{<<"id">> := <<"badValueCaveat">>, <<"details">> := #{<<"caveat">> := CaveatJson}}) ->
    ?ERROR_BAD_VALUE_CAVEAT(CaveatJson);

from_json(#{<<"id">> := <<"badValueQoSParameters">>}) ->
    ?ERROR_BAD_VALUE_QOS_PARAMETERS;

from_json(#{<<"id">> := <<"timeSeriesCollectionMissingLayout">>, <<"details">> := #{<<"missingLayout">> := MissingLayout}}) ->
    ?ERROR_TSC_MISSING_LAYOUT(MissingLayout);

from_json(#{<<"id">> := <<"timeSeriesCollectionTooManyMetrics">>, <<"details">> := #{<<"limit">> := Limit}}) ->
    ?ERROR_TSC_TOO_MANY_METRICS(Limit);

from_json(#{<<"id">> := <<"badValueTimeSeriesCollectionConflictingMetricConfig">>, <<"details">> := #{
    <<"timeSeriesName">> := TSName,
    <<"metricName">> := MetricName,
    <<"existingMetricConfig">> := ExistingMetricConfig,
    <<"conflictingMetricConfig">> := ConflictingMetricConfig
}}) ->
    ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(
        TSName, MetricName,
        jsonable_record:from_json(ExistingMetricConfig, metric_config),
        jsonable_record:from_json(ConflictingMetricConfig, metric_config)
    );

from_json(#{<<"id">> := <<"badGuiPackage">>}) ->
    ?ERROR_BAD_GUI_PACKAGE;

from_json(#{<<"id">> := <<"guiPackageTooLarge">>}) ->
    ?ERROR_GUI_PACKAGE_TOO_LARGE;

from_json(#{<<"id">> := <<"guiPackageUnverified">>, <<"details">> := #{<<"shaSum">> := ShaSum}}) ->
    ?ERROR_GUI_PACKAGE_UNVERIFIED(ShaSum);

from_json(#{<<"id">> := <<"invalidQosExpression">>, <<"details">> := #{<<"reason">> := Reason}}) ->
    ?ERROR_INVALID_QOS_EXPRESSION(Reason);

from_json(#{<<"id">> := <<"illegalSupportStageTransition">>, <<"details">> := #{
    <<"currentProviderStage">> := ProviderStageJson,
    <<"currentStorageStage">> := StorageStageJson
}}) ->
    ProviderStage = support_stage:deserialize(provider, ProviderStageJson),
    StorageStage = support_stage:deserialize(storage, StorageStageJson),
    ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ProviderStage, StorageStage);

%% -----------------------------------------------------------------------------
%% oz_worker errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"basicAuthNotSupported">>}) ->
    ?ERROR_BASIC_AUTH_NOT_SUPPORTED;

from_json(#{<<"id">> := <<"basicAuthDisabled">>}) ->
    ?ERROR_BASIC_AUTH_DISABLED;

from_json(#{<<"id">> := <<"subdomainDelegationNotSupported">>}) ->
    ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED;

from_json(#{<<"id">> := <<"subdomainDelegationDisabled">>}) ->
    ?ERROR_SUBDOMAIN_DELEGATION_DISABLED;

from_json(#{<<"id">> := <<"spaceMarketplaceDisabled">>}) ->
    ?ERROR_SPACE_MARKETPLACE_DISABLED;

from_json(#{<<"id">> := <<"protectedGroup">>}) ->
    ?ERROR_PROTECTED_GROUP;

from_json(#{<<"id">> := <<"atmLambdaInUse">>, <<"details">> := #{<<"atmWorkflowSchemas">> := AtmWorkflowSchemas}}) ->
    ?ERROR_ATM_LAMBDA_IN_USE(AtmWorkflowSchemas);

from_json(#{<<"id">> := <<"cannotRemoveLastOwner">>, <<"details">> := #{<<"entityType">> := EntType, <<"entityId">> := EntId}}) ->
    ?ERROR_CANNOT_REMOVE_LAST_OWNER(binary_to_existing_atom(EntType, utf8), EntId);

from_json(#{<<"id">> := <<"cannotDeleteEntity">>, <<"details">> := #{<<"entityType">> := EntType, <<"entityId">> := EntId}}) ->
    ?ERROR_CANNOT_DELETE_ENTITY(binary_to_existing_atom(EntType, utf8), EntId);

from_json(#{<<"id">> := <<"cannotAddRelationToSelf">>}) ->
    ?ERROR_CANNOT_ADD_RELATION_TO_SELF;

from_json(#{<<"id">> := <<"relationDoesNotExist">>, <<"details">> := #{
    <<"childType">> := ChType, <<"childId">> := ChId, <<"parentType">> := ParType, <<"parentId">> := ParId}
}) ->
    ChTypeAtom = binary_to_existing_atom(ChType, utf8),
    ParTypeAtom = binary_to_existing_atom(ParType, utf8),
    ?ERROR_RELATION_DOES_NOT_EXIST(ChTypeAtom, ChId, ParTypeAtom, ParId);

from_json(#{<<"id">> := <<"relationAlreadyExists">>, <<"details">> := #{
    <<"childType">> := ChType, <<"childId">> := ChId, <<"parentType">> := ParType, <<"parentId">> := ParId}
}) ->
    ChTypeAtom = gri:deserialize_type(ChType),
    ParTypeAtom = gri:deserialize_type(ParType),
    ?ERROR_RELATION_ALREADY_EXISTS(ChTypeAtom, ChId, ParTypeAtom, ParId);
from_json(#{<<"id">> := <<"spaceAlreadySupportedWithImportedStorage">>, <<"details">> := #{
    <<"spaceId">> := SpaceId, <<"storageId">> := StorageId}
}) ->
    ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(SpaceId, StorageId);
from_json(#{<<"id">> := <<"cannotDeleteNonEmptyHandleService">>}) ->
    ?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE;

%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
from_json(#{<<"id">> := <<"userNotSupported">>}) ->
    ?ERROR_USER_NOT_SUPPORTED;

from_json(#{<<"id">> := <<"autoCleaningDisabled">>}) ->
    ?ERROR_AUTO_CLEANING_DISABLED;

from_json(#{<<"id">> := <<"filePopularityDisabled">>}) ->
    ?ERROR_FILE_POPULARITY_DISABLED;

from_json(#{<<"id">> := <<"spaceNotSupportedBy">>, <<"details">> := #{
    <<"spaceId">> := SpaceId,
    <<"providerId">> := ProviderId
}}) ->
    ?ERROR_SPACE_NOT_SUPPORTED_BY(SpaceId, ProviderId);

from_json(#{<<"id">> := <<"notALocalStorageSupportingSpace">>, <<"details">> := #{
    <<"providerId">> := ProviderId,
    <<"storageId">> := StorageId,
    <<"spaceId">> := SpaceId
}}) ->
    ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(ProviderId, StorageId, SpaceId);

from_json(#{<<"id">> := <<"storageInUse">>}) ->
    ?ERROR_STORAGE_IN_USE;

from_json(#{<<"id">> := <<"requiresAutoStorageImportMode">>}) ->
    ?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE;

from_json(#{<<"id">> := <<"storageTestFailed">>, <<"details">> := #{<<"operation">> := Operation}})
    when Operation == <<"read">>; Operation == <<"write">>; Operation == <<"remove">> ->
    ?ERROR_STORAGE_TEST_FAILED(binary_to_atom(Operation, utf8));

from_json(#{<<"id">> := <<"requiresNonImportedStorage">>, <<"details">> := #{<<"storageId">> := StorageId}}) ->
    ?ERROR_REQUIRES_NON_IMPORTED_STORAGE(StorageId);

from_json(#{<<"id">> := <<"requiresImportedStorage">>, <<"details">> := #{<<"storageId">> := StorageId}}) ->
    ?ERROR_REQUIRES_IMPORTED_STORAGE(StorageId);

from_json(#{<<"id">> := <<"requiresReadonlyStorage">>, <<"details">> := #{<<"storageIdOrType">> := StorageIdOrType}}) ->
    ?ERROR_REQUIRES_READONLY_STORAGE(StorageIdOrType);

from_json(#{<<"id">> := <<"requiresPosixCompatibleStorage">>, <<"details">> := #{
    <<"storageId">> := StorageId,
    <<"posixCompatibleStorages">> := PosixCompatibleStorages
}}) ->
    ?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(StorageId, PosixCompatibleStorages);

from_json(#{<<"id">> := <<"autoStorageImportNotSupported">>, <<"details">> := #{
    <<"storageId">> := StorageId,
    <<"supportedStorages">> := SupportedStorages,
    <<"supportedObjectStorages">> := SupportedObjectStorages
}}) ->
    ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages);


from_json(#{<<"id">> := <<"storageImportNotSupported">>, <<"details">> := #{
    <<"storageId">> := StorageId,
    <<"objectStorages">> := ObjectStorages
}}) ->
    ?ERROR_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, ObjectStorages);

from_json(#{<<"id">> := <<"statOperationNotSupported">>, <<"details">> := #{
    <<"storageId">> := StorageId
}}) ->
    ?ERROR_STAT_OPERATION_NOT_SUPPORTED(StorageId);

from_json(#{<<"id">> := <<"transferAlreadyEnded">>}) ->
    ?ERROR_TRANSFER_ALREADY_ENDED;

from_json(#{<<"id">> := <<"transferNotEnded">>}) ->
    ?ERROR_TRANSFER_NOT_ENDED;

from_json(#{<<"id">> := <<"viewNotExistsOn">>, <<"details">> := #{<<"providerId">> := ProviderId}}) ->
    ?ERROR_VIEW_NOT_EXISTS_ON(ProviderId);

from_json(#{<<"id">> := <<"viewQueryFailed">>, <<"details">> := #{
    <<"category">> := Category,
    <<"description">> := Description
}}) ->
    ?ERROR_VIEW_QUERY_FAILED(Category, Description);

from_json(#{<<"id">> := <<"quotaExceeded">>}) ->
    ?ERROR_QUOTA_EXCEEDED;

from_json(#{
    <<"id">> := <<"atmUnsupportedDataType">>,
    <<"details">> := #{
        <<"type">> := TypeJson,
        <<"allowed">> := SupportedTypesJson
    }
}) ->
    Type = atm_data_type:type_from_json(TypeJson),
    SupportedTypes = lists:map(fun atm_data_type:type_from_json/1, SupportedTypesJson),

    ?ERROR_ATM_UNSUPPORTED_DATA_TYPE(Type, SupportedTypes);

from_json(#{
    <<"id">> := <<"atmDataTypeUnverified">>,
    <<"details">> := #{
        <<"value">> := Value,
        <<"expType">> := ExpTypeJson
    }
}) ->
    ?ERROR_ATM_DATA_TYPE_UNVERIFIED(Value, atm_data_type:type_from_json(ExpTypeJson));

from_json(#{
    <<"id">> := <<"atmDataValueConstraintUnverified">>,
    <<"details">> := #{
        <<"value">> := Value,
        <<"type">> := TypeJson,
        <<"valueConstraints">> := ValueConstraintsJson
    }
}) ->
    Type = atm_data_type:type_from_json(TypeJson),

    ?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(Value, Type, ValueConstraintsJson);

from_json(#{
    <<"id">> := <<"atmStoreCreationFailed">>,
    <<"details">> := #{
        <<"atmStoreSchemaId">> := AtmStoreSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_STORE_CREATION_FAILED(AtmStoreSchemaId, from_json(SpecificErrorJson));

from_json(#{<<"id">> := <<"atmStoreMissingRequiredInitialContent">>}) ->
    ?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT;

from_json(#{
    <<"id">> := <<"atmStoreFrozen">>,
    <<"details">> := #{
        <<"atmStoreSchemaId">> := AtmStoreSchemaId
    }
}) ->
    ?ERROR_ATM_STORE_FROZEN(AtmStoreSchemaId);

from_json(#{
    <<"id">> := <<"atmStoreTypeDisallowed">>,
    <<"details">> := #{
        <<"atmStoreSchemaId">> := AtmStoreSchemaId,
        <<"allowed">> := AllowedTypesJson
    }
}) ->
    AllowedTypes = lists:map(fun automation:store_type_from_json/1, AllowedTypesJson),
    ?ERROR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, AllowedTypes);

from_json(#{
    <<"id">> := <<"atmStoreContentNotSet">>,
    <<"details">> := #{
        <<"atmStoreSchemaId">> := AtmStoreSchemaId
    }
}) ->
    ?ERROR_ATM_STORE_CONTENT_NOT_SET(AtmStoreSchemaId);

from_json(#{
    <<"id">> := <<"atmStoreNotFound">>,
    <<"details">> := #{
        <<"atmStoreSchemaId">> := AtmStoreSchemaId
    }
}) ->
    ?ERROR_ATM_STORE_NOT_FOUND(AtmStoreSchemaId);

from_json(#{<<"id">> := <<"atmWorkflowEmpty">>}) ->
    ?ERROR_ATM_WORKFLOW_EMPTY;

from_json(#{<<"id">> := <<"atmWorkflowExecutionStopping">>}) ->
    ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING;

from_json(#{<<"id">> := <<"atmWorkflowExecutionStopped">>}) ->
    ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED;

from_json(#{<<"id">> := <<"atmWorkflowExecutionNotStopped">>}) ->
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED;

from_json(#{<<"id">> := <<"atmWorkflowExecutionEnded">>}) ->
    ?ERROR_ATM_WORKFLOW_EXECUTION_ENDED;

from_json(#{<<"id">> := <<"atmWorkflowExecutionNotEnded">>}) ->
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED;

from_json(#{<<"id">> := <<"atmWorkflowExecutionNotResumable">>}) ->
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE;

from_json(#{
    <<"id">> := <<"atmLaneEmpty">>,
    <<"details">> := #{
        <<"atmLaneSchemaId">> := AtmLaneSchemaId
    }
}) ->
    ?ERROR_ATM_LANE_EMPTY(AtmLaneSchemaId);

from_json(#{
    <<"id">> := <<"atmLaneExecutionCreationFailed">>,
    <<"details">> := #{
        <<"atmLaneSchemaId">> := AtmLaneSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(AtmLaneSchemaId, from_json(SpecificErrorJson));

from_json(#{
    <<"id">> := <<"atmLaneExecutionInitiationFailed">>,
    <<"details">> := #{
        <<"atmLaneSchemaId">> := AtmLaneSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(AtmLaneSchemaId, from_json(SpecificErrorJson));

from_json(#{<<"id">> := <<"atmLaneExecutionRetryFailed">>}) ->
    ?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED;

from_json(#{<<"id">> := <<"atmLaneExecutionRerunFailed">>}) ->
    ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED;

from_json(#{
    <<"id">> := <<"atmParallelBoxEmpty">>,
    <<"details">> := #{
        <<"atmParallelBoxSchemaId">> := AtmParallelBoxSchemaId
    }
}) ->
    ?ERROR_ATM_PARALLEL_BOX_EMPTY(AtmParallelBoxSchemaId);

from_json(#{
    <<"id">> := <<"atmParallelBoxExecutionCreationFailed">>,
    <<"details">> := #{
        <<"atmParallelBoxSchemaId">> := AtmParallelBoxSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(
        AtmParallelBoxSchemaId,
        from_json(SpecificErrorJson)
    );

from_json(#{
    <<"id">> := <<"atmParallelBoxExecutionInitiationFailed">>,
    <<"details">> := #{
        <<"atmParallelBoxSchemaId">> := AtmParallelBoxSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(
        AtmParallelBoxSchemaId,
        from_json(SpecificErrorJson)
    );

from_json(#{
    <<"id">> := <<"atmTaskExecutionCreationFailed">>,
    <<"details">> := #{
        <<"atmTaskSchemaId">> := AtmTaskSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(AtmTaskSchemaId, from_json(SpecificErrorJson));

from_json(#{
    <<"id">> := <<"atmTaskExecutionInitiationFailed">>,
    <<"details">> := #{
        <<"atmTaskSchemaId">> := AtmTaskSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(AtmTaskSchemaId, from_json(SpecificErrorJson));

from_json(#{
    <<"id">> := <<"atmLambdaConfigBadValue">>,
    <<"details">> := #{
        <<"parameterName">> := ParameterName,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(ParameterName, from_json(SpecificErrorJson));

from_json(#{
    <<"id">> := <<"atmTaskArgMapperForRequiredLambdaArgMissing">>,
    <<"details">> := #{
        <<"argument">> := ArgName
    }
}) ->
    ?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(ArgName);

from_json(#{
    <<"id">> := <<"atmTaskArgMapperForNonexistentLambdaArg">>,
    <<"details">> := #{
        <<"argument">> := ArgName
    }
}) ->
    ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(ArgName);

from_json(#{
    <<"id">> := <<"atmTaskArgMappingFailed">>,
    <<"details">> := #{
        <<"argument">> := ArgName,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_TASK_ARG_MAPPING_FAILED(ArgName, from_json(SpecificErrorJson));

from_json(#{
    <<"id">> := <<"atmTaskArgMapperUnsupportedValueBuilder">>,
    <<"details">> := #{
        <<"type">> := TypeJson,
        <<"supported">> := SupportedTypesJson
    }
}) ->
    Type = atm_task_argument_value_builder:type_from_json(TypeJson),
    SupportedTypes = lists:map(fun atm_task_argument_value_builder:type_from_json/1, SupportedTypesJson),

    ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, SupportedTypes);

from_json(#{
    <<"id">> := <<"atmTaskArgMapperIteratedItemQueryFailed">>,
    <<"details">> := #{
        <<"value">> := Value,
        <<"query">> := Query
    }
}) ->
    ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(Value, Query);

from_json(#{
    <<"id">> := <<"atmTaskResultMissing">>,
    <<"details">> := #{
        <<"missingResultName">> := MissingResultName,
        <<"receivedResultNames">> := ReceivedResultNames
    }
}) ->
    ?ERROR_ATM_TASK_RESULT_MISSING(MissingResultName, ReceivedResultNames);

from_json(#{
    <<"id">> := <<"atmTaskResultMappingFailed">>,
    <<"details">> := #{
        <<"result">> := ResultName,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_TASK_RESULT_MAPPING_FAILED(ResultName, from_json(SpecificErrorJson));

from_json(#{
    <<"id">> := <<"atmTaskResultDispatchFailed">>,
    <<"details">> := #{
        <<"atmStoreSchemaId">> := AtmStoreSchemaId,
        <<"specificError">> := SpecificErrorJson
    }
}) ->
    ?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(AtmStoreSchemaId, from_json(SpecificErrorJson));

from_json(#{<<"id">> := <<"atmTaskExecutionEnded">>}) ->
    ?ERROR_ATM_TASK_EXECUTION_STOPPED;

from_json(#{
    <<"id">> := <<"atmJobBatchWithdrawn">>,
    <<"details">> := #{<<"reason">> := Reason}
}) ->
    ?ERROR_ATM_JOB_BATCH_WITHDRAWN(Reason);
from_json(#{
    <<"id">> := <<"atmJobBatchCrashed">>,
    <<"details">> := #{<<"reason">> := Reason}
}) ->
    ?ERROR_ATM_JOB_BATCH_CRASHED(Reason);

from_json(#{<<"id">> := <<"atmOpenfaasNotConfigured">>}) ->
    ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED;

from_json(#{<<"id">> := <<"atmOpenfaasUnreachable">>}) ->
    ?ERROR_ATM_OPENFAAS_UNREACHABLE;

from_json(#{<<"id">> := <<"atmOpenfaasUnhealthy">>}) ->
    ?ERROR_ATM_OPENFAAS_UNHEALTHY;

from_json(#{
    <<"id">> := <<"atmOpenfaasQueryFailed">>,
    <<"details">> := #{
        <<"reason">> := Reason
    }
}) ->
    ?ERROR_ATM_OPENFAAS_QUERY_FAILED(Reason);

from_json(#{<<"id">> := <<"atmOpenfaasQueryFailed">>}) ->
    ?ERROR_ATM_OPENFAAS_QUERY_FAILED;

from_json(#{<<"id">> := <<"atmOpenfaasFunctionRegistrationFailed">>}) ->
    ?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED;

from_json(#{
    <<"id">> := <<"atmInvalidStatusTransition">>,
    <<"details">> := #{
        <<"prevStatus">> := PrevStatusBin,
        <<"newStatus">> := NewStatusBin
    }
}) ->
    ?ERROR_ATM_INVALID_STATUS_TRANSITION(
        binary_to_atom(PrevStatusBin, utf8),
        binary_to_atom(NewStatusBin, utf8)
    );

from_json(#{<<"id">> := <<"dirStatsDisabledForSpace">>}) ->
    ?ERROR_DIR_STATS_DISABLED_FOR_SPACE;

from_json(#{<<"id">> := <<"dirStatsNotReady">>}) ->
    ?ERROR_DIR_STATS_NOT_READY;

from_json(#{
    <<"id">> := <<"forbiddenForCurrentArchiveState">>,
    <<"details">> := #{
        <<"allowedStates">> := AllowedStates,
        <<"currentState">> := CurrentState
    }
}) ->
    ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(
        binary_to_existing_atom(json_utils:decode(CurrentState)),
        [binary_to_existing_atom(StateBin) || StateBin <- json_utils:decode(AllowedStates)]
    );

from_json(#{
    <<"id">> := <<"nestedArchiveDeletionForbidden">>,
    <<"details">> := #{<<"parentArchiveId">> := ParentArchiveId}
}) ->
    ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(ParentArchiveId);

from_json(#{<<"id">> := <<"recallTargetConflict">>}) ->
    ?ERROR_RECALL_TARGET_CONFLICT;

%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
from_json(#{<<"id">> := <<"errorOnNodes">>, <<"details">> := #{
    <<"error">> := Error, <<"hostnames">> := Hostnames}}) ->
    ?ERROR_ON_NODES(from_json(Error), Hostnames);

from_json(#{<<"id">> := <<"dnsServersUnreachable">>, <<"details">> := #{<<"servers">> := UsedServers}}) ->
    Servers = lists:map(fun
        (?DNS_DEFAULTS) -> default;
        (IP) -> element(2, {ok, _} = ip_utils:to_ip4_address(IP))
    end, UsedServers),
    ?ERROR_DNS_SERVERS_UNREACHABLE(Servers);

from_json(#{<<"id">> := <<"fileAllocation">>, <<"details">> := #{
    <<"actualSize">> := ActualSize, <<"targetSize">> := TargetSize}}) ->
    ?ERROR_FILE_ALLOCATION(ActualSize, TargetSize);

from_json(#{<<"id">> := <<"letsEncryptNotReachable">>}) ->
    ?ERROR_LETS_ENCRYPT_NOT_REACHABLE;

from_json(#{<<"id">> := <<"letsEncryptResponse">>, <<"details">> := #{
    <<"problemDocument">> := ProblemDocument, <<"errorMessage">> := ErrorMessage
}}) ->
    ?ERROR_LETS_ENCRYPT_RESPONSE(utils:null_to_undefined(ProblemDocument), ErrorMessage);

from_json(#{<<"id">> := <<"nodeAlreadyInCluster">>,
    <<"details">> := #{<<"hostname">> := Hostname}}) ->
    ?ERROR_NODE_ALREADY_IN_CLUSTER(Hostname);

from_json(#{<<"id">> := <<"nodeNotCompatible">>,
    <<"details">> := #{<<"hostname">> := Hostname, <<"clusterType">> := ClusterType}}) ->
    ?ERROR_NODE_NOT_COMPATIBLE(Hostname, binary_to_existing_atom(ClusterType, utf8));

from_json(#{<<"id">> := <<"noConnectionToNewNode">>,
    <<"details">> := #{<<"hostname">> := Hostname}}) ->
    ?ERROR_NO_CONNECTION_TO_NEW_NODE(Hostname);

from_json(#{<<"id">> := <<"noServiceNodes">>, <<"details">> := #{<<"service">> := Service}}) ->
    ?ERROR_NO_SERVICE_NODES(Service);

from_json(#{<<"id">> := <<"userNotInCluster">>}) ->
    ?ERROR_USER_NOT_IN_CLUSTER;

%%--------------------------------------------------------------------
%% Unknown error
%%--------------------------------------------------------------------
from_json(ErrorAsJson) when is_map(ErrorAsJson) ->
    ?ERROR_UNRECOGNIZED_ERROR(ErrorAsJson).


-spec to_http_code(error()) ->
    400 | 401 | 403 | 404 | 409 | 500 | 501 | 503.
%% -----------------------------------------------------------------------------
%% General errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_BAD_MESSAGE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NO_CONNECTION_TO_ONEZONE) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_NO_CONNECTION_TO_CLUSTER_NODE) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_UNREGISTERED_ONEPROVIDER) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_INTERNAL_SERVER_ERROR) -> ?HTTP_500_INTERNAL_SERVER_ERROR;
to_http_code(?ERROR_INTERNAL_SERVER_ERROR(_)) -> ?HTTP_500_INTERNAL_SERVER_ERROR;
to_http_code(?ERROR_NOT_IMPLEMENTED) -> ?HTTP_501_NOT_IMPLEMENTED;
to_http_code(?ERROR_NOT_SUPPORTED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SERVICE_UNAVAILABLE) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_TIMEOUT) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_TEMPORARY_FAILURE) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(_)) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_UNAUTHORIZED(_)) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_UNAUTHORIZED) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_FORBIDDEN(_)) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_FORBIDDEN) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_NOT_FOUND) -> ?HTTP_404_NOT_FOUND;
to_http_code(?ERROR_ALREADY_EXISTS) -> ?HTTP_409_CONFLICT;
to_http_code(?ERROR_FILE_ACCESS(_, _)) -> ?HTTP_500_INTERNAL_SERVER_ERROR;
to_http_code(?ERROR_LIMIT_REACHED(_, _)) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% POSIX errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_POSIX(_)) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% Auth errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_USER_BLOCKED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_BASIC_CREDENTIALS) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_IDP_ACCESS_TOKEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_TOKEN) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_SERVICE_TOKEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_CONSUMER_TOKEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_INVALID) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_REVOKED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_TOO_LARGE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NOT_AN_ACCESS_TOKEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NOT_AN_IDENTITY_TOKEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NOT_AN_INVITE_TOKEN(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_CAVEAT_UNKNOWN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_SUBJECT_INVALID) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_SERVICE_FORBIDDEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_INVITE_TOKEN_CONSUMER_INVALID(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_SESSION_INVALID) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% Graph Sync errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_EXPECTED_HANDSHAKE_MESSAGE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_HANDSHAKE_ALREADY_DONE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VERSION(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_GRI) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_RPC_UNDEFINED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NOT_SUBSCRIBABLE) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% Data validation errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_MALFORMED_DATA) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_MISSING_REQUIRED_VALUE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_MISSING_AT_LEAST_ONE_VALUE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_DATA(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_DATA(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_EMPTY(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_BOOLEAN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_ATOM(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_LIST_OF_ATOMS(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_BINARY(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TEXT_TOO_LARGE(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_LIST_OF_BINARIES(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_INTEGER(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_FLOAT(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_JSON(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_XML(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TOKEN(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TOKEN_TYPE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_INVITE_TYPE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_IPV4_ADDRESS(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TOO_LOW(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TOO_HIGH(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_NOT_IN_RANGE(_, _, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_NOT_ALLOWED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_ID_NOT_FOUND(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_AMBIGUOUS_ID(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_IDENTIFIER(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_OCTAL(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_FILE_PATH) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_FULL_NAME) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_USERNAME) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_PASSWORD) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_EMAIL) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_NAME) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_NAME(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_DOMAIN) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_SUBDOMAIN) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_CAVEAT(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_QOS_PARAMETERS) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TSC_MISSING_LAYOUT(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TSC_TOO_MANY_METRICS(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(_, _, _, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_GUI_PACKAGE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_GUI_PACKAGE_TOO_LARGE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_GUI_PACKAGE_UNVERIFIED(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_INVALID_QOS_EXPRESSION(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(_, _)) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% oz_worker errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_BASIC_AUTH_NOT_SUPPORTED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BASIC_AUTH_DISABLED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SUBDOMAIN_DELEGATION_DISABLED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SPACE_MARKETPLACE_DISABLED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_PROTECTED_GROUP) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_ATM_LAMBDA_IN_USE(_)) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_CANNOT_REMOVE_LAST_OWNER(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_CANNOT_DELETE_ENTITY(_, _)) -> ?HTTP_500_INTERNAL_SERVER_ERROR;
to_http_code(?ERROR_CANNOT_ADD_RELATION_TO_SELF) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_RELATION_DOES_NOT_EXIST(_, _, _, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _)) -> ?HTTP_409_CONFLICT;
to_http_code(?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(_, _)) -> ?HTTP_409_CONFLICT;
to_http_code(?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE) -> ?HTTP_400_BAD_REQUEST;

%%--------------------------------------------------------------------
%% op_worker errors
%%--------------------------------------------------------------------
to_http_code(?ERROR_USER_NOT_SUPPORTED) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_AUTO_CLEANING_DISABLED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_FILE_POPULARITY_DISABLED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SPACE_NOT_SUPPORTED_BY(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(_, _, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_STORAGE_IN_USE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_STORAGE_TEST_FAILED(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_REQUIRES_NON_IMPORTED_STORAGE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_REQUIRES_IMPORTED_STORAGE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_REQUIRES_READONLY_STORAGE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(_, _, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_STORAGE_IMPORT_NOT_SUPPORTED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_STAT_OPERATION_NOT_SUPPORTED(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TRANSFER_ALREADY_ENDED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TRANSFER_NOT_ENDED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_VIEW_NOT_EXISTS_ON(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_VIEW_QUERY_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_QUOTA_EXCEEDED) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_UNSUPPORTED_DATA_TYPE(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_DATA_TYPE_UNVERIFIED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(_, _, _)) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_STORE_CREATION_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_STORE_FROZEN(_)) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_ATM_STORE_TYPE_DISALLOWED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_STORE_CONTENT_NOT_SET(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_STORE_NOT_FOUND(_)) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_WORKFLOW_EMPTY) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_WORKFLOW_EXECUTION_ENDED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_LANE_EMPTY(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_PARALLEL_BOX_EMPTY(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_ARG_MAPPING_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_RESULT_MISSING(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_RESULT_MAPPING_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_TASK_EXECUTION_STOPPED) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_JOB_BATCH_WITHDRAWN(_)) -> ?HTTP_404_NOT_FOUND;
to_http_code(?ERROR_ATM_JOB_BATCH_CRASHED(_)) -> ?HTTP_500_INTERNAL_SERVER_ERROR;

to_http_code(?ERROR_ATM_OPENFAAS_NOT_CONFIGURED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_OPENFAAS_UNREACHABLE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_OPENFAAS_UNHEALTHY) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_ATM_OPENFAAS_QUERY_FAILED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_OPENFAAS_QUERY_FAILED(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_ATM_INVALID_STATUS_TRANSITION(_, _)) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_DIR_STATS_DISABLED_FOR_SPACE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_DIR_STATS_NOT_READY) -> ?HTTP_400_BAD_REQUEST;

to_http_code(?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_RECALL_TARGET_CONFLICT) -> ?HTTP_400_BAD_REQUEST;

%%--------------------------------------------------------------------
%% onepanel errors
%%--------------------------------------------------------------------
to_http_code(?ERROR_ON_NODES(Error, _)) -> to_http_code(Error);
to_http_code(?ERROR_DNS_SERVERS_UNREACHABLE(_)) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_FILE_ALLOCATION(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_LETS_ENCRYPT_NOT_REACHABLE) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_LETS_ENCRYPT_RESPONSE(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NODE_ALREADY_IN_CLUSTER(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NODE_NOT_COMPATIBLE(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NO_CONNECTION_TO_NEW_NODE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NO_SERVICE_NODES(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_USER_NOT_IN_CLUSTER) -> ?HTTP_403_FORBIDDEN;

%% -----------------------------------------------------------------------------
%% Unknown error
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_UNRECOGNIZED_ERROR(_)) -> ?HTTP_500_INTERNAL_SERVER_ERROR.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec join_values_with_commas([term()]) -> binary().
join_values_with_commas(Values) ->
    str_utils:join_as_binaries(Values, <<", ">>).

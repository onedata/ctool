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

-include("errors.hrl").
-include("validation.hrl").
-include("http/codes.hrl").

-type general() :: {bad_message, json_utils:json_term()} | no_connection_to_oz
| no_connection_to_peer_provider | unregistered_provider | internal_server_error
| not_implemented | not_supported | timeout | temporary_failure | not_found |
already_exists | unauthorized | forbidden.

-type auth() :: bad_basic_credentials | {bad_idp_access_token, IdP :: atom()}
| bad_token | bad_audience_token | token_invalid | token_revoked
| not_an_access_token | {not_an_invite_token, tokens:invite_token_type()}
| invite_token_issuer_not_authorized
| {token_caveat_unverified, caveats:caveat()} | token_subject_invalid
| {token_audience_forbidden, aai:audience()} | token_session_invalid
| {token_time_caveat_required, time_utils:seconds()}.

-type graph_sync() :: expected_handshake_message | handshake_already_done
| {bad_version, {supported, [Version :: integer()]}} | bad_gri
| rpc_undefined | not_subscribable.

% Name of the key in data structure that caused the error, e.g. <<"name">>.
-type key() :: binary().
-type data_validation() :: malformed_data | {missing_required_value, key()}
| {missing_at_least_one_value, [key()]} | {bad_data, key()}
| {empty_value, key()} | {bad_value_atom, key()}
| {bad_value_list_of_atoms, key()} | {bad_value_boolean, key()}
| {bad_value_binary, key()} | {bad_value_list_of_binaries, key()}
| {bad_value_integer, key()} | {bad_value_float, key()}
| {bad_value_json, key()}
| {bad_value_token, key(), auth()} | {bad_value_token_type, key()}
| {bad_value_ipv4_address, key()} | {bad_value_list_of_ipv4_addresses, key()}
| {value_too_low, key(), {min, integer()}}
| {value_too_high, key(), {max, integer()}}
| {value_not_in_range, key(), {range, Low :: integer(), High :: integer()}}
| {value_not_allowed, key(), {allowed, [term()]}}
| {values_not_allowed, key(), {allowed, [term()]}} | {id_not_found, key()}
| {ambiguous_id, key()} | {bad_identifier, key()} | {identifier_occupied, key()}
| bad_full_name | bad_username | bad_password | bad_value_email | bad_name
| bad_value_domain | bad_value_subdomain | bad_caveats | bad_gui_package
| gui_package_too_large | gui_package_unverified.

-type state() :: basic_auth_not_supported | basic_auth_disabled
| subdomain_delegation_not_supported | subdomain_delegation_disabled
| protected_group | {cannot_delete_entity, gri:entity_type(), gri:entity_id()}
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
} | {space_not_supported_by, ProviderId :: binary()}
| {view_not_exists_on, ProviderId :: binary()}
| transfer_already_ended | transfer_not_ended.

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

-type unknown() :: {unknown_error, as_json()}.

-type reason() :: general() | auth() | graph_sync() | data_validation() | state() | posix() | unknown().
-type error() :: {error, reason()}.
-type as_json() :: json_utils:json_term().
-export_type([error/0, reason/0, as_json/0]).

%% API
-export([to_json/1, from_json/1, to_http_code/1]).

-define(FMT(Format, Args), str_utils:format_bin(Format, Args)).

%%%===================================================================
%%% API
%%%===================================================================

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
to_json(?ERROR_UNREGISTERED_ONEPROVIDER) -> #{
    <<"id">> => <<"unregisteredOneprovider">>,
    <<"description">> => <<"This Oneprovider is not registered.">>
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
to_json(?ERROR_TIMEOUT) -> #{
    <<"id">> => <<"timeout">>,
    <<"description">> => <<"Operation timed out.">>
};
to_json(?ERROR_TEMPORARY_FAILURE) -> #{
    <<"id">> => <<"temporaryFailure">>,
    <<"description">> => <<"Temporary failure - please try again later.">>
};
to_json(?ERROR_UNAUTHORIZED) -> #{
    <<"id">> => <<"unauthorized">>,
    <<"description">> => <<"You must authenticate yourself to perform this operation.">>
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

%% -----------------------------------------------------------------------------
%% POSIX errors
%% -----------------------------------------------------------------------------
to_json(?ERROR_POSIX(Errno)) -> #{
    <<"id">> => <<"posix">>,
    <<"details">> => #{
        <<"errno">> => atom_to_binary(Errno, utf8)
    },
    <<"description">> => ?FMT("Operation failed with POSIX error: ~p.", [Errno])
};

%% -----------------------------------------------------------------------------
%% Auth errors
%% -----------------------------------------------------------------------------
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
to_json(?ERROR_BAD_AUDIENCE_TOKEN(TokenError)) -> #{
    <<"id">> => <<"badAudienceToken">>,
    <<"details">> => #{
        <<"tokenError">> => to_json(TokenError)
    },
    <<"description">> => <<"Provided audience token is not valid.">>
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
    <<"description">> => ?FMT("Provided token exceeds the allowed size (~B bytes).", [SizeLimit])
};
to_json(?ERROR_NOT_AN_ACCESS_TOKEN) -> #{
    <<"id">> => <<"notAnAccessToken">>,
    <<"description">> => <<"Provided token is not an access token (invite token?).">>
};
to_json(?ERROR_NOT_AN_INVITE_TOKEN(Type)) -> #{
    <<"id">> => <<"notAnInviteToken">>,
    <<"details">> => #{
        <<"expectedType">> => atom_to_binary(Type, utf8)
    },
    <<"description">> => ?FMT("Expected a ~p token.", [Type])
};
to_json(?ERROR_INVITE_TOKEN_ISSUER_NOT_AUTHORIZED) -> #{
    <<"id">> => <<"inviteTokenIssuerNotAuthorized">>,
    <<"description">> => <<"The creator of this token is no longer authorized to issue such invites.">>
};
to_json(?ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat)) -> #{
    <<"id">> => <<"tokenCaveatUnverified">>,
    <<"details">> => #{
        <<"caveat">> => caveats:serialize(Caveat)
    },
    <<"description">> => ?FMT("Provided token is not valid - ~ts.", [caveats:unverified_description(Caveat)])
};
to_json(?ERROR_TOKEN_SUBJECT_INVALID) -> #{
    <<"id">> => <<"tokenSubjectInvalid">>,
    <<"description">> => <<"Token cannot be created for requested subject.">>
};
to_json(?ERROR_TOKEN_AUDIENCE_FORBIDDEN(Audience)) -> #{
    <<"id">> => <<"tokenAudienceForbidden">>,
    <<"details">> => #{
        <<"audience">> => aai:serialize_audience(Audience)
    },
    <<"description">> => ?FMT("The audience ~s is forbidden for this subject.", [aai:audience_to_printable(Audience)])
};
to_json(?ERROR_TOKEN_SESSION_INVALID) -> #{
    <<"id">> => <<"tokenSessionInvalid">>,
    <<"description">> => <<"This token was issued for a session that no longer exists.">>
};
to_json(?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)) -> #{
    <<"id">> => <<"tokenTimeCaveatRequired">>,
    <<"details">> => #{
        <<"maxTtl">> => MaxTtl
    },
    <<"description">> => ?FMT("You must specify a time caveat with maximum TTL of ~B seconds.", [MaxTtl])
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
    <<"description">> => ?FMT("Bad version - supported versions: ~p", [SupportedVersions])
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
to_json(?ERROR_BAD_DATA(Key)) -> #{
    <<"id">> => <<"badData">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" could not be understood by the server.", [Key])
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
to_json(?ERROR_BAD_VALUE_TOKEN(Key, TokenError)) -> #{
    <<"id">> => <<"badValueToken">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"tokenError">> => to_json(TokenError)
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid token.", [Key])
};
to_json(?ERROR_BAD_VALUE_TOKEN_TYPE(Key)) -> #{
    <<"id">> => <<"badValueTokenType">>,
    <<"details">> => #{
        <<"key">> => Key
    },
    <<"description">> => ?FMT("Bad value: provided \"~s\" is not a valid token type.", [Key])
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
    <<"id">> => <<"badValueIntentifier">>,
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
    <<"description">> =><<"Bad value: ", (?PASSWORD_REQUIREMENTS_DESCRIPTION)/binary>>
};
to_json(?ERROR_BAD_VALUE_EMAIL) -> #{
    <<"id">> => <<"badValueEmail">>,
    <<"description">> => <<"Bad value: provided e-mail is not valid.">>
};
to_json(?ERROR_BAD_VALUE_NAME) -> #{
    <<"id">> => <<"badValueName">>,
    <<"description">> => <<"Bad value: ", (?NAME_REQUIREMENTS_DESCRIPTION)/binary>>
};
to_json(?ERROR_BAD_VALUE_DOMAIN) -> #{
    <<"id">> => <<"badValueDomain">>,
    <<"description">> => <<"Bad value: provided domain is not valid.">>
};
to_json(?ERROR_BAD_VALUE_SUBDOMAIN) -> #{
    <<"id">> => <<"badValueSubdomain">>,
    <<"description">> => <<"Bad value: provided subdomain is not valid.">>
};
to_json(?ERROR_BAD_VALUE_CAVEATS) -> #{
    <<"id">> => <<"badValueCaveats">>,
    <<"description">> => <<"Provided caveats are invalid.">>
};
to_json(?ERROR_BAD_VALUE_QOS_PARAMETERS) -> #{
    <<"id">> => <<"badValueQoSParameters">>,
    <<"description">> => <<"Provided QoS parameters are invalid.">>
};
to_json(?ERROR_BAD_GUI_PACKAGE) -> #{
    <<"id">> => <<"badGuiPackage">>,
    <<"description">> => <<"Provider GUI package could not be understood by the server.">>
};
to_json(?ERROR_GUI_PACKAGE_TOO_LARGE) -> #{
    <<"id">> => <<"guiPackageTooLarge">>,
    <<"description">> => <<"Provider GUI package is too large.">>
};
to_json(?ERROR_GUI_PACKAGE_UNVERIFIED) -> #{
    <<"id">> => <<"guiPackageUnverified">>,
    <<"description">> => <<"Provider GUI package could not be verified.">>
};

%%--------------------------------------------------------------------
%% State errors
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
to_json(?ERROR_PROTECTED_GROUP) -> #{
    <<"id">> => <<"protectedGroup">>,
    <<"description">> => <<"Specified group is protected and cannot be deleted.">>
};
to_json(?ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId)) -> #{
    <<"id">> => <<"cannotDeleteEntity">>,
    <<"details">> => #{
        <<"entityType">> => EntityType,
        <<"entityId">> => EntityId
    },
    <<"description">> => ?FMT("Cannot delete ~s:~s; failed to delete some dependent relations.", [
        gri:serialize_type(EntityType, regular), EntityId
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
        <<"description">> => ?FMT("Bad value: ~s:~s ~s ~s:~s", [
            gri:serialize_type(ChType, regular), ChId,
            RelationToString,
            gri:serialize_type(ParType, regular), ParId
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
            <<"childType">> => ChType,
            <<"childId">> => ChId,
            <<"parentType">> => ParType,
            <<"parentId">> => ParId
        },
        <<"description">> => ?FMT("Bad value: ~s:~s ~s ~s:~s", [
            gri:serialize_type(ChType, regular), ChId,
            RelationToString,
            gri:serialize_type(ParType, regular), ParId
        ])
    };
to_json(?ERROR_SPACE_NOT_SUPPORTED_BY(ProviderId)) -> #{
    <<"id">> => <<"spaceNotSupportedBy">>,
    <<"details">> => #{
        <<"providerId">> => ProviderId
    },
    <<"description">> => ?FMT("Specified space is not supported by provider ~s", [ProviderId])
};
to_json(?ERROR_VIEW_NOT_EXISTS_ON(ProviderId)) -> #{
    <<"id">> => <<"viewNotExistsOn">>,
    <<"details">> => #{
        <<"providerId">> => ProviderId
    },
    <<"description">> => ?FMT("Specified view does not exist on provider ~s", [ProviderId])
};
to_json(?ERROR_TRANSFER_ALREADY_ENDED) -> #{
    <<"id">> => <<"transferAlreadyEnded">>,
    <<"description">> => <<"Specified transfer has already ended.">>
};
to_json(?ERROR_TRANSFER_NOT_ENDED) -> #{
    <<"id">> => <<"transferNotEnded">>,
    <<"description">> => <<"Specified transfer has not ended yet.">>
};

%%--------------------------------------------------------------------
%% Unknown error
%%--------------------------------------------------------------------
to_json(?ERROR_UNKNOWN_ERROR(ErrorAsJson)) ->
    case maps:is_key(<<"description">>, ErrorAsJson) of
        true ->
            ErrorAsJson;
        false ->
            ErrorAsJson#{<<"description">> => <<"No description (unknown error).">>}
    end.


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

from_json(#{<<"id">> := <<"unregisteredOneprovider">>}) ->
    ?ERROR_UNREGISTERED_ONEPROVIDER;

from_json(#{<<"id">> := <<"internalServerError">>}) ->
    ?ERROR_INTERNAL_SERVER_ERROR;

from_json(#{<<"id">> := <<"notImplemented">>}) ->
    ?ERROR_NOT_IMPLEMENTED;

from_json(#{<<"id">> := <<"notSupported">>}) ->
    ?ERROR_NOT_SUPPORTED;

from_json(#{<<"id">> := <<"timeout">>}) ->
    ?ERROR_TIMEOUT;

from_json(#{<<"id">> := <<"temporaryFailure">>}) ->
    ?ERROR_TEMPORARY_FAILURE;

from_json(#{<<"id">> := <<"unauthorized">>}) ->
    ?ERROR_UNAUTHORIZED;

from_json(#{<<"id">> := <<"forbidden">>}) ->
    ?ERROR_FORBIDDEN;

from_json(#{<<"id">> := <<"notFound">>}) ->
    ?ERROR_NOT_FOUND;

from_json(#{<<"id">> := <<"alreadyExists">>}) ->
    ?ERROR_ALREADY_EXISTS;

%% -----------------------------------------------------------------------------
%% POSIX errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"posix">>, <<"details">> := #{<<"errno">> := Errno}}) ->
    ?ERROR_POSIX(binary_to_existing_atom(Errno, utf8));

%% -----------------------------------------------------------------------------
%% Auth errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"badBasicCredentials">>}) ->
    ?ERROR_BAD_BASIC_CREDENTIALS;

from_json(#{<<"id">> := <<"badIdpAccessToken">>, <<"details">> := #{<<"idp">> := IdP}}) ->
    ?ERROR_BAD_IDP_ACCESS_TOKEN(IdP);

from_json(#{<<"id">> := <<"badToken">>}) ->
    ?ERROR_BAD_TOKEN;

from_json(#{<<"id">> := <<"badAudienceToken">>, <<"details">> := #{<<"tokenError">> := TokenError}}) ->
    ?ERROR_BAD_AUDIENCE_TOKEN(from_json(TokenError));

from_json(#{<<"id">> := <<"tokenInvalid">>}) ->
    ?ERROR_TOKEN_INVALID;

from_json(#{<<"id">> := <<"tokenRevoked">>}) ->
    ?ERROR_TOKEN_REVOKED;

from_json(#{<<"id">> := <<"tokenTooLarge">>, <<"details">> := #{<<"limit">> := SizeLimit}}) ->
    ?ERROR_TOKEN_TOO_LARGE(SizeLimit);

from_json(#{<<"id">> := <<"notAnAccessToken">>}) ->
    ?ERROR_NOT_AN_ACCESS_TOKEN;

from_json(#{<<"id">> := <<"notAnInviteToken">>, <<"details">> := #{<<"expectedType">> := Type}}) ->
    ?ERROR_NOT_AN_INVITE_TOKEN(binary_to_existing_atom(Type, utf8));

from_json(#{<<"id">> := <<"inviteTokenIssuerNotAuthorized">>}) ->
    ?ERROR_INVITE_TOKEN_ISSUER_NOT_AUTHORIZED;

from_json(#{<<"id">> := <<"tokenCaveatUnverified">>, <<"details">> := #{<<"caveat">> := Caveat}}) ->
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:deserialize(Caveat));

from_json(#{<<"id">> := <<"tokenSubjectInvalid">>}) ->
    ?ERROR_TOKEN_SUBJECT_INVALID;

from_json(#{<<"id">> := <<"tokenAudienceForbidden">>, <<"details">> := #{<<"audience">> := Audience}}) ->
    ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(aai:deserialize_audience(Audience));

from_json(#{<<"id">> := <<"tokenSessionInvalid">>}) ->
    ?ERROR_TOKEN_SESSION_INVALID;

from_json(#{<<"id">> := <<"tokenTimeCaveatRequired">>, <<"details">> := #{<<"maxTtl">> := MaxTtl}}) ->
    ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl);

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

from_json(#{<<"id">> := <<"badData">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_DATA(Key);

from_json(#{<<"id">> := <<"badValueEmpty">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_EMPTY(Key);

from_json(#{<<"id">> := <<"badValueBoolean">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_BOOLEAN(Key);

from_json(#{<<"id">> := <<"badValueString">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_BINARY(Key);

from_json(#{<<"id">> := <<"badValueListOfStrings">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key);

from_json(#{<<"id">> := <<"badValueInteger">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_INTEGER(Key);

from_json(#{<<"id">> := <<"badValueFloat">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_FLOAT(Key);

from_json(#{<<"id">> := <<"badValueJSON">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_JSON(Key);

from_json(#{<<"id">> := <<"badValueToken">>, <<"details">> := #{<<"key">> := Key, <<"tokenError">> := TokenError}}) ->
    ?ERROR_BAD_VALUE_TOKEN(Key, from_json(TokenError));

from_json(#{<<"id">> := <<"badValueTokenType">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_TOKEN_TYPE(Key);

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

from_json(#{<<"id">> := <<"badValueIntentifier">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_IDENTIFIER(Key);

from_json(#{<<"id">> := <<"badValueIdentifierOccupied">>, <<"details">> := #{<<"key">> := Key}}) ->
    ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key);

from_json(#{<<"id">> := <<"badValueFullName">>}) ->
    ?ERROR_BAD_VALUE_FULL_NAME;

from_json(#{<<"id">> := <<"badValueUsername">>}) ->
    ?ERROR_BAD_VALUE_USERNAME;

from_json(#{<<"id">> := <<"badValuePassword">>}) ->
    ?ERROR_BAD_VALUE_PASSWORD;

from_json(#{<<"id">> := <<"badValueEmail">>}) ->
    ?ERROR_BAD_VALUE_EMAIL;

from_json(#{<<"id">> := <<"badValueName">>}) ->
    ?ERROR_BAD_VALUE_NAME;

from_json(#{<<"id">> := <<"badValueDomain">>}) ->
    ?ERROR_BAD_VALUE_DOMAIN;

from_json(#{<<"id">> := <<"badValueSubdomain">>}) ->
    ?ERROR_BAD_VALUE_SUBDOMAIN;

from_json(#{<<"id">> := <<"badValueCaveats">>}) ->
    ?ERROR_BAD_VALUE_CAVEATS;

from_json(#{<<"id">> := <<"badValueQoSParameters">>}) ->
    ?ERROR_BAD_VALUE_QOS_PARAMETERS;

from_json(#{<<"id">> := <<"badGuiPackage">>}) ->
    ?ERROR_BAD_GUI_PACKAGE;

from_json(#{<<"id">> := <<"guiPackageTooLarge">>}) ->
    ?ERROR_GUI_PACKAGE_TOO_LARGE;

from_json(#{<<"id">> := <<"guiPackageUnverified">>}) ->
    ?ERROR_GUI_PACKAGE_UNVERIFIED;

%% -----------------------------------------------------------------------------
%% State errors
%% -----------------------------------------------------------------------------
from_json(#{<<"id">> := <<"basicAuthNotSupported">>}) ->
    ?ERROR_BASIC_AUTH_NOT_SUPPORTED;

from_json(#{<<"id">> := <<"basicAuthDisabled">>}) ->
    ?ERROR_BASIC_AUTH_DISABLED;

from_json(#{<<"id">> := <<"subdomainDelegationNotSupported">>}) ->
    ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED;

from_json(#{<<"id">> := <<"subdomainDelegationDisabled">>}) ->
    ?ERROR_SUBDOMAIN_DELEGATION_DISABLED;

from_json(#{<<"id">> := <<"protectedGroup">>}) ->
    ?ERROR_PROTECTED_GROUP;

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
    ChTypeAtom = binary_to_existing_atom(ChType, utf8),
    ParTypeAtom = binary_to_existing_atom(ParType, utf8),
    ?ERROR_RELATION_ALREADY_EXISTS(ChTypeAtom, ChId, ParTypeAtom, ParId);

from_json(#{<<"id">> := <<"spaceNotSupportedBy">>, <<"details">> := #{<<"providerId">> := ProviderId}}) ->
    ?ERROR_SPACE_NOT_SUPPORTED_BY(ProviderId);

from_json(#{<<"id">> := <<"viewNotExistsOn">>, <<"details">> := #{<<"providerId">> := ProviderId}}) ->
    ?ERROR_VIEW_NOT_EXISTS_ON(ProviderId);

from_json(#{<<"id">> := <<"transferAlreadyEnded">>}) ->
    ?ERROR_TRANSFER_ALREADY_ENDED;

from_json(#{<<"id">> := <<"transferNotEnded">>}) ->
    ?ERROR_TRANSFER_NOT_ENDED;

from_json(ErrorAsJson) when is_map(ErrorAsJson) ->
    ?ERROR_UNKNOWN_ERROR(ErrorAsJson).


-spec to_http_code(error()) -> 400 | 401 | 403 | 404 | 500 | 501 | 503.
%% -----------------------------------------------------------------------------
%% General errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_BAD_MESSAGE(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NO_CONNECTION_TO_ONEZONE) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_UNREGISTERED_ONEPROVIDER) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_INTERNAL_SERVER_ERROR) -> ?HTTP_500_INTERNAL_SERVER_ERROR;
to_http_code(?ERROR_NOT_IMPLEMENTED) -> ?HTTP_501_NOT_IMPLEMENTED;
to_http_code(?ERROR_NOT_SUPPORTED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TIMEOUT) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_TEMPORARY_FAILURE) -> ?HTTP_503_SERVICE_UNAVAILABLE;
to_http_code(?ERROR_UNAUTHORIZED) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_FORBIDDEN) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_NOT_FOUND) -> ?HTTP_404_NOT_FOUND;
to_http_code(?ERROR_ALREADY_EXISTS) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% POSIX errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_POSIX(_)) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% Auth errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_BAD_BASIC_CREDENTIALS) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_BAD_IDP_ACCESS_TOKEN(_)) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_BAD_TOKEN) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_BAD_AUDIENCE_TOKEN(_)) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_TOKEN_INVALID) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_TOKEN_REVOKED) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_TOKEN_TOO_LARGE(_)) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_NOT_AN_ACCESS_TOKEN) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_NOT_AN_INVITE_TOKEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_INVITE_TOKEN_ISSUER_NOT_AUTHORIZED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_)) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_TOKEN_SUBJECT_INVALID) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_TOKEN_AUDIENCE_FORBIDDEN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TOKEN_SESSION_INVALID) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(_)) -> ?HTTP_400_BAD_REQUEST;

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
to_http_code(?ERROR_BAD_VALUE_EMPTY(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_BOOLEAN(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_ATOM(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_LIST_OF_ATOMS(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_BINARY(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_LIST_OF_BINARIES(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_INTEGER(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_FLOAT(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_JSON(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TOKEN(_, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_TOKEN_TYPE(_)) -> ?HTTP_400_BAD_REQUEST;
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
to_http_code(?ERROR_BAD_VALUE_FULL_NAME) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_USERNAME) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_PASSWORD) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_EMAIL) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_NAME) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_DOMAIN) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_SUBDOMAIN) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_CAVEATS) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_VALUE_QOS_PARAMETERS) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_BAD_GUI_PACKAGE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_GUI_PACKAGE_TOO_LARGE) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_GUI_PACKAGE_UNVERIFIED) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% State errors
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_BASIC_AUTH_NOT_SUPPORTED) -> ?HTTP_401_UNAUTHORIZED;
to_http_code(?ERROR_BASIC_AUTH_DISABLED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SUBDOMAIN_DELEGATION_DISABLED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_PROTECTED_GROUP) -> ?HTTP_403_FORBIDDEN;
to_http_code(?ERROR_CANNOT_DELETE_ENTITY(_, _)) -> ?HTTP_500_INTERNAL_SERVER_ERROR;
to_http_code(?ERROR_CANNOT_ADD_RELATION_TO_SELF) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_RELATION_DOES_NOT_EXIST(_, _, _, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_SPACE_NOT_SUPPORTED_BY(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_VIEW_NOT_EXISTS_ON(_)) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TRANSFER_ALREADY_ENDED) -> ?HTTP_400_BAD_REQUEST;
to_http_code(?ERROR_TRANSFER_NOT_ENDED) -> ?HTTP_400_BAD_REQUEST;

%% -----------------------------------------------------------------------------
%% Unknown error
%% -----------------------------------------------------------------------------
to_http_code(?ERROR_UNKNOWN_ERROR(_)) -> ?HTTP_400_BAD_REQUEST.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec join_values_with_commas([term()]) -> binary().
join_values_with_commas(Values) ->
    StrValues = lists:map(fun
        (Value) when is_binary(Value) -> Value;
        (Value) -> str_utils:format_bin("~tp", [Value])
    end, Values),
    str_utils:join_binary(StrValues, <<", ">>).

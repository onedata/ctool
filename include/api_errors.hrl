%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Error definitions used across all APIs in Oneprovider and Onezone.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(API_ERRORS_HRL).
-define(API_ERRORS_HRL, 1).

% Graph Sync errors
-define(ERROR_UNCLASSIFIED_ERROR(__ReadableDescription),
    {error, {unclassified_error, __ReadableDescription}}
).
-define(ERROR_BAD_MESSAGE(__Msg), {error, {bad_message, __Msg}}).
-define(ERROR_EXPECTED_HANDSHAKE_MESSAGE, {error, expected_handshake_message}).
-define(ERROR_HANDSHAKE_ALREADY_DONE, {error, handshake_already_done}).
-define(ERROR_BAD_VERSION(__SupportedVersions),
    {error, {bad_version, {supported, __SupportedVersions}}}
).
-define(ERROR_BAD_TYPE, {error, bad_type}).
-define(ERROR_RPC_UNDEFINED, {error, rpc_undefined}).
-define(ERROR_NOT_SUBSCRIBABLE, {error, not_subscribable}).

% General errors
-define(ERROR_NO_CONNECTION_TO_OZ, {error, no_connection_to_oz}).
-define(ERROR_UNREGISTERED_PROVIDER, {error, unregistered_provider}).
-define(ERROR_INTERNAL_SERVER_ERROR, {error, internal_server_error}).
-define(ERROR_NOT_IMPLEMENTED, {error, not_implemented}).
-define(ERROR_NOT_SUPPORTED, {error, not_supported}).
-define(ERROR_TIMEOUT, {error, timeout}).
-define(ERROR_NOT_FOUND, {error, not_found}).
-define(ERROR_UNAUTHORIZED, {error, unauthorized}).
-define(ERROR_FORBIDDEN, {error, forbidden}).

% Errors connected with macaroons
% Bad macaroon - provided term cannot be understood
-define(ERROR_BAD_MACAROON, {error, bad_macaroon}).
% Macaroon invalid - provided term is a macaroon, but it is not valid (caveats
% could not be verified).
-define(ERROR_MACAROON_INVALID, {error, macaroon_invalid}).
-define(ERROR_MACAROON_EXPIRED, {error, macaroon_expired}).
-define(ERROR_MACAROON_TTL_TO_LONG(__MaxTtl), {error, {macaroon_ttl_too_long, __MaxTtl}}).
% Requested audience is forbidden to consume the token (e.g. user is not
% supported by the provider specified in audience)
-define(ERROR_TOKEN_AUDIENCE_FORBIDDEN, {error, token_audience_forbidden}).
-define(ERROR_TOKEN_SESSION_INVALID, {error, token_session_invalid}).
% The presented audience token cannot be understood
-define(ERROR_BAD_AUDIENCE_TOKEN, {error, bad_audience_token}).

% Errors connected with bad data
-define(ERROR_MALFORMED_DATA, {error, malformed_data}).
-define(ERROR_BAD_BASIC_CREDENTIALS, {error, bad_basic_credentials}).
-define(ERROR_BAD_IDP_ACCESS_TOKEN(__OAuthProviderId),
    {error, {bad_idp_access_token, __OAuthProviderId}}
).
-define(ERROR_MISSING_REQUIRED_VALUE(__Key),
    {error, {missing_required_value, __Key}}
).
-define(ERROR_MISSING_AT_LEAST_ONE_VALUE(__Keys),
    {error, {missing_at_least_one_value, __Keys}}
).
-define(ERROR_BAD_DATA(__Key), {error, {bad_data, __Key}}).
-define(ERROR_BAD_VALUE_EMPTY(__Key), {error, {empty_value, __Key}}).
-define(ERROR_BAD_VALUE_ATOM(__Key), {error, {bad_value_atom, __Key}}).
-define(ERROR_BAD_VALUE_BOOLEAN(__Key), {error, {bad_value_boolean, __Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_ATOMS(__Key),
    {error, {bad_value_list_of_atoms, __Key}}
).
-define(ERROR_BAD_VALUE_BINARY(__Key), {error, {bad_value_binary, __Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_BINARIES(__Key),
    {error, {bad_value_list_of_binaries, __Key}}
).
-define(ERROR_BAD_VALUE_INTEGER(__Key), {error, {bad_value_integer, __Key}}).
-define(ERROR_BAD_VALUE_FLOAT(__Key), {error, {bad_value_float, __Key}}).
-define(ERROR_BAD_VALUE_JSON(__Key), {error, {bad_value_json, __Key}}).
-define(ERROR_BAD_VALUE_TOKEN(__Key), {error, {bad_value_token, __Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(__Key), {error, {bad_value_list_of_ipv4_addresses, __Key}}).
-define(ERROR_BAD_VALUE_DOMAIN(__Key), {error, {bad_value_domain, __Key}}).
-define(ERROR_BAD_VALUE_SUBDOMAIN, {error, bad_value_subdomain}).
-define(ERROR_BAD_VALUE_EMAIL, {error, bad_value_email}).
-define(ERROR_BAD_VALUE_TOO_LOW(__Key, __Threshold),
    {error, {value_too_low, __Key, {min, __Threshold}}}
).
-define(ERROR_BAD_VALUE_TOO_HIGH(__Key, __Threshold),
    {error, {value_too_high, __Key, {max, __Threshold}}}
).
-define(ERROR_BAD_VALUE_NOT_IN_RANGE(__Key, __Low, __High),
    {error, {value_not_in_range, __Key, {range, __Low, __High}}}
).
-define(ERROR_BAD_VALUE_NOT_ALLOWED(__Key, __AllowedVals),
    {error, {value_not_allowed, __Key, {allowed, __AllowedVals}}}
).
-define(ERROR_BAD_VALUE_LIST_NOT_ALLOWED(__Key, __AllowedVals),
    {error, {list_of_values_not_allowed, __Key, {allowed, __AllowedVals}}}
).
-define(ERROR_BAD_VALUE_ID_NOT_FOUND(__Key), {error, {id_not_found, __Key}}).
-define(ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(__Key), {error, {identifier_occupied, __Key}}).
-define(ERROR_BAD_VALUE_BAD_TOKEN_TYPE(__Key), {error, {bad_token_type, __Key}}).
-define(ERROR_BAD_VALUE_IDENTIFIER(__Key), {error, {bad_identifier, __Key}}).
-define(ERROR_BAD_VALUE_FULL_NAME, {error, bad_full_name}).
-define(ERROR_BAD_VALUE_USERNAME, {error, bad_username}).
-define(ERROR_BAD_VALUE_PASSWORD, {error, bad_password}).
-define(ERROR_BAD_VALUE_NAME, {error, bad_name}).

% Errors caused by illegal state
% Subdomain delegation is (currently) not supported by this Onezone
-define(ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED, {error, subdomain_delegation_not_supported}).
% Subdomain delegation is disabled for given Oneprovider
-define(ERROR_SUBDOMAIN_DELEGATION_DISABLED, {error, subdomain_delegation_disabled}).
% Basic auth is not (currently) supported by this Onezone
-define(ERROR_BASIC_AUTH_NOT_SUPPORTED, {error, basic_auth_not_supported}).
% Basic auth is disabled for given user
-define(ERROR_BASIC_AUTH_DISABLED, {error, basic_auth_disabled}).

% Errors connected with relations between entities
-define(ERROR_RELATION_DOES_NOT_EXIST(__ChType, __ChId, __ParType, __ParId),
    {error, {relation_does_not_exist, __ChType, __ChId, __ParType, __ParId}}
).
-define(ERROR_RELATION_ALREADY_EXISTS(__ChType, __ChId, __ParType, __ParId),
    {error, {relation_already_exists, __ChType, __ChId, __ParType, __ParId}}
).
-define(ERROR_CANNOT_ADD_RELATION_TO_SELF, {error, cannot_add_relation_to_self}).
-define(ERROR_CANNOT_DELETE_ENTITY(__EntityType, __EntityId),
    {error, {cannot_delete_entity, __EntityType, __EntityId}}
).
-define(ERROR_PROTECTED_GROUP, {error, protected_group}).

-define(ERROR_TEMPORARY_FAILURE, {error, temporary_failure}).

% Clproto connection errors
-define(ERROR_NO_CONNECTION_TO_PEER_PROVIDER, {error, no_connection_to_peer_provider}).

% Errors related to GUI packages
-define(ERROR_BAD_GUI_PACKAGE, {error, bad_gui_package}).
-define(ERROR_GUI_PACKAGE_TOO_LARGE, {error, gui_package_too_large}).
-define(ERROR_GUI_PACKAGE_UNVERIFIED, {error, gui_package_unverified}).

-endif.

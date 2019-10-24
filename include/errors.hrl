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
-define(ERROR_UNREGISTERED_ONEPROVIDER, {error, unregistered_oneprovider}).
-define(ERROR_INTERNAL_SERVER_ERROR, {error, internal_server_error}).
-define(ERROR_NOT_IMPLEMENTED, {error, not_implemented}).
-define(ERROR_NOT_SUPPORTED, {error, not_supported}).
-define(ERROR_TIMEOUT, {error, timeout}).
-define(ERROR_TEMPORARY_FAILURE, {error, temporary_failure}).
-define(ERROR_UNAUTHORIZED, {error, unauthorized}).
-define(ERROR_FORBIDDEN, {error, forbidden}).
-define(ERROR_NOT_FOUND, {error, not_found}).
-define(ERROR_ALREADY_EXISTS, {error, already_exists}).


%%--------------------------------------------------------------------
%% POSIX errors
%%--------------------------------------------------------------------
-define(ERROR_POSIX(Errno), {error, {posix, Errno}}).


%%--------------------------------------------------------------------
%% Auth errors
%%--------------------------------------------------------------------
-define(ERROR_BAD_BASIC_CREDENTIALS, {error, bad_basic_credentials}).
-define(ERROR_BAD_IDP_ACCESS_TOKEN(IdP), {error, {bad_idp_access_token, IdP}}).
% The presented token cannot be understood by the server
-define(ERROR_BAD_TOKEN, {error, bad_token}).
% The presented audience token cannot be understood by the server
-define(ERROR_BAD_AUDIENCE_TOKEN(TokenError), {error, {bad_audience_token, TokenError}}).
% The token is comprehensible, but not valid
-define(ERROR_TOKEN_INVALID, {error, token_invalid}).
-define(ERROR_TOKEN_REVOKED, {error, token_revoked}).
-define(ERROR_TOKEN_TOO_LARGE(SizeLimit), {error, {token_too_large, {max, SizeLimit}}}).
-define(ERROR_NOT_AN_ACCESS_TOKEN, {error, not_an_access_token}).
-define(ERROR_NOT_AN_INVITE_TOKEN(Type), {error, {not_an_invite_token, Type}}).
-define(ERROR_INVITE_TOKEN_ISSUER_NOT_AUTHORIZED, {error, invite_token_issuer_not_authorized}).
-define(ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat), {error, {token_caveat_unverified, Caveat}}).
% Token cannot be created for requested subject
-define(ERROR_TOKEN_SUBJECT_INVALID, {error, token_subject_invalid}).
% Requested audience is forbidden to consume the token (e.g. user is not
% supported by the provider specified in audience)
-define(ERROR_TOKEN_AUDIENCE_FORBIDDEN(Audience), {error, {token_audience_forbidden, Audience}}).
-define(ERROR_TOKEN_SESSION_INVALID, {error, token_session_invalid}).
-define(ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl), {error, {token_time_caveat_required, MaxTtl}}).


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
-define(ERROR_BAD_VALUE_EMPTY(Key), {error, {empty_value, Key}}).
-define(ERROR_BAD_VALUE_BOOLEAN(Key), {error, {bad_value_boolean, Key}}).
-define(ERROR_BAD_VALUE_ATOM(Key), {error, {bad_value_atom, Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_ATOMS(Key), {error, {bad_value_list_of_atoms, Key}}).
-define(ERROR_BAD_VALUE_BINARY(Key), {error, {bad_value_binary, Key}}).
-define(ERROR_BAD_VALUE_LIST_OF_BINARIES(Key), {error, {bad_value_list_of_binaries, Key}}).
-define(ERROR_BAD_VALUE_INTEGER(Key), {error, {bad_value_integer, Key}}).
-define(ERROR_BAD_VALUE_FLOAT(Key), {error, {bad_value_float, Key}}).
-define(ERROR_BAD_VALUE_JSON(Key), {error, {bad_value_json, Key}}).
-define(ERROR_BAD_VALUE_TOKEN(Key, TokenError), {error, {bad_value_token, Key, TokenError}}).
-define(ERROR_BAD_VALUE_TOKEN_TYPE(Key), {error, {bad_value_token_type, Key}}).
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
-define(ERROR_BAD_VALUE_FULL_NAME, {error, bad_full_name}).
-define(ERROR_BAD_VALUE_USERNAME, {error, bad_username}).
-define(ERROR_BAD_VALUE_PASSWORD, {error, bad_password}).
-define(ERROR_BAD_VALUE_EMAIL, {error, bad_value_email}).
-define(ERROR_BAD_VALUE_NAME, {error, bad_name}).
-define(ERROR_BAD_VALUE_DOMAIN, {error, bad_value_domain}).
-define(ERROR_BAD_VALUE_SUBDOMAIN, {error, bad_value_subdomain}).
-define(ERROR_BAD_VALUE_CAVEATS, {error, bad_caveats}).
-define(ERROR_BAD_VALUE_QOS_PARAMETERS, {error, bad_qos_parameters}).
-define(ERROR_BAD_GUI_PACKAGE, {error, bad_gui_package}).
-define(ERROR_GUI_PACKAGE_TOO_LARGE, {error, gui_package_too_large}).
-define(ERROR_GUI_PACKAGE_UNVERIFIED, {error, gui_package_unverified}).


%%--------------------------------------------------------------------
%% State errors
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
-define(ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId), {error, {cannot_delete_entity, EntityType, EntityId}}).
-define(ERROR_CANNOT_ADD_RELATION_TO_SELF, {error, cannot_add_relation_to_self}).
-define(ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId),
    {error, {relation_does_not_exist, ChType, ChId, ParType, ParId}}
).
-define(ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId),
    {error, {relation_already_exists, ChType, ChId, ParType, ParId}}
).
-define(ERROR_SPACE_NOT_SUPPORTED_BY(ProviderId), {error, {space_not_supported_by, ProviderId}}).
-define(ERROR_VIEW_NOT_EXISTS_ON(ProviderId), {error, {view_not_exists_on, ProviderId}}).
-define(ERROR_TRANSFER_ALREADY_ENDED, {error, transfer_already_ended}).
-define(ERROR_TRANSFER_NOT_ENDED, {error, transfer_not_ended}).


%%--------------------------------------------------------------------
%% Unknown error
%%--------------------------------------------------------------------
-define(ERROR_UNKNOWN_ERROR(ErrorAsJson), {error, {unknown_error, ErrorAsJson}}).

-endif.


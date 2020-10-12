%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common definitions related to authentication and
%%% authorization in Onedata.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(AAI_HRL).
-define(AAI_HRL, 1).

-include("../onedata.hrl").
-include("caveats.hrl").

% Refer to aai.erl for description of below records

-record(subject, {
    type = nobody :: aai:subject_type(),
    % Applicable only to ?ONEPROVIDER subject to differentiate between
    % ?OP_WORKER and ?OP_PANEL services, that both use the same auth
    subtype = undefined :: aai:subject_subtype(),
    id = undefined :: aai:subject_id()
}).

-record(service_spec, {
    type :: onedata:service(),
    id :: onedata:service_id()
}).

-record(auth, {
    subject = #subject{} :: aai:subject(),
    caveats = [] :: [caveats:caveat()],
    peer_ip = undefined :: undefined | ip_utils:ip(),
    % can be undefined if the auth object is not related to any session
    session_id = undefined :: aai:session_id()
}).

-record(auth_ctx, {
    current_timestamp = 0 :: time_utils:seconds(),
    %% @todo VFS-6098 deprecated, kept for backward compatibility
    scope = unlimited :: unlimited | tokens:scope(),
    ip = undefined :: undefined | ip_utils:ip(),
    interface = undefined :: undefined | cv_interface:interface(),
    service = undefined :: undefined | aai:service_spec(),
    consumer = undefined :: undefined | aai:consumer_spec(),
    % indicates that the auth should be checked in the context of a specific session, can be:
    %   'any' - the token can be coupled with any session (including undefined)
    %   'undefined' - the context requires that the token is not coupled with any session
    %   SessionId - the token must be coupled exactly with given SessionId
    % all these cases apply to the ?ACCESS_TOKEN(SessionId) type only, otherwise this field is ignored
    session_id = undefined :: any | aai:session_id(),
    data_access_caveats_policy = disallow_data_access_caveats :: data_access_caveats:policy(),
    group_membership_checker = undefined :: undefined | aai:group_membership_checker()
}).

% Current (newest) version of tokens used in Onedata
-define(CURRENT_TOKEN_VERSION, 2).

-record(token, {
    version = ?CURRENT_TOKEN_VERSION :: tokens:version(),
    onezone_domain :: tokens:onezone_domain(),
    id :: tokens:id(),
    persistence :: tokens:persistence(),
    subject = #subject{type = nobody} :: aai:subject(),
    type :: tokens:type(),
    macaroon = undefined :: undefined | macaroon:macaroon()
}).

% Record for specifying parameterized token types
-record(access_token_typespec, {
    % (optional) session with which the token is linked
    session = undefined :: undefined | aai:session_id()
}).
-record(identity_token_typespec, {
    % identity tokens do not have any parameters
}).
-record(invite_token_typespec, {
    invite_type :: token_type:invite_type(),
    target_entity :: gri:entity_id(),
    parameters :: token_type:invite_parameters()
}).

% Convenience macros for concise code
-define(SUB(Type), #subject{type = Type}).
-define(SUB(Type, Id), #subject{type = Type, id = Id}).
-define(SUB(Type, SubType, Id), #subject{type = Type, subtype = SubType, id = Id}).

-define(SERVICE(Type, Id), #service_spec{type = Type, id = Id}).

-define(NOBODY, #auth{subject = #subject{type = nobody}}).
-define(ROOT, #auth{subject = #subject{type = root}}).
-define(USER, #auth{subject = #subject{type = user}}).
-define(USER(Id), #auth{subject = #subject{type = user, id = Id}}).
-define(USER(Id, SessionId), #auth{
    subject = #subject{type = user, id = Id},
    session_id = SessionId
}).
-define(PROVIDER, #auth{subject = #subject{type = ?ONEPROVIDER}}).
-define(PROVIDER(Id), #auth{subject = #subject{type = ?ONEPROVIDER, id = Id}}).

-define(ACCESS_TOKEN, #access_token_typespec{}).
-define(ACCESS_TOKEN(SessionId), #access_token_typespec{session = SessionId}).
-define(IDENTITY_TOKEN, #identity_token_typespec{}).
-define(INVITE_TOKEN, #invite_token_typespec{}).
-define(INVITE_TOKEN(InviteType), #invite_token_typespec{
    invite_type = InviteType
}).
-define(INVITE_TOKEN(InviteType, EntityId), #invite_token_typespec{
    invite_type = InviteType, target_entity = EntityId
}).
-define(INVITE_TOKEN(InviteType, EntityId, Parameters), #invite_token_typespec{
    invite_type = InviteType, target_entity = EntityId, parameters = Parameters
}).

% Subtypes of invite tokens
-define(USER_JOIN_GROUP, user_join_group).
-define(GROUP_JOIN_GROUP, group_join_group).
-define(USER_JOIN_SPACE, user_join_space).
-define(GROUP_JOIN_SPACE, group_join_space).
-define(SUPPORT_SPACE, support_space).
-define(HARVESTER_JOIN_SPACE, harvester_join_space).
-define(REGISTER_ONEPROVIDER, register_oneprovider).
-define(USER_JOIN_CLUSTER, user_join_cluster).
-define(GROUP_JOIN_CLUSTER, group_join_cluster).
-define(USER_JOIN_HARVESTER, user_join_harvester).
-define(GROUP_JOIN_HARVESTER, group_join_harvester).
-define(SPACE_JOIN_HARVESTER, space_join_harvester).

-endif.

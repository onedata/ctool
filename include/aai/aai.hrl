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

-record(subject, {
    type = nobody :: aai:subject_type(),
    % Applicable only to ?ONEPROVIDER subject to differentiate between
    % ?OP_WORKER and ?OP_PANEL services, that both use the same auth
    subtype = undefined :: aai:subject_subtype(),
    id = undefined :: aai:subject_id()
}).

-record(audience, {
    type = user :: aai:audience_type(),
    id :: aai:audience_id()
}).

-record(auth, {
    subject = #subject{} :: aai:subject(),
    caveats = [] :: [caveats:caveat()],
    peer_ip = undefined :: undefined | ip_utils:ip(),
    % Can be undefined if the auth object is not related to any session
    session_id = undefined :: aai:session_id()
}).

% Context carrying information required to verify a token
-record(auth_ctx, {
    current_timestamp :: time_utils:seconds(),
    ip = undefined :: undefined | ip_utils:ip(),
    audience :: aai:audience(),
    group_membership_checker :: aai:group_membership_checker()
}).


% Current (newest) version of tokens used in Onedata
-define(CURRENT_TOKEN_VERSION, 2).

% Types of tokens
-define(ACCESS_TOKEN, access_token).
-define(GUI_ACCESS_TOKEN(SessionId), {gui_token, SessionId}).
-define(INVITE_TOKEN(SubType, EntityId), {invite_token, SubType, EntityId}).
% Subtypes of invite tokens
-define(GROUP_INVITE_USER_TOKEN, group_invite_user_token).
-define(GROUP_INVITE_GROUP_TOKEN, group_invite_group_token).
-define(SPACE_INVITE_USER_TOKEN, space_invite_user_token).
-define(SPACE_INVITE_GROUP_TOKEN, space_invite_group_token).
-define(SPACE_SUPPORT_TOKEN, space_support_token).
-define(CLUSTER_INVITE_USER_TOKEN, cluster_invite_user_token).
-define(CLUSTER_INVITE_GROUP_TOKEN, cluster_invite_group_token).
-define(PROVIDER_REGISTRATION_TOKEN, provider_registration_token).
-define(HARVESTER_INVITE_USER_TOKEN, harvester_invite_user_token).
-define(HARVESTER_INVITE_GROUP_TOKEN, harvester_invite_group_token).
-define(HARVESTER_INVITE_SPACE_TOKEN, harvester_invite_space_token).

-record(token, {
    version = ?CURRENT_TOKEN_VERSION :: tokens:version(),
    onezone_domain :: tokens:onezone_domain(),
    nonce :: tokens:nonce(),
    persistent :: tokens:persistent(),
    subject = #subject{type = nobody} :: aai:subject(),
    type = ?ACCESS_TOKEN :: tokens:type(),
    macaroon = undefined :: undefined | macaroon:macaroon()
}).

% Convenience macros for concise code
-define(SUB(Type), #subject{type = Type}).
-define(SUB(Type, Id), #subject{type = Type, id = Id}).
-define(SUB(Type, SubType, Id), #subject{type = Type, subtype = SubType, id = Id}).

-define(AUD(Type, Id), #audience{type = Type, id = Id}).

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

-endif.

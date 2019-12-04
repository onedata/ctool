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

-record(auth_ctx, {
    current_timestamp :: time_utils:seconds(),
    ip = undefined :: undefined | ip_utils:ip(),
    interface = undefined :: undefined | cv_interface:interface(),
    audience :: aai:audience(),
    data_access_caveats_policy = disallow_data_access_caveats :: data_access_caveats:policy(),
    group_membership_checker :: aai:group_membership_checker()
}).


% Current (newest) version of tokens used in Onedata
-define(CURRENT_TOKEN_VERSION, 2).

% Types of tokens
-define(ACCESS_TOKEN, access_token).
-define(GUI_ACCESS_TOKEN(SessionId), {gui_access_token, SessionId}).
-define(INVITE_TOKEN(SubType, EntityId), {invite_token, SubType, EntityId}).
% Subtypes of invite tokens
-define(USER_JOIN_GROUP, user_join_group).
-define(GROUP_JOIN_GROUP, group_join_group).
-define(USER_JOIN_SPACE, user_join_space).
-define(GROUP_JOIN_SPACE, group_join_space).
-define(SUPPORT_SPACE, support_space).
-define(REGISTER_ONEPROVIDER, register_oneprovider).
-define(USER_JOIN_CLUSTER, user_join_cluster).
-define(GROUP_JOIN_CLUSTER, group_join_cluster).
-define(USER_JOIN_HARVESTER, user_join_harvester).
-define(GROUP_JOIN_HARVESTER, group_join_harvester).
-define(SPACE_JOIN_HARVESTER, space_join_harvester).

-record(token, {
    version = ?CURRENT_TOKEN_VERSION :: tokens:version(),
    onezone_domain :: tokens:onezone_domain(),
    id :: tokens:id(),
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

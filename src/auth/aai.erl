%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module includes some common functions and types related to AAI in Onedata.
%%% AAI stands for Authentication and Authorization Infrastructure.
%%% AAI in Onedata assumes the approach of IBAC (Identity Based Access Control).
%%% This means that authorization to perform certain operations is based on
%%% the client's identity (represented by #subject{} in Onedata) and other
%%% attributes of the resources (privileges, existing relations, token caveats).
%%%
%%% Onedata uses tokens to carry client's identity and possibly some contextual
%%% confinements taken into account during authorization. They have the following
%%% characteristics:
%%%   * each token is issued for a certain subject - user or Oneprovider
%%%   * tokens allow to perform operations on behalf of the subject - authority
%%%     delegation can be easily achieved by passing the token to another party
%%%   * tokens can have contextual confinements which all must be satisfied
%%%     during request authorization
%%%   * tokens are represented by the #token{} record
%%%   * tokens are implemented using the macaroons library
%%%
%%% The #auth{} object is used universally as the successful result of client
%%% *authentication*, not only regarding Onedata tokens, but also basic auth or
%%% third party tokens. It carries the client's identity (#subject{}) and other
%%% contextual information that is later used for *authorization*, for example
%%% the IP of the client or contextual caveats that were included in the
%%% presented token.
%%%
%%% There are three types of tokens:
%%%   * access token - carries subject's authentication and authorization to
%%%     perform certain operations. Can be linked to specific user's session.
%%%   * identity token - can be used only for identity verification (does not
%%%     carry any authorization).
%%%   * invite token - used to create relations in the system by inviting
%%%     users or providers to join an entity.
%%%
%%% In order to verify a token, it is required to collect the request context,
%%% against which the token caveats are checked. The context is expressed using
%%% the #auth_ctx{} record. It consists of:
%%%     * current timestamp
%%%     * scope (unlimited or identity_token) stating what level of authorization
%%%       is required from the token
%%%       (@todo VFS-6098 to be removed in the next major version)
%%%     * peer IP from which the request has been made
%%%     * interface to which the client has connected
%%%     * service in which the token was used (described below)
%%%     * consumer - token bearer that consumes the token (described below)
%%%     * data_access_caveats_policy - information if data access caveats are
%%%       allowed in the context of this request (see data_access_caveats module)
%%%     * group_membership_checker - callback to check group membership
%%%
%%% The service in which the token was used (i.e. service which received the
%%% request from a client) is denoted by the #service_spec{} record. The service
%%% that tries to authorize an operation with a token on behalf of a subject can
%%% present its authentication by sending its identity token in the the
%%% x-onedata-service-token header, or perform the operation on a private
%%% channel (GraphSync). If the service does not authenticate itself, it defaults
%%% to undefined. The token may include a cv_service caveat - in this case,
%%% the service must be whitelisted in the caveat for the request to succeed.
%%%
%%% The request consumer is the token bearer that consumes the token - either a
%%% user or a ?ONEPROVIDER service, expressed using the #subject{} record. The
%%% consumer can authenticate themselves by sending its identity token in the
%%% the x-onedata-consumer-token header, or perform the operation on a private
%%% channel (GraphSync). Otherwise, the consumer defaults to undefined. The
%%% token may include a cv_consumer caveat - in this case, the consumer must be
%%% whitelisted in the caveat for the request to succeed.
%%%
%%% ?ONEPROVIDER access tokens are a specific case where the serialized form can
%%% have a three letter indicator of service type that is authorizing itself
%%% (op-worker or op-panel), e.g. opw-MDax34Gh5TyOP032... It is added using the
%%% tokens:add_oneprovider_service_indication/2 function (consult for details). This
%%% is merely an indication for Onezone which of the services has authenticated,
%%% as both services use the same access token.
%%% @end
%%%-------------------------------------------------------------------
-module(aai).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("onedata.hrl").

-type subject() :: #subject{}.
% Tokens can be issued only for a user or ?ONEPROVIDER subject - this means that
% clients are able to authenticate themselves only as a user or Oneprovider.
% Other subject types are used in internal application logic:
%   * The 'root' subject is authorized to do everything and must be used with caution.
%   * The 'group' subject is used in cv_consumer caveats to allow token consumption
%     for any member of specified group
-type subject_type() :: nobody | root | user | group | ?ONEPROVIDER.
% Applicable only in case of ?ONEPROVIDER type to differentiate between
% ?OP_WORKER and ?OP_PANEL services, that both use the same access token
-type subject_subtype() :: undefined | ?OP_WORKER | ?OP_PANEL.
% Applicable in case of user, group or ?ONEPROVIDER type
-type subject_id() :: undefined | binary().

% Special wildcard id <<"*">> can be used to match any id in a service
-type service_spec() :: #service_spec{}.

% Consumers are expressed using the subject record. This type is introduced for
% clearer code and semantics. Special wildcard id <<"*">> can be used to match
% any id in a consumer.
-type consumer_spec() :: subject().

% Can be undefined if the auth object is not related to any session
-type session_id() :: undefined | binary().

-type group_membership_checker() :: fun((subject(), GroupId :: gri:entity_id()) -> boolean()).

-type auth() :: #auth{}.
-type auth_ctx() :: #auth_ctx{}.

-export_type([subject/0, subject_type/0, subject_subtype/0, subject_id/0]).
-export_type([service_spec/0, consumer_spec/0]).
-export_type([session_id/0]).
-export_type([group_membership_checker/0]).
-export_type([auth/0, auth_ctx/0]).

%%% API
-export([root_auth/0, nobody_auth/0, user_auth/1]).
-export([normalize_subject/1]).
-export([subject_to_json/1, subject_from_json/1]).
-export([serialize_subject/1, deserialize_subject/1]).
-export([service_to_json/1, service_from_json/1]).
-export([serialize_service/1, deserialize_service/1]).
-export([auth_to_printable/1]).
-export([subject_to_printable/1]).
-export([service_to_printable/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the Auth object representing ROOT authorization
%% (allowed to perform all operations).
%% @end
%%--------------------------------------------------------------------
-spec root_auth() -> auth().
root_auth() ->
    ?ROOT.


%%--------------------------------------------------------------------
%% @doc
%% Returns the Auth object representing NOBODY authorization
%% (allowed only to perform publicly available operations).
%% @end
%%--------------------------------------------------------------------
-spec nobody_auth() -> auth().
nobody_auth() ->
    ?NOBODY.


%%--------------------------------------------------------------------
%% @doc
%% Returns the Auth object representing USER authorization for given UserId.
%% @end
%%--------------------------------------------------------------------
-spec user_auth(UserId :: subject_id()) -> auth().
user_auth(UserId) ->
    ?USER(UserId).


%%--------------------------------------------------------------------
%% @doc
%% Ensures that the subject represents an identity of an external actor in the
%% system - user, Oneprovider or nobody (a.k.a. guest or anonymous). Transforms
%% other subjects that are valid only in internal application logic
%% (root, group) to nobody.
%% @end
%%--------------------------------------------------------------------
-spec normalize_subject(aai:subject()) -> aai:subject().
normalize_subject(?SUB(user, UserId)) -> ?SUB(user, UserId);
normalize_subject(?SUB(?ONEPROVIDER, PrId)) -> ?SUB(?ONEPROVIDER, PrId);
normalize_subject(_) -> ?SUB(nobody).


-spec subject_to_json(subject()) -> json_utils:json_term().
subject_to_json(?SUB(nobody)) ->
    #{<<"type">> => <<"nobody">>, <<"id">> => null};
% root subject must not have a representation outside of the application
subject_to_json(?SUB(root)) ->
    #{<<"type">> => <<"nobody">>, <<"id">> => null};
subject_to_json(?SUB(user, UserId)) ->
    #{<<"type">> => <<"user">>, <<"id">> => UserId};
subject_to_json(?SUB(group, GroupId)) ->
    #{<<"type">> => <<"group">>, <<"id">> => GroupId};
subject_to_json(?SUB(?ONEPROVIDER, PrId)) ->
    #{<<"type">> => <<"oneprovider">>, <<"id">> => PrId};
subject_to_json(_) ->
    error(badarg).


-spec subject_from_json(json_utils:json_term()) -> subject().
subject_from_json(#{<<"type">> := <<"nobody">>, <<"id">> := null}) ->
    ?SUB(nobody);
subject_from_json(#{<<"type">> := <<"user">>, <<"id">> := UserId}) ->
    ?SUB(user, UserId);
subject_from_json(#{<<"type">> := <<"group">>, <<"id">> := GroupId}) ->
    ?SUB(group, GroupId);
subject_from_json(#{<<"type">> := <<"oneprovider">>, <<"id">> := PrId}) ->
    ?SUB(?ONEPROVIDER, PrId);
subject_from_json(_) ->
    error(badarg).


-spec serialize_subject(subject()) -> binary().
serialize_subject(?SUB(nobody)) -> <<"nobody">>;
% root subject must not have a representation outside of the application
serialize_subject(?SUB(root)) -> <<"nobody">>;
serialize_subject(?SUB(user, UserId)) -> <<"usr-", UserId/binary>>;
serialize_subject(?SUB(group, GroupId)) -> <<"grp-", GroupId/binary>>;
serialize_subject(?SUB(?ONEPROVIDER, Provider)) -> <<"prv-", Provider/binary>>;
serialize_subject(_) -> error(badarg).


-spec deserialize_subject(binary()) -> subject().
deserialize_subject(<<"nobody">>) -> ?SUB(nobody);
deserialize_subject(<<"usr-", UserId/binary>>) -> ?SUB(user, UserId);
deserialize_subject(<<"grp-", GroupId/binary>>) -> ?SUB(group, GroupId);
deserialize_subject(<<"prv-", Provider/binary>>) -> ?SUB(?ONEPROVIDER, Provider);
deserialize_subject(_) -> error(badarg).


-spec service_to_json(service_spec()) -> json_utils:json_term().
service_to_json(?SERVICE(?OZ_WORKER, Id)) -> #{<<"type">> => <<"oz_worker">>, <<"id">> => Id};
service_to_json(?SERVICE(?OZ_PANEL, Id)) -> #{<<"type">> => <<"oz_panel">>, <<"id">> => Id};
service_to_json(?SERVICE(?OP_WORKER, Id)) -> #{<<"type">> => <<"op_worker">>, <<"id">> => Id};
service_to_json(?SERVICE(?OP_PANEL, Id)) -> #{<<"type">> => <<"op_panel">>, <<"id">> => Id};
service_to_json(_) -> error(badarg).


-spec service_from_json(json_utils:json_term()) -> service_spec().
service_from_json(#{<<"type">> := <<"oz_worker">>, <<"id">> := Id}) -> ?SERVICE(?OZ_WORKER, Id);
service_from_json(#{<<"type">> := <<"oz_panel">>, <<"id">> := Id}) -> ?SERVICE(?OZ_PANEL, Id);
service_from_json(#{<<"type">> := <<"op_worker">>, <<"id">> := Id}) -> ?SERVICE(?OP_WORKER, Id);
service_from_json(#{<<"type">> := <<"op_panel">>, <<"id">> := Id}) -> ?SERVICE(?OP_PANEL, Id);
service_from_json(_) -> error(badarg).


-spec serialize_service(service_spec()) -> binary().
serialize_service(?SERVICE(Type, Id)) ->
    <<(onedata:service_shortname(Type))/binary, "-", Id/binary>>.


-spec deserialize_service(binary()) -> service_spec().
deserialize_service(<<Type:3/binary, "-", Id/binary>>) ->
    ?SERVICE(onedata:service_by_shortname(Type), Id);
deserialize_service(_) ->
    error(badarg).


-spec auth_to_printable(auth()) -> string().
auth_to_printable(?NOBODY) -> "nobody (unauthenticated client)";
auth_to_printable(?ROOT) -> "root";
auth_to_printable(?USER(UId)) -> str_utils:format("user:~ts", [UId]);
auth_to_printable(?PROVIDER(PId)) -> str_utils:format("provider:~ts", [PId]).


-spec subject_to_printable(subject()) -> string().
subject_to_printable(?SUB(Type, Id)) -> str_utils:format("~ts:~ts", [Type, Id]).


-spec service_to_printable(service_spec()) -> string().
service_to_printable(?SERVICE(Type, Id)) -> str_utils:format("~ts:~ts", [Type, Id]).

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements a high-level API for all kinds of tokens in Onedata.
%%% Tokens are issued by Onezone services and can only be verified by them.
%%% The underlying implementation uses macaroons, which allows confining
%%% the tokens (adding contextual caveats).
%%% @end
%%%-------------------------------------------------------------------
-module(tokens).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("onedata.hrl").
-include("errors.hrl").
-include("logging.hrl").
-include("http/headers.hrl").

% Token represented by internal record
-type token() :: #token{}.
% Serialized token in binary form, or any binary token
-type serialized() :: binary().

% Version of the token. Legacy tokens are identified by version 1 and must be 
% handled in legacy way to ensure backward compatibility.
-type version() :: non_neg_integer().
% Domain of the issuing Onezone
-type onezone_domain() :: binary().
% A random string that uniquely identifies the token
-type id() :: binary().
% Token persistence can be one of:
%   named - the token's secret and possibly some additional information is
%       stored by the issuer Onezone, retrievable by id - such tokens are
%       revocable and traceable in the system
%   {temporary, Generation} - the token uses a shared secret and is not
%       persisted anywhere, which means it cannot be revoked individually or
%       have any attached information apart from that carried by the token
%       itself. Temporary tokens can be revoked by changing the shared secret -
%       in such case all temporary tokens sharing the secret become invalid.
%       The Generation is an increasing number that denotes the generation of
%       shared secret - if the secret changes, the generation is incremented.
%       This information is inscribed in the token and can be used to detect
%       revocation.
-type temporary_token_generation() :: non_neg_integer().
-type persistence() :: named | {temporary, temporary_token_generation()}.
% Type of the token as recognized across Onedata components
-type type() :: access_token | {gui_access_token, aai:session_id()} |
{invite_token, invite_token_type(), gri:entity_id()}.
% Subtypes of invite tokens
-type invite_token_type() :: ?USER_JOIN_GROUP | ?GROUP_JOIN_GROUP |
?USER_JOIN_SPACE | ?GROUP_JOIN_SPACE | ?SUPPORT_SPACE |
?USER_JOIN_CLUSTER | ?GROUP_JOIN_CLUSTER |
?REGISTER_ONEPROVIDER |
?USER_JOIN_HARVESTER | ?GROUP_JOIN_HARVESTER | ?SPACE_JOIN_HARVESTER.
% A secret for verifying the token, known only to the issuing Onezone
-type secret() :: binary().

% Internal type used for encoding basic information about the token (version,
% id, persistence, subject and type), it is used as the identifier in the
% underlying macaroon, which allows creating self contained tokens that do not
% need to be stored anywhere.
-type token_identifier() :: binary().

-export_type([token/0, serialized/0]).
-export_type([version/0, onezone_domain/0]).
-export_type([temporary_token_generation/0, persistence/0]).
-export_type([type/0, invite_token_type/0]).
-export_type([id/0, secret/0]).

%%% API
-export([construct/3]).
-export([verify/4]).
-export([get_caveats/1]).
-export([confine/2]).
-export([serialize/1, build_service_access_token/2]).
-export([deserialize/1]).
-export([is_token/1]).
-export([is_invite_token/2]).
-export([invite_token_types/0]).
-export([sanitize_type/1, sanitize_invite_token_type/1, type_to_printable/1]).
-export([serialize_type/1, deserialize_type/1]).
-export([type_to_json/1, json_to_type/1]).
-export([invite_token_type_to_str/1, str_to_invite_token_type/1]).
-export([generate_secret/0]).
-export([build_access_token_header/1, parse_access_token_header/1]).
-export([supported_access_token_headers/0]).
-export([build_audience_token_header/1, parse_audience_token_header/1]).

-define(MAX_TOKEN_SIZE, ctool:get_env(max_token_size, 1048576)). % 1MiB

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Constructs a new token based on a prototype - partially filled #token{}
%% record.
%% @end
%%--------------------------------------------------------------------
-spec construct(Prototype :: token(), secret(), [caveats:caveat()]) -> token().
% Subject is supported for versions 2 and above, make sure that it is valid
construct(#token{version = V, subject = ?SUB(S)}, _, _) when V > 1 andalso S /= user andalso S /= ?ONEPROVIDER ->
    throw(?ERROR_TOKEN_SUBJECT_INVALID);
construct(Prototype = #token{onezone_domain = OzDomain}, Secret, Caveats) ->
    Identifier = to_identifier(Prototype),
    Macaroon = macaroon:create(OzDomain, Secret, Identifier),
    Prototype#token{
        macaroon = caveats:add(Caveats, Macaroon)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Verifies given token against given secret and supported caveats.
%% Returns the resulting #auth{} object, expressing subject's authorization
%% carried by the token.
%% @end
%%--------------------------------------------------------------------
-spec verify(token(), secret(), aai:auth_ctx(), [caveats:type()]) ->
    {ok, aai:auth()} | errors:error().
verify(Token = #token{macaroon = Macaroon}, Secret, AuthCtx, SupportedCaveats) ->
    Verifier = caveats:build_verifier(AuthCtx, SupportedCaveats),
    try macaroon_verifier:verify(Verifier, Macaroon, Secret) of
        ok ->
            {ok, #auth{
                subject = Token#token.subject,
                caveats = caveats:get_caveats(Macaroon),
                peer_ip = AuthCtx#auth_ctx.ip,
                session_id = case Token#token.type of
                    ?GUI_ACCESS_TOKEN(SessionId) -> SessionId;
                    _ -> undefined
                end
            }};
        {error, {unverified_caveat, Serialized}} ->
            ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:deserialize(Serialized));
        _ ->
            ?ERROR_TOKEN_INVALID
    catch
        throw:{error, _} = Error ->
            Error;
        _:_ ->
            ?ERROR_TOKEN_INVALID
    end.


-spec get_caveats(token()) -> [caveats:caveat()].
get_caveats(#token{macaroon = Macaroon}) ->
    caveats:get_caveats(Macaroon).


%%--------------------------------------------------------------------
%% @doc
%% Confines the token by adding one or more contextual caveats.
%% @end
%%--------------------------------------------------------------------
-spec confine(Token, caveats:caveat() | [caveats:caveat()]) -> Token when
    Token :: token() | serialized().
confine(Token = #token{macaroon = Macaroon}, Confinement) ->
    Token#token{macaroon = caveats:add(Confinement, Macaroon)};
confine(Serialized, Confinement) when is_binary(Serialized) ->
    {ok, Token} = deserialize(Serialized),
    {ok, Limited} = serialize(confine(Token, Confinement)),
    Limited.


%%--------------------------------------------------------------------
%% @doc
%% Serializes a token to portable binary form.
%% @end
%%--------------------------------------------------------------------
-spec serialize(token()) -> {ok, serialized()} | errors:error().
serialize(Token) ->
    try
        {ok, Token64} = macaroon:serialize(Token#token.macaroon),
        {ok, base62:from_base64(Token64)}
    catch
        _:_ -> ?ERROR_BAD_TOKEN
    end.


%%--------------------------------------------------------------------
%% @doc
%% Builds a service access token based on service (op-worker or op-panel) and
%% corresponding Oneprovider's access token. Service access token is essentially
%% an access token with a hint which Oneprovider's service is authorizing
%% itself. This hint is useful for Onezone, because both op-worker and op-panel
%% use exactly the same access token. This way, Onezone is able to resolve the
%% subject's subtype upon successful verification of the access token. The
%% knowledge whether this is a op-worker or op-panel service is later used for
%% example when specific audience is expected to verify another token.
%% @end
%%--------------------------------------------------------------------
-spec build_service_access_token(?OP_WORKER | ?OP_PANEL, serialized()) -> tokens:serialized().
build_service_access_token(Service, Serialized) when Service == ?OP_WORKER orelse Service == ?OP_PANEL ->
    <<(onedata:service_shortname(Service))/binary, "-", Serialized/binary>>;
build_service_access_token(_, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Deserializes a token from portable binary form.
%% @end
%%--------------------------------------------------------------------
-spec deserialize(serialized()) -> {ok, token()} | errors:error().
deserialize(<<>>) -> ?ERROR_BAD_TOKEN;
deserialize(Serialized) when is_binary(Serialized) ->
    try
        MaxTokenSize = ?MAX_TOKEN_SIZE,
        case size(Serialized) > MaxTokenSize of
            true ->
                ?ERROR_TOKEN_TOO_LARGE(MaxTokenSize);
            false ->
                % Resolve subject subtype if it was provided in the serialized token form
                {SubjectSubtype, SerializedToken} = case Serialized of
                    <<Shortname:3/binary, "-", Rest/binary>> ->
                        Service = onedata:service_by_shortname(Shortname),
                        Service == ?OP_WORKER orelse Service == ?OP_PANEL orelse error(badarg),
                        {Service, Rest};
                    _ ->
                        {undefined, Serialized}
                end,
                {ok, Macaroon} = macaroon:deserialize(base62:to_base64(SerializedToken)),
                Identifier = macaroon:identifier(Macaroon),
                OnezoneDomain = macaroon:location(Macaroon),
                Token = from_identifier(Identifier, OnezoneDomain),
                {ok, Token#token{
                    macaroon = Macaroon,
                    subject = Token#token.subject#subject{subtype = SubjectSubtype}
                }}
        end
    catch
        Type:Reason ->
            ?debug_stacktrace("Cannot deserialize token (~p) due to ~p:~p", [
                Serialized, Type, Reason
            ]),
            ?ERROR_BAD_TOKEN
    end;
deserialize(_) -> ?ERROR_BAD_TOKEN.


-spec is_token(term()) -> boolean().
is_token(#token{macaroon = Macaroon}) ->
    macaroon:is_macaroon(Macaroon);
is_token(_) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Checks if given term is an invite token of expected type - 'any' can be
%% used to indicate that any invite token is expected.
%% @end
%%--------------------------------------------------------------------
-spec is_invite_token(term(), ExpectedType :: any | invite_token_type()) -> boolean().
is_invite_token(#token{type = ?INVITE_TOKEN(_, _)}, any) ->
    true;
is_invite_token(#token{type = ?INVITE_TOKEN(ExpType, _)}, ExpType) ->
    true;
is_invite_token(_, _) ->
    false.


-spec invite_token_types() -> [invite_token_type()].
invite_token_types() -> [
    ?USER_JOIN_GROUP,
    ?GROUP_JOIN_GROUP,
    ?USER_JOIN_SPACE,
    ?GROUP_JOIN_SPACE,
    ?SUPPORT_SPACE,
    ?REGISTER_ONEPROVIDER,
    ?USER_JOIN_CLUSTER,
    ?GROUP_JOIN_CLUSTER,
    ?USER_JOIN_HARVESTER,
    ?GROUP_JOIN_HARVESTER,
    ?SPACE_JOIN_HARVESTER
].


-spec sanitize_type(type() | json_utils:json_term()) -> {true, type()} | false.
sanitize_type(?ACCESS_TOKEN) ->
    {true, ?ACCESS_TOKEN};
sanitize_type(?GUI_ACCESS_TOKEN(SessionId)) when is_binary(SessionId) ->
    {true, ?GUI_ACCESS_TOKEN(SessionId)};
sanitize_type(?INVITE_TOKEN(InviteTokenType, EntityId)) when is_binary(EntityId) ->
    case lists:member(InviteTokenType, invite_token_types()) of
        true -> {true, ?INVITE_TOKEN(InviteTokenType, EntityId)};
        false -> false
    end;
sanitize_type(AsJson) when is_map(AsJson) ->
    try
        sanitize_type(json_to_type(AsJson))
    catch _:_ ->
        false
    end;
sanitize_type(_) ->
    false.


-spec sanitize_invite_token_type(invite_token_type() | binary()) ->
    {true, invite_token_type()} | false.
sanitize_invite_token_type(InviteTokenType) when is_atom(InviteTokenType) ->
    case lists:member(InviteTokenType, invite_token_types()) of
        true -> {true, InviteTokenType};
        false -> false
    end;
sanitize_invite_token_type(InviteTokenType) when is_binary(InviteTokenType) ->
    try
        sanitize_invite_token_type(str_to_invite_token_type(InviteTokenType))
    catch _:_ ->
        false
    end;
sanitize_invite_token_type(_) ->
    false.


-spec type_to_printable(type()) -> string().
type_to_printable(?ACCESS_TOKEN) ->
    "access token";
type_to_printable(?GUI_ACCESS_TOKEN(SessionId)) ->
    str_utils:format("GUI access token for session \"~s\"", [SessionId]);
type_to_printable(?INVITE_TOKEN(?USER_JOIN_GROUP, GroupId)) ->
    str_utils:format("invitation token for a user to join group \"~s\"", [GroupId]);
type_to_printable(?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupId)) ->
    str_utils:format("invitation token for a group to join group \"~s\"", [GroupId]);
type_to_printable(?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceId)) ->
    str_utils:format("invitation token for a user to join space \"~s\"", [SpaceId]);
type_to_printable(?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceId)) ->
    str_utils:format("invitation token for a group to join space \"~s\"", [SpaceId]);
type_to_printable(?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId)) ->
    str_utils:format("invitation token to grant support for space \"~s\"", [SpaceId]);
type_to_printable(?INVITE_TOKEN(?REGISTER_ONEPROVIDER, AdminUserId)) ->
    str_utils:format("invitation token to register a Oneprovider for admin user \"~s\"", [AdminUserId]);
type_to_printable(?INVITE_TOKEN(?USER_JOIN_CLUSTER, ClusterId)) ->
    str_utils:format("invitation token for a user to join cluster \"~s\"", [ClusterId]);
type_to_printable(?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, ClusterId)) ->
    str_utils:format("invitation token for a group to join cluster \"~s\"", [ClusterId]);
type_to_printable(?INVITE_TOKEN(?USER_JOIN_HARVESTER, HarvesterId)) ->
    str_utils:format("invitation token for a user to join harvester \"~s\"", [HarvesterId]);
type_to_printable(?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, HarvesterId)) ->
    str_utils:format("invitation token for a group to join harvester \"~s\"", [HarvesterId]);
type_to_printable(?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, HarvesterId)) ->
    str_utils:format("invitation token for a space to become a metadata source for harvester \"~s\"", [HarvesterId]).


-spec serialize_type(type()) -> binary().
serialize_type(?ACCESS_TOKEN) ->
    <<"act">>;
serialize_type(?GUI_ACCESS_TOKEN(SessionId)) ->
    <<"gui-", SessionId/binary>>;
serialize_type(?INVITE_TOKEN(Type, EntityId)) ->
    <<(serialize_invite_token_type(Type))/binary, "-", EntityId/binary>>.


-spec deserialize_type(binary()) -> type().
deserialize_type(<<"act">>) ->
    ?ACCESS_TOKEN;
deserialize_type(<<"gui-", SessionId/binary>>) when size(SessionId) > 0 ->
    ?GUI_ACCESS_TOKEN(SessionId);
deserialize_type(<<InviteTokenType:3/binary, "-", EntityId/binary>>) when size(EntityId) > 0 ->
    ?INVITE_TOKEN(deserialize_invite_token_type(InviteTokenType), EntityId).


-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(?ACCESS_TOKEN) ->
    #{<<"accessToken">> => #{}};
type_to_json(?GUI_ACCESS_TOKEN(SessionId)) ->
    #{<<"guiAccessToken">> => #{
        <<"sessionId">> => SessionId
    }};
type_to_json(?INVITE_TOKEN(InvTokenType, EntityId)) ->
    InviteTokenParams = case InvTokenType of
        ?USER_JOIN_GROUP -> #{<<"groupId">> => EntityId};
        ?GROUP_JOIN_GROUP -> #{<<"groupId">> => EntityId};
        ?USER_JOIN_SPACE -> #{<<"spaceId">> => EntityId};
        ?GROUP_JOIN_SPACE -> #{<<"spaceId">> => EntityId};
        ?SUPPORT_SPACE -> #{<<"spaceId">> => EntityId};
        ?REGISTER_ONEPROVIDER -> #{<<"adminUserId">> => EntityId};
        ?USER_JOIN_CLUSTER -> #{<<"clusterId">> => EntityId};
        ?GROUP_JOIN_CLUSTER -> #{<<"clusterId">> => EntityId};
        ?USER_JOIN_HARVESTER -> #{<<"harvesterId">> => EntityId};
        ?GROUP_JOIN_HARVESTER -> #{<<"harvesterId">> => EntityId};
        ?SPACE_JOIN_HARVESTER -> #{<<"harvesterId">> => EntityId}
    end,
    #{<<"inviteToken">> => InviteTokenParams#{
        <<"subtype">> => invite_token_type_to_str(InvTokenType)
    }}.


-spec json_to_type(json_utils:json_term()) -> type().
json_to_type(#{<<"accessToken">> := _}) ->
    ?ACCESS_TOKEN;
json_to_type(#{<<"guiAccessToken">> := #{<<"sessionId">> := SessionId}}) ->
    ?GUI_ACCESS_TOKEN(SessionId);
json_to_type(#{<<"inviteToken">> := InviteTokenParams = #{<<"subtype">> := Subtype}}) ->
    InvTokenType = str_to_invite_token_type(Subtype),
    EntityId = case InvTokenType of
        ?USER_JOIN_GROUP -> maps:get(<<"groupId">>, InviteTokenParams);
        ?GROUP_JOIN_GROUP -> maps:get(<<"groupId">>, InviteTokenParams);
        ?USER_JOIN_SPACE -> maps:get(<<"spaceId">>, InviteTokenParams);
        ?GROUP_JOIN_SPACE -> maps:get(<<"spaceId">>, InviteTokenParams);
        ?SUPPORT_SPACE -> maps:get(<<"spaceId">>, InviteTokenParams);
        ?REGISTER_ONEPROVIDER -> maps:get(<<"adminUserId">>, InviteTokenParams);
        ?USER_JOIN_CLUSTER -> maps:get(<<"clusterId">>, InviteTokenParams);
        ?GROUP_JOIN_CLUSTER -> maps:get(<<"clusterId">>, InviteTokenParams);
        ?USER_JOIN_HARVESTER -> maps:get(<<"harvesterId">>, InviteTokenParams);
        ?GROUP_JOIN_HARVESTER -> maps:get(<<"harvesterId">>, InviteTokenParams);
        ?SPACE_JOIN_HARVESTER -> maps:get(<<"harvesterId">>, InviteTokenParams)
    end,
    ?INVITE_TOKEN(InvTokenType, EntityId).


-spec invite_token_type_to_str(invite_token_type()) -> binary().
invite_token_type_to_str(?USER_JOIN_GROUP) -> <<"userJoinGroup">>;
invite_token_type_to_str(?GROUP_JOIN_GROUP) -> <<"groupJoinGroup">>;
invite_token_type_to_str(?USER_JOIN_SPACE) -> <<"userJoinSpace">>;
invite_token_type_to_str(?GROUP_JOIN_SPACE) -> <<"groupJoinSpace">>;
invite_token_type_to_str(?SUPPORT_SPACE) -> <<"supportSpace">>;
invite_token_type_to_str(?REGISTER_ONEPROVIDER) -> <<"registerOneprovider">>;
invite_token_type_to_str(?USER_JOIN_CLUSTER) -> <<"userJoinCluster">>;
invite_token_type_to_str(?GROUP_JOIN_CLUSTER) -> <<"groupJoinCluster">>;
invite_token_type_to_str(?USER_JOIN_HARVESTER) -> <<"userJoinHarvester">>;
invite_token_type_to_str(?GROUP_JOIN_HARVESTER) -> <<"groupJoinHarvester">>;
invite_token_type_to_str(?SPACE_JOIN_HARVESTER) -> <<"spaceJoinHarvester">>.


-spec str_to_invite_token_type(binary()) -> invite_token_type().
str_to_invite_token_type(<<"userJoinGroup">>) -> ?USER_JOIN_GROUP;
str_to_invite_token_type(<<"groupJoinGroup">>) -> ?GROUP_JOIN_GROUP;
str_to_invite_token_type(<<"userJoinSpace">>) -> ?USER_JOIN_SPACE;
str_to_invite_token_type(<<"groupJoinSpace">>) -> ?GROUP_JOIN_SPACE;
str_to_invite_token_type(<<"supportSpace">>) -> ?SUPPORT_SPACE;
str_to_invite_token_type(<<"registerOneprovider">>) -> ?REGISTER_ONEPROVIDER;
str_to_invite_token_type(<<"userJoinCluster">>) -> ?USER_JOIN_CLUSTER;
str_to_invite_token_type(<<"groupJoinCluster">>) -> ?GROUP_JOIN_CLUSTER;
str_to_invite_token_type(<<"userJoinHarvester">>) -> ?USER_JOIN_HARVESTER;
str_to_invite_token_type(<<"groupJoinHarvester">>) -> ?GROUP_JOIN_HARVESTER;
str_to_invite_token_type(<<"spaceJoinHarvester">>) -> ?SPACE_JOIN_HARVESTER.


-spec generate_secret() -> secret().
generate_secret() ->
    str_utils:rand_hex(macaroon:suggested_secret_length()).


-spec build_access_token_header(serialized()) -> cowboy:http_headers().
build_access_token_header(SerializedToken) ->
    #{?HDR_X_AUTH_TOKEN => SerializedToken}.


-spec parse_access_token_header(cowboy_req:req()) -> undefined | serialized().
parse_access_token_header(#{headers := #{?HDR_X_AUTH_TOKEN := T}}) -> T;
parse_access_token_header(#{headers := #{?HDR_AUTHORIZATION := <<"Bearer ", T/binary>>}}) -> T;
parse_access_token_header(#{headers := #{?HDR_MACAROON := T}}) -> T; % @todo VFS-5554 Deprecated
parse_access_token_header(_) -> undefined.


-spec supported_access_token_headers() -> [binary()].
supported_access_token_headers() ->
    % @todo VFS-5554 macaroon header is deprecated
    [?HDR_X_AUTH_TOKEN, ?HDR_AUTHORIZATION, ?HDR_MACAROON].


-spec build_audience_token_header(serialized()) -> cowboy:http_headers().
build_audience_token_header(SerializedToken) ->
    #{?HDR_X_ONEDATA_AUDIENCE_TOKEN => SerializedToken}.


-spec parse_audience_token_header(cowboy_req:req()) -> undefined | serialized().
parse_audience_token_header(#{headers := #{?HDR_X_ONEDATA_AUDIENCE_TOKEN := T}}) -> T;
parse_audience_token_header(_) -> undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec to_identifier(token()) -> token_identifier().
to_identifier(Token = #token{version = 1}) ->
    % Legacy tokens do not carry any information in the identifier
    Token#token.id;
to_identifier(Token = #token{version = 2}) ->
    <<
        "2",
        "/",
        (serialize_persistence(Token#token.persistence))/binary,
        "/",
        (aai:serialize_subject(Token#token.subject))/binary,
        "/",
        (serialize_type(Token#token.type))/binary,
        "/",
        (Token#token.id)/binary
    >>.


%% @private
-spec from_identifier(token_identifier(), onezone_domain()) -> token().
from_identifier(Identifier, OnezoneDomain) ->
    case binary:split(Identifier, <<"/">>, [global]) of
        [Identifier] ->
            from_identifier(1, [Identifier], OnezoneDomain);
        [VersionBin | Fragments] ->
            try binary_to_integer(VersionBin) of
                Version -> from_identifier(Version, Fragments, OnezoneDomain)
            catch _:_ ->
                from_identifier(1, [Identifier], OnezoneDomain)
            end
    end.


%% @private
-spec from_identifier(version(), Fragments :: [binary()], onezone_domain()) -> token().
from_identifier(1, [Identifier], OnezoneDomain) ->
    #token{
        version = 1,
        onezone_domain = OnezoneDomain,
        id = Identifier,
        persistence = named,
        type = ?ACCESS_TOKEN
    };
from_identifier(2, [Persistent, Subject, Type, Id], OnezoneDomain) ->
    #token{
        version = 2,
        onezone_domain = OnezoneDomain,
        id = Id,
        persistence = deserialize_persistence(Persistent),
        subject = aai:deserialize_subject(Subject),
        type = deserialize_type(Type)
    }.


%% @private
-spec serialize_persistence(persistence()) -> binary().
serialize_persistence(named) -> <<"nmd">>;
serialize_persistence({temporary, Generation}) -> <<"tmp-", (integer_to_binary(Generation))/binary>>.


%% @private
-spec deserialize_persistence(binary()) -> persistence().
deserialize_persistence(<<"nmd">>) -> named;
deserialize_persistence(<<"tmp-", Generation/binary>>) -> {temporary, binary_to_integer(Generation)}.


%% @private
-spec serialize_invite_token_type(invite_token_type()) -> binary().
serialize_invite_token_type(?USER_JOIN_GROUP) -> <<"ujg">>;
serialize_invite_token_type(?GROUP_JOIN_GROUP) -> <<"gjg">>;
serialize_invite_token_type(?USER_JOIN_SPACE) -> <<"ujs">>;
serialize_invite_token_type(?GROUP_JOIN_SPACE) -> <<"gjs">>;
serialize_invite_token_type(?SUPPORT_SPACE) -> <<"ssp">>;
serialize_invite_token_type(?REGISTER_ONEPROVIDER) -> <<"rop">>;
serialize_invite_token_type(?USER_JOIN_CLUSTER) -> <<"ujc">>;
serialize_invite_token_type(?GROUP_JOIN_CLUSTER) -> <<"gjc">>;
serialize_invite_token_type(?USER_JOIN_HARVESTER) -> <<"ujh">>;
serialize_invite_token_type(?GROUP_JOIN_HARVESTER) -> <<"gjh">>;
serialize_invite_token_type(?SPACE_JOIN_HARVESTER) -> <<"sjh">>.


%% @private
-spec deserialize_invite_token_type(binary()) -> invite_token_type().
deserialize_invite_token_type(<<"ujg">>) -> ?USER_JOIN_GROUP;
deserialize_invite_token_type(<<"gjg">>) -> ?GROUP_JOIN_GROUP;
deserialize_invite_token_type(<<"ujs">>) -> ?USER_JOIN_SPACE;
deserialize_invite_token_type(<<"gjs">>) -> ?GROUP_JOIN_SPACE;
deserialize_invite_token_type(<<"ssp">>) -> ?SUPPORT_SPACE;
deserialize_invite_token_type(<<"rop">>) -> ?REGISTER_ONEPROVIDER;
deserialize_invite_token_type(<<"ujc">>) -> ?USER_JOIN_CLUSTER;
deserialize_invite_token_type(<<"gjc">>) -> ?GROUP_JOIN_CLUSTER;
deserialize_invite_token_type(<<"ujh">>) -> ?USER_JOIN_HARVESTER;
deserialize_invite_token_type(<<"gjh">>) -> ?GROUP_JOIN_HARVESTER;
deserialize_invite_token_type(<<"sjh">>) -> ?SPACE_JOIN_HARVESTER.

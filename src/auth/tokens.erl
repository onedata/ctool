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
-type nonce() :: binary().
% Indicates if given token is persistent:
%   true -  the token's secret and possibly some additional information is
%           stored by the issuer Onezone, retrievable by nonce - such tokens
%           are revocable and traceable in the system
%   false - the token uses a shared secret and is not persisted anywhere, which
%           means it cannot be revoked or have any attached information apart
%           from that carried by the token itself
-type persistent() :: boolean().
% Type of the token as recognized across Onedata components
-type type() :: access_token | {gui_token, aai:session_id()} |
{invite_token, invite_token_type(), gri:entity_id()}.
% Subtypes of invite tokens
-type invite_token_type() :: ?GROUP_INVITE_USER_TOKEN | ?GROUP_INVITE_GROUP_TOKEN |
?SPACE_INVITE_USER_TOKEN | ?SPACE_INVITE_GROUP_TOKEN | ?SPACE_SUPPORT_TOKEN |
?CLUSTER_INVITE_USER_TOKEN | ?CLUSTER_INVITE_GROUP_TOKEN |
?PROVIDER_REGISTRATION_TOKEN |
?HARVESTER_INVITE_USER_TOKEN | ?HARVESTER_INVITE_GROUP_TOKEN | ?HARVESTER_INVITE_SPACE_TOKEN.
% A secret for verifying the token, known only to the issuing Onezone
-type secret() :: binary().

% Internal type used for encoding basic information about the token (version,
% nonce, persistence, subject and type), it is used as the identifier in the
% underlying macaroon, which allows creating self contained tokens that do not
% need to be stored anywhere.
-type token_identifier() :: binary().

-export_type([token/0, serialized/0]).
-export_type([version/0, onezone_domain/0, persistent/0]).
-export_type([type/0, invite_token_type/0]).
-export_type([nonce/0, secret/0]).

%%% API
-export([construct/3]).
-export([verify/4]).
-export([get_caveats/1]).
-export([confine/2]).
-export([serialize/1, build_service_access_token/2]).
-export([deserialize/1]).
-export([is_token/1]).
-export([sanitize_type/1, serialize_type/1, deserialize_type/1]).
-export([invite_token_types/0]).
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
    {ok, aai:auth()} | {error, term()}.
verify(Token = #token{macaroon = Macaroon}, Secret, AuthCtx, SupportedCaveats) ->
    Verifier = caveats:build_verifier(AuthCtx, SupportedCaveats),
    case macaroon_verifier:verify(Verifier, Macaroon, Secret) of
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
-spec serialize(token()) -> {ok, serialized()} | {error, term()}.
serialize(Token) ->
    try
        {ok, Token64} = macaroon:serialize(Token#token.macaroon),
        {ok, base62:from_base64(Token64)}
    catch
        _:_ -> ?ERROR_BAD_TOKEN
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds an indication of service (op-worker or op-panel) to a provider access
%% token, which is later used during deserialization to resolve subject subtype.
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
-spec deserialize(serialized()) -> {ok, token()} | {error, term()}.
deserialize(<<>>) -> ?ERROR_BAD_TOKEN;
deserialize(Serialized) ->
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
    end.


-spec is_token(term()) -> boolean().
is_token(#token{macaroon = Macaroon}) ->
    macaroon:is_macaroon(Macaroon);
is_token(_) ->
    false.


-spec sanitize_type(type() | binary()) -> {true, type()} | false.
sanitize_type(?ACCESS_TOKEN) ->
    {true, ?ACCESS_TOKEN};
sanitize_type(?GUI_ACCESS_TOKEN(SessionId)) when is_binary(SessionId) ->
    {true, ?GUI_ACCESS_TOKEN(SessionId)};
sanitize_type(?INVITE_TOKEN(Type, EntityId)) when is_binary(EntityId) ->
    case lists:member(Type, invite_token_types()) of
        true -> {true, ?INVITE_TOKEN(Type, EntityId)};
        false -> false
    end;
sanitize_type(Serialized) when is_binary(Serialized) ->
    try
        sanitize_type(deserialize_type(Serialized))
    catch _:_ ->
        false
    end;
sanitize_type(_) ->
    false.


-spec invite_token_types() -> [invite_token_type()].
invite_token_types() -> [
    ?GROUP_INVITE_USER_TOKEN,
    ?GROUP_INVITE_GROUP_TOKEN,
    ?SPACE_INVITE_USER_TOKEN,
    ?SPACE_INVITE_GROUP_TOKEN,
    ?SPACE_SUPPORT_TOKEN,
    ?CLUSTER_INVITE_USER_TOKEN,
    ?CLUSTER_INVITE_GROUP_TOKEN,
    ?PROVIDER_REGISTRATION_TOKEN,
    ?HARVESTER_INVITE_USER_TOKEN,
    ?HARVESTER_INVITE_GROUP_TOKEN,
    ?HARVESTER_INVITE_SPACE_TOKEN
].


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


-spec generate_secret() -> secret().
generate_secret() ->
    str_utils:rand_hex(macaroon:suggested_secret_length()).


-spec build_access_token_header(serialized()) -> cowboy:http_headers().
build_access_token_header(SerializedToken) ->
    #{<<"x-auth-token">> => SerializedToken}.


-spec parse_access_token_header(cowboy_req:req()) -> undefined | serialized().
parse_access_token_header(#{headers := #{<<"x-auth-token">> := T}}) -> T;
parse_access_token_header(#{headers := #{<<"authorization">> := <<"Bearer ", T/binary>>}}) -> T;
parse_access_token_header(#{headers := #{<<"macaroon">> := T}}) -> T; % @todo VFS-5554 Deprecated
parse_access_token_header(_) -> undefined.


-spec supported_access_token_headers() -> [binary()].
supported_access_token_headers() ->
    % @todo VFS-5554 macaroon header is deprecated
    [<<"x-auth-token">>, <<"authorization">>, <<"macaroon">>].


-spec build_audience_token_header(serialized()) -> cowboy:http_headers().
build_audience_token_header(SerializedToken) ->
    #{<<"x-onedata-audience-token">> => SerializedToken}.


-spec parse_audience_token_header(cowboy_req:req()) -> undefined | serialized().
parse_audience_token_header(#{headers := #{<<"x-onedata-audience-token">> := T}}) -> T;
parse_audience_token_header(_) -> undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec to_identifier(token()) -> token_identifier().
to_identifier(Token = #token{version = 1}) ->
    % Legacy tokens do not carry any information in the identifier
    Token#token.nonce;
to_identifier(Token = #token{version = 2}) ->
    <<
        "2",
        "/",
        (serialize_persistence(Token#token.persistent))/binary,
        "/",
        (aai:serialize_subject(Token#token.subject))/binary,
        "/",
        (serialize_type(Token#token.type))/binary,
        "/",
        (Token#token.nonce)/binary
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
        nonce = Identifier,
        persistent = true,
        type = ?ACCESS_TOKEN
    };
from_identifier(2, [Persistent, Subject, Type, Nonce], OnezoneDomain) ->
    #token{
        version = 2,
        onezone_domain = OnezoneDomain,
        nonce = Nonce,
        persistent = deserialize_persistence(Persistent),
        subject = aai:deserialize_subject(Subject),
        type = deserialize_type(Type)
    }.


%% @private
-spec serialize_persistence(boolean()) -> binary().
serialize_persistence(true) -> <<"pst">>;
serialize_persistence(false) -> <<"tmp">>.


%% @private
-spec deserialize_persistence(binary()) -> boolean().
deserialize_persistence(<<"pst">>) -> true;
deserialize_persistence(<<"tmp">>) -> false.


%% @private
-spec serialize_invite_token_type(invite_token_type()) -> binary().
serialize_invite_token_type(?GROUP_INVITE_USER_TOKEN) -> <<"giu">>;
serialize_invite_token_type(?GROUP_INVITE_GROUP_TOKEN) -> <<"gig">>;
serialize_invite_token_type(?SPACE_INVITE_USER_TOKEN) -> <<"siu">>;
serialize_invite_token_type(?SPACE_INVITE_GROUP_TOKEN) -> <<"sig">>;
serialize_invite_token_type(?SPACE_SUPPORT_TOKEN) -> <<"ssu">>;
serialize_invite_token_type(?CLUSTER_INVITE_USER_TOKEN) -> <<"ciu">>;
serialize_invite_token_type(?CLUSTER_INVITE_GROUP_TOKEN) -> <<"cig">>;
serialize_invite_token_type(?PROVIDER_REGISTRATION_TOKEN) -> <<"pre">>;
serialize_invite_token_type(?HARVESTER_INVITE_USER_TOKEN) -> <<"hiu">>;
serialize_invite_token_type(?HARVESTER_INVITE_GROUP_TOKEN) -> <<"hig">>;
serialize_invite_token_type(?HARVESTER_INVITE_SPACE_TOKEN) -> <<"his">>.


%% @private
-spec deserialize_invite_token_type(binary()) -> invite_token_type().
deserialize_invite_token_type(<<"giu">>) -> ?GROUP_INVITE_USER_TOKEN;
deserialize_invite_token_type(<<"gig">>) -> ?GROUP_INVITE_GROUP_TOKEN;
deserialize_invite_token_type(<<"siu">>) -> ?SPACE_INVITE_USER_TOKEN;
deserialize_invite_token_type(<<"sig">>) -> ?SPACE_INVITE_GROUP_TOKEN;
deserialize_invite_token_type(<<"ssu">>) -> ?SPACE_SUPPORT_TOKEN;
deserialize_invite_token_type(<<"ciu">>) -> ?CLUSTER_INVITE_USER_TOKEN;
deserialize_invite_token_type(<<"cig">>) -> ?CLUSTER_INVITE_GROUP_TOKEN;
deserialize_invite_token_type(<<"pre">>) -> ?PROVIDER_REGISTRATION_TOKEN;
deserialize_invite_token_type(<<"hiu">>) -> ?HARVESTER_INVITE_USER_TOKEN;
deserialize_invite_token_type(<<"hig">>) -> ?HARVESTER_INVITE_GROUP_TOKEN;
deserialize_invite_token_type(<<"his">>) -> ?HARVESTER_INVITE_SPACE_TOKEN.

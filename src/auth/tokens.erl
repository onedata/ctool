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
-type type() :: token_type:type().
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
% A secret for verifying the token, known only to the issuing Onezone
-type secret() :: binary().

%% @todo VFS-6098 deprecated, kept for backward compatibility
% Limits the scope in which the token can be used - if equal to identity_token,
% the token can be used only to prove identity of the holder and carries no
% authorization for any operations in the system.
-type scope() :: unlimited | identity_token.

-define(DEFAULT_SUPPORTED_ACCESS_TOKEN_CAVEATS, [
    cv_time,
    cv_ip, cv_asn, cv_country, cv_region,
    cv_service, cv_consumer,
    cv_interface, cv_api,
    cv_data_readonly, cv_data_path, cv_data_objectid
]).
-define(DEFAULT_SUPPORTED_IDENTITY_TOKEN_CAVEATS, [
    cv_time,
    cv_ip, cv_asn, cv_country, cv_region,
    cv_consumer,
    cv_interface
]).
-define(DEFAULT_SUPPORTED_INVITE_TOKEN_CAVEATS, [
    cv_time,
    cv_ip, cv_asn, cv_country, cv_region,
    cv_consumer
]).

% Internal type used for encoding basic information about the token (version,
% id, persistence, subject and type), it is used as the identifier in the
% underlying macaroon, which allows creating self contained tokens that do not
% need to be stored anywhere.
-type token_identifier() :: binary().

-export_type([token/0, type/0, serialized/0]).
-export_type([version/0, onezone_domain/0]).
-export_type([temporary_token_generation/0, persistence/0]).
-export_type([id/0, secret/0]).
-export_type([scope/0]).

%%% API
-export([construct/3]).
-export([verify/3, verify/4]).
-export([get_caveats/1]).
-export([confine/2]).
-export([serialize/1]).
-export([add_oneprovider_service_indication/2, check_for_oneprovider_service_indication/1]).
-export([deserialize/1]).
-export([is_token/1]).
-export([is_invite_token/2]).
-export([generate_secret/0]).
-export([access_token_header/1, parse_access_token_header/1]).
-export([supported_access_token_headers/0]).
-export([service_token_header/1, parse_service_token_header/1]).
-export([consumer_token_header/1, parse_consumer_token_header/1]).

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
%% Verifies given token against given secret, context and default supported
%% caveats for the token type and scope.
%% Returns the resulting #auth{} object, expressing subject's authorization
%% carried by the token.
%% @end
%%--------------------------------------------------------------------
-spec verify(token(), secret(), aai:auth_ctx()) ->
    {ok, aai:auth()} | errors:error().
verify(#token{type = ?ACCESS_TOKEN} = Token, Secret, #auth_ctx{scope = unlimited} = AuthCtx) ->
    verify(Token, Secret, AuthCtx, ?DEFAULT_SUPPORTED_ACCESS_TOKEN_CAVEATS);
%% @todo VFS-6098 deprecated, kept for backward compatibility
verify(#token{type = ?ACCESS_TOKEN} = Token, Secret, #auth_ctx{scope = identity_token} = AuthCtx) ->
    verify(Token, Secret, AuthCtx, [cv_scope | ?DEFAULT_SUPPORTED_IDENTITY_TOKEN_CAVEATS]);
verify(#token{type = ?IDENTITY_TOKEN} = Token, Secret, AuthCtx) ->
    verify(Token, Secret, AuthCtx, ?DEFAULT_SUPPORTED_IDENTITY_TOKEN_CAVEATS);
verify(#token{type = ?INVITE_TOKEN} = Token, Secret, AuthCtx) ->
    verify(Token, Secret, AuthCtx, ?DEFAULT_SUPPORTED_INVITE_TOKEN_CAVEATS).


%%--------------------------------------------------------------------
%% @doc
%% Verifies given token against given secret, context and supported caveats.
%% Returns the resulting #auth{} object, expressing subject's authorization
%% carried by the token.
%% @end
%%--------------------------------------------------------------------
-spec verify(token(), secret(), aai:auth_ctx(), SupportedCaveats :: [caveats:type()]) ->
    {ok, aai:auth()} | errors:error().
verify(Token = #token{macaroon = Macaroon}, Secret, AuthCtx, SupportedCaveats) ->
    Verifier = caveats:build_verifier(AuthCtx, SupportedCaveats),
    try
        case macaroon_verifier:verify(Verifier, Macaroon, Secret) of
            ok ->
                {ok, #auth{
                    subject = Token#token.subject,
                    caveats = caveats:get_caveats(Macaroon),
                    peer_ip = AuthCtx#auth_ctx.ip,
                    session_id = examine_session_id(Token, AuthCtx)
                }};
            {error, {unverified_caveat, Serialized}} ->
                ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:deserialize(Serialized));
            _ ->
                ?ERROR_TOKEN_INVALID
        end
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
%% Adds Oneprovider service (op-worker or op-panel) indication to a serialized
%% token, which is used as a hint which Oneprovider's service is authenticating
%% itself. This hint is useful for Onezone, because both op-worker and op-panel
%% use exactly the same access/identity tokens. This way, Onezone is able to
%% resolve the subject's subtype upon successful verification of the token.
%% The knowledge whether this is a op-worker or op-panel service is later used
%% for example when specific service is expected to verify another token.
%% @end
%%--------------------------------------------------------------------
-spec add_oneprovider_service_indication(?OP_WORKER | ?OP_PANEL, serialized()) -> serialized().
add_oneprovider_service_indication(Service, Serialized) when Service == ?OP_WORKER orelse Service == ?OP_PANEL ->
    <<(onedata:service_shortname(Service))/binary, "-", Serialized/binary>>;
add_oneprovider_service_indication(_, _) ->
    error(badarg).


-spec check_for_oneprovider_service_indication(serialized()) ->
    {undefined | ?OP_WORKER | ?OP_PANEL, serialized()}.
check_for_oneprovider_service_indication(<<Shortname:3/binary, "-", ProperToken/binary>>) ->
    Service = onedata:service_by_shortname(Shortname),
    Service == ?OP_WORKER orelse Service == ?OP_PANEL orelse error(badarg),
    {Service, ProperToken};
check_for_oneprovider_service_indication(Serialized) ->
    {undefined, Serialized}.


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
                {SubjectSubtype, ProperToken} = check_for_oneprovider_service_indication(Serialized),
                {ok, Macaroon} = macaroon:deserialize(base62:to_base64(ProperToken)),
                Identifier = macaroon:identifier(Macaroon),
                OnezoneDomain = macaroon:location(Macaroon),
                Token = #token{subject = Subject} = from_identifier(Identifier, OnezoneDomain),
                {ok, Token#token{
                    macaroon = Macaroon,
                    subject = case Subject of
                        ?SUB(?ONEPROVIDER) -> Subject#subject{subtype = SubjectSubtype};
                        _ -> Subject
                    end
                }}
        end
    catch
        Type:Reason:Stacktrace ->
            ?debug_stacktrace("Cannot deserialize token (~p) due to ~p:~p", [
                Serialized, Type, Reason
            ], Stacktrace),
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
-spec is_invite_token(term(), ExpectedType :: any | token_type:invite_type()) -> boolean().
is_invite_token(#token{type = ?INVITE_TOKEN}, any) ->
    true;
is_invite_token(#token{type = ?INVITE_TOKEN(ExpType)}, ExpType) ->
    true;
is_invite_token(_, _) ->
    false.


-spec generate_secret() -> secret().
generate_secret() ->
    str_utils:rand_hex(macaroon:suggested_secret_length()).


-spec access_token_header(serialized()) -> cowboy:http_headers().
access_token_header(SerializedToken) ->
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


-spec service_token_header(serialized()) -> cowboy:http_headers().
service_token_header(SerializedToken) ->
    #{?HDR_X_ONEDATA_SERVICE_TOKEN => SerializedToken}.


-spec parse_service_token_header(cowboy_req:req()) -> undefined | serialized().
parse_service_token_header(#{headers := #{?HDR_X_ONEDATA_SERVICE_TOKEN := T}}) -> T;
parse_service_token_header(_) -> undefined.


-spec consumer_token_header(serialized()) -> cowboy:http_headers().
consumer_token_header(SerializedToken) ->
    #{?HDR_X_ONEDATA_CONSUMER_TOKEN => SerializedToken}.


-spec parse_consumer_token_header(cowboy_req:req()) -> undefined | serialized().
parse_consumer_token_header(#{headers := #{?HDR_X_ONEDATA_CONSUMER_TOKEN := T}}) -> T;
parse_consumer_token_header(_) -> undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec examine_session_id(token(), aai:auth_ctx()) -> aai:session_id() | no_return().
examine_session_id(#token{type = ?ACCESS_TOKEN(SessId)}, #auth_ctx{session_id = any}) ->
    SessId;
examine_session_id(#token{type = ?ACCESS_TOKEN(SessId)}, #auth_ctx{session_id = SessId}) ->
    SessId;
examine_session_id(#token{type = ?ACCESS_TOKEN(_SessA)}, #auth_ctx{session_id = _SessB}) ->
    throw(?ERROR_TOKEN_SESSION_INVALID);
examine_session_id(#token{type = _}, #auth_ctx{session_id = _}) ->
    undefined.


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
        (token_type:serialize(Token#token.type))/binary,
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
        type = token_type:deserialize(Type)
    }.


%% @private
-spec serialize_persistence(persistence()) -> binary().
serialize_persistence(named) -> <<"nmd">>;
serialize_persistence({temporary, Generation}) -> <<"tmp-", (integer_to_binary(Generation))/binary>>.


%% @private
-spec deserialize_persistence(binary()) -> persistence().
deserialize_persistence(<<"nmd">>) -> named;
deserialize_persistence(<<"tmp-", Generation/binary>>) -> {temporary, binary_to_integer(Generation)}.
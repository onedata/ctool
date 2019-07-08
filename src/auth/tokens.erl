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
%%% the tokens.
%%% @end
%%%-------------------------------------------------------------------
-module(tokens).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("onedata.hrl").
-include("api_errors.hrl").

% @todo VFS-5524 rename to #token{} after refactoring Onezone invite tokens
% Token represented by internal record
-type token() :: #auth_token{}.
% Token represented by internal record
-type audience_token() :: #audience_token{}.
% Serialized token in binary from, or any binary token
-type serialized() :: binary().

% Version of the token. Legacy tokens are identified by version 1 and must be 
% handled in legacy way to ensure backward compatibility.
-type version() :: integer().
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
-type persistent() :: binary().
% Type of the token as recognized across Onedata components
-type type() :: access_token | {gui_token, aai:session_id()}.

% A secret for verifying the token, known only to the issuing Onezone
-type secret() :: binary().

-type caveat() :: macaroons:caveat().
-type caveat_verifier() :: macaroons:caveat_verifier().

% Internal type used for encoding basic information about the token (version,
% nonce, persistence, subject and type), it is used as the identifier in the
% underlying macaroon, which allows creating self contained tokens that do not
% need to be stored anywhere.
-type token_identifier() :: binary().

-export_type([token/0, audience_token/0, serialized/0]).
-export_type([version/0, onezone_domain/0, nonce/0, persistent/0, type/0]).
-export_type([secret/0, caveat/0, caveat_verifier/0]).

%% @todo VFS-5554 Deprecated, kept for backward compatibility
-type discharge_macaroons() :: [macaroon:macaroon()].
-export_type([discharge_macaroons/0]).

%%% API
-export([construct/3]).
-export([verify/4]).
-export([add_caveat/2]).
-export([serialize/1, deserialize/1]).
-export([is_token/1]).
-export([generate_secret/0]).
-export([serialize_audience_token/1, serialize_audience_token/2]).
-export([deserialize_audience_token/1]).
-export([build_access_token_header/1, parse_access_token_header/1]).
-export([supported_access_token_headers/0]).
-export([build_audience_token_header/1, parse_audience_token_header/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Constructs a new token based on a prototype - partially filled #auth_token{}
%% record.
%% @end
%%--------------------------------------------------------------------
-spec construct(Prototype :: token(), secret(), [caveat()]) -> token().
construct(Prototype = #auth_token{onezone_domain = OzDomain}, Secret, Caveats) ->
    Identifier = to_identifier(Prototype),
    Prototype#auth_token{
        macaroon = macaroons:create(OzDomain, Identifier, Secret, Caveats)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Verifies given token against given secret, discharge macaroons and known caveats.
%% @end
%%--------------------------------------------------------------------
-spec verify(token(), secret(), discharge_macaroons(), [caveat_verifier()]) ->
    {ok, aai:subject()} | {error, term()}.
verify(Token, Secret, DischargeMacaroons, CaveatVerifiers) ->
    case macaroons:verify(Token#auth_token.macaroon, Secret, DischargeMacaroons, CaveatVerifiers) of
        ok -> {ok, Token#auth_token.subject};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds a caveat (contextual confinement) to an existing token.
%% @end
%%--------------------------------------------------------------------
-spec add_caveat(token(), caveat()) -> token().
add_caveat(Token = #auth_token{macaroon = Macaroon}, Caveat) ->
    Token#auth_token{macaroon = macaroons:add_caveat(Macaroon, Caveat)}.


%%--------------------------------------------------------------------
%% @doc
%% Serializes a token to portable binary form.
%% @end
%%--------------------------------------------------------------------
-spec serialize(token()) -> {ok, serialized()} | {error, term()}.
serialize(#auth_token{macaroon = Macaroon}) ->
    macaroons:serialize(Macaroon);
serialize(_) ->
    ?ERROR_BAD_MACAROON.


%%--------------------------------------------------------------------
%% @doc
%% Deserializes a token from portable binary form.
%% @end
%%--------------------------------------------------------------------
-spec deserialize(serialized()) -> {ok, token()} | {error, term()}.
deserialize(Serialized) ->
    case macaroons:deserialize(Serialized) of
        {ok, Macaroon} ->
            Identifier = macaroon:identifier(Macaroon),
            OnezoneDomain = macaroon:location(Macaroon),
            Token = from_identifier(Identifier, OnezoneDomain),
            {ok, Token#auth_token{
                macaroon = Macaroon
            }};
        {error, _} = Error ->
            Error
    end.


-spec is_token(term()) -> boolean().
is_token(#auth_token{macaroon = Macaroon}) ->
    macaroon:is_macaroon(Macaroon);
is_token(_) ->
    false.


-spec generate_secret() -> secret().
generate_secret() ->
    BinSecret = crypto:strong_rand_bytes(macaroon:suggested_secret_length()),
    <<<<Y>> || <<X:4>> <= BinSecret, Y <- integer_to_list(X, 16)>>.


-spec serialize_audience_token(audience_token()) -> {ok, serialized()} | {error, term()}.
serialize_audience_token(#audience_token{audience_type = AudienceType, token = Token}) ->
    case serialize(Token) of
        {ok, SerializedAudienceToken} ->
            {ok, serialize_audience_token(AudienceType, SerializedAudienceToken)};
        {error, _} = Error ->
            Error
    end.


-spec serialize_audience_token(aai:audience_type(), serialized()) -> serialized().
serialize_audience_token(AudienceType, SerializedAudienceToken) ->
    <<(aai:encode_audience_type(AudienceType))/binary, "-", SerializedAudienceToken/binary>>.


-spec deserialize_audience_token(serialized()) -> {ok, audience_token()} | {error, term()}.
deserialize_audience_token(<<Type:3/binary, $-, SerializedToken/binary>>) ->
    try
        AudienceType = aai:decode_audience_type(Type),
        case deserialize(SerializedToken) of
            {ok, Token} ->
                {ok, #audience_token{audience_type = AudienceType, token = Token}};
            {error, _} ->
                ?ERROR_BAD_AUDIENCE_TOKEN
        end
    catch _:_ ->
        ?ERROR_BAD_AUDIENCE_TOKEN
    end;
deserialize_audience_token(SerializedToken) ->
    case deserialize(SerializedToken) of
        {ok, Token} ->
            % If no audience indicator is given, assume it is a user
            {ok, #audience_token{audience_type = user, token = Token}};
        {error, _} = Error ->
            Error
    end.


-spec build_access_token_header(serialized()) -> cowboy:http_headers().
build_access_token_header(SerializedToken) ->
    #{<<"x-auth-token">> => SerializedToken}.


-spec parse_access_token_header(cowboy_req:req()) -> undefined | tokens:serialized().
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


-spec parse_audience_token_header(cowboy_req:req()) -> undefined | tokens:serialized().
parse_audience_token_header(#{headers := #{<<"x-onedata-audience-token">> := T}}) -> T;
parse_audience_token_header(_) -> undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec to_identifier(token()) -> token_identifier().
to_identifier(Token = #auth_token{version = 1}) ->
    % Legacy tokens do not carry any information in the identifier
    Token#auth_token.nonce;
to_identifier(Token = #auth_token{version = 2}) ->
    <<
        "2",
        "/",
        (encode_persistence(Token#auth_token.persistent))/binary,
        "/",
        (encode_subject(Token#auth_token.subject))/binary,
        "/",
        (encode_type(Token#auth_token.type))/binary,
        "/",
        (Token#auth_token.nonce)/binary
    >>.


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


-spec from_identifier(version(), Fragments :: [binary()], onezone_domain()) -> token().
from_identifier(1, [Identifier], OnezoneDomain) ->
    #auth_token{
        version = 1,
        onezone_domain = OnezoneDomain,
        nonce = Identifier,
        persistent = true,
        type = ?ACCESS_TOKEN
    };
from_identifier(2, [Persistent, Subject, Type, Nonce], OnezoneDomain) ->
    #auth_token{
        version = 2,
        onezone_domain = OnezoneDomain,
        nonce = Nonce,
        persistent = decode_persistence(Persistent),
        subject = decode_subject(Subject),
        type = decode_type(Type)
    }.


-spec encode_persistence(boolean()) -> binary().
encode_persistence(true) -> <<"pst">>;
encode_persistence(false) -> <<"tmp">>.


-spec decode_persistence(binary()) -> boolean().
decode_persistence(<<"pst">>) -> true;
decode_persistence(<<"tmp">>) -> false.


-spec encode_subject(aai:subject()) -> binary().
encode_subject(?SUB(user, UserId)) -> <<"usr-", UserId/binary>>;
encode_subject(?SUB(?ONEPROVIDER, Provider)) -> <<"prv-", Provider/binary>>.


-spec decode_subject(binary()) -> aai:subject().
decode_subject(<<"usr-", UserId/binary>>) -> ?SUB(user, UserId);
decode_subject(<<"prv-", Provider/binary>>) -> ?SUB(?ONEPROVIDER, Provider).


-spec encode_type(type()) -> binary().
encode_type(?ACCESS_TOKEN) -> <<"act">>;
encode_type(?GUI_TOKEN(SessionId)) -> <<"gui-", SessionId/binary>>.


-spec decode_type(binary()) -> type().
decode_type(<<"act">>) -> ?ACCESS_TOKEN;
decode_type(<<"gui-", SessionId/binary>>) -> ?GUI_TOKEN(SessionId).

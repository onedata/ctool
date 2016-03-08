%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for OpenID management in OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_openid).

-include("oz/oz_runner.hrl").
-include("oz/oz_openid.hrl").

%% API
-export([get_client_tokens/1, revoke_client_token/2,
    modify_client_token_details/3]).
-export([get_provider_tokens/1, revoke_provider_token/2,
    modify_provider_token_details/3]).
-export([get_client_authorization_code/1, verify_client/2,
    get_token_response/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns list of oz_endpoint:client()s' token details.
%% @end
%%--------------------------------------------------------------------
-spec get_client_tokens(Client :: oz_endpoint:client()) ->
    {ok, Tokens :: [#token_details{}]} | {error, Reason :: term()}.
get_client_tokens(Client) ->
    URN = "/openid/client/tokens",
    get_tokens(Client, URN).

%%--------------------------------------------------------------------
%% @doc Revokes oz_endpoint:client()'s token validity.
%% @end
%%--------------------------------------------------------------------
-spec revoke_client_token(Client :: oz_endpoint:client(),
    AccessId :: binary()) -> ok | {error, Reason :: term()}.
revoke_client_token(Client, AccessId) ->
    URN = "/openid/client/tokens/" ++ binary_to_list(AccessId),
    revoke_token(Client, URN).

%%--------------------------------------------------------------------
%% @doc Modifies public details about oz_endpoint:client()'s token.
%% Parameters may contain: token's "oz_endpoint:client()Name".
%% @end
%%--------------------------------------------------------------------
-spec modify_client_token_details(Client :: oz_endpoint:client(),
    AccessId :: binary(), Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
modify_client_token_details(Client, AccessId, Parameters) ->
    URN = "/openid/client/tokens/" ++ binary_to_list(AccessId),
    modify_token_details(Client, URN, Parameters).

%%--------------------------------------------------------------------
%% @doc Returns list of providers' token details.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_tokens(Client :: oz_endpoint:client()) ->
    {ok, Tokens :: [#token_details{}]} | {error, Reason :: term()}.
get_provider_tokens(Client) ->
    URN = "/openid/provider/tokens",
    get_tokens(Client, URN).

%%--------------------------------------------------------------------
%% @doc Revokes provider's token validity.
%% @end
%%--------------------------------------------------------------------
-spec revoke_provider_token(Client :: oz_endpoint:client(),
    AccessId :: binary()) -> ok | {error, Reason :: term()}.
revoke_provider_token(Client, AccessId) ->
    URN = "/openid/provider/tokens/" ++ binary_to_list(AccessId),
    revoke_token(Client, URN).

%%--------------------------------------------------------------------
%% @doc Modifies public details about provider's token. 
%% Parameters may contain: token's "oz_endpoint:client()Name".
%% @end
%%--------------------------------------------------------------------
-spec modify_provider_token_details(Client :: oz_endpoint:client(),
    AccessId :: binary(), Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
modify_provider_token_details(Client, AccessId, Parameters) ->
    URN = "/openid/provider/tokens/" ++ binary_to_list(AccessId),
    modify_token_details(Client, URN, Parameters).

%%--------------------------------------------------------------------
%% @doc Returns oz_endpoint:client() authorization code.
%% @end
%%--------------------------------------------------------------------
-spec get_client_authorization_code(Client :: oz_endpoint:client()) ->
    {ok, AuthorizationCode :: binary()} | {error, Reason :: term()}.
get_client_authorization_code(Client) ->
    ?run(fun() ->
        URN = "/openid/client/authorization_code",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        AuthZCode = proplists:get_value(<<"authorizationCode">>, Proplist),
        {ok, AuthZCode}
    end).

%%--------------------------------------------------------------------
%% @doc Verifies oz_endpoint:client() identity in OZ.
%% Parameters should contain: "userId" of oz_endpoint:client()
%% to be verified and associated with oz_endpoint:client() "secret" token.
%% @end
%%--------------------------------------------------------------------
-spec verify_client(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) ->
    {ok, VerifyStatus :: boolean()} | {error, Reason :: term()}.
verify_client(Client, Parameters) ->
    ?run(fun() ->
        URN = "/openid/client/verify",
        Body = json_utils:encode(Parameters),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, post, Body),
        Proplist = json_utils:decode(ResponseBody),
        VerifyStatus = proplists:get_value(<<"verified">>, Proplist),
        {ok, VerifyStatus}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token response.
%% Parameters should contain: "grant_type" (either "code" or "refresh_token")
%% and "code" (if grant_type=code) or "refresh_token"
%% (if grant_type=refresh_token).
%% @end
%%--------------------------------------------------------------------
-spec get_token_response(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) ->
    {ok, Tokens :: #token_response{}} | {error, Reason :: term()}.
get_token_response(Client, Parameters) ->
    ?run(fun() ->
        Body = json_utils:encode(Parameters),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            case Client of
                client ->
                    URN = "/openid/client/tokens",
                    oz_endpoint:noauth_request(Client, URN, post, Body);
                _ ->
                    URN = "/openid/provider/tokens",
                    oz_endpoint:auth_request(Client, URN, post, Body)
            end,
        Proplist = json_utils:decode(ResponseBody),
        IdToken = proplists:get_value(<<"id_token">>, Proplist),
        [_Header, Payload, _Signtre] = binary:split(IdToken, <<".">>, [global]),
        IdTokenProps = json_utils:decode(http_utils:base64url_decode(Payload)),
        LoginsProplist = proplists:get_value(<<"logins">>, IdTokenProps),
        Logins = lists:map(fun(LoginProplist) ->
            #id_token_login{
                provider_id = binary_to_atom(
                    proplists:get_value(
                        <<"provider_id">>, LoginProplist, <<>>
                    ), utf8
                ),
                login = proplists:get_value(<<"login">>, LoginProplist, <<>>)
            }
        end, LoginsProplist),
        TokenResponse = #token_response{
            access_token = proplists:get_value(<<"access_token">>, Proplist),
            token_type = proplists:get_value(<<"token_type">>, Proplist),
            expires_in = proplists:get_value(<<"expires_in">>, Proplist),
            refresh_token = proplists:get_value(<<"refresh_token">>, Proplist),
            scope = proplists:get_value(<<"scope">>, Proplist),
            id_token = #id_token{
                iss = proplists:get_value(<<"iss">>, IdTokenProps),
                sub = proplists:get_value(<<"sub">>, IdTokenProps),
                aud = proplists:get_value(<<"aud">>, IdTokenProps),
                name = proplists:get_value(<<"name">>, IdTokenProps),
                logins = Logins,
                emails = proplists:get_value(<<"emails">>, IdTokenProps),
                exp = proplists:get_value(<<"exp">>, IdTokenProps),
                iat = proplists:get_value(<<"iat">>, IdTokenProps)
            }
        },
        {ok, TokenResponse}
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns list of token details for oz_endpoint:client() or provider.
%% @end
%%--------------------------------------------------------------------
-spec get_tokens(Client :: oz_endpoint:client(), oz_endpoint:urn()) ->
    {ok, Tokens :: [#token_details{}]} | {error, Reason :: term()}.
get_tokens(Client, URN) ->
    ?run(fun() ->
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        TokenDetails = proplists:get_value(<<"tokenInfo">>, Proplist),
        Tokens = lists:map(fun(Token) ->
            #token_details{
                access_id = proplists:get_value(<<"accessId">>, Token),
                client_name = proplists:get_value(<<"clientName">>, Token)
            }
        end, TokenDetails),
        {ok, Tokens}
    end).

%%--------------------------------------------------------------------
%% @doc Revokes token validity for oz_endpoint:client() or provider.
%% @end
%%--------------------------------------------------------------------
-spec revoke_token(Client :: oz_endpoint:client(), oz_endpoint:urn()) ->
    ok | {error, Reason :: term()}.
revoke_token(Client, URN) ->
    ?run(fun() ->
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about token. Parameters may contain:
%% token's "oz_endpoint:client()Name".
%% @end
%%--------------------------------------------------------------------
-spec modify_token_details(Client :: oz_endpoint:client(), oz_endpoint:urn(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_token_details(Client, URN, Parameters) ->
    ?run(fun() ->
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, patch, Body),
        ok
    end).

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc This module allows for OpenID management in Global Registry.
%% @end
%% ===================================================================

-module(gr_openid).

-include("global_registry/gr_runner.hrl").
-include("global_registry/gr_types.hrl").
-include("global_registry/gr_openid.hrl").

%% API
-export([get_client_tokens/1, revoke_client_token/2, modify_client_token_details/3]).
-export([get_provider_tokens/1, revoke_provider_token/2, modify_provider_token_details/3]).
-export([get_client_authorization_code/1, verify_client/2, get_token_response/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% get_client_tokens/1
%% ====================================================================
%% @doc Returns list of clients' token details.
%% @end
-spec get_client_tokens(Client :: client()) -> Result when
    Result :: {ok, Tokens :: [#token_details{}]} | {error, Reason :: term()}.
%% ====================================================================
get_client_tokens(Client) ->
    URN = "/openid/client/tokens",
    get_tokens(Client, URN).


%% revoke_client_token/2
%% ====================================================================
%% @doc Revokes client's token validity.
%% @end
-spec revoke_client_token(Client :: client(), AccessId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
revoke_client_token(Client, AccessId) ->
    URN = "/openid/client/tokens/" ++ binary_to_list(AccessId),
    revoke_token(Client, URN).


%% modify_client_token_details/3
%% ====================================================================
%% @doc Modifies public details about client's token.
%% Parameters may contain: token's "clientName".
%% @end
-spec modify_client_token_details(Client :: client(), AccessId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_client_token_details(Client, AccessId, Parameters) ->
    URN = "/openid/client/tokens/" ++ binary_to_list(AccessId),
    modify_token_details(Client, URN, Parameters).


%% get_provider_tokens/1
%% ====================================================================
%% @doc Returns list of providers' token details.
%% @end
-spec get_provider_tokens(Client :: client()) -> Result when
    Result :: {ok, Tokens :: [#token_details{}]} | {error, Reason :: term()}.
%% ====================================================================
get_provider_tokens(Client) ->
    URN = "/openid/provider/tokens",
    get_tokens(Client, URN).


%% revoke_provider_token/2
%% ====================================================================
%% @doc Revokes provider's token validity.
%% @end
-spec revoke_provider_token(Client :: client(), AccessId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
revoke_provider_token(Client, AccessId) ->
    URN = "/openid/provider/tokens/" ++ binary_to_list(AccessId),
    revoke_token(Client, URN).


%% modify_provider_token_details/3
%% ====================================================================
%% @doc Modifies public details about provider's token. 
%% Parameters may contain: token's "clientName".
%% @end
-spec modify_provider_token_details(Client :: client(), AccessId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_provider_token_details(Client, AccessId, Parameters) ->
    URN = "/openid/provider/tokens/" ++ binary_to_list(AccessId),
    modify_token_details(Client, URN, Parameters).


%% get_client_authorization_code/1
%% ====================================================================
%% @doc Returns client authorization code.
%% @end
-spec get_client_authorization_code(Client :: client()) -> Result when
    Result :: {ok, AuthorizationCode :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_client_authorization_code(Client) ->
    ?run(fun() ->
        URN = "/openid/client/authorization_code",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        AuthorizationCode = proplists:get_value(<<"authorizationCode">>, Proplist),
        {ok, AuthorizationCode}
    end).


%% verify_client/2
%% ====================================================================
%% @doc Verifies client identity in Global Registry.
%% Parameters should contain: "userId" of client to be verified and
%% associated with client "secret" token.
%% @end
-spec verify_client(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, VerifyStatus :: boolean()} | {error, Reason :: term()}.
%% ====================================================================
verify_client(Client, Parameters) ->
    ?run(fun() ->
        URN = "/openid/client/verify",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        VerifyStatus = proplists:get_value(<<"verified">>, Proplist),
        {ok, VerifyStatus}
    end).


%% get_token_response/2
%% ====================================================================
%% @doc Returns token response.
%% Parameters should contain: "grant_type" (either "code" or "refresh_token")
%% and "code" (if grant_type=code) or "refresh_token"
%% (if grant_type=refresh_token).
%% @end
-spec get_token_response(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, Tokens :: #token_response{}} | {error, Reason :: term()}.
%% ====================================================================
get_token_response(Client, Parameters) ->
    ?run(fun() ->
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} =
            case Client of
                client ->
                    URN = "/openid/client/tokens",
                    gr_endpoint:noauth_request(Client, URN, post, Body);
                _ ->
                    URN = "/openid/provider/tokens",
                    gr_endpoint:auth_request(Client, URN, post, Body)
            end,
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        IdToken = proplists:get_value(<<"id_token">>, Proplist),
        [_Header, Payload, _Signature] = binary:split(IdToken, <<".">>, [global]),
        IdTokenProplist = mochijson2:decode(mochiweb_base64url:decode(Payload), [{format, proplist}]),
        RawLogins = proplists:get_value(<<"logins">>, IdTokenProplist),
        Logins = [#id_token_login{provider_id = binary_to_atom(proplists:get_value(<<"provider_id">>, PropList), utf8),
                                  login = proplists:get_value(<<"login">>, PropList)} || PropList <- RawLogins],
        TokenResponse = #token_response{
            access_token = proplists:get_value(<<"access_token">>, Proplist),
            token_type = proplists:get_value(<<"token_type">>, Proplist),
            expires_in = proplists:get_value(<<"expires_in">>, Proplist),
            refresh_token = proplists:get_value(<<"refresh_token">>, Proplist),
            scope = proplists:get_value(<<"scope">>, Proplist),
            id_token = #id_token{
                iss = proplists:get_value(<<"iss">>, IdTokenProplist),
                sub = proplists:get_value(<<"sub">>, IdTokenProplist),
                aud = proplists:get_value(<<"aud">>, IdTokenProplist),
                name = proplists:get_value(<<"name">>, IdTokenProplist),
                logins = Logins,
                emails = proplists:get_value(<<"emails">>, IdTokenProplist),
                exp = proplists:get_value(<<"exp">>, IdTokenProplist),
                iat = proplists:get_value(<<"iat">>, IdTokenProplist)
            }
        },
        {ok, TokenResponse}
    end).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% get_tokens/2
%% ====================================================================
%% @doc Returns list of token details for client or provider.
%% @end
-spec get_tokens(Client :: client(), URN :: string()) -> Result when
    Result :: {ok, Tokens :: [#token_details{}]} | {error, Reason :: term()}.
%% ====================================================================
get_tokens(Client, URN) ->
    ?run(fun() ->
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        TokenInfo = proplists:get_value(<<"tokenInfo">>, Proplist),
        Tokens = lists:map(fun(Token) ->
            #token_details{
                access_id = proplists:get_value(<<"accessId">>, Token),
                client_name = proplists:get_value(<<"clientName">>, Token)
            }
        end, TokenInfo),
        {ok, Tokens}
    end).


%% revoke_token/2
%% ====================================================================
%% @doc Revokes token validity for client or provider.
%% @end
-spec revoke_token(Client :: client(), URN :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
revoke_token(Client, URN) ->
    ?run(fun() ->
        {ok, "202", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).


%% modify_token_details/3
%% ====================================================================
%% @doc Modifies public details about token. Parameters may contain:
%% token's "clientName".
%% @end
-spec modify_token_details(Client :: client(), URN :: string(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_token_details(Client, URN, Parameters) ->
    ?run(fun() ->
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, patch, Body),
        ok
    end).

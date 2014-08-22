%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module allows for OpenID management in Global Registry.
%% @end
%% ===================================================================

-module(gr_openid).

-include("global_registry/gr_types.hrl").
-include("global_registry/gr_tokens.hrl").

%% API
-export([get_client_access_code/1, get_client_tokens/1, remove_client_token/2, verify_client/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% get_client_access_code/1
%% ====================================================================
%% @doc Returns client access code.
-spec get_client_access_code(Client :: client()) -> Result when
    Result :: {ok, AccessCode :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_client_access_code(Client) ->
    try
        URI = "/openid/client/access_code",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        AccessCode = proplists:get_value(<<"accessCode">>, Proplist),
        {ok, AccessCode}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_client_tokens/1
%% ====================================================================
%% @doc Returns list of client tokens details.
-spec get_client_tokens(Client :: client()) -> Result when
    Result :: {ok, Tokens :: [#client_token_details{}]} | {error, Reason :: term()}.
%% ====================================================================
get_client_tokens(Client) ->
    try
        URI = "/openid/client/tokens",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        TokenInfo = proplists:get_value(<<"tokenInfo">>, Proplist),
        Tokens = lists:map(fun(Token) ->
            #client_token_details{
                id = proplists:get_value(<<"accessId">>, Token),
                name = proplists:get_value(<<"clientName">>, Token)
            }
        end, TokenInfo),
        {ok, Tokens}
    catch
        _:Reason -> {error, Reason}
    end.


%% remove_client_token/2
%% ====================================================================
%% @doc Deletes client token.
-spec remove_client_token(Client :: client(), AccessId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_client_token(Client, AccessId) ->
    try
        URI = "/openid/client/tokens/" ++ binary_to_list(AccessId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% verify_client/2
%% ====================================================================
%% @doc Verifies client identity in Global Registry.
%% Parameters should contain: "userId" of client to be verified and
%% associated with client "secret" token.
-spec verify_client(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, VerifyStatus :: binary()} | {error, Reason :: term()}.
%% ====================================================================
verify_client(Client, Parameters) ->
    try
        URI = "openid/client/verify",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        VerifyStatus = proplists:get_value(<<"verified">>, Proplist),
        {ok, VerifyStatus}
    catch
        _:Reason -> {error, Reason}
    end.
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
-include("global_registry/gr_openid.hrl").

%% API
-export([get_client_access_code/1, get_client_tokens/1, remove_client_token/2, verify_client/2]).
-export([get_grant_token/2]).

%% Test API
-ifdef(TEST).
-export([base64decode/1]).
-endif.

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
        URN = "/openid/client/access_code",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
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
    Result :: {ok, Tokens :: [#client_token{}]} | {error, Reason :: term()}.
%% ====================================================================
get_client_tokens(Client) ->
    try
        URN = "/openid/client/tokens",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        TokenInfo = proplists:get_value(<<"tokenInfo">>, Proplist),
        Tokens = lists:map(fun(Token) ->
            #client_token{
                access_id = proplists:get_value(<<"accessId">>, Token),
                client_name = proplists:get_value(<<"clientName">>, Token)
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
        URN = "/openid/client/tokens/" ++ binary_to_list(AccessId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, delete),
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
    Result :: {ok, VerifyStatus :: boolean()} | {error, Reason :: term()}.
%% ====================================================================
verify_client(Client, Parameters) ->
    try
        URN = "openid/client/verify",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        VerifyStatus = proplists:get_value(<<"verified">>, Proplist),
        {ok, VerifyStatus}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_grant_token/2
%% ====================================================================
%% @doc Returns grant token.
%% Parameters should contain: client authorization "code" and "grant_type"
%% of provided authorization code.
-spec get_grant_token(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, Tokens :: [#client_token{}]} | {error, Reason :: term()}.
%% ====================================================================
get_grant_token(Client, Parameters) ->
    try
        URN = "/openid/provider/tokens",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        IdToken = proplists:get_value(<<"id_token">>, Proplist),
        [_Header, Payload, _Signature] = binary:split(IdToken, <<".">>, [global]),
        IdTokenProplist = mochijson2:decode(base64decode(Payload), [{format, proplist}]),
        GrantToken = #grant_token{
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
                email = proplists:get_value(<<"email">>, IdTokenProplist),
                exp = proplists:get_value(<<"exp">>, IdTokenProplist),
                iat = proplists:get_value(<<"iat">>, IdTokenProplist)
            }
        },
        {ok, GrantToken}
    catch
        _:Reason -> {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% base64decode/0
%% ====================================================================
%% @doc
%% Decodes a base64 encoded term.
%% @end
-spec base64decode(binary() | string()) -> term().
%% ====================================================================
base64decode(Bin) when is_binary(Bin) ->
    Bin2 = case byte_size(Bin) rem 4 of
               2 -> <<Bin/binary, "==">>;
               3 -> <<Bin/binary, "=">>;
               _ -> Bin
           end,
    base64:decode(<<<<(urldecode_digit(D))>> || <<D>> <= Bin2>>);
base64decode(L) when is_list(L) ->
    base64decode(iolist_to_binary(L)).


%% urldecode_digit/0
%% ====================================================================
%% @doc
%% Urlencodes a single char in base64.
%% @end
-spec urldecode_digit(binary()) -> binary().
%% ====================================================================
urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D) -> D.
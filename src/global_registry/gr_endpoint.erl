%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc This is a communication layer module. It sends requests to
%% Global Registry using REST API and returns responses.
%% @end
%% ===================================================================

-module(gr_endpoint).

-include("global_registry/gr_types.hrl").

%% API
-export([auth_request/3, auth_request/4, auth_request/5, auth_request/6]).
-export([noauth_request/3, noauth_request/4, noauth_request/5, noauth_request/6]).

%% ====================================================================
%% API functions
%% ====================================================================

%% auth_request/3
%% ====================================================================
%% @equiv auth_request(Client, URN, Method, []).
-spec auth_request(Client :: client(), URN :: urn(), Method :: method()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
auth_request(Client, URN, Method) ->
    auth_request(Client, URN, Method, []).


%% auth_request/4
%% ====================================================================
%% @equiv auth_request(Client, URN, Method, Body, []).
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(), Body :: body()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
auth_request(Client, URN, Method, Body) ->
    auth_request(Client, URN, Method, Body, []).


%% auth_request/5
%% ====================================================================
%% @equiv auth_request(Client, URN, Method, [], Body, Options).
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
auth_request(Client, URN, Method, Body, Options) ->
    auth_request(Client, URN, Method, [], Body, Options).


%% auth_request/6
%% ====================================================================
%% @doc Sends authenticated request to Global Registry.
%% Context depends on client type.
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
auth_request(client, URN, Method, Headers, Body, Options) ->
    do_auth_request(URN, Method, Headers, Body, Options);

auth_request(provider, URN, Method, Headers, Body, Options) ->
    do_auth_request(URN, Method, Headers, Body, Options);

auth_request({Type, undefined}, URN, Method, Headers, Body, Options) when Type =:= user; Type =:= try_user ->
    do_auth_request(URN, Method, Headers, Body, Options);

auth_request({Type, AccessToken}, URN, Method, Headers, Body, Options) when Type =:= user; Type =:= try_user ->
    AuthorizationHeader = {"authorization", "Bearer " ++ binary_to_list(AccessToken)},
    do_auth_request(URN, Method, [AuthorizationHeader | Headers], Body, Options).


%% noauth_request/3
%% ====================================================================
%% @equiv noauth_request(Client, URN, Method, [])
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
noauth_request(Client, URN, Method) ->
    noauth_request(Client, URN, Method, []).


%% noauth_request/4
%% ====================================================================
%% @equiv noauth_request(Client, URN, Method, Body, [])
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(), Body :: body()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
noauth_request(Client, URN, Method, Body) ->
    noauth_request(Client, URN, Method, Body, []).


%% noauth_request/5
%% ====================================================================
%% @equiv noauth_request(Client, URN, Method, [], Body, Options)
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
noauth_request(Client, URN, Method, Body, Options) ->
    noauth_request(Client, URN, Method, [], Body, Options).


%% noauth_request/6
%% ====================================================================
%% @doc Sends unauthenticated request to Global Registry.
%% Context depends on client type.
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
noauth_request(client, URN, Method, Headers, Body, Options) ->
    do_noauth_request(URN, Method, Headers, Body, Options);

noauth_request(provider, URN, Method, Headers, Body, Options) ->
    do_noauth_request(URN, Method, Headers, Body, Options);

noauth_request({Type, undefined}, URN, Method, Headers, Body, Options) when Type =:= user; Type =:= try_user ->
    do_noauth_request(URN, Method, Headers, Body, Options);

noauth_request({Type, AccessToken}, URN, Method, Headers, Body, Options) when Type =:= user; Type =:= try_user ->
    AuthorizationHeader = {"authorization", "Bearer " ++ binary_to_list(AccessToken)},
    do_noauth_request(URN, Method, [AuthorizationHeader | Headers], Body, Options).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% do_auth_request/5
%% ====================================================================
%% @doc Sends request to Global Registry using REST API.
%% Request is authenticated with provider certificate.
-spec do_auth_request(URN :: urn(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
do_auth_request(URN, Method, Headers, Body, Options) ->
    KeyPath = gr_plugin:get_key_path(),
    CertPath = gr_plugin:get_cert_path(),
    CACertPath = gr_plugin:get_cacert_path(),
    {ok, Key} = file:read_file(KeyPath),
    {ok, Cert} = file:read_file(CertPath),
    {ok, CACert} = file:read_file(CACertPath),
    [{KeyType, KeyEncoded, _} | _] = public_key:pem_decode(Key),
    [{_, CertEncoded, _} | _] = public_key:pem_decode(Cert),
    [{_, CACertEncoded, _} | _] = public_key:pem_decode(CACert),
    SSLOptions = {ssl_options, [{cacerts, [CACertEncoded]}, {key, {KeyType, KeyEncoded}}, {cert, CertEncoded}]},
    do_noauth_request(URN, Method, Headers, Body, [SSLOptions | Options]).


%% do_noauth_request/5
%% ====================================================================
%% @doc Sends request to Global Registry using REST API.
%% Request is not authenticated with provider certificate.
-spec do_noauth_request(URN :: urn(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
do_noauth_request(URN, Method, Headers, Body, Options) ->
    URL = gr_plugin:get_gr_url(),
    ibrowse:send_req(URL ++ URN, [{"content-type", "application/json"} | Headers], Method, Body, Options).
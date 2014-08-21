%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module handles requests to Global Registry using REST API.
%% @end
%% ===================================================================

-module(gr_endpoint).

-include("global_registry/gr_types.hrl").

%% API
-export([request/3, request/4, request/5, request/6]).

%% ====================================================================
%% API functions
%% ====================================================================

%% request/3
%% ====================================================================
%% @doc Sends request to Global Registry with default options and headers.
%% Request body is empty. Context depends on client type.
-spec request(Client :: client(), URI :: uri(), Method :: method()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
request(Client, URI, Method) ->
    request(Client, URI, Method, []).


%% request/4
%% ====================================================================
%% @doc Sends request to Global Registry with default options and headers.
%% Context depends on client type.
-spec request(Client :: client(), URI :: uri(), Method :: method(), Body :: body()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
request(Client, URI, Method, Body) ->
    request(Client, URI, Method, Body, []).


%% request/5
%% ====================================================================
%% @doc Sends request to Global Registry with default headers.
%% Context depends on client type.
-spec request(Client :: client(), URI :: uri(), Method :: method(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
request(Client, URI, Method, Body, Options) ->
    request(Client, URI, Method, [], Body, Options).


%% request/6
%% ====================================================================
%% @doc Sends request to Global Registry. Context depends on client type.
-spec request(Client :: client(), URI :: uri(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
request(provider, URI, Method, Headers, Body, Options) ->
    do_request(URI, Method, Headers, Body, Options);

request({user, AccessToken}, URI, Method, Headers, Body, Options) ->
    AuthorizationHeader = {"authorization", <<"Bearer ", AccessToken/binary>>},
    do_request(URI, Method, [AuthorizationHeader | Headers], Body, Options).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% do_request/5
%% ====================================================================
%% @doc Sends request to Global Registry using REST API.
-spec do_request(URI :: uri(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
do_request(URI, Method, Headers, Body, Options) ->
    URL = gr_plugin:get_gr_url(),
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
    ibrowse:send_req(URL ++ URI, [{"content_type", "application/json"} | Headers], Method, Body, [SSLOptions | Options]).
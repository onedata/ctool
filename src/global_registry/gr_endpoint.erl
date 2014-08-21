%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module handles secure_requests to Global Registry using REST API.
%% @end
%% ===================================================================

-module(gr_endpoint).

-include("global_registry/gr_types.hrl").

%% API
-export([secure_request/3, secure_request/4, secure_request/5, secure_request/6]).
-export([insecure_request/3, insecure_request/4, insecure_request/5, insecure_request/6]).

%% ====================================================================
%% API functions
%% ====================================================================

%% secure_request/3
%% ====================================================================
%% @doc Sends secure request to Global Registry with default options
%% and headers. Request body is empty. Context depends on client type.
-spec secure_request(Client :: client(), URI :: uri(), Method :: method()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
secure_request(Client, URI, Method) ->
    secure_request(Client, URI, Method, []).


%% secure_request/4
%% ====================================================================
%% @doc Sends secure request to Global Registry with default options
%% and headers. Context depends on client type.
-spec secure_request(Client :: client(), URI :: uri(), Method :: method(), Body :: body()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
secure_request(Client, URI, Method, Body) ->
    secure_request(Client, URI, Method, Body, []).


%% secure_request/5
%% ====================================================================
%% @doc Sends secure request to Global Registry with default headers.
%% Context depends on client type.
-spec secure_request(Client :: client(), URI :: uri(), Method :: method(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
secure_request(Client, URI, Method, Body, Options) ->
    secure_request(Client, URI, Method, [], Body, Options).


%% secure_request/6
%% ====================================================================
%% @doc Sends secure request to Global Registry.
%% Context depends on client type.
-spec secure_request(Client :: client(), URI :: uri(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
secure_request(provider, URI, Method, Headers, Body, Options) ->
    do_secure_request(URI, Method, Headers, Body, Options);

secure_request({user, AccessToken}, URI, Method, Headers, Body, Options) ->
    AuthorizationHeader = {"authorization", <<"Bearer ", AccessToken/binary>>},
    do_secure_request(URI, Method, [AuthorizationHeader | Headers], Body, Options).


%% insecure_request/3
%% ====================================================================
%% @doc Sends insecure request to Global Registry with default options
%% and headers. Request body is empty. Context depends on client type.
-spec insecure_request(Client :: client(), URI :: uri(), Method :: method()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
insecure_request(Client, URI, Method) ->
    insecure_request(Client, URI, Method, []).


%% insecure_request/4
%% ====================================================================
%% @doc Sends insecure_request to Global Registry with default options
%% and headers. Context depends on client type.
-spec insecure_request(Client :: client(), URI :: uri(), Method :: method(), Body :: body()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
insecure_request(Client, URI, Method, Body) ->
    insecure_request(Client, URI, Method, Body, []).


%% insecure_request/5
%% ====================================================================
%% @doc Sends insecure_request to Global Registry with default headers.
%% Context depends on client type.
-spec insecure_request(Client :: client(), URI :: uri(), Method :: method(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
insecure_request(Client, URI, Method, Body, Options) ->
    insecure_request(Client, URI, Method, [], Body, Options).


%% insecure_request/6
%% ====================================================================
%% @doc Sends insecure_request to Global Registry.
%% Context depends on client type.
-spec insecure_request(Client :: client(), URI :: uri(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
insecure_request(provider, URI, Method, Headers, Body, Options) ->
    do_insecure_request(URI, Method, Headers, Body, Options);

insecure_request({user, AccessToken}, URI, Method, Headers, Body, Options) ->
    AuthorizationHeader = {"authorization", <<"Bearer ", AccessToken/binary>>},
    do_insecure_request(URI, Method, [AuthorizationHeader | Headers], Body, Options).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% do_secure_request/5
%% ====================================================================
%% @doc Sends request to Global Registry using REST API which is
%% by default secured by SSL layer.
-spec do_secure_request(URI :: uri(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
do_secure_request(URI, Method, Headers, Body, Options) ->
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
    do_insecure_request(URI, Method, Headers, Body, [SSLOptions | Options]).


%% do_insecure_request/5
%% ====================================================================
%% @doc Sends request to Global Registry using REST API which is not
%% by default secured by SSL layer.
-spec do_insecure_request(URI :: uri(), Method :: method(), Headers :: headers(), Body :: body(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
do_insecure_request(URI, Method, Headers, Body, Options) ->
    URL = gr_plugin:get_gr_url(),
    ibrowse:send_req(URL ++ URI, [{"content-type", "application/json"} | Headers], Method, Body, Options).
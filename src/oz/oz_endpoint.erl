%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This is a communication layer module. It sends requests to
%%% OZ using REST API and returns responses.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_endpoint).

%% API
-export([auth_request/3, auth_request/4, auth_request/5, auth_request/6]).
-export([noauth_request/3, noauth_request/4, noauth_request/5,
    noauth_request/6]).

%% Uniform Resource Name -
%% for more details see: http://pl.wikipedia.org/wiki/Uniform_Resource_Name
-type urn() :: string().
%% HTTP request types
-type method() :: put | post | get | patch | delete.
-type headers() :: [{Key :: binary(), Value :: binary()}].
-type body() :: binary().
-type options() :: http_client:opts().
-type response() :: {ok, Status :: integer(), ResponseHeaders :: headers(),
    ResponseBody :: body()} | {error, Reason :: term()}.
-type params() :: [{Key :: binary(), Value :: binary()}].

%% OZ oz_endpoint:client()
% Tuple containing root macaroon and discharge macaroons for user auth.
-type macaroons() :: {Macaroon :: macaroon:macaroon(),
    DischargeMacaroons :: [macaroon:macaroon()]}.
-type client() :: client | provider | {user, macaroons()} |
{try_user, macaroons()}.

-export_type([client/0, params/0, urn/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @equiv auth_request(Client, URN, Method, [])
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Client :: client(), URN :: urn(), Method :: method()) ->
    response().
auth_request(Client, URN, Method) ->
    oz_endpoint:auth_request(Client, URN, Method, <<>>).

%%--------------------------------------------------------------------
%% @equiv auth_request(Client, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body()) -> response().
auth_request(Client, URN, Method, Body) ->
    oz_endpoint:auth_request(Client, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @equiv auth_request(Client, URN, Method, [], Body, Options)
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body(), Options :: options()) -> response().
auth_request(Client, URN, Method, Body, Options) ->
    oz_endpoint:auth_request(Client, URN, Method, [], Body, Options).

%%--------------------------------------------------------------------
%% @doc Sends authenticated request to OZ.
%% Context depends on oz_endpoint:client() type.
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: body(), Options :: options()) -> response().
auth_request(client, URN, Method, Headers, Body, Options) ->
    do_auth_request(URN, Method, Headers, Body, Options);

auth_request(provider, URN, Method, Headers, Body, Options) ->
    do_auth_request(URN, Method, Headers, Body, Options);

auth_request({Type, undefined}, URN, Method, Headers, Body, Options)
    when Type =:= user; Type =:= try_user ->
    do_auth_request(URN, Method, Headers, Body, Options);

auth_request({Type, Macaroons}, URN, Method, Headers, Body, Options)
    when Type =:= user; Type =:= try_user ->
    AuthHeaders = prepare_auth_headers(Macaroons),
    do_auth_request(URN, Method, AuthHeaders ++ Headers, Body, Options).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Client, URN, Method, [])
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method()) ->
    response().
noauth_request(Client, URN, Method) ->
    oz_endpoint:noauth_request(Client, URN, Method, <<>>).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Client, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body()) -> response().
noauth_request(Client, URN, Method, Body) ->
    oz_endpoint:noauth_request(Client, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Client, URN, Method, [], Body, Options)
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body(), Options :: list()) -> response().
noauth_request(Client, URN, Method, Body, Options) ->
    oz_endpoint:noauth_request(Client, URN, Method, [], Body, Options).

%%--------------------------------------------------------------------
%% @doc Sends unauthenticated request to OZ.
%% Context depends on oz_endpoint:client() type.
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: body(), Options :: options()) -> response().
noauth_request(client, URN, Method, Headers, Body, Options) ->
    do_noauth_request(URN, Method, Headers, Body, Options);

noauth_request(provider, URN, Method, Headers, Body, Options) ->
    do_noauth_request(URN, Method, Headers, Body, Options);

noauth_request({Type, undefined}, URN, Method, Headers, Body, Options)
    when Type =:= user; Type =:= try_user ->
    do_noauth_request(URN, Method, Headers, Body, Options);

noauth_request({Type, Macaroons}, URN, Method, Headers, Body, Options)
    when Type =:= user; Type =:= try_user ->
    AuthHeaders = prepare_auth_headers(Macaroons),
    do_noauth_request(URN, Method, AuthHeaders ++ Headers, Body, Options).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Sends request to OZ using REST API.
%% Request is authenticated with provider certificate.
%% @end
%%--------------------------------------------------------------------
-spec do_auth_request(URN :: urn(), Method :: method(), Headers :: headers(),
    Body :: body(), Options :: options()) -> response().
do_auth_request(URN, Method, Headers, Body, Opts) ->
    KeyPath = apply(oz_plugin, get_key_path, []),
    CertPath = apply(oz_plugin, get_cert_path, []),
    SSLOpts = {ssl_options, [{keyfile, KeyPath}, {certfile, CertPath}]},
    do_noauth_request(URN, Method, Headers, Body, [SSLOpts | Opts]).


%%--------------------------------------------------------------------
%% @doc Sends request to OZ using REST API.
%% Request is not authenticated with provider certificate.
%% @end
%%--------------------------------------------------------------------
-spec do_noauth_request(URN :: urn(), Method :: method(), Headers :: headers(),
    Body :: body(), Options :: options()) -> response().
do_noauth_request(URN, Method, Headers, Body, Options) ->
    Opts = case application:get_env(ctool, verify_oz_cert) of
               {ok, false} -> [insecure | Options];
               _ -> Options
           end,
    NewHeaders = [{<<"content-type">>, <<"application/json">>} | Headers],
    URL = oz_plugin:get_oz_url() ++ URN,
    http_client:request(Method, URL, NewHeaders, Body, Opts).


%%--------------------------------------------------------------------
%% @doc Returns properly formatted headers with macaroons.
%% @end
%%--------------------------------------------------------------------
-spec prepare_auth_headers(Macaroons :: macaroons()) ->
    [{Key :: binary(), Val :: binary()}].
prepare_auth_headers({Macaroon, DischargeMacaroons}) ->
    BoundMacaroons = lists:map(
        fun(DM) ->
            BDM = macaroon:prepare_for_request(Macaroon, DM),
            {ok, SerializedBDM} = macaroon:serialize(BDM),
            SerializedBDM
        end, DischargeMacaroons),
    % Bound discharge macaroons are sent in one header,
    % separated by spaces.
    {ok, SerializedMacaroon} = macaroon:serialize(Macaroon),
    BoundMacaroonsValue = str_utils:join_binary(BoundMacaroons, <<" ">>),
    [
        {<<"macaroon">>, SerializedMacaroon},
        {<<"discharge-macaroons">>, BoundMacaroonsValue}
    ].

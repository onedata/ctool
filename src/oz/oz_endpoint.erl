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
-export([rest_api_root/0]).

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
-type params() :: [{Key :: binary(), Value :: binary() | [binary()]}].

%% OZ oz_endpoint:client()
% Tuple containing root macaroon and discharge macaroons for user auth.
-type macaroons() :: {Macaroon :: macaroon:macaroon(),
    DischargeMacaroons :: [macaroon:macaroon()]}.
-type client() :: client | provider | {user, token, macaroons()} |
% Credentials are in form "Basic base64(user:password)"
{user, basic, Credentials :: binary()}.
% Auth is an arbitrary term, which is treated like a black box by ctool.
% It can carry any information, the only condition is that the project
% using ctool implements the callback oz_plugin:auth_to_rest_client/1.
% The callback changes the Auth term() to rest client type that ctool
% can understand -> see client() type.
-type auth() :: term().

-export_type([auth/0, client/0, params/0, urn/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @equiv auth_request(Auth, URN, Method, [])
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Auth :: auth(), URN :: urn(), Method :: method()) ->
    response().
auth_request(Auth, URN, Method) ->
    oz_endpoint:auth_request(Auth, URN, Method, <<>>).

%%--------------------------------------------------------------------
%% @equiv auth_request(Auth, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body()) -> response().
auth_request(Auth, URN, Method, Body) ->
    oz_endpoint:auth_request(Auth, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @equiv auth_request(Auth, URN, Method, [], Body, Options)
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body(), Options :: options()) -> response().
auth_request(Auth, URN, Method, Body, Options) ->
    oz_endpoint:auth_request(Auth, URN, Method, [], Body, Options).

%%--------------------------------------------------------------------
%% @doc Sends authenticated request to OZ.
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: body(), Options :: options()) -> response().
auth_request(Auth, URN, Method, Headers, Body, Options) ->
    AuthHeaders = prepare_auth_headers(Auth),
    do_auth_request(URN, Method, AuthHeaders ++ Headers, Body, Options).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Auth, URN, Method, [])
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Auth :: auth(), URN :: urn(), Method :: method()) ->
    response().
noauth_request(Auth, URN, Method) ->
    oz_endpoint:noauth_request(Auth, URN, Method, <<>>).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Auth, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body()) -> response().
noauth_request(Auth, URN, Method, Body) ->
    oz_endpoint:noauth_request(Auth, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Auth, URN, Method, [], Body, Options)
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body(), Options :: list()) -> response().
noauth_request(Auth, URN, Method, Body, Options) ->
    oz_endpoint:noauth_request(Auth, URN, Method, [], Body, Options).

%%--------------------------------------------------------------------
%% @doc Sends unauthenticated request to OZ.
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: body(), Options :: options()) -> response().
noauth_request(Auth, URN, Method, Headers, Body, Options) ->
    AuthHeaders = prepare_auth_headers(Auth),
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
    URL = rest_api_root() ++ URN,
    http_client:request(Method, URL, NewHeaders, Body, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Returns root path to OZ REST API, for example:
%%      https://onedata.org:8443/api/v3/onezone
%% based on information obtained from oz_plugin.
%% @end
%%--------------------------------------------------------------------
-spec rest_api_root() -> string().
rest_api_root() ->
    str_utils:format("~s:~B~s", [
        oz_plugin:get_oz_url(),
        oz_plugin:get_oz_rest_port(),
        oz_plugin:get_oz_rest_api_prefix()
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Returns properly formatted auth headers (i.e. macaroon, basic auth),
%% depending on type of REST client.
%% @end
%%--------------------------------------------------------------------
-spec prepare_auth_headers(Auth :: auth()) ->
    [{Key :: binary(), Val :: binary()}].
prepare_auth_headers(Auth) ->
    % Check REST client type and return auth headers if needed.
    case oz_plugin:auth_to_rest_client(Auth) of
        {user, token, {Macaroon, DischargeMacaroons}} ->
            BoundMacaroons = lists:map(
                fun(DM) ->
                    BDM = macaroon:prepare_for_request(Macaroon, DM),
                    {ok, SerializedBDM} = macaroon:serialize(BDM),
                    SerializedBDM
                end, DischargeMacaroons),
            % Bound discharge macaroons are sent in one header,
            % separated by spaces.
            {ok, SerializedMacaroon} = macaroon:serialize(Macaroon),
            BoundMacaroonsVal = str_utils:join_binary(BoundMacaroons, <<" ">>),
            [
                {<<"macaroon">>, SerializedMacaroon},
                {<<"discharge-macaroons">>, BoundMacaroonsVal}
            ];
        {user, basic, BasicAuthHeader} ->
            [
                {<<"Authorization">>, BasicAuthHeader}
            ];
        _ ->
            []
    end.
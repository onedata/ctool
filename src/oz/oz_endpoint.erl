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

-include("logging.hrl").

%% API
-export([get_rest_api_root/0, get_oz_cacerts/0]).
-export([provider_request/3, provider_request/4, provider_request/5,
    provider_request/6]).
-export([request/3, request/4, request/5, request/6]).

-type urn() :: string().
-type method() :: http_client:method().
-type headers() :: http_client:headers().
-type body() :: http_client:body().
-type opts() :: http_client:opts().
-type response() :: http_client:response().
-type params() :: [{Key :: binary(), Value :: binary() | [binary()]}].
-type client() :: client | provider | {user, token, macaroons()} |
%% Credentials are in form "Basic base64(user:password)"
{user, basic, Credentials :: binary()}.
%% Tuple containing root macaroon and discharge macaroons for user auth.
-type macaroons() :: {Macaroon :: macaroon:macaroon(),
    DischargeMacaroons :: [macaroon:macaroon()]}.
%% Auth is an arbitrary term, which is treated like a black box by ctool.
%% It can carry any information, the only condition is that the project
%% using ctool implements the callback oz_plugin:auth_to_rest_client/1.
%% The callback changes the Auth term() to rest client type that ctool
%% can understand -> see client() type.
-type auth() :: term().

-export_type([auth/0, client/0, params/0, urn/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns root path to OZ REST API, for example:
%% 'https://onedata.org:8443/api/v3/onezone' based on information obtained
%% from oz_plugin.
%% @end
%%--------------------------------------------------------------------
-spec get_rest_api_root() -> string().
get_rest_api_root() ->
    str_utils:format("~s:~B~s", [
        oz_plugin:get_oz_url(),
        oz_plugin:get_oz_rest_port(),
        oz_plugin:get_oz_rest_api_prefix()
    ]).

%%--------------------------------------------------------------------
%% @doc Returns cached OZ CA certificates or loads them from a directory given
%% by a oz_plugin:get_cacerts_dir/0 callback and stores them in the cache.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_cacerts() -> CaCerts :: [binary()].
get_oz_cacerts() ->
    case application:get_env(ctool, oz_cacerts) of
        {ok, CaCerts} -> CaCerts;
        undefined ->
            CaCertDir = oz_plugin:get_cacerts_dir(),
            case file_utils:read_files({dir, CaCertDir}) of
                {ok, CaCerts} ->
                    application:set_env(ctool, oz_cacerts, CaCerts),
                    CaCerts;
                {error, Reason} ->
                    ?error("Cannot load OZ CA certificates due to: ~p", [Reason]),
                    []
            end
    end.

%%--------------------------------------------------------------------
%% @doc @equiv provider_request(Auth, URN, Method, [])
%% @end
%%--------------------------------------------------------------------
-spec provider_request(Auth :: auth(), URN :: urn(), Method :: method()) ->
    Response :: response().
provider_request(Auth, URN, Method) ->
    ?MODULE:provider_request(Auth, URN, Method, <<>>).

%%--------------------------------------------------------------------
%% @doc @equiv provider_request(Auth, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec provider_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body()) -> Response :: response().
provider_request(Auth, URN, Method, Body) ->
    ?MODULE:provider_request(Auth, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @doc @equiv provider_request(Auth, URN, Method, [], Body, Opts)
%% @end
%%--------------------------------------------------------------------
-spec provider_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body(), Opts :: opts()) -> Response :: response().
provider_request(Auth, URN, Method, Body, Opts) ->
    ?MODULE:provider_request(Auth, URN, Method, [], Body, Opts).

%%--------------------------------------------------------------------
%% @doc Sends request to onezone with provider certificate.
%% @end
%%--------------------------------------------------------------------
-spec provider_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: body(), Opts :: opts()) -> Response :: response().
provider_request(Auth, URN, Method, Headers, Body, Opts) ->
    KeyFile = oz_plugin:get_key_file(),
    CertFile = oz_plugin:get_cert_file(),
    SSLOpts = lists_utils:key_get(ssl_options, Opts, []),
    SSLOpts2 = lists_utils:key_store([
        {keyfile, KeyFile}, {certfile, CertFile}
    ], SSLOpts),
    Opts2 = lists_utils:key_store(ssl_options, SSLOpts2, Opts),
    ?MODULE:request(Auth, URN, Method, Headers, Body, Opts2).

%%--------------------------------------------------------------------
%% @doc @equiv request(Auth, URN, Method, [])
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method()) ->
    Response :: response().
request(Auth, URN, Method) ->
    ?MODULE:request(Auth, URN, Method, <<>>).

%%--------------------------------------------------------------------
%% @doc @equiv request(Auth, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body()) -> Response :: response().
request(Auth, URN, Method, Body) ->
    ?MODULE:request(Auth, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @doc @equiv request(Auth, URN, Method, [], Body, Opts)
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body(), Opts :: opts()) -> Response :: response().
request(Auth, URN, Method, Body, Opts) ->
    ?MODULE:request(Auth, URN, Method, [], Body, Opts).

%%--------------------------------------------------------------------
%% @doc Sends unauthenticated request to OZ.
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: body(), Opts :: opts()) -> Response :: response().
request(Auth, URN, Method, Headers, Body, Opts) ->
    Opts2 = case application:get_env(ctool, verify_oz_cert) of
        {ok, false} ->
            [insecure | Opts];
        _ ->
            SSLOpts = lists_utils:key_get(ssl_options, Opts, []),
            CaCerts = lists_utils:key_get(cacerts, SSLOpts, []),
            CaCerts2 = CaCerts ++ get_oz_cacerts(),
            SSLOpts2 = lists_utils:key_store(cacerts, CaCerts2, SSLOpts),
            lists_utils:key_store(ssl_options, SSLOpts2, Opts)
    end,
    Headers2 = lists_utils:key_store(<<"content-type">>, <<"application/json">>, Headers),
    Headers3 = prepare_auth_headers(Auth, Headers2),
    URL = get_rest_api_root() ++ URN,
    http_client:request(Method, URL, Headers3, Body, Opts2).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns properly formatted auth headers 
%% (i.e. macaroon, basic auth), depending on type of REST client.
%% @end
%%--------------------------------------------------------------------
-spec prepare_auth_headers(Auth :: auth(), Headers :: headers()) ->
    [{Key :: binary(), Val :: binary()}].
prepare_auth_headers(Auth, Headers) ->
    % Check REST client type and return auth headers if needed.
    case oz_plugin:auth_to_rest_client(Auth) of
        {user, token, {Macaroon, DischargeMacaroons}} ->
            BoundMacaroons = lists:map(
                fun(DM) ->
                    BDM = macaroon:prepare_for_request(Macaroon, DM),
                    {ok, SerializedBDM} = token_utils:serialize62(BDM),
                    SerializedBDM
                end, DischargeMacaroons),
            % Bound discharge macaroons are sent in one header,
            % separated by spaces.
            {ok, SerializedMacaroon} = token_utils:serialize62(Macaroon),
            BoundMacaroonsVal = str_utils:join_binary(BoundMacaroons, <<" ">>),
            lists_utils:key_store([
                {<<"macaroon">>, SerializedMacaroon},
                {<<"discharge-macaroons">>, BoundMacaroonsVal}
            ], Headers);
        {user, basic, BasicAuthHeader} ->
            lists_utils:key_store(<<"Authorization">>, BasicAuthHeader, Headers);
        _ ->
            Headers
    end.
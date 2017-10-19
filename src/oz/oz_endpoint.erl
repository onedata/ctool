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
-export([get_api_root/1, get_oz_cacerts/0, reset_oz_cacerts/0]).
-export([provider_request/3, provider_request/4, provider_request/5,
    provider_request/6]).
-export([request/3, request/4, request/5, request/6]).

-type urn() :: string().
-type method() :: http_client:method().
-type headers() :: http_client:headers().
-type body() :: http_client:body().
-type opts() :: http_client:opts() | [{endpoint, rest | rest_no_auth | gui}].
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
%% @doc Returns root path to OZ API, for example:
%% GUI: 'https://onedata.org'
%% REST: 'https://onedata.org:8443/api/v3/onezone'
%% based on information obtained from oz_plugin.
%% @end
%%--------------------------------------------------------------------
-spec get_api_root(opts()) -> string().
get_api_root(Opts) ->
    case proplists:get_value(endpoint, Opts, rest) of
        rest ->
            % Regular REST endpoint with authorization by provider certs
            str_utils:format("~s:~B~s", [
                oz_plugin:get_oz_url(),
                oz_plugin:get_oz_rest_port(),
                oz_plugin:get_oz_rest_api_prefix()
            ]);
        rest_no_auth ->
            % REST endpoint without authorization on standard 443 HTTPS port
            str_utils:format("~s~s", [
                oz_plugin:get_oz_url(),
                oz_plugin:get_oz_rest_api_prefix()
            ]);
        gui ->
            % Endpoint on standard 443 HTTPS port
            str_utils:format("~s", [
                oz_plugin:get_oz_url()
            ])
    end.

%%--------------------------------------------------------------------
%% @doc Returns cached OZ CA certificates or loads them from a directory given
%% by a oz_plugin:get_cacerts_dir/0 callback and stores them in the cache.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_cacerts() -> CaCerts :: [public_key:der_encoded()].
get_oz_cacerts() ->
    case application:get_env(ctool, oz_cacerts) of
        {ok, CaCerts} ->
            CaCerts;
        undefined ->
            CaCerts = cert_utils:load_ders_in_dir(oz_plugin:get_cacerts_dir()),
            application:set_env(ctool, oz_cacerts, CaCerts),
            CaCerts
    end.

%%--------------------------------------------------------------------
%% @doc Clears OZ CA certificates cache.
%% @end
%%--------------------------------------------------------------------
-spec reset_oz_cacerts() -> ok.
reset_oz_cacerts() ->
    application:unset_env(ctool, oz_cacerts).

%%--------------------------------------------------------------------
%% @doc @equiv provider_request(Auth, URN, Method, <<>>)
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
%% @doc @equiv provider_request(Auth, URN, Method, #{}, Body, Opts)
%% @end
%%--------------------------------------------------------------------
-spec provider_request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body(), Opts :: opts()) -> Response :: response().
provider_request(Auth, URN, Method, Body, Opts) ->
    ?MODULE:provider_request(Auth, URN, Method, #{}, Body, Opts).

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
%% @doc @equiv request(Auth, URN, Method, <<>>)
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
%% @doc @equiv request(Auth, URN, Method, #{}, Body, Opts)
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: body(), Opts :: opts()) -> Response :: response().
request(Auth, URN, Method, Body, Opts) ->
    ?MODULE:request(Auth, URN, Method, #{}, Body, Opts).

%%--------------------------------------------------------------------
%% @doc Sends unauthenticated request to OZ.
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: body(), Opts :: opts()) -> Response :: response().
request(Auth, URN, Method, Headers, Body, Opts) ->
    SSLOpts = lists_utils:key_get(ssl_options, Opts, []),
    Opts2 = lists_utils:key_store(ssl_options, [
        {cacerts, get_oz_cacerts()} | SSLOpts
    ], Opts),
    Headers2 = Headers#{<<"content-type">> => <<"application/json">>},
    Headers3 = prepare_auth_headers(Auth, Headers2),
    URL = get_api_root(Opts) ++ URN,
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
    headers().
prepare_auth_headers(Auth, Headers) ->
    % Check REST client type and return auth headers if needed.
    case oz_plugin:auth_to_rest_client(Auth) of
        {user, token, Token} ->
            Headers#{<<"X-Auth-Token">> => Token};
        {user, macaroon, {MacaroonBin, DischargeMacaroonsBin}} ->
            {ok, Macaroon} = token_utils:deserialize(MacaroonBin),
            BoundMacaroons = lists:map(
                fun(DischargeMacaroonBin) ->
                    {ok, DM} = token_utils:deserialize(DischargeMacaroonBin),
                    BDM = macaroon:prepare_for_request(Macaroon, DM),
                    {ok, SerializedBDM} = token_utils:serialize62(BDM),
                    SerializedBDM
                end, DischargeMacaroonsBin),
            % Bound discharge macaroons are sent in one header,
            % separated by spaces.
            BoundMacaroonsVal = str_utils:join_binary(BoundMacaroons, <<" ">>),
            Headers#{
                <<"Macaroon">> => MacaroonBin,
                <<"Discharge-Macaroons">> => BoundMacaroonsVal
            };
        {user, basic, BasicAuthHeader} ->
            Headers#{<<"Authorization">> => BasicAuthHeader};
        _ ->
            Headers
    end.
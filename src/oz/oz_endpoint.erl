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
-include("global_definitions.hrl").

-define(OZ_PLUGIN, (application:get_env(ctool, oz_plugin_module, oz_plugin))).

%% API
-export([get_api_root/1, get_cacerts/0, reset_cacerts/0]).
-export([request/3, request/4, request/5, request/6]).

-type urn() :: string().
-type method() :: http_client:method().
-type headers() :: http_client:headers().
-type request_body() :: http_client:request_body().
-type opts() :: http_client:opts() | [{endpoint, rest | rest_no_auth | gui}].
-type response() :: http_client:response().
-type params() :: [{Key :: binary(), Value :: binary() | [binary()]}] | map().
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
                ?OZ_PLUGIN:get_oz_url(),
                ?OZ_PLUGIN:get_oz_rest_port(),
                ?OZ_PLUGIN:get_oz_rest_api_prefix()
            ]);
        rest_no_auth ->
            % REST endpoint without authorization on standard 443 HTTPS port
            str_utils:format("~s~s", [
                ?OZ_PLUGIN:get_oz_url(),
                ?OZ_PLUGIN:get_oz_rest_api_prefix()
            ]);
        gui ->
            % Endpoint on standard 443 HTTPS port
            str_utils:format("~s", [
                ?OZ_PLUGIN:get_oz_url()
            ])
    end.

%%--------------------------------------------------------------------
%% @doc Returns cached CA certificates or loads them from a directory given
%% by a oz_plugin:get_cacerts_dir/0 callback and stores them in the cache.
%% @end
%%--------------------------------------------------------------------
-spec get_cacerts() -> CaCerts :: [public_key:der_encoded()].
get_cacerts() ->
    case application:get_env(?CTOOL_APP_NAME, cacerts) of
        {ok, CaCerts} ->
            CaCerts;
        undefined ->
            CaCerts = cert_utils:load_ders_in_dir(?OZ_PLUGIN:get_cacerts_dir()),
            application:set_env(?CTOOL_APP_NAME, cacerts, CaCerts),
            CaCerts
    end.

%%--------------------------------------------------------------------
%% @doc Clears CA certificates cache.
%% @end
%%--------------------------------------------------------------------
-spec reset_cacerts() -> ok.
reset_cacerts() ->
    application:unset_env(?CTOOL_APP_NAME, cacerts).

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
    Body :: request_body()) -> Response :: response().
request(Auth, URN, Method, Body) ->
    ?MODULE:request(Auth, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @doc @equiv request(Auth, URN, Method, #{}, Body, Opts)
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method(),
    Body :: request_body(), Opts :: opts()) -> Response :: response().
request(Auth, URN, Method, Body, Opts) ->
    ?MODULE:request(Auth, URN, Method, #{}, Body, Opts).

%%--------------------------------------------------------------------
%% @doc Sends unauthenticated request to OZ.
%% @end
%%--------------------------------------------------------------------
-spec request(Auth :: auth(), URN :: urn(), Method :: method(),
    Headers :: headers(), Body :: request_body(), Opts :: opts()) -> Response :: response().
request(Auth, URN, Method, Headers, Body, Opts) ->
    SSLOpts = lists_utils:key_get(ssl_options, Opts, []),
    Opts2 = lists_utils:key_store(ssl_options, [
        {cacerts, get_cacerts()} | SSLOpts
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
    case ?OZ_PLUGIN:auth_to_rest_client(Auth) of
        {headers, Map} -> maps:merge(Headers, Map);
        {user, token, Token} ->
            Headers#{<<"X-Auth-Token">> => Token};
        {user, macaroon, {MacaroonBin, DischargeMacaroonsBin}} ->
            {ok, Macaroon} = macaroons:deserialize(MacaroonBin),
            BoundMacaroons = lists:map(
                fun(DischargeMacaroonBin) ->
                    {ok, DM} = macaroons:deserialize(DischargeMacaroonBin),
                    BDM = macaroon:prepare_for_request(Macaroon, DM),
                    {ok, SerializedBDM} = macaroons:serialize(BDM),
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
        {provider, Macaroon} ->
            Headers#{<<"Macaroon">> => Macaroon};
        none ->
            Headers
    end.
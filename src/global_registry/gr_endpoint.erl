%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This is a communication layer module. It sends requests to
%%% Global Registry using REST API and returns responses.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_endpoint).

%% API
-export([auth_request/3, auth_request/4, auth_request/5, auth_request/6]).
-export([noauth_request/3, noauth_request/4, noauth_request/5, noauth_request/6]).

%% HTTP request types
-type urn() :: string(). %% Uniform Resource Name - for more details see: http://pl.wikipedia.org/wiki/Uniform_Resource_Name
-type method() :: put | post | get | patch | delete.
-type header() :: atom() | string().
-type value() :: term().
-type headers() :: [{header(), value()}].
-type response() :: {ok,
    Status :: string(),
    ResponseHeaders :: [{Name :: string(), Value :: string()}],
    ResponseBody :: string()
} | {ibrowse_req_id, req_id()} | {error, Reason :: term()}.
-type req_id() :: term().
-type body() :: [] | string() | binary().
-type options() :: [option()].
-type option() :: {ssl_options, [SSLOpt :: term()]} | {content_type, string()}.
-type parameters() :: [{Key :: binary(), Value :: binary()}].

%% Global Registry gr_endpoint:client()
% Tuple containing root macaroon and discharge macaroons for user auth
% (all macaroons are serialized).
-type macaroons() :: {Macaroon :: binary(), DischMacaroons :: [binary()]}.
-type client() :: client | provider | {user, macaroons()} |
{try_user, macaroons()}.

-export_type([client/0, parameters/0, urn/0]).

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
    gr_endpoint:auth_request(Client, URN, Method, []).

%%--------------------------------------------------------------------
%% @equiv auth_request(Client, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body()) -> response().
auth_request(Client, URN, Method, Body) ->
    gr_endpoint:auth_request(Client, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @equiv auth_request(Client, URN, Method, [], Body, Options)
%% @end
%%--------------------------------------------------------------------
-spec auth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body(), Options :: options()) -> response().
auth_request(Client, URN, Method, Body, Options) ->
    gr_endpoint:auth_request(Client, URN, Method, [], Body, Options).

%%--------------------------------------------------------------------
%% @doc Sends authenticated request to Global Registry.
%% Context depends on gr_endpoint:client() type.
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
    gr_endpoint:noauth_request(Client, URN, Method, []).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Client, URN, Method, Body, [])
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body()) -> response().
noauth_request(Client, URN, Method, Body) ->
    gr_endpoint:noauth_request(Client, URN, Method, Body, []).

%%--------------------------------------------------------------------
%% @equiv noauth_request(Client, URN, Method, [], Body, Options)
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(Client :: client(), URN :: urn(), Method :: method(),
    Body :: body(), Options :: list()) -> response().
noauth_request(Client, URN, Method, Body, Options) ->
    gr_endpoint:noauth_request(Client, URN, Method, [], Body, Options).

%%--------------------------------------------------------------------
%% @doc Sends unauthenticated request to Global Registry.
%% Context depends on gr_endpoint:client() type.
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
%% @doc Sends request to Global Registry using REST API.
%% Request is authenticated with provider certificate.
%% @end
%%--------------------------------------------------------------------
-spec do_auth_request(URN :: urn(), Method :: method(), Headers :: headers(),
    Body :: body(), Options :: options()) -> response().
do_auth_request(URN, Method, Headers, Body, Options) ->
    KeyPath = apply(gr_plugin, get_key_path, []),
    CertPath = apply(gr_plugin, get_cert_path, []),
    CACertPath = apply(gr_plugin, get_cacert_path, []),
    {ok, Key} = file:read_file(KeyPath),
    {ok, Cert} = file:read_file(CertPath),
    {ok, CACert} = file:read_file(CACertPath),
    [{KeyType, KeyEncoded, _} | _] = public_key:pem_decode(Key),
    [{_, CertEncoded, _} | _] = public_key:pem_decode(Cert),
    [{_, CACertEncoded, _} | _] = public_key:pem_decode(CACert),
    SSLOptions = {ssl_options, [
        {cacerts, [CACertEncoded]},
        {key, {KeyType, KeyEncoded}},
        {cert, CertEncoded}
    ]},
    do_noauth_request(URN, Method, Headers, Body, [SSLOptions | Options]).

%%--------------------------------------------------------------------
%% @doc Sends request to Global Registry using REST API.
%% Request is not authenticated with provider certificate.
%% @end
%%--------------------------------------------------------------------
-spec do_noauth_request(URN :: urn(), Method :: method(), Headers :: headers(),
    Body :: body(), Options :: list()) -> response().
do_noauth_request(URN, Method, Headers, Body, Options) ->
    URL = apply(gr_plugin, get_gr_url, []),
    NewHeaders = [{"content-type", "application/json"} | Headers],
    ibrowse:send_req(URL ++ URN, NewHeaders, Method, Body, Options).


-spec prepare_auth_headers(Macaroons :: macaroons()) ->
    [{Key :: string(), Val :: string()}].
prepare_auth_headers({SrlzdMacaroon, SrlzdDischMacaroons}) ->
    {ok, Macaroon} = macaroon:deserialize(SrlzdMacaroon),
    BoundMacaroons = lists:map(
        fun(SrlzdDischMacaroon) ->
            {ok, DM} = macaroon:deserialize(SrlzdDischMacaroon),
            {ok, BDM} = macaroon:prepare_for_request(Macaroon, DM),
            {ok, SBDM} = macaroon:serialize(BDM),
            binary_to_list(SBDM)
        end, SrlzdDischMacaroons),
    [
        {"macaroon", binary_to_list(SrlzdMacaroon)},
        % Bound discharge macaroons are sent in one header,
        % separated by spaces.
        {"discharge-macaroons", string:join(BoundMacaroons, " ")}
    ].

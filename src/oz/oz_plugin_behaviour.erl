%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This behaviour specifies an API for OZ plugin -
%%% a module that provides necessary data to connect to OZ.
%%% Every project using OZ REST API client must
%%% implement this behaviour and the implementing module
%%% must be called oz_plugin.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_plugin_behaviour).

%%%===================================================================
%%% Callbacks descriptions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Should return OZ URL.
%%--------------------------------------------------------------------
-callback get_oz_url() -> string().

%%--------------------------------------------------------------------
%% @doc Should return OZ REST port.
%%--------------------------------------------------------------------
-callback get_oz_rest_port() -> integer().

%%--------------------------------------------------------------------
%% @doc Should return OZ REST API prefix - for example /api/v3/onezone.
%%--------------------------------------------------------------------
-callback get_oz_rest_api_prefix() -> string().

%%--------------------------------------------------------------------
%% @doc Should return a path to the directory containing OZ
%% CA certificates.
%% @end
%%--------------------------------------------------------------------
-callback get_cacerts_dir() -> file:name_all().

%%--------------------------------------------------------------------
%% @doc
%% This callback is used to convert Auth term, which is transparent to ctool,
%% into one of possible authorization methods. Thanks to this, the code
%% using OZ API can always use its specific Auth terms and they are converted
%% when request is done.
%% @end
%%--------------------------------------------------------------------
-callback auth_to_rest_client(Auth :: term()) -> {user, token, binary()} |
{headers, #{Header :: binary() => Value :: binary()}} |
{user, macaroon, {Macaroon :: binary(), DischargeMacaroons :: [binary()]}} |
{user, basic, binary()} | {provider, Macaroon :: binary()} | none.
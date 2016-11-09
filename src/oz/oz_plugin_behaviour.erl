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
%% @doc Should return a path to file the containing provider's private key.
%%--------------------------------------------------------------------
-callback get_key_file() -> file:name_all().

%%--------------------------------------------------------------------
%% @doc Should return a path to the file containing provider's CSR.
%%--------------------------------------------------------------------
-callback get_csr_file() -> file:name_all().

%%--------------------------------------------------------------------
%% @doc Should return a path to the file containing provider's public
%% certificate signed by OZ.
%% @end
%%--------------------------------------------------------------------
-callback get_cert_file() -> file:name_all().

%%--------------------------------------------------------------------
%% @doc Should return a path to the directory containing OZ
%% CA certificates.
%% @end
%%--------------------------------------------------------------------
-callback get_cacerts_dir() -> file:name_all().
%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This behaviour specifies an API for Global Registry plugin -
%%% a module that provides necessary data to connect to Global Registry.
%%% Every project using Global Registry REST API gr_endpoint:client() must
%%% implement this behaviour and the implementing module must be called gr_plugin.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_plugin_behaviour).

%%%===================================================================
%%% Callbacks descriptions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Should return a Global Registry URL.
%%--------------------------------------------------------------------
-callback get_gr_url() -> string().

%%--------------------------------------------------------------------
%% @doc Should return a path to file containing provider's private key.
%%--------------------------------------------------------------------
-callback get_key_path() -> file:name_all().

%%--------------------------------------------------------------------
%% @doc Should return a path to file containing provider's CSR.
%%--------------------------------------------------------------------
-callback get_csr_path() -> file:name_all().

%%--------------------------------------------------------------------
%% @doc Should return a path to file containing provider's public
%% certificate signed by Global Registry.
%% @end
%%--------------------------------------------------------------------
-callback get_cert_path() -> file:name_all().

%%--------------------------------------------------------------------
%% @doc Should return a path to file containing Global Registry
%% CA certificate.
%% @end
%%--------------------------------------------------------------------
-callback get_cacert_path() -> file:name_all().
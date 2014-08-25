%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This behaviour specifies an API for Global Registry plugin -
%% a module that provides necessary data to connect to Global Registry.
%% Every project using Global Registry REST API client must implement
%% this behaviour and the implementing module must be called gr_plugin.
%% @end
%% ===================================================================

-module(gr_plugin_behaviour).

%% ====================================================================
%% Callbacks descriptions
%% ====================================================================

%% get_gr_url/0
%% ====================================================================
%% @doc Should return a Global Registry URL.
-callback get_gr_url() -> string().


%% get_key_path/0
%% ====================================================================
%% @doc Should return a path to file containing provider's private key.
-callback get_key_path() -> binary().


%% get_cert_path/0
%% ====================================================================
%% @doc Should return a path to file containing provider's public
%% certificate signed by Global Registry.
-callback get_cert_path() -> binary().


%% get_cacert_path/0
%% ====================================================================
%% @doc Should return a path to file containing Global Registry
%% CA certificate.
-callback get_cacert_path() -> binary().
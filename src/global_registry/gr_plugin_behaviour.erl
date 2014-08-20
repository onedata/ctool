%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This behaviour specifies an API for Global Registry plugin -
%% a module that provides necessary data to connect to Global Registry.
%% Every project using Global Registry endpoint must implement this
%% behaviour and the implmeneting module must be called gr_plugin.
%% @end
%% ===================================================================

-module(gr_plugin_behaviour).

%% API
-export([behaviour_info/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% behaviour_info/1
%% ====================================================================
%% @doc Defines the behaviour (lists the callbacks and their arity)
-spec behaviour_info(Arg) -> Result when
    Arg :: callbacks | Other,
    Result :: [Fun_def]
    | undefined,
    Fun_def :: tuple(),
    Other :: any().
%% ====================================================================
behaviour_info(callbacks) ->
    [
        {get_gr_url, 0},
        {get_key_path, 0},
        {get_cert_path, 0},
        {get_cacert_path, 0}
    ];

behaviour_info(_Other) ->
    undefined.


%% ====================================================================
%% Callbacks descriptions
%% ====================================================================

%% get_gr_url/0
%% ====================================================================
%% Function: get_gr_url() -> string().
%% Desription: Should return a Global Registry URL.
%% ====================================================================


%% get_key_path/0
%% ====================================================================
%% Function: get_key_path() -> binary().
%% Desription: Should return a path to file containing provider's
%% private key.
%% ====================================================================


%% get_cert_path/0
%% ====================================================================
%% Function: get_cert_path() -> binary().
%% Desription: Should return a path to file containing provider's
%% public certificate signed by Global Registry.
%% ====================================================================


%% get_cacert_path/0
%% ====================================================================
%% Function: get_cacert_path() -> binary().
%% Desription: Should return a path to file containing Global Registry
%% CA certificate.
%% ====================================================================
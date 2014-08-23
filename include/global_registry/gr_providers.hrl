%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition of provider record
%% @end
%% ===================================================================

-ifndef(GR_PROVIDERS_HRL).
-define(GR_PROVIDERS_HRL, 1).

%% provider_details record contains following fields:
%% * id               - unique provider ID assigned by Global Registry
%% * urls             - URL addresses of all VeilCluster nodes
%% * redirectionPoint - URL address where VeilCluster GUI is available of all VeilCluster nodes
-record(provider_details, {
    id :: binary(),
    redirection_point :: binary(),
    urls :: [URL :: binary()]
}).

-endif.
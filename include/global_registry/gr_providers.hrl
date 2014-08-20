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

%% This record defines a provider whoes GUI can be reached by "redirectionPoint"
%% and cluster nodes are available at "urls"
-record(provider_info, {
    id :: binary(),
    redirection_point :: binary(),
    urls :: [URL :: binary()]
}).

-endif.
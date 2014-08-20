%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition for provider record
%% @end
%% ===================================================================

-ifndef(GR_PROVIDER_HRL).
-define(GR_PROVIDER_HRL, 1).

%% This record defines a provider who support spaces and can be reached via url
-record(provider_info, {
    id :: binary(),
    redirection_point :: binary(),
    urls :: [URL :: binary()]
}).

-endif.
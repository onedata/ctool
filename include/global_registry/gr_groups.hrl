%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition of group record
%% @end
%% ===================================================================

-ifndef(GR_GROUPS_HRL).
-define(GR_GROUPS_HRL, 1).

%% This record defines a group details
-record(group_info, {
    id :: binary(),
    name :: binary()
}).

-endif.
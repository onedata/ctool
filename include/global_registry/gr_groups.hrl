%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition of group record.
%% @end
%% ===================================================================

-ifndef(GR_GROUPS_HRL).
-define(GR_GROUPS_HRL, 1).

%% group_details record contains following fields:
%% * id   - unique group ID assigned by Global Registry
%% * name - group name
-record(group_details, {
    id :: binary(),
    name :: binary()
}).

-endif.
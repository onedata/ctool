%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc OZ definition of group record.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_GROUPS_HRL).
-define(OZ_GROUPS_HRL, 1).

%% @doc group_details record contains following fields:
%% * id   - unique group ID assigned by OZ
%% * name - group name
%% * type - group type (can be undefined)
%% @end
-record(group_details, {
    id :: binary(),
    name :: binary(),
    % Public means that only public data could be retrieved.
    type :: undefined | public | organization | unit | team | role
}).

-endif.
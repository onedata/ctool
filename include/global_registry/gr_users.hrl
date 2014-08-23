%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition of user record.
%% @end
%% ===================================================================

-ifndef(GR_USERS_HRL).
-define(GR_USERS_HRL, 1).

%% user_details record contains following fields:
%% * id   - unique user ID assigned by Global Registry
%% * name - username
-record(user_details, {
    id :: binary(),
    name :: binary()
}).

-endif.
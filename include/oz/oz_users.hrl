%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc OZ definition of user record.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_USERS_HRL).
-define(OZ_USERS_HRL, 1).

%% user_details record contains following fields:
%% * id   - unique user ID assigned by OZ
%% * name - username
-record(user_details, {
    id :: binary(),
    name :: binary()
}).

%% user_spaces record contains following fields:
%% * ids     - list of IDs of user's Spaces
%% * default - id of default user's Space
-record(user_spaces, {
    ids :: [binary()],
    default :: binary()
}).

-endif.
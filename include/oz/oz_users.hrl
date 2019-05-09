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
%% * full_name - first name and surname
%% * username - unique, human readable identifier, can used for authentication
%% * linked_accounts - list of proplists with info about linked oauth accounts,
%% contains data provided by oauth: idp, subject_id, username, name, emails
%% * emails - list of connected emails
-record(user_details, {
    id :: binary(),
    full_name :: binary(),
    username :: binary() | undefined,
    linked_accounts :: [proplists:proplist()] | undefined,
    emails :: [binary()] | undefined
}).

%% user_spaces record contains following fields:
%% * ids     - list of IDs of user's Spaces
%% * default - id of default user's Space
-record(user_spaces, {
    ids :: [binary()],
    default :: binary()
}).

-endif.

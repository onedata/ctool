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
%% * id - unique user ID assigned by OZ
%% * full_name - given names and surname
%% * username - a human-readable identifier, unique across the system,
%%              can be used for authentication
%% * linked_accounts - list of maps with info about linked accounts,
%%                     contains data provided by the IdP: idp, subject_id,
%%                     fullName, username, emails
%% * emails - list of connected emails
-record(user_details, {
    id :: binary(),
    full_name :: binary(),
    username :: binary() | undefined,
    linked_accounts :: [map()] | undefined,
    emails :: [binary()] | undefined
}).

-endif.

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry types definitions.
%% @end
%% ===================================================================

-ifndef(GR_TYPES_HRL).
-define(GR_TYPES_HRL, 1).

-export_type([urn/0, method/0, header/0, value/0, headers/0, body/0]).
-export_type([client/0, space_privilege/0, group_privilege/0]).

%% HTTP request types
-type urn() :: string().
-type method() :: post | get | patch | delete.
-type header() :: atom() | string().
-type value() :: term().
-type headers() :: [{header(), value()}].
-type body() :: [] | string() | binary().

%% Global Registry client
-type client() :: provider | {user, AccessToken :: binary()} | {try_user, AccessToken :: binary()}.

%% User privileges with regards to Space management.
-type space_privilege() :: space_invite_user | space_remove_user |
space_invite_group | space_remove_group | space_set_privileges |
space_remove | space_add_provider | space_remove_provider |
space_change_data | space_view_data.


%% User privileges with regards to group management.
-type group_privilege() :: group_change_data | group_invite_user |
group_remove_user | group_join_space | group_create_space |
group_set_privileges | group_remove | group_leave_space |
group_view_data | group_create_space_token.

-endif.
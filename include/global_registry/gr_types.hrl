%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry types definitions
%% @end
%% ===================================================================

-ifndef(GR_TYPES_HRL).
-define(GR_TYPES_HRL, 1).

-export_type([uri/0, method/0, header/0, value/0, headers/0, body/0]).
-export_type([client/0, parameters/0]).

%% HTTP request types
-type uri() :: string().
-type method() :: post | get | patch | delete.
-type header() :: atom() | string().
-type value() :: term().
-type headers() :: [{header(), value()}].
-type body() :: [] | string() | binary().

%% Global Registry types
-type client() :: provider | {user, AccessToken :: binary()}.

-endif.
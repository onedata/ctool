%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition of Space record
%% @end
%% ===================================================================

-ifndef(GR_OPENID_HRL).
-define(GR_OPENID_HRL, 1).

%% @todo: Check types and add fields description

%% client_token record contains following fields:
%% * access_id   - ???
%% * client_name - client token name
-record(client_token, {
    access_id :: binary(),
    client_name :: binary()
}).

-record(id_token, {
    iss :: binary(),
    sub :: binary(),
    aud :: binary(),
    name :: binary(),
    email :: [binary()],
    exp :: term(),
    iat :: term()
}).

-record(grant_token, {
    access_token :: binary(),
    token_type :: binary(),
    expires_in :: term(),
    refresh_token :: term(),
    scope :: term(),
    id_token :: #id_token{}
}).

-endif.
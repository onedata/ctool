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

%% client_token_details record contains following fields:
%% * id     - unique client token ID assigned by Global Registry
%% * name   - client token name
-record(client_token_details, {
    id :: binary(),
    name :: binary()
}).

-endif.
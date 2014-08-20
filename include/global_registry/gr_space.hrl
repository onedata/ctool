%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition of space record
%% @end
%% ===================================================================

-ifndef(GR_SPACE_HRL).
-define(GR_SPACE_HRL, 1).

%% This record defines a space details
-record(space_info, {
    id :: binary(),
    name :: binary()
}).

-endif.
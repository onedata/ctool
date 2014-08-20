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

-ifndef(GR_SPACES_HRL).
-define(GR_SPACES_HRL, 1).

%% This record defines a Space details
-record(space_info, {
    id :: binary(),
    name :: binary()
}).

-endif.
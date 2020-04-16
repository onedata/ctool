%%%-------------------------------------------------------------------
%%% @author Michał Wrzeszcz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains record definitions used by consistent hashing.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(CONSISTENT_HASHING_HRL).
-define(CONSISTENT_HASHING_HRL, 1).

% Record representing information about routing
-record(node_routing_info, {
    assigned_nodes :: [node()],
    failed_nodes :: [node()],
    all_nodes :: [node()]
}).

% Ring generations (see consistent_hashing.erl)
-define(CURRENT_RING, current_consistent_hashing_ring).
-define(FUTURE_RING, future_consistent_hashing_ring).
-define(PREVIOUS_RING, previous_consistent_hashing_ring).

-endif.
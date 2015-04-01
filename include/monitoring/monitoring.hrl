%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains definitions of records used accross all modules
%%% that collect or utilize monitoring data.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(NODE_MONITORING_HRL).
-define(NODE_MONITORING_HRL, 1).

-record(node_state, {
    node = node() :: node(),
    ip_addr = {127, 0, 0, 1} :: {A :: byte(), B :: byte(), C :: byte(), D :: byte()},
    cpu_usage = [] :: float(),
    mem_usage = [] :: float(),
    net_usage = [] :: integer()
}).

-endif.
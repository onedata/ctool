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

%% Record containing node state. It's used by node managers to send
%% monitoring data to cluster manager.
-record(node_state, {
    node = node() :: node(),
    cpu_usage = [] :: float(),
    mem_usage = [] :: float(),
    net_usage = [] :: float()
}).

-endif.
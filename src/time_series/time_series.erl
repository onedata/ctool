%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates common concepts related to time series  -
%%% collections of data points (measurements) aggregated within time
%%% windows of constant width.
%%% @end
%%%-------------------------------------------------------------------
-module(time_series).
-author("Lukasz Opiola").


-type time() :: time:seconds().
-export_type([time/0]).

% user defined name of the metric
-type metric_legend() ::  binary().
% width of a single time window
-type metric_resolution() ::  time().  % 0 means infinity
% number of windows to store in the metric (older windows are pruned)
-type metric_retention() ::  pos_integer().
% aggregator function applied when a new measurement is inserted into a time window
-type metric_aggregator() ::  sum | max | min | last | first. % | {gather, Max}. % TODO VFS-8164 - extend functions list
-export_type([metric_legend/0, metric_resolution/0, metric_retention/0, metric_aggregator/0]).

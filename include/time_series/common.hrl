%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions related to time series concepts.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(TIME_SERIES_COMMON_HRL).
-define(TIME_SERIES_COMMON_HRL, 1).


% Record expressing configuration of a single metric in a time series
-record(metric_config, {
    resolution :: metric_config:resolution(),
    retention :: metric_config:retention(),
    aggregator :: metric_config:aggregator()
}).


% Typical metric resolutions
-define(INFINITY_RESOLUTION, 0).
-define(SECOND_RESOLUTION, 1).
-define(FIVE_SECONDS_RESOLUTION, 5).
-define(MINUTE_RESOLUTION, 60).
-define(HOUR_RESOLUTION, 3600).
-define(DAY_RESOLUTION, 86400).
-define(WEEK_RESOLUTION, 604800).
-define(MONTH_RESOLUTION, 2592000).  % 30 days
-define(YEAR_RESOLUTION, 31536000).  % 365 days


-define(ALLOWED_METRIC_RESOLUTIONS, [
    ?INFINITY_RESOLUTION,
    ?SECOND_RESOLUTION,
    ?FIVE_SECONDS_RESOLUTION,
    ?MINUTE_RESOLUTION,
    ?HOUR_RESOLUTION,
    ?DAY_RESOLUTION,
    ?WEEK_RESOLUTION,
    ?MONTH_RESOLUTION,
    ?YEAR_RESOLUTION
]).

-define(MAX_METRIC_RETENTION, 1000000).

-define(ALLOWED_METRIC_AGGREGATORS, [
    sum, max, min, first, last
]).

-endif.

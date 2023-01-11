%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates common concepts related to time series,
%%% i.e. collections of data points (measurements) aggregated within time
%%% windows of constant width.
%%% @end
%%%-------------------------------------------------------------------
-module(time_series).
-author("Lukasz Opiola").


%% API
-export([unit_to_json/1, unit_from_json/1]).
-export([dynamic_source_type_to_json/1, dynamic_source_type_from_json/1]).


% expresses time (either a timestamp, or a time interval) in unit common across time series implementation
-type time_seconds() :: time:seconds().
-export_type([time_seconds/0]).

%% @formatter:off
-type unit() :: none
              | milliseconds | seconds
              | bits | bytes
              | hertz | counts_per_sec | operations_per_sec | requests_per_sec
              | bits_per_sec | bytes_per_sec
              | reads_per_sec | writes_per_sec | io_operations_per_sec
              | percent | percent_normalized
              | boolean
              | {custom, binary()}.
-export_type([unit/0]).
%% @formatter:on


% Types of sources for dynamic chart series data/config loading.
% Currently, only one source type is supported.
-type dynamic_source_type() :: external.
-export_type([dynamic_source_type/0]).

% a human readable name assigned to a time series
-type name() :: binary().
% a human readable name that uniquely identifies a metric within a time series
-type metric_name() :: binary().
% A set of metrics along with their configs, where each metric is identified by name;
% an integral part of time series configuration (each time series has a set of metrics,
% identified by unique names within the time series).
-type metric_composition() :: #{metric_name() => metric_config:record()}.
-export_type([name/0, metric_name/0, metric_composition/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec unit_to_json(unit()) -> json_utils:json_term().
unit_to_json(none) -> <<"none">>;
unit_to_json(milliseconds) -> <<"milliseconds">>;
unit_to_json(seconds) -> <<"seconds">>;
unit_to_json(bits) -> <<"bits">>;
unit_to_json(bytes) -> <<"bytes">>;
unit_to_json(hertz) -> <<"hertz">>;
unit_to_json(counts_per_sec) -> <<"countsPerSec">>;
unit_to_json(operations_per_sec) -> <<"operationsPerSec">>;
unit_to_json(requests_per_sec) -> <<"requestsPerSec">>;
unit_to_json(bytes_per_sec) -> <<"bytesPerSec">>;
unit_to_json(bits_per_sec) -> <<"bitsPerSec">>;
unit_to_json(reads_per_sec) -> <<"readsPerSec">>;
unit_to_json(writes_per_sec) -> <<"writesPerSec">>;
unit_to_json(io_operations_per_sec) -> <<"ioOperationsPerSec">>;
unit_to_json(percent) -> <<"percent">>;
unit_to_json(percent_normalized) -> <<"percentNormalized">>;
unit_to_json(boolean) -> <<"boolean">>;
unit_to_json({custom, UnitName}) -> <<"custom:", UnitName/binary>>.


-spec unit_from_json(json_utils:json_term()) -> unit().
unit_from_json(<<"none">>) -> none;
unit_from_json(<<"milliseconds">>) -> milliseconds;
unit_from_json(<<"seconds">>) -> seconds;
unit_from_json(<<"bits">>) -> bits;
unit_from_json(<<"bytes">>) -> bytes;
unit_from_json(<<"hertz">>) -> hertz;
unit_from_json(<<"countsPerSec">>) -> counts_per_sec;
unit_from_json(<<"operationsPerSec">>) -> operations_per_sec;
unit_from_json(<<"requestsPerSec">>) -> requests_per_sec;
unit_from_json(<<"bytesPerSec">>) -> bytes_per_sec;
unit_from_json(<<"bitsPerSec">>) -> bits_per_sec;
unit_from_json(<<"readsPerSec">>) -> reads_per_sec;
unit_from_json(<<"writesPerSec">>) -> writes_per_sec;
unit_from_json(<<"ioOperationsPerSec">>) -> io_operations_per_sec;
unit_from_json(<<"percent">>) -> percent;
unit_from_json(<<"percentNormalized">>) -> percent_normalized;
unit_from_json(<<"boolean">>) -> boolean;
unit_from_json(<<"custom:", UnitName/binary>>) -> {custom, UnitName}.


-spec dynamic_source_type_to_json(dynamic_source_type()) -> json_utils:json_term().
dynamic_source_type_to_json(external) -> <<"external">>.


-spec dynamic_source_type_from_json(json_utils:json_term()) -> dynamic_source_type().
dynamic_source_type_from_json(<<"external">>) -> external.

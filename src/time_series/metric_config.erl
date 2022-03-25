%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing configuration of a time series metric.
%%% @end
%%%-------------------------------------------------------------------
-module(metric_config).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/common.hrl").
-include("errors.hrl").


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


% width of a single time window
-type resolution() :: time_series:time_seconds().  % 0 means infinity
% number of windows to store in the metric (older windows are pruned)
-type retention() :: pos_integer().
% aggregator function applied when a new measurement is inserted into a time window
-type aggregator() :: sum | max | min | last | first. % | {gather, Max}. % TODO VFS-8164 - extend functions list
-export_type([resolution/0, retention/0, aggregator/0]).

-type record() :: #metric_config{}.
-export_type([record/0]).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    encode(Record).


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    decode(validate, RecordJson).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    encode(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    decode(skip_validation, RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

% TODO VFS-8164 - extend functions list
%% @private
-spec aggregator_to_json(metric_config:aggregator()) -> json_utils:json_term().
aggregator_to_json(sum) -> <<"sum">>;
aggregator_to_json(max) -> <<"max">>;
aggregator_to_json(min) -> <<"min">>;
aggregator_to_json(first) -> <<"first">>;
aggregator_to_json(last) -> <<"last">>.


%% @private
-spec aggregator_from_json(json_utils:json_term()) -> metric_config:aggregator().
aggregator_from_json(<<"sum">>) -> sum;
aggregator_from_json(<<"max">>) -> max;
aggregator_from_json(<<"min">>) -> min;
aggregator_from_json(<<"first">>) -> first;
aggregator_from_json(<<"last">>) -> last.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode(record()) -> json_utils:json_term().
encode(Record) ->
    #{
        <<"resolution">> => Record#metric_config.resolution,
        <<"retention">> => Record#metric_config.retention,
        <<"aggregator">> => aggregator_to_json(Record#metric_config.aggregator)
    }.


%% @private
-spec decode(automation:validation_strategy(), json_utils:json_term()) -> record().
decode(skip_validation, RecordJson) ->
    #metric_config{
        resolution = maps:get(<<"resolution">>, RecordJson),
        retention = maps:get(<<"retention">>, RecordJson),
        aggregator = aggregator_from_json(maps:get(<<"aggregator">>, RecordJson))
    };
decode(validate, RecordJson) ->
    Spec = decode(skip_validation, RecordJson),
    lists:member(Spec#metric_config.resolution, ?ALLOWED_METRIC_RESOLUTIONS) orelse throw(
        ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resolution">>, ?ALLOWED_METRIC_RESOLUTIONS)
    ),
    Spec.

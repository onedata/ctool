%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper module encapsulating the logic of time series naming, especially
%%% mapping of names specified in measurements to names in time series stores.
%%%
%%% Time series measurements are expressed using `atm_time_series_measurements_type`.
%%% A single measurement looks like the following:
%%%
%%%     `{tsName: file_mp3, timestamp: 16416319480, value: 17}`
%%%
%%% It must conform to `atm_time_series_measurements_spec` that serves as
%%% value constraints for the data type and limit the time series names of measurements
%%% that can be returned from a task.
%%%
%%% The measurements can be inserted into a time series store using a proper
%%% `atm_task_schema_result_mapper` that specifies `atm_time_series_content_update_options`.
%%% The latter record includes dispatch rules how that capture a matching measurement
%%% and specify the name of time series in the target time series store where the
%%% measurement will be inserted, referencing a concrete `atm_time_series_schema`.
%%%
%%% Example mapping process of a measurement with `{tsName: file_mp3}`:
%%%   1.  The list of `atm_time_series_measurements_spec` specified as value constraints in
%%%       the data type is analyzed top-down to find a matching spec.
%%%   2a. If no matching spec is found, workflow execution fails with an error
%%%       (it is required that all measurements produced by a lambda are covered).
%%%   2b. The first spec that matches is taken. In this example, assume that the
%%%       following spec has been matched:
%%%           #atm_time_series_measurements_spec{
%%%               name_matcher_type = has_prefix,
%%%               name_matcher = <<"file_">>,
%%%               ...
%%%           }
%%%   3.  The list of dispatch rules in `atm_time_series_content_update_options` included
%%%       in `atm_task_schema_result_mapper` is analyzed top-down to find a rule that
%%%       specifies the `name_matcher` from 2b.
%%%   3a. If no such rule is found, the measurement is discarded (the task creator
%%%       may freely choose which measurements should be taken into account).
%%%   3b. The rule that specifies `<<"file_">> name_matcher` is taken. In this example,
%%%       assume that the following rule has been used:
%%%           #atm_time_series_dispatch_rule{
%%%               measurement_time_series_name_matcher = <<"file_">>,
%%%               target_time_series_name_generator = <<"file_count_">>,
%%%               prefix_combiner = converge
%%%           }
%%%   4.  The `atm_time_series_schema` that specifies `<<"file_count_">> name_generator` from
%%%       the target time series store is taken. In this example, assume that the following
%%%       schema has been used:
%%%           #atm_time_series_schema{
%%%               name_generator_type = add_prefix,
%%%               name_generator = <<"file_count_">>,
%%%               ...
%%%           }
%%%   5.  Now, the three records from above examples are considered in order to determine the final
%%%       time series name that will receive the measurement:
%%%           * `atm_time_series_measurements_spec`
%%%           * `atm_time_series_schema`
%%%           * `atm_time_series_dispatch_rule`
%%%       The dispatch rule's `prefix_combiner` defines how the prefixes will be processed.
%%%       In this example, both the matcher and generator use prefixes and they are converged
%%%       to finally yield `<<"file_count_mp3">>` target time series name in the target store.
%%%
%%%
%%% Below table shows some possible combinations of the name mapping.
%%%
%%% | MEAS. TS NAME | MEAS. TS NAME MATCHER  | TARGET TS NAME GENERATOR  | PREFIX COMB. | FINAL TS NAME
%%% ---------------------------------------------------------------------------------------------
%%% | files_mp3     | exact : files_mp3      | exact: count_mp3          | <ignored>    |  count_mp3
%%% | files_mp3     | has_prefix : files_    | exact: count_mp3          | <ignored>    |  count_mp3
%%% | files_mp3     | exact : files_mp3      | add_prefix: count_        | <ignored>    |  count_files_mp3
%%% | files_mp3     | has_prefix : files_    | add_prefix: count_        | concatenate  |  count_files_mp3
%%% | files_mp3     | has_prefix : files_    | add_prefix: count_        | converge     |  count_files_mp3
%%% | files_mp3     | has_prefix : files_    | add_prefix: count_        | overwrite    |  count_mp3
%%% | count_mp3     | has_prefix : count_    | add_prefix: count_        | concatenate  |  count_count_mp3
%%% | count_mp3     | has_prefix : count_    | add_prefix: count_        | converge     |  count_mp3
%%% | count_mp3     | has_prefix : count_    | add_prefix: count_        | overwrite    |  count_mp3
%%% | count_f_mp3   | has_prefix : count_f_  | add_prefix: count_        | concatenate  |  count_count_f_mp3
%%% | count_f_mp3   | has_prefix : count_f_  | add_prefix: count_        | converge     |  count_f_mp3
%%% | count_f_mp3   | has_prefix : count_f_  | add_prefix: count_        | overwrite    |  count_mp3
%%% | cmp3          | has_prefix : c         | add_prefix: count_        | concatenate  |  count_cmp3
%%% | cmp3          | has_prefix : c         | add_prefix: count_        | converge     |  count_cmp3
%%% | cmp3          | has_prefix : c         | add_prefix: count_        | overwrite    |  count_mp3
%%%
%%% Consult the types in this module for detailed explanation and possible values.
%%%
%%% Consult the eunit tests (@see atm_time_series_names_tests) for more examples of name mapping.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_names).
-author("Lukasz Opiola").

-include("automation/automation.hrl").


%% API
-export([measurement_ts_name_matcher_type_to_json/1, measurement_ts_name_matcher_type_from_json/1]).
-export([measurement_ts_name_matcher_to_json/1, measurement_ts_name_matcher_from_json/1]).
-export([target_ts_name_generator_type_to_json/1, target_ts_name_generator_type_from_json/1]).
-export([target_ts_name_generator_to_json/1, target_ts_name_generator_from_json/1]).
-export([prefix_combiner_to_json/1, prefix_combiner_from_json/1]).

-export([find_matching_measurements_spec/2]).
-export([find_referencing_dispatch_rule/2]).
-export([find_referenced_time_series_schema/2]).
-export([resolve_target_ts_name/4]).


% Name of a time series assigned to a single entry in the array of measurements represented
% by atm_time_series_measurements_type data type.
-type measurement_ts_name() :: binary().
-export_type([measurement_ts_name/0]).

% This pair defines if the corresponding name matcher should match only the exact same
% time series names, or those that start with specified prefix, e.g.
%   * If `name_matcher_type` is `exact` and `name_matcher` is `<<"latency">>`, it will match
%     only the measurements with `{tsName: "latency"}`.
%
%   * If `name_matcher_type` is `has_prefix` and `name_matcher` is `<<"file_count_">>`, it will
%     match any time series name with the prefix `<<"file_count_">>`, e.g. measurements with
%     `{tsName: "file_count_mp3"}` and `{tsName: "file_count_"}`.
-type measurement_ts_name_matcher_type() :: exact | has_prefix.
-type measurement_ts_name_matcher() :: binary().
-export_type([measurement_ts_name_matcher_type/0, measurement_ts_name_matcher/0]).

% This pair defines a generator of time series names that will be applied during
% workflow execution, when measurements are inserted into a time series store, e.g.
%   * If `name_generator_type` is `exact` and `name_generator` is `<<"latency">>`, the
%     generated time series name will be exactly `<<"latency">>` - all measurements
%     captured by a dispatch rule that define this name generator will be inserted into
%     the time series with name `<<"latency">>`.
%
%   * If `name_generator_type` is `add_prefix` and `name_generator` is `<<"file_count_">>`, the
%     generated time series name will depend on the measurement name matcher and
%     optionally the prefix combiner from the dispatch rule (if both name matcher and
%     generator use prefixes).
-type target_ts_name_generator_type() :: exact | add_prefix.
-type target_ts_name_generator() :: binary().
-export_type([target_ts_name_generator_type/0, target_ts_name_generator/0]).

% Prefix combiner is employed only when a dispatch rule specifies a name matcher of type
% `has_prefix` and name generator of type `add_prefix`. It indicates how the prefixes
% should be combined when determining the target time series name for dispatching measurements:
%   * concatenate - the prefixes are concatenated (the matcher's prefix goes first),
%   * overwrite - only the generator's prefix is added to the target name,
%   * converge - if the name generator is a prefix of the name matcher, the prefix part
%                is not repeated (effectively the original measurement ts name is taken as final),
%                otherwise works like concatenate.
-type prefix_combiner() :: concatenate | converge | overwrite.
-export_type([prefix_combiner/0]).


% Name of a target time series in the target time series store, obtained by applying
% a matching dispatch rule on a measurement time series name. Indicates to which time
% series in the store's collection the measurement should be inserted.
-type target_ts_name() :: binary().
-export_type([target_ts_name/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec measurement_ts_name_matcher_type_to_json(measurement_ts_name_matcher_type()) -> json_utils:json_term().
measurement_ts_name_matcher_type_to_json(exact) -> <<"exact">>;
measurement_ts_name_matcher_type_to_json(has_prefix) -> <<"hasPrefix">>.

-spec measurement_ts_name_matcher_type_from_json(json_utils:json_term()) -> measurement_ts_name_matcher_type().
measurement_ts_name_matcher_type_from_json(<<"exact">>) -> exact;
measurement_ts_name_matcher_type_from_json(<<"hasPrefix">>) -> has_prefix.


-spec measurement_ts_name_matcher_to_json(measurement_ts_name_matcher()) -> json_utils:json_term().
measurement_ts_name_matcher_to_json(NameMatcher) when is_binary(NameMatcher) -> NameMatcher.

-spec measurement_ts_name_matcher_from_json(json_utils:json_term()) -> measurement_ts_name_matcher().
measurement_ts_name_matcher_from_json(NameMatcher) when is_binary(NameMatcher) -> NameMatcher.


-spec target_ts_name_generator_type_to_json(target_ts_name_generator_type()) -> json_utils:json_term().
target_ts_name_generator_type_to_json(exact) -> <<"exact">>;
target_ts_name_generator_type_to_json(add_prefix) -> <<"addPrefix">>.

-spec target_ts_name_generator_type_from_json(json_utils:json_term()) -> target_ts_name_generator_type().
target_ts_name_generator_type_from_json(<<"exact">>) -> exact;
target_ts_name_generator_type_from_json(<<"addPrefix">>) -> add_prefix.


-spec target_ts_name_generator_to_json(target_ts_name_generator()) -> json_utils:json_term().
target_ts_name_generator_to_json(NameMatcher) when is_binary(NameMatcher) -> NameMatcher.

-spec target_ts_name_generator_from_json(json_utils:json_term()) -> target_ts_name_generator().
target_ts_name_generator_from_json(NameMatcher) when is_binary(NameMatcher) -> NameMatcher.


-spec prefix_combiner_to_json(prefix_combiner()) -> json_utils:json_term().
prefix_combiner_to_json(concatenate) -> <<"concatenate">>;
prefix_combiner_to_json(converge) -> <<"converge">>;
prefix_combiner_to_json(overwrite) -> <<"overwrite">>.

-spec prefix_combiner_from_json(json_utils:json_term()) -> prefix_combiner().
prefix_combiner_from_json(<<"concatenate">>) -> concatenate;
prefix_combiner_from_json(<<"converge">>) -> converge;
prefix_combiner_from_json(<<"overwrite">>) -> overwrite.


-spec find_matching_measurements_spec(measurement_ts_name(), [atm_time_series_measurements_spec:record()]) ->
    {ok, atm_time_series_measurements_spec:record()} | error.
find_matching_measurements_spec(MeasurementTSName, TimeSeriesMeasurementsSpecs) ->
    lists_utils:find(fun
        (#atm_time_series_measurements_spec{name_matcher_type = exact, name_matcher = NameMatcher}) ->
            NameMatcher =:= MeasurementTSName;
        (#atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = NameMatcher}) ->
            str_utils:binary_starts_with(MeasurementTSName, NameMatcher)
    end, TimeSeriesMeasurementsSpecs).


-spec find_referencing_dispatch_rule(atm_time_series_measurements_spec:record(), [atm_time_series_dispatch_rule:record()]) ->
    {ok, atm_time_series_dispatch_rule:record()} | error.
find_referencing_dispatch_rule(#atm_time_series_measurements_spec{
    name_matcher = NameMatcher
}, DispatchRules) ->
    lists_utils:find(fun(#atm_time_series_dispatch_rule{measurement_ts_name_matcher = MeasurementTsNameMatcher}) ->
        MeasurementTsNameMatcher =:= NameMatcher
    end, DispatchRules).


-spec find_referenced_time_series_schema(atm_time_series_dispatch_rule:record(), [atm_time_series_schema:record()]) ->
    {ok, atm_time_series_schema:record()} | error.
find_referenced_time_series_schema(#atm_time_series_dispatch_rule{
    target_ts_name_generator = TargetTsNameGenerator
}, TimeSeriesSchemas) ->
    lists_utils:find(fun(#atm_time_series_schema{name_generator = NameGenerator}) ->
        TargetTsNameGenerator =:= NameGenerator
    end, TimeSeriesSchemas).


-spec resolve_target_ts_name(
    measurement_ts_name(),
    atm_time_series_measurements_spec:record(),
    atm_time_series_schema:record(),
    prefix_combiner()
) ->
    target_ts_name().
resolve_target_ts_name(
    _MeasurementTSName,
    _TimeSeriesMeasurementsSpec,
    #atm_time_series_schema{name_generator_type = exact, name_generator = NameGenerator},
    _PrefixCombiner
) ->
    NameGenerator;
resolve_target_ts_name(
    MeasurementTSName,
    #atm_time_series_measurements_spec{name_matcher_type = exact},
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator},
    _PrefixCombiner
) ->
    <<NameGenerator/binary, MeasurementTSName/binary>>;
resolve_target_ts_name(
    MeasurementTSName,
    #atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = _NameMatcher},
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator},
    concatenate
) ->
    <<NameGenerator/binary, MeasurementTSName/binary>>;
resolve_target_ts_name(
    MeasurementTSName,
    #atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = NameMatcher} = TSMeasurementsSpec,
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator} = TSSchema,
    converge
) ->
    case str_utils:binary_starts_with(NameMatcher, NameGenerator) of
        true ->
            MeasurementTSName;
        false ->
            resolve_target_ts_name(MeasurementTSName, TSMeasurementsSpec, TSSchema, concatenate)
    end;
resolve_target_ts_name(
    MeasurementTSName,
    #atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = NameMatcher},
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator},
    overwrite
) ->
    Suffix = string:prefix(MeasurementTSName, NameMatcher),
    <<NameGenerator/binary, Suffix/binary>>.

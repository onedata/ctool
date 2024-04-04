%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper module encapsulating the logic of time series naming in the context
%%% of automation, especially mapping of names specified in measurements to
%%% names in time series stores.
%%%
%%% Time series measurements are expressed using `atm_time_series_measurement_type`,
%%% which carries a single measurement that looks like the following:
%%%
%%%     `{"tsName": "file_mp3", "timestamp": 16416319480, "value": 17}`
%%%
%%% It must conform to one of `atm_time_series_measurement_spec` records that serve as
%%% value constraints for the data type and limit the time series names of measurements
%%% that can be returned from a lambda. Another role of the measurements specs is to
%%% provide a clear contract so that a lambda user can learn what kind of measurements
%%% are expected to be produced by the lambda.
%%%
%%% Whenever a value of `atm_time_series_measurement_type` is passed (either as a lambda
%%% argument that was produced by a store iterator, or as a lambda result that is to be
%%% mapped to a store), it undergoes validation. The list of `atm_time_series_measurement_spec`
%%% specified as value constraints in the data type is analyzed top-down to find a matching
%%% spec. If there is such spec, validation succeeds. If there is none, it is considered
%%% as an error during workflow execution.
%%%
%%% The measurements can be inserted into a time series store in two ways:
%%%     * During lambda result mapping - using a proper `atm_task_schema_result_mapper`
%%%       that specifies `atm_time_series_content_update_options`.
%%%     * Using the store update API, where one can insert arbitrary measurements and
%%%       must specify `atm_time_series_content_update_options`. @TODO VFS-8042 Not yet implemented
%%%
%%% The `atm_time_series_content_update_options` record includes dispatch rules that
%%% capture a matching measurement and specify the name of time series in the target time
%%% series store where the measurement will be inserted, referencing a concrete
%%% `time_series_schema`.
%%%
%%% Below is an example of time series name mapping process applied when a
%%% measurement is inserted into a store. In the example, a measurement with
%%% `{"tsName": "file_mp3"}` is processed.
%%%
%%%   1.  The list of dispatch rules in `atm_time_series_content_update_options` is
%%%       analyzed top-down to find matching rules - there can be more than one matching
%%%       rule, in such case the measurement will be effectively duplicated and inserted
%%%       into multiple target time series.
%%%   2a. If no such rule is found, the measurement is discarded (the task creator
%%%       may freely choose which measurements should be taken into account).
%%%   2b. All matching rules are applied one by one for the processed measurement
%%%       (steps 3 to 5 are repeated for each rule).
%%%   3.  In this example, assume that the following dispatch rule is being applied:
%%%           #atm_time_series_dispatch_rule{
%%%               measurement_time_series_name_matcher_type = has_prefix,
%%%               measurement_time_series_name_matcher = <<"file_">>,
%%%               target_time_series_name_generator = <<"file_count_">>,
%%%               prefix_combiner = converge
%%%           }
%%%       NOTE: The dispatch rules reuse the `name_matcher_type` and `name_matcher` fields
%%%       of `atm_time_series_measurement_spec`. In case of lambda result mapping, the
%%%       values of these fields in a dispatch rule are validated against
%%%       `atm_time_series_measurement_spec` records specified in the lambda's result
%%%       type constraints, making sure that the matcher has a chance of matching an actual
%%%       time series name produced by a lambda. When using the store update API, the name matcher
%%%       in the dispatch rules can be arbitrary, as there is no input data contract in this
%%%       context. In both scenarios, the `target_time_series_name_generator` field must
%%%       reference `time_series_schema` that exists in the store schema, by specifying
%%%       the field `name_generator` with the same value.
%%%   4.  The `time_series_schema` that specifies `<<"file_count_">>` `name_generator` from
%%%       the target time series store is taken. In this example, assume that the following
%%%       schema has been used:
%%%           #time_series_schema{
%%%               name_generator_type = add_prefix,
%%%               name_generator = <<"file_count_">>,
%%%               ...
%%%           }
%%%   5.  Now, the two records from above examples are considered so as to determine the final
%%%       time series name that will receive the measurement:
%%%           * `atm_time_series_dispatch_rule`
%%%           * `time_series_schema`
%%%       The dispatch rule's `prefix_combiner` defines how the prefixes will be processed.
%%%       In this example, both the matcher and generator use prefixes and they are converged
%%%       to finally yield `<<"file_count_mp3">>` target time series name in the target store.
%%%
%%% The name generator, found in a time series schema and referenced by a dispatch rule (as
%%% described above), defines what time series names can be instantiated from the schema, e.g.:
%%%   * If `name_generator_type` is `exact` and `name_generator` is `<<"latency">>`, the
%%%     generated time series name will be exactly `<<"latency">>` - all measurements
%%%     captured by a dispatch rule that defines this name generator will be inserted into
%%%     the time series with name `<<"latency">>`.
%%%
%%%   * If `name_generator_type` is `add_prefix` and `name_generator` is `<<"file_count_">>`, the
%%%     generated time series name will depend on the measurement name matcher and
%%%     optionally the prefix combiner from the dispatch rule (if both name matcher and
%%%     generator use prefixes).
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
%%% Consult the eunit tests (@see atm_time_series_names_tests) for examples how this
%%% module can be used for time series name mapping.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_names).
-author("Lukasz Opiola").

-include("automation/automation.hrl").


%% API
-export([measurement_ts_name_matcher_type_to_json/1, measurement_ts_name_matcher_type_from_json/1]).
-export([measurement_ts_name_matcher_to_json/1, measurement_ts_name_matcher_from_json/1]).
-export([prefix_combiner_to_json/1, prefix_combiner_from_json/1]).

-export([find_matching_measurement_spec/2]).
-export([find_matching_dispatch_rules/2]).
-export([select_referenced_time_series_schema/2]).
-export([resolve_target_ts_name/3]).


% Name of a time series assigned to a single measurement represented
% by atm_time_series_measurement_type data type.
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

% Prefix combiner is employed only when a dispatch rule specifies a name matcher of type
% `has_prefix` and name generator of type `add_prefix`. It indicates how the prefixes
% should be combined when determining the target time series name for dispatching measurements:
%   * concatenate - the prefixes are concatenated, as a result the final name starts with
%                   concatenated generator and matcher prefixes (in this order),
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


-spec prefix_combiner_to_json(prefix_combiner()) -> json_utils:json_term().
prefix_combiner_to_json(concatenate) -> <<"concatenate">>;
prefix_combiner_to_json(converge) -> <<"converge">>;
prefix_combiner_to_json(overwrite) -> <<"overwrite">>.

-spec prefix_combiner_from_json(json_utils:json_term()) -> prefix_combiner().
prefix_combiner_from_json(<<"concatenate">>) -> concatenate;
prefix_combiner_from_json(<<"converge">>) -> converge;
prefix_combiner_from_json(<<"overwrite">>) -> overwrite.


-spec find_matching_measurement_spec(measurement_ts_name(), [atm_time_series_measurement_spec:record()]) ->
    {ok, atm_time_series_measurement_spec:record()} | error.
find_matching_measurement_spec(MeasurementTSName, TimeSeriesMeasurementSpecs) ->
    lists_utils:find(fun(#atm_time_series_measurement_spec{
        name_matcher_type = NameMatcherType,
        name_matcher = NameMatcher
    }) ->
        ts_name_matches(MeasurementTSName, NameMatcherType, NameMatcher)
    end, TimeSeriesMeasurementSpecs).


-spec find_matching_dispatch_rules(measurement_ts_name(), [atm_time_series_dispatch_rule:record()]) ->
    [atm_time_series_dispatch_rule:record()].
find_matching_dispatch_rules(MeasurementTSName, DispatchRules) ->
    lists:filter(fun(#atm_time_series_dispatch_rule{
        measurement_ts_name_matcher_type = NameMatcherType,
        measurement_ts_name_matcher = NameMatcher
    }) ->
        ts_name_matches(MeasurementTSName, NameMatcherType, NameMatcher)
    end, DispatchRules).


-spec select_referenced_time_series_schema(atm_time_series_dispatch_rule:record(), [time_series_schema:record()]) ->
    time_series_schema:record() | no_return().
select_referenced_time_series_schema(#atm_time_series_dispatch_rule{
    target_ts_name_generator = TargetTsNameGenerator
}, TimeSeriesSchemas) ->
    FindResult = lists_utils:find(fun(#time_series_schema{name_generator = NameGenerator}) ->
        TargetTsNameGenerator =:= NameGenerator
    end, TimeSeriesSchemas),
    case FindResult of
        {ok, TimeSeriesSchema} ->
            TimeSeriesSchema;
        error ->
            throw(?ERROR_BAD_DATA(<<"dispatchRules">>, str_utils:format_bin(
                "Time series name generator '~ts' specified in one of the dispatch rules "
                "does not reference any defined time series schema",
                [TargetTsNameGenerator]
            )))
    end.


-spec resolve_target_ts_name(
    measurement_ts_name(),
    time_series_schema:record(),
    atm_time_series_dispatch_rule:record()
) ->
    target_ts_name().
resolve_target_ts_name(
    _MeasurementTSName,
    #time_series_schema{name_generator_type = exact, name_generator = NameGenerator},
    _TimeSeriesDispatchRule
) ->
    NameGenerator;
resolve_target_ts_name(
    MeasurementTSName,
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator},
    #atm_time_series_dispatch_rule{
        measurement_ts_name_matcher_type = exact
    }
) ->
    <<NameGenerator/binary, MeasurementTSName/binary>>;
resolve_target_ts_name(
    MeasurementTSName,
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator},
    #atm_time_series_dispatch_rule{
        measurement_ts_name_matcher_type = has_prefix,
        prefix_combiner = concatenate
    }
) ->
    <<NameGenerator/binary, MeasurementTSName/binary>>;
resolve_target_ts_name(
    MeasurementTSName,
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator} = TSSchema,
    #atm_time_series_dispatch_rule{
        measurement_ts_name_matcher_type = has_prefix,
        measurement_ts_name_matcher = NameMatcher,
        prefix_combiner = converge
    } = DispatchRule
) ->
    case str_utils:binary_starts_with(NameMatcher, NameGenerator) of
        true ->
            MeasurementTSName;
        false ->
            resolve_target_ts_name(MeasurementTSName, TSSchema, DispatchRule#atm_time_series_dispatch_rule{
                prefix_combiner = concatenate
            })
    end;
resolve_target_ts_name(
    MeasurementTSName,
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenerator},
    #atm_time_series_dispatch_rule{
        measurement_ts_name_matcher_type = has_prefix,
        measurement_ts_name_matcher = NameMatcher,
        prefix_combiner = overwrite
    }
) ->
    Suffix = string:prefix(MeasurementTSName, NameMatcher),
    <<NameGenerator/binary, Suffix/binary>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec ts_name_matches(measurement_ts_name(), measurement_ts_name_matcher_type(), measurement_ts_name_matcher()) ->
    boolean().
ts_name_matches(MeasurementTSName, exact, NameMatcher) ->
    NameMatcher =:= MeasurementTSName;
ts_name_matches(MeasurementTSName, has_prefix, NameMatcher) ->
    str_utils:binary_starts_with(MeasurementTSName, NameMatcher).
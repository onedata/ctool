%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper module handling configurable attributes of time series, both
%%% concerning time series measurements (used as input to time series store)
%%% and time series schemas (used to create time series collections during
%%% workflow execution).
%%%
%%% Units are used to provide semantics for bare values inserted to, or stored
%%% inside time series. They help to understand what measurements can a lambda
%%% emit and properly map them to matching time series. Nevertheless, the unit
%%% only acts as an information for the users and there is no strict validation
%%% if the users try to combine non-matching units.
%%%
%%% @TODO VFS-8958 The information below is preliminary, the concepts will be reworked
%%% (Refine and implement time series selectors and store mappers)
%%%
%%% All time series are identified by their name, unique across a
%%% atm_time_series_measurements_type data type or a time series store. These
%%% names are used to map chosen measurements to a specific time series instance
%%% within a time series store. Names can either be fixed, or a pattern,
%%% which is indicated by the name selector field:
%%%
%%%    fixed: will match only the exact name of the time series.
%%%
%%%    pattern: must have exactly one wildcard ('*') character, will match
%%%             names that match the pattern, i. e. pattern '*_files' will
%%%             match time series name 'video_files'.
%%%
%%% @TODO VFS-8958 Implement wildcard matching and name generation logic + eunit tests
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_attribute).
-author("Lukasz Opiola").

-include("errors.hrl").


%% API
-export([name_selector_to_json/1, name_selector_from_json/1]).
-export([name_to_json/1, name_from_json/1]).
-export([unit_to_json/1, unit_from_json/1]).
-export([validate_name/2]).

%% @formatter:off
-type name_selector() :: fixed | pattern.
-type name() :: automation:name().
-type unit() :: none
              | milliseconds | seconds
              | bits | bytes
              | hertz | counts_per_sec | bytes_per_sec | ops_per_sec | requests_per_sec
              | reads_per_sec | writes_per_sec | io_ops_per_sec
              | percent | percent_normalized
              | boolean
              | {custom, automation:name()}.
-export_type([name_selector/0, name/0, unit/0]).
%% @formatter:on

%%%===================================================================
%%% API
%%%===================================================================

-spec name_selector_to_json(name_selector()) -> json_utils:json_term().
name_selector_to_json(fixed) -> <<"fixed">>;
name_selector_to_json(pattern) -> <<"pattern">>.


-spec name_selector_from_json(json_utils:json_term()) -> name_selector().
name_selector_from_json(<<"fixed">>) -> fixed;
name_selector_from_json(<<"pattern">>) -> pattern.


-spec name_to_json(name()) -> json_utils:json_term().
name_to_json(Binary) when is_binary(Binary) -> Binary.


-spec name_from_json(json_utils:json_term()) -> name().
name_from_json(Binary) when is_binary(Binary) -> Binary.


-spec unit_to_json(unit()) -> json_utils:json_term().
unit_to_json(none) -> <<"none">>;
unit_to_json(milliseconds) -> <<"milliseconds">>;
unit_to_json(seconds) -> <<"seconds">>;
unit_to_json(bits) -> <<"bits">>;
unit_to_json(bytes) -> <<"bytes">>;
unit_to_json(hertz) -> <<"hertz">>;
unit_to_json(counts_per_sec) -> <<"countsPerSec">>;
unit_to_json(bytes_per_sec) -> <<"bytesPerSec">>;
unit_to_json(ops_per_sec) -> <<"opsPerSec">>;
unit_to_json(requests_per_sec) -> <<"requestsPerSec">>;
unit_to_json(reads_per_sec) -> <<"readsPerSec">>;
unit_to_json(writes_per_sec) -> <<"writesPerSec">>;
unit_to_json(io_ops_per_sec) -> <<"ioOpsPerSec">>;
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
unit_from_json(<<"bytesPerSec">>) -> bytes_per_sec;
unit_from_json(<<"opsPerSec">>) -> ops_per_sec;
unit_from_json(<<"requestsPerSec">>) -> requests_per_sec;
unit_from_json(<<"readsPerSec">>) -> reads_per_sec;
unit_from_json(<<"writesPerSec">>) -> writes_per_sec;
unit_from_json(<<"ioOpsPerSec">>) -> io_ops_per_sec;
unit_from_json(<<"percent">>) -> percent;
unit_from_json(<<"percentNormalized">>) -> percent_normalized;
unit_from_json(<<"boolean">>) -> boolean;
unit_from_json(<<"custom:", UnitName/binary>>) -> {custom, UnitName}.


-spec validate_name(name_selector(), automation:name()) -> ok | no_return().
validate_name(fixed, Binary) when is_binary(Binary) ->
    ok;
validate_name(pattern, Pattern) ->
    case string:split(Pattern, <<"*">>, all) of
        [_, _] ->
            ok;
        _ ->
            throw(?ERROR_BAD_DATA(<<"name">>, <<"The name pattern must contain exacly one wildcard character (*)">>))
    end.

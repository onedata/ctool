%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the time series measurement data spec used in
%%% automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_measurement_data_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_time_series_measurement_data_spec{}.
-export_type([record/0]).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================


-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(validate, RecordJson, fun jsonable_record:from_json/2).


%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================


-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, NestedRecordEncoder) ->
    encode_with(Record, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, NestedRecordDecoder) ->
    decode_with(skip_validation, RecordJson, NestedRecordDecoder).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_map().
encode_with(#atm_time_series_measurement_data_spec{specs = Specs}, NestedRecordEncoder) ->
    #{<<"specs">> => [NestedRecordEncoder(S, atm_time_series_measurement_spec) || S <- Specs]}.


%% @private
-spec decode_with(jsonable_record:validation_strategy(), json_utils:json_map(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, #{<<"specs">> := SpecsJson}, NestedRecordDecoder) ->
    #atm_time_series_measurement_data_spec{
        specs = [NestedRecordDecoder(S, atm_time_series_measurement_spec) || S <- SpecsJson]
    };

decode_with(validate, RecordJson, NestedRecordDecoder) ->
    Record = decode_with(skip_validation, RecordJson, NestedRecordDecoder),
    lists:foldl(fun(#atm_time_series_measurement_spec{name_matcher = NameMatcher}, AlreadyUsedNameMatchers) ->
        ordsets:is_element(NameMatcher, AlreadyUsedNameMatchers) andalso throw(
            ?ERROR_BAD_DATA(
                <<"specs">>,
                <<"There cannot be two measurement specs with the same name matcher">>
            )
        ),
        ordsets:add_element(NameMatcher, AlreadyUsedNameMatchers)
    end, ordsets:new(), Record#atm_time_series_measurement_data_spec.specs),
    Record.

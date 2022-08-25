%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series collection schema, which specifies what time
%%% series can appear in the collection and how to display them using a dashboard.
%%% @end
%%%-------------------------------------------------------------------
-module(time_series_collection_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/common.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #time_series_collection_schema{}.
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
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"timeSeriesSchemas">> => [NestedRecordEncoder(S, time_series_schema) || S <- Record#time_series_collection_schema.time_series_schemas]
    }.


%% @private
-spec decode_with(jsonable_record:validation_strategy(), json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    #time_series_collection_schema{
        time_series_schemas = [NestedRecordDecoder(S, time_series_schema) || S <- maps:get(<<"timeSeriesSchemas">>, RecordJson)]
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    Config = decode_with(skip_validation, RecordJson, NestedRecordDecoder),

    Config#time_series_collection_schema.time_series_schemas == [] andalso throw(?ERROR_BAD_VALUE_EMPTY(<<"timeSeriesSchemas">>)),

    lists:foldl(fun(TimeSeriesSchema, AlreadyCheckedSchemas) ->
        lists:foreach(fun(AlreadyCheckedSchema) ->
            are_name_generators_conflicting(TimeSeriesSchema, AlreadyCheckedSchema) andalso throw(
                ?ERROR_BAD_DATA(<<"timeSeriesSchemas">>, <<
                    "Provided time series schemas have conflicting name generators; the generators "
                    "cannot have the same values and no 'add_prefix' generator can be a prefix "
                    "of any other generator."
                >>)
            )
        end, AlreadyCheckedSchemas),
        [TimeSeriesSchema | AlreadyCheckedSchemas]
    end, [], Config#time_series_collection_schema.time_series_schemas),
    Config.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate determining if two time series schemas have conflicting name generators,
%% i.e. such that can possibly yield the same resulting time series name. A conflict
%% is possible when two generators have exactly the same value or an `add_prefix`
%% generator is a prefix of another generator - so that it could cause a conflict
%% after concatenating a specific suffix to it.
%% @end
%%--------------------------------------------------------------------
-spec are_name_generators_conflicting(time_series_schema:record(), time_series_schema:record()) ->
    boolean().
are_name_generators_conflicting(
    #time_series_schema{name_generator_type = exact, name_generator = NameGenA},
    #time_series_schema{name_generator_type = exact, name_generator = NameGenB}
) ->
    NameGenA =:= NameGenB;
are_name_generators_conflicting(
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenA},
    #time_series_schema{name_generator_type = exact, name_generator = NameGenB}
) ->
    str_utils:binary_starts_with(NameGenB, NameGenA);
are_name_generators_conflicting(
    #time_series_schema{name_generator_type = exact, name_generator = NameGenA},
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenB}
) ->
    str_utils:binary_starts_with(NameGenA, NameGenB);
are_name_generators_conflicting(
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenA},
    #time_series_schema{name_generator_type = add_prefix, name_generator = NameGenB}
) ->
    str_utils:binary_starts_with(NameGenA, NameGenB) orelse str_utils:binary_starts_with(NameGenB, NameGenA).

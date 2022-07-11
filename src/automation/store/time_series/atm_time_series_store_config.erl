%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series store config used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_store_config).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_time_series_store_config{}.
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
        <<"schemas">> => [NestedRecordEncoder(S, atm_time_series_schema) || S <- Record#atm_time_series_store_config.schemas],
        <<"dashboardSpec">> => case Record#atm_time_series_store_config.dashboard_spec of
            undefined -> null;
            DashboardSpec -> NestedRecordEncoder(DashboardSpec, ts_dashboard_spec)
        end
    }.


%% @private
-spec decode_with(automation:validation_strategy(), json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    #atm_time_series_store_config{
        schemas = [NestedRecordDecoder(S, atm_time_series_schema) || S <- maps:get(<<"schemas">>, RecordJson)],
        dashboard_spec = case maps:get(<<"dashboardSpec">>, RecordJson, null) of
            null -> undefined;
            DashboardSpec -> NestedRecordDecoder(DashboardSpec, ts_dashboard_spec)
        end
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    Config = decode_with(skip_validation, RecordJson, NestedRecordDecoder),

    Config#atm_time_series_store_config.schemas == [] andalso throw(?ERROR_BAD_VALUE_EMPTY(<<"schemas">>)),

    lists:foldl(fun(TimeSeriesSchema, AlreadyCheckedSchemas) ->
        lists:foreach(fun(AlreadyCheckedSchema) ->
            are_name_generators_conflicting(TimeSeriesSchema, AlreadyCheckedSchema) andalso throw(
                ?ERROR_BAD_DATA(<<"schemas">>, <<
                    "Provided time series schemas have conflicting name generators; the generators "
                    "cannot have the same values and no 'add_prefix' generator can be a prefix "
                    "of any other generator."
                >>)
            )
        end, AlreadyCheckedSchemas),
        [TimeSeriesSchema | AlreadyCheckedSchemas]
    end, [], Config#atm_time_series_store_config.schemas),
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
-spec are_name_generators_conflicting(atm_time_series_schema:record(), atm_time_series_schema:record()) ->
    boolean().
are_name_generators_conflicting(
    #atm_time_series_schema{name_generator_type = exact, name_generator = NameGenA},
    #atm_time_series_schema{name_generator_type = exact, name_generator = NameGenB}
) ->
    NameGenA =:= NameGenB;
are_name_generators_conflicting(
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenA},
    #atm_time_series_schema{name_generator_type = exact, name_generator = NameGenB}
) ->
    str_utils:binary_starts_with(NameGenB, NameGenA);
are_name_generators_conflicting(
    #atm_time_series_schema{name_generator_type = exact, name_generator = NameGenA},
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenB}
) ->
    str_utils:binary_starts_with(NameGenA, NameGenB);
are_name_generators_conflicting(
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenA},
    #atm_time_series_schema{name_generator_type = add_prefix, name_generator = NameGenB}
) ->
    str_utils:binary_starts_with(NameGenA, NameGenB) orelse str_utils:binary_starts_with(NameGenB, NameGenA).

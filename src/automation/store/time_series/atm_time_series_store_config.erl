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
    decode_with(RecordJson, fun jsonable_record:from_json/2).

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
    decode_with(RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"timeSeriesCollectionSchema">> => NestedRecordEncoder(
            Record#atm_time_series_store_config.time_series_collection_schema, time_series_collection_schema
        ),
        <<"dashboardSpec">> => case Record#atm_time_series_store_config.dashboard_spec of
            undefined -> null;
            DashboardSpec -> NestedRecordEncoder(DashboardSpec, ts_dashboard_spec)
        end
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedRecordDecoder) ->
    #atm_time_series_store_config{
        time_series_collection_schema = NestedRecordDecoder(
            maps:get(<<"timeSeriesCollectionSchema">>, RecordJson), time_series_collection_schema
        ),
        dashboard_spec = case maps:get(<<"dashboardSpec">>, RecordJson, null) of
            null -> undefined;
            DashboardSpec -> NestedRecordDecoder(DashboardSpec, ts_dashboard_spec)
        end
    }.

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a specification of a section of a time series dashboard.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_dashboard_section_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type chart_navigation() :: independent | shared_within_section.
-export_type([chart_navigation/0]).

-type record() :: #ts_dashboard_section_spec{}.
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
encode_with(Record, NestedEncoder) ->
    #{
        <<"title">> => case Record#ts_dashboard_section_spec.title of
            undefined -> null;
            Title -> NestedEncoder(Title, ts_title)
        end,
        <<"description">> => Record#ts_dashboard_section_spec.description,
        <<"isExpandedByDefault">> => Record#ts_dashboard_section_spec.is_expanded_by_default,
        <<"chartNavigation">> => chart_navigation_to_json(Record#ts_dashboard_section_spec.chart_navigation),
        <<"charts">> => [NestedEncoder(R, ts_chart_spec) || R <- Record#ts_dashboard_section_spec.charts],
        <<"sections">> => [NestedEncoder(R, ?MODULE) || R <- Record#ts_dashboard_section_spec.sections]
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_dashboard_section_spec{
        title = case maps:get(<<"title">>, RecordJson, null) of
            null -> undefined;
            Title -> NestedDecoder(Title, ts_title)
        end,
        description = maps:get(<<"description">>, RecordJson, <<"">>),
        is_expanded_by_default = maps:get(<<"isExpandedByDefault">>, RecordJson, false),
        chart_navigation = chart_navigation_from_json(maps:get(<<"chartNavigation">>, RecordJson, <<"independent">>)),
        charts = [NestedDecoder(R, ts_chart_spec) || R <- maps:get(<<"charts">>, RecordJson, [])],
        sections = [NestedDecoder(R, ?MODULE) || R <- maps:get(<<"sections">>, RecordJson, [])]
    }.


%% @private
-spec chart_navigation_to_json(chart_navigation()) -> json_utils:json_term().
chart_navigation_to_json(independent) -> <<"independent">>;
chart_navigation_to_json(shared_within_section) -> <<"sharedWithinSection">>.


%% @private
-spec chart_navigation_from_json(json_utils:json_term()) -> chart_navigation().
chart_navigation_from_json(<<"independent">>) -> independent;
chart_navigation_from_json(<<"sharedWithinSection">>) -> shared_within_section.
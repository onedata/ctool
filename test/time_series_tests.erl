%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of time series related modules.
%%% @end
%%%-------------------------------------------------------------------
-module(time_series_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("time_series/common.hrl").
-include("time_series/dashboard.hrl").
-include("errors.hrl").
-include("logging.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================


encode_decode_time_series_collection_schema_test() ->
    encode_decode_test_base(time_series_test_utils:example_time_series_collection_schema()),

    % time series collection schema must have at least one time series schema defined
    ?assert(eunit_utils:throws_error_during_decode_from_json(
        ?ERROR_BAD_VALUE_EMPTY(<<"timeSeriesSchemas">>),
        #time_series_collection_schema{
            time_series_schemas = []
        }
    )),

    % time series collection schema cannot have conflicting name generators
    MakeSchema = fun(NameGeneratorType, NameGenerator) ->
        ExampleTimeSeriesSchema = time_series_test_utils:example_time_series_schema(),
        ExampleTimeSeriesSchema#time_series_schema{
            name_generator_type = NameGeneratorType,
            name_generator = NameGenerator
        }
    end,
    SchemasTestCases = [
        {conflict, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"count_">>),
            MakeSchema(exact, <<"throughput">>)
        ]},
        {correct, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"files_">>),
            MakeSchema(exact, <<"count_mp3">>)
        ]},
        {conflict, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(exact, <<"count_mp3">>),
            MakeSchema(add_prefix, <<"count_">>)
        ]},
        {correct, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(exact, <<"count/mp3">>),
            MakeSchema(add_prefix, <<"count_">>)
        ]},
        {conflict, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"files_">>),
            MakeSchema(add_prefix, <<"files_count_">>)
        ]},
        {correct, [
            MakeSchema(exact, <<"throughput">>),
            MakeSchema(add_prefix, <<"files_">>),
            MakeSchema(add_prefix, <<"file_count_">>)
        ]}
    ],
    ExpTimeSeriesCfgError = ?ERROR_BAD_DATA(<<"timeSeriesSchemas">>, <<
        "Provided time series schemas have conflicting name generators; the generators "
        "cannot have the same values and no 'add_prefix' generator can be a prefix "
        "of any other generator."
    >>),
    lists:foreach(fun
        ({conflict, ConflictingSchemaSet}) ->
            ?assert(eunit_utils:throws_error_during_decode_from_json(ExpTimeSeriesCfgError, #time_series_collection_schema{
                time_series_schemas = lists_utils:shuffle(ConflictingSchemaSet)
            }));
        ({correct, CorrectSchemaSet}) ->
            encode_decode_test_base(#time_series_collection_schema{
                time_series_schemas = lists_utils:shuffle(CorrectSchemaSet)
            })
    end, SchemasTestCases).


encode_decode_time_series_schema_test() ->
    [Example | _] = ExampleTimeSeriesSpecs = time_series_test_utils:example_time_series_schemas(),
    encode_decode_test_base(ExampleTimeSeriesSpecs),

    ?assert(eunit_utils:throws_error_during_decode_from_json(
        ?ERROR_BAD_VALUE_EMPTY(<<"metrics">>),
        Example#time_series_schema{metrics = #{}}
    )),

    ?assert(eunit_utils:throws_error_during_decode_from_json(
        ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same resolution and aggregator">>),
        Example#time_series_schema{metrics = #{
            <<"metric1">> => #metric_config{resolution = 3600, retention = 12, aggregator = sum},
            <<"metric2">> => #metric_config{resolution = 3600, retention = 13, aggregator = sum}
        }}
    )).


encode_decode_time_series_metric_config_test() ->
    [Example | _] = ExampleTimeSeriesMetricSpecs = time_series_test_utils:example_metric_configs(),
    encode_decode_test_base(ExampleTimeSeriesMetricSpecs),

    ?assert(eunit_utils:throws_error_during_decode_from_json(
        ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resolution">>, ?ALLOWED_METRIC_RESOLUTIONS),
        Example#metric_config{resolution = 1337}
    )).


encode_decode_ts_dashboard_spec_test() ->
    encode_decode_test_base(time_series_test_utils:example_dashboard_specs()).


encode_decode_ts_dashboard_section_spec_test() ->
    encode_decode_test_base(time_series_test_utils:example_dashboard_section_specs()).


encode_decode_ts_title_test() ->
    encode_decode_test_base(time_series_test_utils:example_titles()).


encode_decode_ts_chart_spec_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_specs()).


encode_decode_ts_chart_y_axis_spec_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_y_axis_specs()).


encode_decode_ts_chart_series_group_builder_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_series_group_builders()).


encode_decode_ts_chart_static_series_group_builder_recipe_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_static_series_group_builder_recipes()).


encode_decode_ts_chart_dynamic_series_group_builder_recipe_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_dynamic_series_group_builder_recipes()).


encode_decode_ts_chart_static_series_group_template_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_static_series_group_templates()).


encode_decode_ts_chart_dynamic_series_group_configs_source_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_dynamic_series_group_configs_sources()).


encode_decode_ts_chart_external_series_group_configs_source_spec_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_external_series_group_configs_source_specs()).


encode_decode_ts_chart_dynamic_series_group_template_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_dynamic_series_group_templates()).


encode_decode_ts_chart_series_builder_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_series_builders()).


encode_decode_ts_chart_static_series_builder_recipe_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_static_series_builder_recipes()).


encode_decode_ts_chart_dynamic_series_builder_recipe_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_dynamic_series_builder_recipes()).


encode_decode_ts_chart_static_series_template_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_static_series_templates()).


encode_decode_ts_chart_dynamic_series_configs_source_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_dynamic_series_configs_sources()).


encode_decode_ts_chart_external_series_configs_source_spec_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_external_series_configs_source_specs()).


encode_decode_ts_chart_external_series_configs_source_parameters_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_external_series_configs_source_parameters_records()).


encode_decode_ts_chart_dynamic_series_template_test() ->
    encode_decode_test_base(time_series_test_utils:example_chart_dynamic_series_templates()).


encode_decode_ts_provider_function_test() ->
    encode_decode_test_base(time_series_test_utils:example_provider_functions()).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
encode_decode_test_base(Record) when not is_list(Record) ->
    encode_decode_test_base([Record]);
encode_decode_test_base(Records) ->
    lists:foreach(fun(Record) ->
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(Record)),
        ?assert(eunit_utils:is_equal_after_db_encode_and_decode(Record))
    end, Records).


-endif.

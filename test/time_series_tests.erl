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
-include("time_series/dashboard.hrl").
-include("errors.hrl").
-include("logging.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================

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

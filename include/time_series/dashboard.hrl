%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions related to dashboards in time series.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(TIME_SERIES_DASHBOARD_HRL).
-define(TIME_SERIES_DASHBOARD_HRL, 1).


-record(ts_dashboard_spec, {
    root_section :: ts_dashboard_section_spec:record()
}).

-record(ts_dashboard_section_spec, {
    title :: undefined | ts_title:record(),
    description :: binary(),
    is_expanded_by_default :: boolean(),
    chart_navigation :: ts_dashboard_section_spec:chart_navigation(),
    charts :: [ts_chart_spec:record()],
    sections :: [ts_dashboard_section_spec:record()]
}).

-record(ts_title, {
    content :: binary(),
    tip :: binary()
}).

-record(ts_chart_spec, {
    title :: undefined | ts_title:record(),
    y_axes :: [ts_chart_y_axis_spec:record()],
    series_group_builders :: [ts_chart_series_group_builder:record()],
    series_builders :: [ts_chart_series_builder:record()]
}).

-record(ts_chart_y_axis_spec, {
    id :: binary(),
    name :: binary(),
    min_interval :: float(),
    unit_name :: binary(),
    unit_options :: undefined | ts_chart_y_axis_spec:unit_options(),
    value_provider :: ts_provider_function:record()
}).

-record(ts_chart_series_group_builder, {
    type :: ts_chart_series_group_builder:type(),
    recipe :: ts_chart_static_series_group_builder_recipe:record() | ts_chart_dynamic_series_group_builder_recipe:record()
}).

-record(ts_chart_static_series_group_builder_recipe, {
    series_group_template :: ts_chart_static_series_group_template:record()
}).

-record(ts_chart_dynamic_series_group_builder_recipe, {
    dynamic_series_group_configs_source :: ts_chart_dynamic_series_group_configs_source:record(),
    series_group_template :: ts_chart_dynamic_series_group_template:record()
}).

-record(ts_chart_static_series_group_template, {
    id :: binary(),
    name :: binary(),
    stacked :: boolean(),
    show_sum :: boolean(),
    subgroups :: [ts_chart_static_series_group_template:record()]
}).

-record(ts_chart_dynamic_series_group_configs_source, {
    source_type :: time_series:dynamic_source_type(),
    source_spec :: ts_chart_external_series_group_configs_source_spec:record()
}).

-record(ts_chart_external_series_group_configs_source_spec, {
    name :: binary()
}).

-record(ts_chart_dynamic_series_group_template, {
    id_provider :: ts_provider_function:record(),
    name_provider :: undefined | ts_provider_function:record(),
    stacked_provider :: undefined | ts_provider_function:record(),
    show_sum_provider :: undefined | ts_provider_function:record(),
    subgroups_provider :: undefined | ts_provider_function:record()
}).

-record(ts_chart_series_builder, {
    type :: ts_chart_series_builder:type(),
    recipe :: ts_chart_static_series_builder_recipe:record() | ts_chart_dynamic_series_builder_recipe:record()
}).

-record(ts_chart_static_series_builder_recipe, {
    series_template :: ts_chart_static_series_template:record()
}).

-record(ts_chart_dynamic_series_builder_recipe, {
    dynamic_series_configs_source :: ts_chart_dynamic_series_configs_source:record(),
    series_template :: ts_chart_dynamic_series_template:record()
}).

-record(ts_chart_static_series_template, {
    id :: binary(),
    name :: binary(),
    color :: undefined | binary(),
    type :: line | bar,
    y_axis_id :: binary(),
    group_id :: undefined | binary(),
    data_provider :: ts_provider_function:record()
}).

-record(ts_chart_dynamic_series_configs_source, {
    source_type :: time_series:dynamic_source_type(),
    source_spec :: ts_chart_external_series_configs_source_spec:record()
}).

-record(ts_chart_external_series_configs_source_spec, {
    name :: binary(),
    parameters :: ts_chart_external_series_configs_source_parameters:record()
}).

-record(ts_chart_external_series_configs_source_parameters, {
    collection_ref :: ts_chart_external_series_configs_source_parameters:collection_ref(),
    time_series_name_generator :: time_series_schema:name_generator(),
    metric_names :: [time_series:metric_name()]
}).

-record(ts_chart_dynamic_series_template, {
    id_provider :: ts_provider_function:record(),
    name_provider :: ts_provider_function:record(),
    color_provider :: undefined | ts_provider_function:record(),
    type_provider :: ts_provider_function:record(),
    y_axis_id_provider :: ts_provider_function:record(),
    group_id_provider :: undefined | ts_provider_function:record(),
    data_provider :: ts_provider_function:record()
}).


-record(ts_data_generator_current_value, {
}).

-record(ts_data_generator_get_dynamic_series_config, {
    property_name :: binary()
}).

-record(ts_data_generator_get_dynamic_series_group_config, {
    property_name :: binary()
}).

-record(ts_data_generator_literal, {
    data :: json_utils:json_term()
}).

-record(ts_data_generator_load_series, {
    source_type :: time_series:dynamic_source_type(),
    source_spec_provider :: ts_provider_function:record(),
    replace_empty_parameters_provider :: undefined | ts_provider_function:record()
}).


-record(ts_transformer_abs, {
    input_data_provider :: ts_provider_function:record()
}).

-record(ts_transformer_multiply, {
    operand_providers :: [ts_provider_function:record()]
}).

-record(ts_transformer_replace_empty, {
    input_data_provider :: ts_provider_function:record(),
    fallback_value_provider :: ts_provider_function:record(),
    strategy_provider :: undefined | ts_provider_function:record()
}).


-endif.

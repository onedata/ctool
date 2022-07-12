%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper function for tests of time series related modules.
%%% @end
%%%-------------------------------------------------------------------
-module(time_series_test_utils).
-author("Lukasz Opiola").

-include("time_series/dashboard.hrl").
-include("test/test_utils.hrl").

-export([example_dashboard_spec/0, example_dashboard_specs/0]).
-export([example_dashboard_section_spec/0, example_dashboard_section_specs/0]).
-export([example_title/0, example_titles/0]).
-export([example_chart_spec/0, example_chart_specs/0]).
-export([example_chart_y_axis_spec/0, example_chart_y_axis_specs/0]).
-export([example_chart_series_group_builder/0, example_chart_series_group_builders/0]).
-export([example_chart_static_series_group_builder_recipe/0, example_chart_static_series_group_builder_recipes/0]).
-export([example_chart_dynamic_series_group_builder_recipe/0, example_chart_dynamic_series_group_builder_recipes/0]).
-export([example_chart_static_series_group_template/0, example_chart_static_series_group_templates/0]).
-export([example_chart_dynamic_series_group_configs_source/0, example_chart_dynamic_series_group_configs_sources/0]).
-export([example_chart_external_series_group_configs_source_spec/0, example_chart_external_series_group_configs_source_specs/0]).
-export([example_chart_dynamic_series_group_template/0, example_chart_dynamic_series_group_templates/0]).
-export([example_chart_series_builder/0, example_chart_series_builders/0]).
-export([example_chart_static_series_builder_recipe/0, example_chart_static_series_builder_recipes/0]).
-export([example_chart_dynamic_series_builder_recipe/0, example_chart_dynamic_series_builder_recipes/0]).
-export([example_chart_static_series_template/0, example_chart_static_series_templates/0]).
-export([example_chart_dynamic_series_configs_source/0, example_chart_dynamic_series_configs_sources/0]).
-export([example_chart_external_series_configs_source_spec/0, example_chart_external_series_configs_source_specs/0]).
-export([example_chart_external_series_configs_source_parameters/0, example_chart_external_series_configs_source_parameters_records/0]).
-export([example_chart_dynamic_series_template/0, example_chart_dynamic_series_templates/0]).
-export([example_provider_function/0, example_provider_functions/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec example_dashboard_spec() -> ts_dashboard_spec:record().
example_dashboard_spec() ->
    #ts_dashboard_spec{
        root_section = example_dashboard_section_spec()
    }.

-spec example_dashboard_specs() -> [ts_dashboard_spec:record()].
example_dashboard_specs() ->
    lists_utils:generate(fun example_dashboard_spec/0, 2).


-spec example_dashboard_section_spec() -> ts_dashboard_section_spec:record().
example_dashboard_section_spec() ->
    % maximum nesting is chosen empirically and used to avoid to large records and too long testing time
    example_dashboard_section_spec(2).

-spec example_dashboard_section_spec(pos_integer()) -> ts_dashboard_section_spec:record().
example_dashboard_section_spec(MaxNestingLevel) ->
    #ts_dashboard_section_spec{
        title = ?RAND_ELEMENT([undefined, example_title()]),
        description = ?RAND_STR(),
        is_expanded_by_default = ?RAND_BOOL(),
        chart_navigation = ?RAND_ELEMENT([independent, shared_within_section]),
        charts = ?RAND_SUBLIST(example_chart_specs()),
        sections = case MaxNestingLevel =< 0 of
            true ->
                [];
            false ->
                lists_utils:generate(fun() ->
                    example_dashboard_section_spec(MaxNestingLevel - 1)
                end, ?RAND_INT(0, 2))
        end
    }.

-spec example_dashboard_section_specs() -> [ts_dashboard_section_spec:record()].
example_dashboard_section_specs() ->
    lists_utils:generate(fun example_dashboard_section_spec/0, 2).


-spec example_title() -> ts_title:record().
example_title() ->
    #ts_title{
        content = ?RAND_STR(),
        tip = ?RAND_STR()
    }.

-spec example_titles() -> [ts_title:record()].
example_titles() ->
    lists_utils:generate(fun example_title/0, 3).


-spec example_chart_spec() -> ts_chart_spec:record().
example_chart_spec() ->
    #ts_chart_spec{
        title = ?RAND_ELEMENT([undefined, example_title()]),
        y_axes = ?RAND_SUBLIST(example_chart_y_axis_specs()),
        series_group_builders = ?RAND_SUBLIST(example_chart_series_group_builders()),
        series_builders = ?RAND_SUBLIST(example_chart_series_builders())
    }.

-spec example_chart_specs() -> [ts_chart_spec:record()].
example_chart_specs() ->
    lists_utils:generate(fun example_chart_spec/0, 2).


-spec example_chart_y_axis_spec() -> ts_chart_y_axis_spec:record().
example_chart_y_axis_spec() ->
    #ts_chart_y_axis_spec{
        id = ?RAND_STR(),
        name = ?RAND_STR(),
        min_interval = ?RAND_FLOAT(0.1, 100.0),
        unit_name = ?RAND_STR(),
        unit_options = ?RAND_ELEMENT([
            undefined,
            ?RAND_SUBMAP(#{
                custom_name => ?RAND_STR(),
                use_metric_suffix => ?RAND_BOOL(),
                format => ?RAND_ELEMENT([si, iec])
            })
        ]),
        value_provider = example_provider_function()
    }.

-spec example_chart_y_axis_specs() -> [ts_chart_y_axis_spec:record()].
example_chart_y_axis_specs() ->
    lists_utils:generate(fun example_chart_y_axis_spec/0, 2).


-spec example_chart_series_group_builder() -> ts_chart_series_group_builder:record().
example_chart_series_group_builder() ->
    Type = ?RAND_ELEMENT([static, dynamic]),
    #ts_chart_series_group_builder{
        type = Type,
        recipe = case Type of
            static -> example_chart_static_series_group_builder_recipe();
            dynamic -> example_chart_dynamic_series_group_builder_recipe()
        end
    }.

-spec example_chart_series_group_builders() -> [ts_chart_series_group_builder:record()].
example_chart_series_group_builders() ->
    lists_utils:generate(fun example_chart_series_group_builder/0, 2).


-spec example_chart_static_series_group_builder_recipe() -> ts_chart_static_series_group_builder_recipe:record().
example_chart_static_series_group_builder_recipe() ->
    #ts_chart_static_series_group_builder_recipe{
        series_group_template = example_chart_static_series_group_template()
    }.

-spec example_chart_static_series_group_builder_recipes() -> [ts_chart_static_series_group_builder_recipe:record()].
example_chart_static_series_group_builder_recipes() ->
    lists_utils:generate(fun example_chart_static_series_group_builder_recipe/0, 2).


-spec example_chart_dynamic_series_group_builder_recipe() -> ts_chart_dynamic_series_group_builder_recipe:record().
example_chart_dynamic_series_group_builder_recipe() ->
    #ts_chart_dynamic_series_group_builder_recipe{
        dynamic_series_group_configs_source = example_chart_dynamic_series_group_configs_source(),
        series_group_template = example_chart_dynamic_series_group_template()
    }.

-spec example_chart_dynamic_series_group_builder_recipes() -> [ts_chart_dynamic_series_group_builder_recipe:record()].
example_chart_dynamic_series_group_builder_recipes() ->
    lists_utils:generate(fun example_chart_dynamic_series_group_builder_recipe/0, 2).


-spec example_chart_static_series_group_template() -> ts_chart_static_series_group_template:record().
example_chart_static_series_group_template() ->
    % maximum nesting is chosen empirically and used to avoid to large records and too long testing time
    example_chart_static_series_group_template(2).

-spec example_chart_static_series_group_template(pos_integer()) -> ts_chart_static_series_group_template:record().
example_chart_static_series_group_template(MaxNestingLevel) ->
    #ts_chart_static_series_group_template{
        id = ?RAND_STR(),
        name = ?RAND_STR(),
        stacked = ?RAND_BOOL(),
        show_sum = ?RAND_BOOL(),
        subgroups = case MaxNestingLevel =< 0 of
            true ->
                [];
            false ->
                lists_utils:generate(fun() ->
                    example_chart_static_series_group_template(MaxNestingLevel - 1)
                end, ?RAND_INT(0, 2))
        end
    }.

-spec example_chart_static_series_group_templates() -> [ts_chart_static_series_group_template:record()].
example_chart_static_series_group_templates() ->
    lists_utils:generate(fun example_chart_static_series_group_template/0, 2).


-spec example_chart_dynamic_series_group_configs_source() -> ts_chart_dynamic_series_group_configs_source:record().
example_chart_dynamic_series_group_configs_source() ->
    #ts_chart_dynamic_series_group_configs_source{
        source_type = external,
        source_spec = example_chart_external_series_group_configs_source_spec()
    }.

-spec example_chart_dynamic_series_group_configs_sources() -> [ts_chart_dynamic_series_group_configs_source:record()].
example_chart_dynamic_series_group_configs_sources() ->
    lists_utils:generate(fun example_chart_dynamic_series_group_configs_source/0, 2).


-spec example_chart_external_series_group_configs_source_spec() -> ts_chart_external_series_group_configs_source_spec:record().
example_chart_external_series_group_configs_source_spec() ->
    #ts_chart_external_series_group_configs_source_spec{
        name = ?RAND_STR()
    }.

-spec example_chart_external_series_group_configs_source_specs() -> [ts_chart_external_series_group_configs_source_spec:record()].
example_chart_external_series_group_configs_source_specs() ->
    lists_utils:generate(fun example_chart_external_series_group_configs_source_spec/0, 2).


-spec example_chart_dynamic_series_group_template() -> ts_chart_dynamic_series_group_template:record().
example_chart_dynamic_series_group_template() ->
    #ts_chart_dynamic_series_group_template{
        id_provider = example_provider_function(),
        name_provider = ?RAND_ELEMENT([undefined, example_provider_function()]),
        stacked_provider = ?RAND_ELEMENT([undefined, example_provider_function()]),
        show_sum_provider = ?RAND_ELEMENT([undefined, example_provider_function()]),
        subgroups_provider = ?RAND_ELEMENT([undefined, example_provider_function()])
    }.

-spec example_chart_dynamic_series_group_templates() -> [ts_chart_dynamic_series_group_template:record()].
example_chart_dynamic_series_group_templates() ->
    lists_utils:generate(fun example_chart_dynamic_series_group_template/0, 2).


-spec example_chart_series_builder() -> ts_chart_series_builder:record().
example_chart_series_builder() ->
    Type = ?RAND_ELEMENT([static, dynamic]),
    #ts_chart_series_builder{
        type = Type,
        recipe = case Type of
            static -> example_chart_static_series_builder_recipe();
            dynamic -> example_chart_dynamic_series_builder_recipe()
        end
    }.

-spec example_chart_series_builders() -> [ts_chart_series_builder:record()].
example_chart_series_builders() ->
    lists_utils:generate(fun example_chart_series_builder/0, 2).


-spec example_chart_static_series_builder_recipe() -> ts_chart_static_series_builder_recipe:record().
example_chart_static_series_builder_recipe() ->
    #ts_chart_static_series_builder_recipe{
        series_template = example_chart_static_series_template()
    }.

-spec example_chart_static_series_builder_recipes() -> [ts_chart_static_series_builder_recipe:record()].
example_chart_static_series_builder_recipes() ->
    lists_utils:generate(fun example_chart_static_series_builder_recipe/0, 2).


-spec example_chart_dynamic_series_builder_recipe() -> ts_chart_dynamic_series_builder_recipe:record().
example_chart_dynamic_series_builder_recipe() ->
    #ts_chart_dynamic_series_builder_recipe{
        dynamic_series_configs_source = example_chart_dynamic_series_configs_source(),
        series_template = example_chart_dynamic_series_template()
    }.

-spec example_chart_dynamic_series_builder_recipes() -> [ts_chart_dynamic_series_builder_recipe:record()].
example_chart_dynamic_series_builder_recipes() ->
    lists_utils:generate(fun example_chart_dynamic_series_builder_recipe/0, 2).


-spec example_chart_static_series_template() -> ts_chart_static_series_template:record().
example_chart_static_series_template() ->
    #ts_chart_static_series_template{
        id = ?RAND_STR(),
        name = ?RAND_STR(),
        color = ?RAND_ELEMENT([undefined, ?RAND_STR()]),
        type = ?RAND_ELEMENT([line, bar]),
        y_axis_id = ?RAND_STR(),
        group_id = ?RAND_ELEMENT([undefined, ?RAND_STR()]),
        data_provider = example_provider_function()
    }.

-spec example_chart_static_series_templates() -> [ts_chart_static_series_template:record()].
example_chart_static_series_templates() ->
    lists_utils:generate(fun example_chart_static_series_template/0, 3).


-spec example_chart_dynamic_series_configs_source() -> ts_chart_dynamic_series_configs_source:record().
example_chart_dynamic_series_configs_source() ->
    #ts_chart_dynamic_series_configs_source{
        source_type = external,
        source_spec = example_chart_external_series_configs_source_spec()
    }.

-spec example_chart_dynamic_series_configs_sources() -> [ts_chart_dynamic_series_configs_source:record()].
example_chart_dynamic_series_configs_sources() ->
    lists_utils:generate(fun example_chart_dynamic_series_configs_source/0, 3).


-spec example_chart_external_series_configs_source_spec() -> ts_chart_external_series_configs_source_spec:record().
example_chart_external_series_configs_source_spec() ->
    #ts_chart_external_series_configs_source_spec{
        name = ?RAND_STR(),
        parameters = example_chart_external_series_configs_source_parameters()
    }.

-spec example_chart_external_series_configs_source_specs() -> [ts_chart_external_series_configs_source_spec:record()].
example_chart_external_series_configs_source_specs() ->
    lists_utils:generate(fun example_chart_external_series_configs_source_spec/0, 3).


-spec example_chart_external_series_configs_source_parameters() ->
    ts_chart_external_series_configs_source_parameters:record().
example_chart_external_series_configs_source_parameters() ->
    #ts_chart_external_series_configs_source_parameters{
        time_series_name_generator = ?RAND_STR(),
        metric_names = ?RAND_SUBLIST([?RAND_STR(), ?RAND_STR(), ?RAND_STR(), ?RAND_STR(), ?RAND_STR(), ?RAND_STR()])
    }.

-spec example_chart_external_series_configs_source_parameters_records() ->
    [ts_chart_external_series_configs_source_parameters:record()].
example_chart_external_series_configs_source_parameters_records() ->
    lists_utils:generate(fun example_chart_external_series_configs_source_parameters/0, 3).


-spec example_chart_dynamic_series_template() -> ts_chart_dynamic_series_template:record().
example_chart_dynamic_series_template() ->
    #ts_chart_dynamic_series_template{
        id_provider = example_provider_function(),
        name_provider = example_provider_function(),
        color_provider = ?RAND_ELEMENT([undefined, example_provider_function()]),
        type_provider = example_provider_function(),
        y_axis_id_provider = example_provider_function(),
        group_id_provider = ?RAND_ELEMENT([undefined, example_provider_function()]),
        data_provider = example_provider_function()
    }.

-spec example_chart_dynamic_series_templates() -> [ts_chart_dynamic_series_template:record()].
example_chart_dynamic_series_templates() ->
    lists_utils:generate(fun example_chart_dynamic_series_template/0, 3).


-spec example_provider_function() -> ts_provider_function:record().
example_provider_function() ->
    example_provider_function(3).

-spec example_provider_function(pos_integer()) -> ts_provider_function:record().
example_provider_function(MaxNestingLevel) when MaxNestingLevel =< 0 ->
    % controlled nesting level makes sure that the example generator does
    % not go into an infinite loop (function providers can be arbitrarily nested)
    #ts_data_generator_literal{data = ?RAND_JSON_TERM()};
example_provider_function(MaxNestingLevel) ->
    case rand:uniform(8) of
        1 ->
            #ts_data_generator_literal{data = ?RAND_JSON_TERM()};
        2 ->
            #ts_data_generator_current_value{};
        3 ->
            #ts_data_generator_get_dynamic_series_config{property_name = ?RAND_STR()};
        4 ->
            #ts_data_generator_get_dynamic_series_group_config{property_name = ?RAND_STR()};
        5 ->
            #ts_data_generator_load_series{
                source_type = external,
                source_spec_provider = example_provider_function(MaxNestingLevel - 1),
                replace_empty_parameters_provider = ?RAND_ELEMENT([undefined, example_provider_function(MaxNestingLevel - 1)])
            };
        6 ->
            #ts_transformer_abs{input_data_provider = example_provider_function(MaxNestingLevel - 1)};
        7 ->
            #ts_transformer_multiply{
                operand_providers = lists_utils:generate(fun() ->
                    example_provider_function(MaxNestingLevel - 1)
                end, ?RAND_INT(1, 3))
            };
        8 ->
            #ts_transformer_replace_empty{
                input_data_provider = example_provider_function(MaxNestingLevel - 1),
                fallback_value_provider = example_provider_function(MaxNestingLevel - 1),
                strategy_provider = ?RAND_ELEMENT([undefined, example_provider_function(MaxNestingLevel - 1)])
            }
    end.


-spec example_provider_functions() -> [ts_provider_function:record()].
example_provider_functions() ->
    lists_utils:generate(fun example_provider_function/0, 20).

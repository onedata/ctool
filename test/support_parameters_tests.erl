%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of support_parameters module.
%%% @end
%%%-------------------------------------------------------------------
-module(support_parameters_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test/test_utils.hrl").
-include("space_support/support_parameters.hrl").
-include("errors.hrl").

-define(SP(__ACCOUNTING_ENABLED, __DIR_STATS_SERVICE_ENABLED, __DIR_STATS_SERVICE_STATUS),
    #support_parameters{
    accounting_enabled = __ACCOUNTING_ENABLED,
    dir_stats_service_enabled = __DIR_STATS_SERVICE_ENABLED,
    dir_stats_service_status = __DIR_STATS_SERVICE_STATUS
}).

% Set by default if no specific parameters are requested.
% Currently, all new space supports have dir stats service enabled by default.
-define(DEFAULT_SUPPORT_PARAMETERS, #support_parameters{
    accounting_enabled = false,
    dir_stats_service_enabled = true,
    % the default parameters are still subject to tweaking;
    % (@see support_parameters:ensure_dir_stats_service_status_adequate/1)
    % so the final status will be consistent with the dir_stats_service_enabled flag
    % (also @see support_parameters_tests.erl)
    dir_stats_service_status = disabled
}).

-define(EXP_SETTING_CONFLICT_ERROR, ?ERROR_BAD_DATA(
    <<"dirStatsServiceEnabled">>,
    <<"Dir stats service must be enabled if accounting is enabled">>
)).

%%%===================================================================
%%% Tests
%%%===================================================================

encode_decode_support_parameters_test() ->
    encode_decode_test_base(lists_utils:generate(fun example_support_parameters/0, 20)).


encode_decode_support_parameters_registry_test() ->
    encode_decode_test_base(lists_utils:generate(fun example_support_parameters_registry/0, 20)).


sanitize_support_parameters_test() ->
    lists:foreach(fun({RecordToSanitize, ExpectedResult}) ->
        ?assertEqual(ExpectedResult, support_parameters:sanitize(RecordToSanitize))
    end, [
        {?SP(undefined, undefined, undefined), {ok, ?SP(undefined, undefined, undefined)}},
        {?SP(true, undefined, undefined), {ok, ?SP(true, undefined, undefined)}},
        {?SP(undefined, true, undefined), {ok, ?SP(undefined, true, undefined)}},

        {?SP(false, false, disabled), {ok, ?SP(false, false, disabled)}},
        {?SP(false, false, initializing), {ok, ?SP(false, false, stopping)}},
        {?SP(false, false, enabled), {ok, ?SP(false, false, stopping)}},
        {?SP(false, false, stopping), {ok, ?SP(false, false, stopping)}},

        {?SP(false, true, disabled), {ok, ?SP(false, true, initializing)}},
        {?SP(false, true, initializing), {ok, ?SP(false, true, initializing)}},
        {?SP(false, true, enabled), {ok, ?SP(false, true, enabled)}},
        {?SP(false, true, stopping), {ok, ?SP(false, true, initializing)}},

        {?SP(true, false, disabled), ?EXP_SETTING_CONFLICT_ERROR},
        {?SP(true, false, initializing), ?EXP_SETTING_CONFLICT_ERROR},
        {?SP(true, false, enabled), ?EXP_SETTING_CONFLICT_ERROR},
        {?SP(true, false, stopping), ?EXP_SETTING_CONFLICT_ERROR},

        {?SP(true, true, disabled), {ok, ?SP(true, true, initializing)}},
        {?SP(true, true, initializing), {ok, ?SP(true, true, initializing)}},
        {?SP(true, true, enabled), {ok, ?SP(true, true, enabled)}},
        {?SP(true, true, stopping), {ok, ?SP(true, true, initializing)}}
    ]).


create_support_parameters_test() ->
    BuildExpErrorFun = fun(Field) ->
        ?ERROR_MISSING_REQUIRED_VALUE(<<"supportParameters.", Field/binary>>)
    end,

    DummyProviderId = ?RAND_STR(),
    ExampleRegistry = example_support_parameters_registry(),

    BuildSuccessfulTestCaseFun = fun(SupportParameters) ->
        {SupportParameters, {ok, insert_parameters_into_registry(
            DummyProviderId, SupportParameters, ExampleRegistry
        )}}
    end,

    lists:foreach(fun({Record, ExpectedResult}) ->
        ?assertEqual(
            ExpectedResult,
            support_parameters_registry:create_entry(DummyProviderId, Record, ExampleRegistry)
        )
    end, [
        {?SP(undefined, undefined, undefined), BuildExpErrorFun(<<"accountingEnabled">>)},
        {?SP(true, undefined, undefined), BuildExpErrorFun(<<"dirStatsEnabled">>)},
        {?SP(undefined, true, undefined), BuildExpErrorFun(<<"accountingEnabled">>)},
        {?SP(undefined, undefined, disabled), BuildExpErrorFun(<<"accountingEnabled">>)},
        {?SP(false, undefined, disabled), BuildExpErrorFun(<<"dirStatsEnabled">>)},
        {?SP(undefined, false, disabled), BuildExpErrorFun(<<"accountingEnabled">>)},
        {?SP(false, false, undefined), BuildExpErrorFun(<<"dirStatsStatus">>)},

        BuildSuccessfulTestCaseFun(?SP(false, false, disabled)),
        BuildSuccessfulTestCaseFun(?SP(false, true, enabled)),
        BuildSuccessfulTestCaseFun(?SP(true, true, initializing))
    ]).


update_support_parameters_test() ->
    ExampleRegistry = example_support_parameters_registry(),
    lists:foreach(fun({RecordToUpdate, OverlayRecord, ExpectedResult}) ->
        ?assertEqual(ExpectedResult, support_parameters:update(RecordToUpdate, OverlayRecord)),

        DummyProviderId = ?RAND_STR(),
        DummyRegistry = insert_parameters_into_registry(DummyProviderId, RecordToUpdate, ExampleRegistry),

        case ExpectedResult of
            {error, _} ->
                ?assertEqual(
                    ExpectedResult,
                    support_parameters_registry:update_entry(DummyProviderId, OverlayRecord, DummyRegistry)
                );
            {ok, UpdatedParameters} ->
                ?assertEqual(
                    {ok, insert_parameters_into_registry(DummyProviderId, UpdatedParameters, DummyRegistry)},
                    support_parameters_registry:update_entry(DummyProviderId, OverlayRecord, DummyRegistry)
                )
        end
    end, [
        {?SP(false, false, disabled), ?SP(true, true, enabled), {ok, ?SP(true, true, enabled)}},
        {?SP(false, true, disabled), ?SP(true, undefined, initializing), {ok, ?SP(true, true, initializing)}},
        {?SP(false, false, stopping), ?SP(undefined, undefined, disabled), {ok, ?SP(false, false, disabled)}},
        {?SP(false, true, enabled), ?SP(undefined, undefined, undefined), {ok, ?SP(false, true, enabled)}},

        % cases when the status is tweaked in advance so that it is not confusing
        {?SP(true, true, enabled), ?SP(false, undefined, stopping), {ok, ?SP(false, true, initializing)}},
        {?SP(true, false, disabled), ?SP(undefined, true, undefined), {ok, ?SP(true, true, initializing)}},
        {?SP(true, false, stopping), ?SP(true, true, undefined), {ok, ?SP(true, true, initializing)}},
        {?SP(false, true, initializing), ?SP(false, false, undefined), {ok, ?SP(false, false, stopping)}},
        {?SP(false, true, enabled), ?SP(undefined, false, undefined), {ok, ?SP(false, false, stopping)}},

        % updates starting from the default settings - currently (false, true, disabled)
        % (if the macro changes, the tests need to be adjusted)
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(undefined, undefined, undefined), {ok, ?SP(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(undefined, true, initializing), {ok, ?SP(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(undefined, true, enabled), {ok, ?SP(false, true, enabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(undefined, false, undefined), {ok, ?SP(false, false, disabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(true, true, undefined), {ok, ?SP(true, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(false, false, undefined), {ok, ?SP(false, false, disabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(false, false, disabled), {ok, ?SP(false, false, disabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(false, true, undefined), {ok, ?SP(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(false, true, initializing), {ok, ?SP(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, ?SP(false, true, enabled), {ok, ?SP(false, true, enabled)}},

        % illegal settings combinations
        {?SP(true, true, enabled), ?SP(undefined, false, undefined), ?EXP_SETTING_CONFLICT_ERROR},
        {?SP(true, true, enabled), ?SP(true, false, stopping), ?EXP_SETTING_CONFLICT_ERROR},
        {?SP(false, false, disabled), ?SP(true, false, undefined), ?EXP_SETTING_CONFLICT_ERROR},
        {?SP(false, false, disabled), ?SP(true, false, initializing), ?EXP_SETTING_CONFLICT_ERROR},
        {?SP(false, true, disabled), ?SP(true, false, initializing), ?EXP_SETTING_CONFLICT_ERROR}
    ]).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
example_support_parameters() ->
    #support_parameters{
        accounting_enabled = ?RAND_ELEMENT([undefined, true, false]),
        dir_stats_service_enabled = ?RAND_ELEMENT([undefined, true, false]),
        dir_stats_service_status = ?RAND_ELEMENT([undefined | support_parameters:all_dir_stats_service_statuses()])
    }.


%% @private
example_support_parameters_registry() ->
    #support_parameters_registry{
        registry = maps_utils:generate(fun() ->
            {?RAND_STR(), example_support_parameters()}
        end, ?RAND_INT(0, 20))
    }.


%% @private
insert_parameters_into_registry(ProviderId, SupportParameters, SupportParametersRegistry) ->
    SupportParametersRegistry#support_parameters_registry{
        registry = maps:put(ProviderId, SupportParameters, SupportParametersRegistry#support_parameters_registry.registry)
    }.


%% @private
encode_decode_test_base(Record) when not is_list(Record) ->
    encode_decode_test_base([Record]);
encode_decode_test_base(Records) ->
    lists:foreach(fun(Record) ->
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(Record)),
        ?assert(eunit_utils:is_equal_after_db_encode_and_decode(Record))
    end, Records).


-endif.

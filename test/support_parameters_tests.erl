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


%%%===================================================================
%%% Tests
%%%===================================================================

encode_decode_support_parameters_test() ->
    encode_decode_test_base(lists_utils:generate(fun example_support_parameters/0, 20)).


encode_decode_support_parameters_registry_test() ->
    encode_decode_test_base(lists_utils:generate(fun example_support_parameters_registry/0, 20)).


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
    end, update_support_parameters_test_cases()).

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


%% @private
update_support_parameters_test_cases() ->
    MkParams = fun(AccountingEnabled, DirStatsServiceEnabled, DirStatsServiceStatus) ->
        #support_parameters{
            accounting_enabled = AccountingEnabled,
            dir_stats_service_enabled = DirStatsServiceEnabled,
            dir_stats_service_status = DirStatsServiceStatus
        }
    end,
    ExpectedSettingsConflictError = ?ERROR_BAD_DATA(
        <<"dirStatsServiceEnabled">>,
        <<"Dir stats service must be enabled if accounting is enabled">>
    ),
    [
        {MkParams(false, false, disabled), MkParams(true, true, enabled), {ok, MkParams(true, true, enabled)}},
        {MkParams(false, true, disabled), MkParams(true, undefined, initializing), {ok, MkParams(true, true, initializing)}},
        {MkParams(false, false, stopping), MkParams(undefined, undefined, disabled), {ok, MkParams(false, false, disabled)}},
        {MkParams(false, true, enabled), MkParams(undefined, undefined, undefined), {ok, MkParams(false, true, enabled)}},

        % cases when the status is tweaked in advance so that it is not confusing
        {MkParams(true, true, enabled), MkParams(false, undefined, stopping), {ok, MkParams(false, true, initializing)}},
        {MkParams(true, false, disabled), MkParams(undefined, true, undefined), {ok, MkParams(true, true, initializing)}},
        {MkParams(true, false, stopping), MkParams(true, true, undefined), {ok, MkParams(true, true, initializing)}},
        {MkParams(false, true, initializing), MkParams(false, false, undefined), {ok, MkParams(false, false, stopping)}},
        {MkParams(false, true, enabled), MkParams(undefined, false, undefined), {ok, MkParams(false, false, stopping)}},

        % updates starting from the default settings - currently (false, true, disabled)
        % (if the macro changes, the tests need to be adjusted)
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(undefined, undefined, undefined), {ok, MkParams(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(undefined, true, initializing), {ok, MkParams(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(undefined, true, enabled), {ok, MkParams(false, true, enabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(undefined, false, undefined), {ok, MkParams(false, false, disabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(true, true, undefined), {ok, MkParams(true, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(false, false, undefined), {ok, MkParams(false, false, disabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(false, false, disabled), {ok, MkParams(false, false, disabled)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(false, true, undefined), {ok, MkParams(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(false, true, initializing), {ok, MkParams(false, true, initializing)}},
        {?DEFAULT_SUPPORT_PARAMETERS, MkParams(false, true, enabled), {ok, MkParams(false, true, enabled)}},

        % illegal settings combinations
        {MkParams(true, true, enabled), MkParams(undefined, false, undefined), ExpectedSettingsConflictError},
        {MkParams(true, true, enabled), MkParams(true, false, stopping), ExpectedSettingsConflictError},
        {MkParams(false, false, disabled), MkParams(true, false, undefined), ExpectedSettingsConflictError},
        {MkParams(false, false, disabled), MkParams(true, false, initializing), ExpectedSettingsConflictError},
        {MkParams(false, true, disabled), MkParams(true, false, initializing), ExpectedSettingsConflictError}
    ].


-endif.

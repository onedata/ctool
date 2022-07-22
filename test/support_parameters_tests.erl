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


-define(SPARAMS(AccountingEnabled, DirStatsServiceEnabled, DirStatsServiceStatus), #support_parameters{
    accounting_enabled = AccountingEnabled,
    dir_stats_service_enabled = DirStatsServiceEnabled,
    dir_stats_service_status = DirStatsServiceStatus
}).
-define(DSS_MUST_BE_ENABLED_ERROR, ?ERROR_BAD_DATA(
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


update_support_parameters_test() ->
    ExampleRegistry = example_support_parameters_registry(),
    lists:foreach(fun({RecordToUpdate, OverlayRecord, ExpectedResult}) ->
        ?assertEqual(ExpectedResult, support_parameters:update(RecordToUpdate, OverlayRecord)),

        DummyProviderId = ?RAND_STR(),
        DummyRegistry = ExampleRegistry#support_parameters_registry{
            registry = maps:put(DummyProviderId, RecordToUpdate, ExampleRegistry#support_parameters_registry.registry)
        },
        case ExpectedResult of
            {error, _} ->
                ?assertEqual(
                    ExpectedResult,
                    support_parameters_registry:update_entry(DummyProviderId, OverlayRecord, DummyRegistry)
                );
            {ok, UpdatedParameters} ->
                ?assertEqual(
                    {ok, DummyRegistry#support_parameters_registry{
                        registry = maps:put(DummyProviderId, UpdatedParameters, DummyRegistry#support_parameters_registry.registry)
                    }},
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
encode_decode_test_base(Record) when not is_list(Record) ->
    encode_decode_test_base([Record]);
encode_decode_test_base(Records) ->
    lists:foreach(fun(Record) ->
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(Record)),
        ?assert(eunit_utils:is_equal_after_db_encode_and_decode(Record))
    end, Records).


update_support_parameters_test_cases() -> [
    {?SPARAMS(false, false, disabled), ?SPARAMS(true, true, enabled), {ok, ?SPARAMS(true, true, enabled)}},
    {?SPARAMS(false, true, disabled), ?SPARAMS(true, undefined, initializing), {ok, ?SPARAMS(true, true, initializing)}},
    {?SPARAMS(false, false, stopping), ?SPARAMS(undefined, undefined, disabled), {ok, ?SPARAMS(false, false, disabled)}},
    {?SPARAMS(false, true, enabled), ?SPARAMS(undefined, undefined, undefined), {ok, ?SPARAMS(false, true, enabled)}},
    {?SPARAMS(true, true, enabled), ?SPARAMS(false, undefined, stopping), {ok, ?SPARAMS(false, true, stopping)}},
    {?SPARAMS(true, true, enabled), ?SPARAMS(undefined, false, undefined), ?DSS_MUST_BE_ENABLED_ERROR},
    {?SPARAMS(true, true, enabled), ?SPARAMS(true, false, stopping), ?DSS_MUST_BE_ENABLED_ERROR},
    {?SPARAMS(false, false, disabled), ?SPARAMS(true, false, undefined), ?DSS_MUST_BE_ENABLED_ERROR},
    {?SPARAMS(false, false, disabled), ?SPARAMS(true, false, initializing), ?DSS_MUST_BE_ENABLED_ERROR},
    {?SPARAMS(false, true, disabled), ?SPARAMS(true, false, initializing), ?DSS_MUST_BE_ENABLED_ERROR}
].


-endif.

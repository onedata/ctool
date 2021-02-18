%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of provider_sync_progress module.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_sync_progress_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("space_support/provider_sync_progress.hrl").

-define(PRV_ALPHA, <<"provider-alpha">>).
-define(PRV_BETA, <<"provider-beta">>).
-define(PRV_GAMMA, <<"provider-gamma">>).
-define(PRV_DELTA, <<"provider-delta">>).

-define(DUMMY_TIMESTAMP, 1500000000).

%%%===================================================================
%%% Testcases
%%%===================================================================

provider_sync_progress_test_() ->
    {foreach,
        fun() ->
            clock_freezer_mock:setup_locally([provider_sync_progress]),
            clock_freezer_mock:set_current_time_millis(?DUMMY_TIMESTAMP * 1000),
            node_cache:init()
        end,
        fun(_) ->
            node_cache:destroy(),
            clock_freezer_mock:teardown_locally()
        end,
        [
            {"empty_space_register_first_new_support", fun empty_space_register_first_new_support/0},
            {"unsupport_space_with_single_provider", fun unsupport_space_with_single_provider/0},
            {"empty_space_register_first_legacy_support", fun empty_space_register_first_legacy_support/0},
            {"register_upgrade_of_legacy_support_with_single_provider", fun register_upgrade_of_legacy_support_with_single_provider/0},
            {"legacy_unsupport_space_with_single_provider", fun legacy_unsupport_space_with_single_provider/0},
            {"resupport_space_with_single_provider", fun resupport_space_with_single_provider/0},
            {"resupport_space_with_legacy_provider", fun resupport_space_with_legacy_provider/0},
            {"resupport_space_with_previously_legacy_provider", fun resupport_space_with_previously_legacy_provider/0},
            {"empty_space_register_support_of_another_provider", fun empty_space_register_support_of_another_provider/0},
            {"mixed_versions_support_and_unsupport", fun mixed_versions_support_and_unsupport/0},
            {"mixed_versions_support_and_upgrade", fun mixed_versions_support_and_upgrade/0},
            {"empty_space_unsupport_one_of_two_providers", fun empty_space_unsupport_one_of_two_providers/0},
            {"consume_report_single_provider", fun consume_report_single_provider/0},
            {"consume_report_legacy_provider", fun consume_report_legacy_provider/0},
            {"consume_report_two_providers", fun consume_report_two_providers/0},
            {"consume_report_with_low_timestamps_single_provider", fun consume_report_with_low_timestamps_single_provider/0},
            {"consume_report_with_low_timestamps_two_providers", fun consume_report_with_low_timestamps_two_providers/0},
            {"comprehensive_space_lifecycle", fun comprehensive_space_lifecycle/0}
        ]
    }.


empty_space_register_first_new_support() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithAlpha = register_support(Empty, modern, ?PRV_ALPHA),
    ?assert(check_update_result(WithAlpha, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}}
        }},
        transitioned_from_joining = [?PRV_ALPHA],
        transitioned_from_desync = [?PRV_ALPHA],
        transitioned_to_desync = []
    })),
    ?assertError({support_already_registered, ?PRV_ALPHA}, register_support(WithAlpha, modern, ?PRV_ALPHA)).


unsupport_space_with_single_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithAlpha = register_support(Empty, modern, ?PRV_ALPHA),
    ?assertError({support_not_registered, ?PRV_BETA}, register_unsupport(WithAlpha, modern, ?PRV_BETA)),
    FirstArchival = register_unsupport(WithAlpha, modern, ?PRV_ALPHA),
    ?assertError({support_not_registered, ?PRV_ALPHA}, register_unsupport(FirstArchival, modern, ?PRV_ALPHA)),
    ?assertNotEqual(Empty, FirstArchival),
    ?assert(check_update_result(FirstArchival, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}}
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),
    ?assert(check_update_result(prune_archival_entries(FirstArchival), #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{}},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


empty_space_register_first_legacy_support() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithAlpha = register_support(Empty, legacy, ?PRV_ALPHA),
    ?assert(check_update_result(WithAlpha, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = true, joining = true, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}}
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [?PRV_ALPHA],
        transitioned_to_desync = []
    })),
    ?assertError({support_already_registered, ?PRV_ALPHA}, register_support(WithAlpha, legacy, ?PRV_ALPHA)).


register_upgrade_of_legacy_support_with_single_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    ?assertError({support_not_registered, ?PRV_ALPHA}, register_upgrade_of_legacy_support(Empty, ?PRV_ALPHA)),
    WithAlpha = register_support(Empty, legacy, ?PRV_ALPHA),
    AlphaUpgraded = register_upgrade_of_legacy_support(WithAlpha, ?PRV_ALPHA),
    ?assert(check_update_result(AlphaUpgraded, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}}
        }},
        transitioned_from_joining = [?PRV_ALPHA],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),
    ?assertError({not_a_legacy_provider, ?PRV_ALPHA}, register_upgrade_of_legacy_support(AlphaUpgraded, ?PRV_ALPHA)).


legacy_unsupport_space_with_single_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithAlpha = register_support(Empty, legacy, ?PRV_ALPHA),
    ?assertError({support_not_registered, ?PRV_BETA}, register_unsupport(WithAlpha, legacy, ?PRV_BETA)),
    FirstArchival = register_unsupport(WithAlpha, legacy, ?PRV_ALPHA),
    ?assertError({support_not_registered, ?PRV_ALPHA}, register_unsupport(FirstArchival, legacy, ?PRV_ALPHA)),
    ?assertNotEqual(Empty, FirstArchival),
    ?assert(check_update_result(FirstArchival, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = true, joining = true, archival = true, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}}
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),
    ?assert(check_update_result(prune_archival_entries(FirstArchival), #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{}},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),
    ?assertError({support_not_registered, ?PRV_ALPHA}, register_upgrade_of_legacy_support(FirstArchival, ?PRV_ALPHA)).


resupport_space_with_single_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithAlpha = register_support(Empty, modern, ?PRV_ALPHA),
    FirstArchival = register_unsupport(WithAlpha, modern, ?PRV_ALPHA),
    WithResupportedAlpha = register_support(FirstArchival, modern, ?PRV_ALPHA),
    ?assert(check_update_result(WithResupportedAlpha, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            }
        }},
        transitioned_from_joining = [?PRV_ALPHA],
        transitioned_from_desync = [?PRV_ALPHA],
        transitioned_to_desync = []
    })),
    ?assertError({support_already_registered, ?PRV_ALPHA}, register_support(WithAlpha, modern, ?PRV_ALPHA)).


resupport_space_with_legacy_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithAlpha = register_support(Empty, legacy, ?PRV_ALPHA),
    FirstArchival = register_unsupport(WithAlpha, legacy, ?PRV_ALPHA),
    WithResupportedAlpha = register_support(FirstArchival, legacy, ?PRV_ALPHA),
    ?assert(check_update_result(WithResupportedAlpha, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = true, joining = true, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [?PRV_ALPHA],
        transitioned_to_desync = []
    })),
    ?assertError({support_already_registered, ?PRV_ALPHA}, register_support(WithAlpha, legacy, ?PRV_ALPHA)).


resupport_space_with_previously_legacy_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithAlpha = register_support(Empty, legacy, ?PRV_ALPHA),
    FirstArchival = register_unsupport(WithAlpha, legacy, ?PRV_ALPHA),
    WithResupportedAlpha = register_support(FirstArchival, modern, ?PRV_ALPHA),
    ?assert(check_update_result(WithResupportedAlpha, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            }
        }},
        transitioned_from_joining = [?PRV_ALPHA],
        transitioned_from_desync = [?PRV_ALPHA],
        transitioned_to_desync = []
    })),
    ?assertError({support_already_registered, ?PRV_ALPHA}, register_support(WithAlpha, modern, ?PRV_ALPHA)),
    ?assertError({support_already_registered, ?PRV_ALPHA}, register_support(WithAlpha, legacy, ?PRV_ALPHA)).


empty_space_register_support_of_another_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, modern, ?PRV_ALPHA),
    WithTwo = register_support(WithOne, modern, ?PRV_BETA),
    ?assert(check_update_result(WithTwo, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            }
        }},
        transitioned_from_joining = [?PRV_BETA],
        transitioned_from_desync = [?PRV_BETA],
        transitioned_to_desync = []
    })),
    ?assertError({support_already_registered, ?PRV_BETA}, register_support(WithTwo, modern, ?PRV_BETA)).


mixed_versions_support_and_unsupport() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, modern, ?PRV_ALPHA),
    WithTwo = register_support(WithOne, legacy, ?PRV_BETA),
    ?assert(check_update_result(WithTwo, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            },
            ?PRV_BETA => #provider_sync_progress{legacy = true, joining = true, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [?PRV_BETA],
        transitioned_to_desync = []
    })),
    ?assertError({legacy_provider, ?PRV_BETA}, register_unsupport(WithTwo, modern, ?PRV_BETA)),
    ?assertError({not_a_legacy_provider, ?PRV_ALPHA}, register_unsupport(WithTwo, legacy, ?PRV_ALPHA)),
    BetaUnsupported = register_unsupport(WithTwo, legacy, ?PRV_BETA),
    AlphaUnsupported = register_unsupport(BetaUnsupported, modern, ?PRV_ALPHA),
    ?assert(check_update_result(prune_archival_entries(AlphaUnsupported), #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{}},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


mixed_versions_support_and_upgrade() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, legacy, ?PRV_ALPHA),
    WithTwo = register_support(WithOne, modern, ?PRV_BETA),
    ?assert(check_update_result(WithTwo, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = true, joining = true, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            }
        }},
        transitioned_from_joining = [?PRV_BETA],
        transitioned_from_desync = [?PRV_BETA],
        transitioned_to_desync = []
    })),
    ?assertError({not_a_legacy_provider, ?PRV_BETA}, register_upgrade_of_legacy_support(WithTwo, ?PRV_BETA)),
    BetaUpgraded = register_upgrade_of_legacy_support(WithTwo, ?PRV_ALPHA),
    ?assert(check_update_result(BetaUpgraded, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}}
            }
        }},
        transitioned_from_joining = [?PRV_ALPHA],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


empty_space_unsupport_one_of_two_providers() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, modern, ?PRV_ALPHA),
    WithTwo = register_support(WithOne, modern, ?PRV_BETA),
    ?assertError({support_not_registered, ?PRV_GAMMA}, register_unsupport(WithTwo, modern, ?PRV_GAMMA)),
    FirstArchival = register_unsupport(WithTwo, modern, ?PRV_ALPHA),
    ?assertError({support_not_registered, ?PRV_ALPHA}, register_unsupport(FirstArchival, modern, ?PRV_ALPHA)),
    ?assertNotEqual(WithTwo, FirstArchival),
    ?assertMatch(
        {ok, #provider_sync_progress{archival = true}},
        provider_sync_progress:lookup(FirstArchival#sync_progress_registry_update_result.new_registry, ?PRV_ALPHA)
    ),
    FirstPruned = prune_archival_entries(FirstArchival),
    ?assert(check_update_result(FirstPruned, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                per_peer = #{
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


consume_report_single_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, modern, ?PRV_ALPHA),
    Report = build_collective_report([?PRV_ALPHA], fun
        (?PRV_ALPHA) -> {1565, ?DUMMY_TIMESTAMP + 47}
    end),
    clock_freezer_mock:simulate_seconds_passing(55),
    ReportConsumed = consume_collective_report(WithOne, ?PRV_ALPHA, Report),
    ?assert(check_update_result(ReportConsumed, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 55,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


consume_report_legacy_provider() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, legacy, ?PRV_ALPHA),
    Report = build_collective_report([?PRV_ALPHA], fun
        (?PRV_ALPHA) -> {1565, ?DUMMY_TIMESTAMP + 47}
    end),
    ?assertError({legacy_provider, ?PRV_ALPHA}, consume_collective_report(WithOne, ?PRV_ALPHA, Report)).


consume_report_two_providers() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, modern, ?PRV_ALPHA),
    WithTwo = register_support(WithOne, modern, ?PRV_BETA),
    AllProviders = [?PRV_ALPHA, ?PRV_BETA],

    AlphaReport = build_collective_report(AllProviders, fun
        (?PRV_ALPHA) -> {1565, ?DUMMY_TIMESTAMP + 47};
        (?PRV_BETA) -> {750, ?DUMMY_TIMESTAMP + 130}
    end),
    BetaReport = build_collective_report(AllProviders, fun
        (?PRV_ALPHA) -> {1380, ?DUMMY_TIMESTAMP + 23};
        (?PRV_BETA) -> {750, ?DUMMY_TIMESTAMP + 130}
    end),

    clock_freezer_mock:simulate_seconds_passing(143),
    AlphaConsumed = consume_collective_report(WithTwo, ?PRV_ALPHA, AlphaReport),
    clock_freezer_mock:simulate_seconds_passing(17),
    BetaConsumed = consume_collective_report(AlphaConsumed, ?PRV_BETA, BetaReport),
    ?assert(check_update_result(BetaConsumed, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 143,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 130,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 160,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 130,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


consume_report_with_low_timestamps_single_provider() ->
    % if a provider reports a too low timestamp, it should be rounded up to the space creation time
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, modern, ?PRV_ALPHA),
    Report = build_collective_report([?PRV_ALPHA], fun
        (?PRV_ALPHA) -> {1, 0}
    end),
    clock_freezer_mock:simulate_seconds_passing(13),
    ReportConsumed = consume_collective_report(WithOne, ?PRV_ALPHA, Report),
    ?assert(check_update_result(ReportConsumed, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 13,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


consume_report_with_low_timestamps_two_providers() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, modern, ?PRV_ALPHA),
    WithTwo = register_support(WithOne, modern, ?PRV_BETA),
    AllProviders = [?PRV_ALPHA, ?PRV_BETA],

    AlphaReport = build_collective_report(AllProviders, fun
        (?PRV_ALPHA) -> {18, ?DUMMY_TIMESTAMP - 80};
        (?PRV_BETA) -> {1, 0}
    end),
    BetaReport = build_collective_report(AllProviders, fun
        (?PRV_ALPHA) -> {12, ?DUMMY_TIMESTAMP - 23};
        (?PRV_BETA) -> {1, 0}
    end),

    clock_freezer_mock:simulate_seconds_passing(71),
    AlphaConsumed = consume_collective_report(WithTwo, ?PRV_ALPHA, AlphaReport),
    clock_freezer_mock:simulate_seconds_passing(35),
    BetaConsumed = consume_collective_report(AlphaConsumed, ?PRV_BETA, BetaReport),
    ?assert(check_update_result(BetaConsumed, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 71,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 18, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 106,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 12, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 6, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).


comprehensive_space_lifecycle() ->
    Empty = provider_sync_progress:new_registry(?DUMMY_TIMESTAMP),
    WithOne = register_support(Empty, legacy, ?PRV_ALPHA),
    WithTwo = register_support(WithOne, modern, ?PRV_BETA),

    BetaReport = build_collective_report([?PRV_ALPHA, ?PRV_BETA], fun
        (?PRV_ALPHA) -> {1380, ?DUMMY_TIMESTAMP + 23};
        (?PRV_BETA) -> {750, ?DUMMY_TIMESTAMP + 376}
    end),
    clock_freezer_mock:simulate_seconds_passing(415),
    BetaConsumed = consume_collective_report(WithTwo, ?PRV_BETA, BetaReport),

    WithTwoUpgraded = register_upgrade_of_legacy_support(BetaConsumed, ?PRV_ALPHA),
    AlphaReport = build_collective_report([?PRV_ALPHA, ?PRV_BETA], fun
        (?PRV_ALPHA) -> {1565, ?DUMMY_TIMESTAMP + 47};
        (?PRV_BETA) -> {750, ?DUMMY_TIMESTAMP + 376}
    end),
    clock_freezer_mock:simulate_seconds_passing(113),
    AlphaConsumed = consume_collective_report(WithTwoUpgraded, ?PRV_ALPHA, AlphaReport),

    WithThree = register_support(AlphaConsumed, legacy, ?PRV_GAMMA),
    ?assert(check_update_result(WithThree, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 528,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = true, joining = true, archival = false, desync = true,
                last_report = -1,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 1564, delay = 47, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 749, delay = 376, desync = true},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = [?PRV_GAMMA]
    })),

    WithThreeUpgraded = register_upgrade_of_legacy_support(WithThree, ?PRV_GAMMA),
    ?assert(check_update_result(WithThreeUpgraded, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 528,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = true, archival = false, desync = true,
                last_report = -1,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 1564, delay = 47, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 749, delay = 376, desync = true},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),

    ThreeProviders = [?PRV_ALPHA, ?PRV_BETA, ?PRV_GAMMA],
    GammaReport = build_collective_report(ThreeProviders, fun
        (?PRV_ALPHA) -> {1584, ?DUMMY_TIMESTAMP + 49};
        (?PRV_BETA) -> {336, ?DUMMY_TIMESTAMP + 82};
        (?PRV_GAMMA) -> {14, ?DUMMY_TIMESTAMP + 594}
    end),
    clock_freezer_mock:simulate_seconds_passing(53),
    GammaConsumed = consume_collective_report(WithThreeUpgraded, ?PRV_GAMMA, GammaReport),
    ?assert(check_update_result(GammaConsumed, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 528,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 581,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1584, seq_timestamp = ?DUMMY_TIMESTAMP + 49,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 336, seq_timestamp = ?DUMMY_TIMESTAMP + 82,
                        diff = 414, delay = 294, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 14, seq_timestamp = ?DUMMY_TIMESTAMP + 594,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [?PRV_GAMMA],
        transitioned_from_desync = [?PRV_GAMMA],
        transitioned_to_desync = [?PRV_ALPHA, ?PRV_BETA]
    })),
    ?assertError({support_already_registered, ?PRV_GAMMA}, register_support(GammaConsumed, modern, ?PRV_GAMMA)),

    WithoutBeta = register_unsupport(GammaConsumed, modern, ?PRV_BETA),
    ?assertError({support_not_registered, ?PRV_BETA}, register_unsupport(WithoutBeta, modern, ?PRV_BETA)),

    ?assert(check_update_result(WithoutBeta, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 528,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 581,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1584, seq_timestamp = ?DUMMY_TIMESTAMP + 49,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 336, seq_timestamp = ?DUMMY_TIMESTAMP + 82,
                        diff = 414, delay = 294, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 14, seq_timestamp = ?DUMMY_TIMESTAMP + 594,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),

    % Pruning should not remove archival entries if other providers haven't seen all the seqs
    ?assertEqual(WithoutBeta, prune_archival_entries(WithoutBeta)),

    AlphaBetaDelta = register_support(WithoutBeta, modern, ?PRV_DELTA),
    ?assert(check_update_result(AlphaBetaDelta, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 528,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 581,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1584, seq_timestamp = ?DUMMY_TIMESTAMP + 49,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 336, seq_timestamp = ?DUMMY_TIMESTAMP + 82,
                        diff = 414, delay = 294, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 14, seq_timestamp = ?DUMMY_TIMESTAMP + 594,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_DELTA => #provider_sync_progress{legacy = false, joining = true, archival = false, desync = true,
                last_report = -1,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 1564, delay = 47, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 749, delay = 376, desync = true},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = [?PRV_DELTA]
    })),

    AlphaBetaUnsupportDelta = register_unsupport(AlphaBetaDelta, modern, ?PRV_DELTA),
    ?assert(check_update_result(AlphaBetaUnsupportDelta, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 528,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 581,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1584, seq_timestamp = ?DUMMY_TIMESTAMP + 49,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 336, seq_timestamp = ?DUMMY_TIMESTAMP + 82,
                        diff = 414, delay = 294, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 14, seq_timestamp = ?DUMMY_TIMESTAMP + 594,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_DELTA => #provider_sync_progress{legacy = false, joining = true, archival = true, desync = true,
                last_report = -1,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 1564, delay = 47, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 749, delay = 376, desync = true},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),

    AlphaBetaResupportDelta = register_support(AlphaBetaUnsupportDelta, modern, ?PRV_DELTA),
    ?assertEqual(AlphaBetaResupportDelta, AlphaBetaDelta),

    FourProviders = [?PRV_ALPHA, ?PRV_BETA, ?PRV_GAMMA, ?PRV_DELTA],
    DeltaReport = build_collective_report(FourProviders, fun
        (?PRV_ALPHA) -> {537, ?DUMMY_TIMESTAMP + 22};
        (?PRV_BETA) -> {750, ?DUMMY_TIMESTAMP + 376};
        (?PRV_GAMMA) -> {10, ?DUMMY_TIMESTAMP + 416};
        (?PRV_DELTA) -> {43, ?DUMMY_TIMESTAMP + 7}
    end),
    clock_freezer_mock:simulate_seconds_passing(43),
    DeltaConsumed = consume_collective_report(AlphaBetaDelta, ?PRV_DELTA, DeltaReport),
    ?assert(check_update_result(DeltaConsumed, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 528,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1565, seq_timestamp = ?DUMMY_TIMESTAMP + 47,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 42, delay = 7, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 185, delay = 24, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 42, delay = 7, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 581,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1584, seq_timestamp = ?DUMMY_TIMESTAMP + 49,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 336, seq_timestamp = ?DUMMY_TIMESTAMP + 82,
                        diff = 414, delay = 294, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 14, seq_timestamp = ?DUMMY_TIMESTAMP + 594,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 42, delay = 7, desync = false}
                }
            },
            ?PRV_DELTA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 624,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 537, seq_timestamp = ?DUMMY_TIMESTAMP + 22,
                        diff = 1028, delay = 25, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 10, seq_timestamp = ?DUMMY_TIMESTAMP + 416,
                        diff = 4, delay = 178, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [?PRV_DELTA],
        transitioned_from_desync = [?PRV_DELTA],
        transitioned_to_desync = []
    })),

    AlphaReportBis = build_collective_report(FourProviders, fun
        (?PRV_ALPHA) -> {1843, ?DUMMY_TIMESTAMP + 60};
        (?PRV_BETA) -> {750, ?DUMMY_TIMESTAMP + 376};
        (?PRV_GAMMA) -> {12, ?DUMMY_TIMESTAMP + 555};
        (?PRV_DELTA) -> {43, ?DUMMY_TIMESTAMP + 7}
    end),
    clock_freezer_mock:simulate_seconds_passing(3),
    AlphaConsumedBis = consume_collective_report(DeltaConsumed, ?PRV_ALPHA, AlphaReportBis),
    ?assert(check_update_result(AlphaConsumedBis, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 627,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1843, seq_timestamp = ?DUMMY_TIMESTAMP + 60,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 12, seq_timestamp = ?DUMMY_TIMESTAMP + 555,
                        diff = 2, delay = 39, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 463, delay = 37, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 13, delay = 594, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 42, delay = 7, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 581,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1584, seq_timestamp = ?DUMMY_TIMESTAMP + 49,
                        diff = 259, delay = 11, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 336, seq_timestamp = ?DUMMY_TIMESTAMP + 82,
                        diff = 414, delay = 294, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 14, seq_timestamp = ?DUMMY_TIMESTAMP + 594,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 42, delay = 7, desync = false}
                }
            },
            ?PRV_DELTA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 624,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 537, seq_timestamp = ?DUMMY_TIMESTAMP + 22,
                        diff = 1306, delay = 38, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 10, seq_timestamp = ?DUMMY_TIMESTAMP + 416,
                        diff = 4, delay = 178, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [?PRV_ALPHA],
        transitioned_to_desync = []
    })),

    % Pruning should not remove archival entries if other providers haven't seen all the seqs
    ?assertEqual(
        AlphaConsumedBis#sync_progress_registry_update_result{transitioned_from_desync = []},
        prune_archival_entries(AlphaConsumedBis)
    ),

    GammaReportBis = build_collective_report(FourProviders, fun
        (?PRV_ALPHA) -> {1590, ?DUMMY_TIMESTAMP + 58};
        (?PRV_BETA) -> {750, ?DUMMY_TIMESTAMP + 376};
        (?PRV_GAMMA) -> {16, ?DUMMY_TIMESTAMP + 780};
        (?PRV_DELTA) -> {43, ?DUMMY_TIMESTAMP + 7}
    end),
    clock_freezer_mock:simulate_seconds_passing(13),
    GammaConsumedBis = consume_collective_report(AlphaConsumedBis, ?PRV_GAMMA, GammaReportBis),
    ?assert(check_update_result(GammaConsumedBis, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 627,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1843, seq_timestamp = ?DUMMY_TIMESTAMP + 60,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 12, seq_timestamp = ?DUMMY_TIMESTAMP + 555,
                        diff = 4, delay = 225, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_BETA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 415,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1380, seq_timestamp = ?DUMMY_TIMESTAMP + 23,
                        diff = 463, delay = 37, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 15, delay = 780, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 1, seq_timestamp = ?DUMMY_TIMESTAMP,
                        diff = 42, delay = 7, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 640,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1590, seq_timestamp = ?DUMMY_TIMESTAMP + 58,
                        diff = 253, delay = 2, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 16, seq_timestamp = ?DUMMY_TIMESTAMP + 780,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_DELTA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 624,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 537, seq_timestamp = ?DUMMY_TIMESTAMP + 22,
                        diff = 1306, delay = 38, desync = false},
                    ?PRV_BETA => #sync_progress_with_peer{seen_seq = 750, seq_timestamp = ?DUMMY_TIMESTAMP + 376,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 10, seq_timestamp = ?DUMMY_TIMESTAMP + 416,
                        diff = 6, delay = 364, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = [?PRV_DELTA]
    })),

    DeltaUnsupportedAgain = register_unsupport(GammaConsumedBis, modern, ?PRV_DELTA),

    % Provider beta and delta have unsupported the space. All providers have seen
    % beta's latest seq, and all providers *except* beta have seen the latest
    % delta's seq. Pruning should remove the beta provider, and after its that,
    % it should also remove the delta provider.

    BetaPruned = prune_archival_entries(DeltaUnsupportedAgain),
    ?assert(check_update_result(BetaPruned, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 627,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1843, seq_timestamp = ?DUMMY_TIMESTAMP + 60,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 12, seq_timestamp = ?DUMMY_TIMESTAMP + 555,
                        diff = 4, delay = 225, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 640,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1590, seq_timestamp = ?DUMMY_TIMESTAMP + 58,
                        diff = 253, delay = 2, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 16, seq_timestamp = ?DUMMY_TIMESTAMP + 780,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            },
            ?PRV_DELTA => #provider_sync_progress{legacy = false, joining = false, archival = true, desync = true,
                last_report = ?DUMMY_TIMESTAMP + 624,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 537, seq_timestamp = ?DUMMY_TIMESTAMP + 22,
                        diff = 1306, delay = 38, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 10, seq_timestamp = ?DUMMY_TIMESTAMP + 416,
                        diff = 6, delay = 364, desync = true},
                    ?PRV_DELTA => #sync_progress_with_peer{seen_seq = 43, seq_timestamp = ?DUMMY_TIMESTAMP + 7,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })),

    DeltaPruned = prune_archival_entries(BetaPruned),
    ?assert(check_update_result(DeltaPruned, #sync_progress_registry_update_result{
        new_registry = #sync_progress_registry{per_provider = #{
            ?PRV_ALPHA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 627,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1843, seq_timestamp = ?DUMMY_TIMESTAMP + 60,
                        diff = 0, delay = 0, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 12, seq_timestamp = ?DUMMY_TIMESTAMP + 555,
                        diff = 4, delay = 225, desync = false}
                }
            },
            ?PRV_GAMMA => #provider_sync_progress{legacy = false, joining = false, archival = false, desync = false,
                last_report = ?DUMMY_TIMESTAMP + 640,
                per_peer = #{
                    ?PRV_ALPHA => #sync_progress_with_peer{seen_seq = 1590, seq_timestamp = ?DUMMY_TIMESTAMP + 58,
                        diff = 253, delay = 2, desync = false},
                    ?PRV_GAMMA => #sync_progress_with_peer{seen_seq = 16, seq_timestamp = ?DUMMY_TIMESTAMP + 780,
                        diff = 0, delay = 0, desync = false}
                }
            }
        }},
        transitioned_from_joining = [],
        transitioned_from_desync = [],
        transitioned_to_desync = []
    })).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% Wrappers for the functions that modify the registry for simpler test code.
%% The functions return the result wrapped in #registry_update_result{} record
%% including the new registry, which must be normally extracted for the
%% consecutive call. These wrappers do the extracting if needed.
register_support(#sync_progress_registry_update_result{new_registry = Registry}, SupportModel, ProviderId) ->
    register_support(Registry, SupportModel, ProviderId);
register_support(Registry, SupportModel, ProviderId) ->
    provider_sync_progress:register_support(Registry, SupportModel, ProviderId).


register_unsupport(#sync_progress_registry_update_result{new_registry = Registry}, SupportModel, ProviderId) ->
    register_unsupport(Registry, SupportModel, ProviderId);
register_unsupport(Registry, SupportModel, ProviderId) ->
    provider_sync_progress:register_unsupport(Registry, SupportModel, ProviderId).


register_upgrade_of_legacy_support(#sync_progress_registry_update_result{new_registry = Registry}, ProviderId) ->
    register_upgrade_of_legacy_support(Registry, ProviderId);
register_upgrade_of_legacy_support(Registry, ProviderId) ->
    provider_sync_progress:register_upgrade_of_legacy_support(Registry, ProviderId).


consume_collective_report(#sync_progress_registry_update_result{new_registry = Registry}, ProviderId, Report) ->
    consume_collective_report(Registry, ProviderId, Report);
consume_collective_report(Registry, ProviderId, Report) ->
    provider_sync_progress:consume_collective_report(Registry, ProviderId, Report).


prune_archival_entries(#sync_progress_registry_update_result{new_registry = Registry}) ->
    prune_archival_entries(Registry);
prune_archival_entries(Registry) ->
    provider_sync_progress:prune_archival_entries(Registry).


%% Builds a collective report and checks the encode/decode functions at the same time
build_collective_report(AllProviders, BuildReportForProvider) ->
    CollectiveReport = provider_sync_progress:build_collective_report(AllProviders, BuildReportForProvider),
    ?assertEqual(CollectiveReport, provider_sync_progress:collective_report_from_json(json_utils:decode(
        json_utils:encode(provider_sync_progress:collective_report_to_json(CollectiveReport))
    ))),
    CollectiveReport.


%% Allows comparing the actual and expected update result with in a more verbose way
%% and performs some additional checks at the same time.
check_update_result(ActualResult, ExpectedResult) ->
    ActualRegistry = ActualResult#sync_progress_registry_update_result.new_registry,

    ActualNormalized = ActualResult#sync_progress_registry_update_result{
        transitioned_from_joining = lists:sort(ActualResult#sync_progress_registry_update_result.transitioned_from_joining),
        transitioned_from_desync = lists:sort(ActualResult#sync_progress_registry_update_result.transitioned_from_desync),
        transitioned_to_desync = lists:sort(ActualResult#sync_progress_registry_update_result.transitioned_to_desync)
    },

    ExpectedNormalized = ExpectedResult#sync_progress_registry_update_result{
        new_registry = ExpectedResult#sync_progress_registry_update_result.new_registry#sync_progress_registry{
            space_creation_time = ActualRegistry#sync_progress_registry.space_creation_time
        },
        transitioned_from_joining = lists:sort(ExpectedResult#sync_progress_registry_update_result.transitioned_from_joining),
        transitioned_from_desync = lists:sort(ExpectedResult#sync_progress_registry_update_result.transitioned_from_desync),
        transitioned_to_desync = lists:sort(ExpectedResult#sync_progress_registry_update_result.transitioned_to_desync)
    },

    ?assertEqual(ActualRegistry, provider_sync_progress:registry_from_json(json_utils:decode(
        json_utils:encode(provider_sync_progress:registry_to_json(ActualRegistry))
    ))),

    case ActualNormalized =:= ExpectedNormalized of
        true ->
            maps:map(fun(ProviderId, ProviderSyncProgress) ->
                % make sure the lookup function works correctly
                ?assertEqual({ok, ProviderSyncProgress}, provider_sync_progress:lookup(ActualRegistry, ProviderId))
            end, ActualRegistry#sync_progress_registry.per_provider),
            true;
        false ->
            % improves the output readability (otherwise the print gets tangled in between the eunit output)
            timer:sleep(300),
            io:format(user,
                "~n~nUpdate result different than expected~n"
                "Actual:   ~p~n"
                "Expected: ~p~n~n",
                [ActualNormalized, ExpectedNormalized]
            ),
            false
    end.


-endif.

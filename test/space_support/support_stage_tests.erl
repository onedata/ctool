%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of support_stage module.
%%% @end
%%%-------------------------------------------------------------------
-module(support_stage_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("space_support/support_stage.hrl").
-include("errors.hrl").

-define(RAND_ID(), str_utils:rand_hex(16)).
-define(SMALL, 723648473).
-define(LARGE, 83625172641873).

-define(ST_A, <<"storage-a">>).
-define(ST_B, <<"storage-b">>).
-define(ST_C, <<"storage-c">>).

-define(ALL_STORAGE_STATES, [
    joining, active, {resizing, ?SMALL}, {resizing, ?LARGE}, {resizing, 0},
    purging, retiring, retired
]).

-define(STAGES(ProviderStage, PerStorage), #support_stage_details{
    provider_stage = ProviderStage,
    per_storage = PerStorage
}).

-define(VALID_STAGE_DETAILS_EXAMPLES, [
    ?STAGES(joining, #{?RAND_ID() => joining}),
    ?STAGES(active, #{?RAND_ID() => active, ?RAND_ID() => active}),
    ?STAGES(remodelling, #{?RAND_ID() => active, ?RAND_ID() => {resizing, 0}, ?RAND_ID() => {resizing, ?LARGE}}),
    ?STAGES(remodelling, #{?RAND_ID() => joining, ?RAND_ID() => active, ?RAND_ID() => purging}),
    ?STAGES(remodelling, #{?RAND_ID() => joining, ?RAND_ID() => {resizing, ?SMALL}, ?RAND_ID() => retiring}),
    ?STAGES(evicting_replicas, #{?RAND_ID() => {resizing, 0}}),
    ?STAGES(purging_storages, #{?RAND_ID() => purging, ?RAND_ID() => retiring}),
    ?STAGES(purging_database, #{?RAND_ID() => retiring, ?RAND_ID() => retiring}),
    ?STAGES(retired, #{?RAND_ID() => retired, ?RAND_ID() => retired, ?RAND_ID() => retired})
]).

%%%===================================================================
%%% Test functions - cover possible states and transitions
%%%===================================================================

legacy_support_test() ->
    ProviderId = ?RAND_ID(),
    {ok, WithLegacySupport} = support_stage:insert_legacy_support_entry(#{}, ProviderId),
    ?assertMatch(?LEGACY_SUPPORT, support_stage:lookup_details(WithLegacySupport, ProviderId)),
    {ok, LegacySupportRevoked} = support_stage:mark_legacy_support_revocation(#{}, ProviderId),
    % Legacy supports are not retained in support history, but removed completely.
    ?assertMatch(error, support_stage:lookup_details(LegacySupportRevoked, ProviderId)),
    ?assertMatch(#{}, LegacySupportRevoked).


legacy_support_upgrade_test() ->
    InitialStages = ?LEGACY_SUPPORT,
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(joining, #{?ST_A => joining});
        (_) -> illegal
    end)).


first_support_test() ->
    InitialStages = none,
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(joining, #{?ST_A => joining});
        (_) -> illegal
    end)).


only_storage_transition_from_joining_test() ->
    InitialStages = ?STAGES(joining, #{?ST_A => joining}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (active) -> ?STAGES(active, #{?ST_A => active});
        ({resizing, 0}) -> ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}});
        (_) -> illegal
    end)).


only_storage_transition_from_active_test() ->
    InitialStages = ?STAGES(active, #{?ST_A => active}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        ({resizing, ?SMALL}) -> ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) -> ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}});
        ({resizing, 0}) -> ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}});
        (_) -> illegal
    end)).


only_storage_transition_from_resizing_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (active) -> ?STAGES(active, #{?ST_A => active});
        ({resizing, ?SMALL}) -> ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) -> ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}});
        ({resizing, 0}) -> ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}});
        (_) -> illegal
    end)).


only_storage_transition_from_evicting_replicas_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (purging) -> ?STAGES(purging_storages, #{?ST_A => purging});
        ({resizing, ?SMALL}) -> ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) -> ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}});
        ({resizing, 0}) -> ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}});
        (_) -> illegal
    end)).


only_storage_transition_from_purging_storages_test() ->
    InitialStages = ?STAGES(purging_storages, #{?ST_A => purging}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (retiring) -> ?STAGES(purging_database, #{?ST_A => retiring});
        (_) -> illegal
    end)).


only_storage_transition_from_retiring_test() ->
    InitialStages = ?STAGES(purging_database, #{?ST_A => retiring}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (retired) -> ?STAGES(retired, #{?ST_A => retired});
        (_) -> illegal
    end)).


only_storage_resupport_test() ->
    InitialStages = ?STAGES(retired, #{?ST_A => retired}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(joining, #{?ST_A => joining});
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

two_storages_join_test() ->
    InitialStages = ?STAGES(joining, #{?ST_A => joining}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (joining) -> ?STAGES(joining, #{?ST_A => joining, ?ST_B => joining});
        (_) -> illegal
    end)).


all_storages_join_test() ->
    InitialStages = ?STAGES(joining, #{?ST_A => joining, ?ST_B => joining}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (joining) -> ?STAGES(joining, #{?ST_A => joining, ?ST_B => joining, ?ST_C => joining});
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

one_storage_transition_from_joining_test() ->
    InitialStages = ?STAGES(joining, #{?ST_A => joining, ?ST_B => joining, ?ST_C => joining}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (active) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => joining, ?ST_C => joining});
        ({resizing, 0}) -> ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => joining, ?ST_C => joining});
        (_) -> illegal
    end)).

two_storages_transition_from_joining_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => joining, ?ST_C => active}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (active) -> ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => active, ?ST_C => active});
        ({resizing, 0}) -> ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => {resizing, 0}, ?ST_C => active});
        (_) -> illegal
    end)).

all_storages_transition_from_joining_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => active, ?ST_B => active, ?ST_C => joining}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (active) -> ?STAGES(active, #{?ST_A => active, ?ST_B => active, ?ST_C => active});
        ({resizing, 0}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => active, ?ST_C => {resizing, 0}});
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

transition_from_active_test() ->
    InitialStages = ?STAGES(active, #{?ST_A => active, ?ST_B => active, ?ST_C => active}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        ({resizing, ?SMALL}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, ?SMALL}, ?ST_C => active});
        ({resizing, ?LARGE}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, ?LARGE}, ?ST_C => active});
        ({resizing, 0}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, 0}, ?ST_C => active});
        (_) -> illegal
    end)).

new_storage_when_active_test() ->
    InitialStages = ?STAGES(active, #{?ST_B => active, ?ST_C => active}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => active, ?ST_C => active});
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

transition_from_remodelling_variant1_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => active, ?ST_C => joining}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, ?SMALL}, ?ST_C => joining});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, ?LARGE}, ?ST_C => joining});
        ({resizing, 0}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => joining});
        (_) ->
            illegal
    end)).

transition_from_remodelling_variant2_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, ?SMALL}, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (active) ->
            ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, ?SMALL}, ?ST_C => purging});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}, ?ST_B => {resizing, ?SMALL}, ?ST_C => purging});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, ?SMALL}, ?ST_C => purging});
        ({resizing, 0}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, ?SMALL}, ?ST_C => purging});
        (_) ->
            illegal
    end)).


transition_from_remodelling_variant3_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}, ?ST_B => {resizing, ?LARGE}, ?ST_C => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (active) ->
            ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, ?LARGE}, ?ST_C => {resizing, 0}});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}, ?ST_B => {resizing, ?LARGE}, ?ST_C => {resizing, 0}});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, ?LARGE}, ?ST_C => {resizing, 0}});
        ({resizing, 0}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, ?LARGE}, ?ST_C => {resizing, 0}});
        (_) ->
            illegal
    end)).

transition_from_remodelling_variant4_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, 0}, ?ST_C => active}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (purging) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => purging, ?ST_C => active});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, ?SMALL}, ?ST_C => active});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, ?LARGE}, ?ST_C => active});
        ({resizing, 0}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, 0}, ?ST_C => active});
        (_) ->
            illegal
    end)).

transition_from_remodelling_variant5_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging, ?ST_C => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (purging) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging, ?ST_C => purging});
        ({resizing, ?SMALL}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging, ?ST_C => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging, ?ST_C => {resizing, ?LARGE}});
        ({resizing, 0}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging, ?ST_C => {resizing, 0}});
        (_) -> illegal
    end)).

transition_from_remodelling_variant6_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, 0}, ?ST_C => active}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (purging) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging, ?ST_C => active});
        ({resizing, ?SMALL}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, ?SMALL}, ?ST_C => active});
        ({resizing, ?LARGE}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, ?LARGE}, ?ST_C => active});
        ({resizing, 0}) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, 0}, ?ST_C => active});
        (_) -> illegal
    end)).

transition_from_remodelling_variant7_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => active, ?ST_C => {resizing, ?SMALL}}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (retiring) -> ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => active, ?ST_C => {resizing, ?SMALL}});
        (_) -> illegal
    end)).

transition_from_remodelling_variant8_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => joining}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (retired) -> ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => retired, ?ST_C => joining});
        (_) -> illegal
    end)).

transition_from_remodelling_variant9_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => {resizing, ?LARGE}, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (active) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => {resizing, ?LARGE}, ?ST_C => purging});
        ({resizing, 0}) -> ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, ?LARGE}, ?ST_C => purging});
        (_) -> illegal
    end)).

new_storage_when_remodelling_variant1_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => purging, ?ST_C => joining});
        (_) -> illegal
    end)).

new_storage_when_remodelling_variant2_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_B => {resizing, ?SMALL}, ?ST_C => retiring}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => {resizing, ?SMALL}, ?ST_C => retiring});
        (_) -> illegal
    end)).

new_storage_when_remodelling_variant3_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_C => joining}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => joining, ?ST_C => joining});
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

all_storages_unsupport_start_variant1_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => purging, ?ST_C => active}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => purging, ?ST_C => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => purging, ?ST_C => {resizing, ?LARGE}});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => purging, ?ST_C => {resizing, 0}});
        (_) ->
            illegal
    end)).

all_storages_unsupport_start_variant2_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, ?SMALL}}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (active) ->
            ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => active});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, ?LARGE}});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, 0}});
        (_) ->
            illegal
    end)).

%%------------------------------------------------------------------------------

one_storage_transition_from_evicting_replicas_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (purging) ->
            ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => purging});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => {resizing, ?LARGE}});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => {resizing, 0}});
        (_) ->
            illegal
    end)).

two_storages_transition_from_evicting_replicas_variant1_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (purging) ->
            ?STAGES(evicting_replicas, #{?ST_A => purging, ?ST_B => {resizing, 0}, ?ST_C => purging});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}, ?ST_B => {resizing, 0}, ?ST_C => purging});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => {resizing, 0}, ?ST_C => purging});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => {resizing, 0}, ?ST_C => purging});
        (_) ->
            illegal
    end)).

two_storages_transition_from_evicting_replicas_variant2_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (purging) ->
            ?STAGES(evicting_replicas, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => purging});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, ?LARGE}});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => {resizing, 0}});
        (_) ->
            illegal
    end)).

all_storages_transition_from_evicting_replicas_variant1_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => purging, ?ST_B => {resizing, 0}, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (purging) ->
            ?STAGES(purging_storages, #{?ST_A => purging, ?ST_B => purging, ?ST_C => purging});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => {resizing, ?SMALL}, ?ST_C => purging});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => {resizing, ?LARGE}, ?ST_C => purging});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => purging, ?ST_B => {resizing, 0}, ?ST_C => purging});
        (_) ->
            illegal
    end)).

all_storages_transition_from_evicting_replicas_variant2_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (purging) ->
            ?STAGES(purging_storages, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => purging});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => {resizing, ?SMALL}});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => {resizing, ?LARGE}});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => {resizing, 0}});
        (_) ->
            illegal
    end)).

all_storages_transition_from_evicting_replicas_variant3_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => retiring, ?ST_C => retired}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (purging) ->
            ?STAGES(purging_storages, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => retired});
        ({resizing, ?SMALL}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?SMALL}, ?ST_B => retiring, ?ST_C => retired});
        ({resizing, ?LARGE}) ->
            ?STAGES(remodelling, #{?ST_A => {resizing, ?LARGE}, ?ST_B => retiring, ?ST_C => retired});
        ({resizing, 0}) ->
            ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_B => retiring, ?ST_C => retired});
        (_) ->
            illegal
    end)).

new_storage_when_evicting_replicas_variant1_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_B => {resizing, 0}, ?ST_C => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => {resizing, 0}, ?ST_C => {resizing, 0}});
        (_) -> illegal
    end)).

new_storage_when_evicting_replicas_variant2_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => {resizing, 0}, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => {resizing, 0}, ?ST_B => joining, ?ST_C => purging});
        (_) -> illegal
    end)).

new_storage_when_evicting_replicas_variant3_test() ->
    InitialStages = ?STAGES(evicting_replicas, #{?ST_A => retiring, ?ST_B => {resizing, 0}}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => retiring, ?ST_B => {resizing, 0}, ?ST_C => joining});
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

one_storage_transition_from_purging_storages_test() ->
    InitialStages = ?STAGES(purging_storages, #{?ST_A => purging, ?ST_B => purging, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (retiring) -> ?STAGES(purging_storages, #{?ST_A => purging, ?ST_B => purging, ?ST_C => retiring});
        (_) -> illegal
    end)).

two_storages_transition_from_purging_storages_test() ->
    InitialStages = ?STAGES(purging_storages, #{?ST_A => retiring, ?ST_B => purging, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (retiring) -> ?STAGES(purging_storages, #{?ST_A => retiring, ?ST_B => retiring, ?ST_C => purging});
        (_) -> illegal
    end)).

all_storages_transition_from_purging_storages_test() ->
    InitialStages = ?STAGES(purging_storages, #{?ST_A => purging, ?ST_B => retiring, ?ST_C => retiring}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (retiring) -> ?STAGES(purging_database, #{?ST_A => retiring, ?ST_B => retiring, ?ST_C => retiring});
        (_) -> illegal
    end)).

new_storage_when_purging_storages_variant1_test() ->
    InitialStages = ?STAGES(purging_storages, #{?ST_A => purging, ?ST_C => purging}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => joining, ?ST_C => purging});
        (_) -> illegal
    end)).

new_storage_when_purging_storages_variant2_test() ->
    InitialStages = ?STAGES(purging_storages, #{?ST_A => purging, ?ST_B => retired}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => purging, ?ST_B => retired, ?ST_C => joining});
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

one_storage_transition_from_purging_database_test() ->
    InitialStages = ?STAGES(purging_database, #{?ST_A => retiring, ?ST_B => retiring, ?ST_C => retiring}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (retired) -> ?STAGES(purging_database, #{?ST_A => retired, ?ST_B => retiring, ?ST_C => retiring});
        (_) -> illegal
    end)).

two_storages_transition_from_purging_database_test() ->
    InitialStages = ?STAGES(purging_database, #{?ST_A => retired, ?ST_B => retiring, ?ST_C => retiring}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (retired) -> ?STAGES(purging_database, #{?ST_A => retired, ?ST_B => retiring, ?ST_C => retired});
        (_) -> illegal
    end)).

all_storages_transition_from_purging_database_test() ->
    InitialStages = ?STAGES(purging_database, #{?ST_A => retired, ?ST_B => retiring, ?ST_C => retired}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (retired) -> ?STAGES(retired, #{?ST_A => retired, ?ST_B => retired, ?ST_C => retired});
        (_) -> illegal
    end)).

new_storage_when_purging_database_test() ->
    InitialStages = ?STAGES(purging_database, #{?ST_B => retiring, ?ST_C => retiring}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
    % Joining a new storage is not allowed when the database is being purged
        (joining) -> illegal;
        (_) -> illegal
    end)).

%%------------------------------------------------------------------------------

one_storage_resupport_test() ->
    InitialStages = ?STAGES(retired, #{?ST_A => retired, ?ST_B => retired, ?ST_C => retired}),
    ?assert(test_combinations(InitialStages, ?ST_B, fun
        (joining) -> ?STAGES(joining, #{?ST_A => retired, ?ST_B => joining, ?ST_C => retired});
        (_) -> illegal
    end)).

two_storages_resupport_variant1_test() ->
    InitialStages = ?STAGES(joining, #{?ST_A => retired, ?ST_B => joining, ?ST_C => retired}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(joining, #{?ST_A => joining, ?ST_B => joining, ?ST_C => retired});
        (_) -> illegal
    end)).

two_storages_resupport_variant2_test() ->
    InitialStages = ?STAGES(active, #{?ST_A => retired, ?ST_B => retired, ?ST_C => active}),
    ?assert(test_combinations(InitialStages, ?ST_A, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => joining, ?ST_B => retired, ?ST_C => active});
        (_) -> illegal
    end)).

all_storages_resupport_variant1_test() ->
    InitialStages = ?STAGES(remodelling, #{?ST_A => active, ?ST_B => joining, ?ST_C => retired}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => joining, ?ST_C => joining});
        (_) -> illegal
    end)).

all_storages_resupport_variant2_test() ->
    InitialStages = ?STAGES(active, #{?ST_A => active, ?ST_B => active, ?ST_C => retired}),
    ?assert(test_combinations(InitialStages, ?ST_C, fun
        (joining) -> ?STAGES(remodelling, #{?ST_A => active, ?ST_B => active, ?ST_C => joining});
        (_) -> illegal
    end)).

%%%===================================================================
%%% Internal functions - running the test combinations
%%%===================================================================

% Tests the support_stage_details handling by randomizing a preexisting state
% (stage per provider), adding an additional provider for which the tests
% are run and checking results of all possible transitions.
% A random number of retired storages is added to the support stage details
% to check that they do not affect the transitions (should be kept only
% for archival purposes).
% InitialStageDetails can be:
%   * none   - if there was no support previously
%   * ?LEGACY_SUPPORT - if previously the supporting provider was in legacy version
test_combinations(InitialStageDetails, StorageId, GenExpectationFun) ->
    RandomPreexistingState = lists:foldl(fun(StageDetailsExample, Acc) ->
        Acc#{?RAND_ID() => StageDetailsExample}
    end, #{}, lists_utils:random_sublist(?VALID_STAGE_DETAILS_EXAMPLES)),

    ProviderId = ?RAND_ID(),

    % add some randomly generated retired storages to check if they do not impact
    % the stage transitions, but not if the initial stage was legacy
    DummyRetiredStorages = case InitialStageDetails of
        ?LEGACY_SUPPORT ->
            #{};
        _ ->
            lists:foldl(fun(RetiredStorageId, Acc) ->
                Acc#{RetiredStorageId => retired}
            end, #{}, lists_utils:random_sublist([?RAND_ID(), ?RAND_ID(), ?RAND_ID(), ?RAND_ID()]))
    end,

    StageRegistry = case InitialStageDetails of
        #support_stage_details{} ->
            RandomPreexistingState#{
                ProviderId => inject_storage_stages(InitialStageDetails, DummyRetiredStorages)
            };
        ?LEGACY_SUPPORT ->
            {ok, WithLegacy} = support_stage:insert_legacy_support_entry(RandomPreexistingState, ProviderId),
            WithLegacy;
        none ->
            case DummyRetiredStorages of
                Map when map_size(Map) == 0 ->
                    RandomPreexistingState;
                _ ->
                    RandomPreexistingState#{ProviderId => #support_stage_details{
                        provider_stage = retired,
                        per_storage = DummyRetiredStorages
                    }}
            end
    end,

    ?assertEqual(StageRegistry, support_stage:registry_from_json(json_utils:decode(
        json_utils:encode(support_stage:registry_to_json(StageRegistry))
    ))),

    {PrevProviderStage, PrevStorageStage} = case support_stage:lookup_details(StageRegistry, ProviderId) of
        {ok, Details = #support_stage_details{per_storage = PerStorage}} ->
            StorageStage = maps:get(StorageId, PerStorage, none),
            {Details#support_stage_details.provider_stage, StorageStage};
        _ -> % covers none and ?LEGACY_SUPPORT
            {none, none}
    end,

    % Returns true if all checks were successful
    lists:all(fun(TransitionTo) ->
        ExpectedNewStageDetails = GenExpectationFun(TransitionTo),
        ExpectedTransitionResult = case ExpectedNewStageDetails of
            illegal ->
                ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(PrevProviderStage, PrevStorageStage);
            _ ->
                {ok, StageRegistry#{
                    ProviderId => inject_storage_stages(ExpectedNewStageDetails, DummyRetiredStorages)
                }}
        end,
        Result = support_stage:apply_transition(StageRegistry, ProviderId, StorageId, TransitionTo),
        CompareResult = compare_transition_result(ProviderId, StorageId, TransitionTo, ExpectedTransitionResult, Result),

        % If the transition was successful, check that the new stage (stage per provider) is as expected.
        case {Result, ExpectedTransitionResult} of
            {{ok, NewStagesRegistry}, {ok, _}} ->
                ?assertEqual(
                    {ok, inject_storage_stages(ExpectedNewStageDetails, DummyRetiredStorages)},
                    support_stage:lookup_details(NewStagesRegistry, ProviderId)
                ),
                % check that calling the corresponding high-level operation gives the same result
                ?assertEqual(Result, call_high_level_operation(
                    StageRegistry, ProviderId, StorageId, PrevStorageStage, TransitionTo
                )),
                % make sure that only the stage details of subject provider have changed
                ?assertEqual(
                    maps:without([ProviderId], StageRegistry),
                    maps:without([ProviderId], NewStagesRegistry)
                ),
                ?assertEqual(NewStagesRegistry, support_stage:registry_from_json(json_utils:decode(
                    json_utils:encode(support_stage:registry_to_json(NewStagesRegistry))
                )));
            _ ->
                ok
        end,

        % Returns true if the check was successful
        CompareResult
    end, ?ALL_STORAGE_STATES).


compare_transition_result(ProviderId, StorageId, TransitionTo, ExpectedResult, ActualResult) ->
    case ExpectedResult =:= ActualResult of
        true ->
            true;
        false ->
            io:format(user,
                "~nStage transition result different than expected~n"
                "ProviderId: ~s~n"
                "StorageId: ~s~n"
                "TransitionTo: ~p~n"
                "Expected: ~p~n"
                "Got: ~p~n",
                [ProviderId, StorageId, TransitionTo, ExpectedResult, ActualResult]
            ),
            false
    end.


inject_storage_stages(StageDetails, StorageStages) ->
    StageDetails#support_stage_details{
        per_storage = maps:merge(StageDetails#support_stage_details.per_storage, StorageStages)
    }.


% Tests use the low level API to test all transitions, including illegal ones.
% Legal transitions can be mapped to high level operations, as below, this way
% the high level API can also be tested.
call_high_level_operation(Registry, ProviderId, StorageId, none, joining) ->
    support_stage:init_support(Registry, ProviderId, StorageId);
call_high_level_operation(Registry, ProviderId, StorageId, retired, joining) ->
    support_stage:init_support(Registry, ProviderId, StorageId);
call_high_level_operation(Registry, ProviderId, StorageId, joining, active) ->
    support_stage:finalize_support(Registry, ProviderId, StorageId);
call_high_level_operation(Registry, ProviderId, StorageId, joining, {resizing, 0}) ->
    support_stage:init_unsupport(Registry, ProviderId, StorageId);

call_high_level_operation(Registry, ProviderId, StorageId, active, {resizing, Target}) when Target > 0 ->
    support_stage:init_resize(Registry, ProviderId, StorageId, Target);
call_high_level_operation(Registry, ProviderId, StorageId, {resizing, _}, {resizing, Target}) when Target > 0 ->
    support_stage:init_resize(Registry, ProviderId, StorageId, Target);
call_high_level_operation(Registry, ProviderId, StorageId, {resizing, Target}, active) when Target > 0 ->
    support_stage:finalize_resize(Registry, ProviderId, StorageId);

call_high_level_operation(Registry, ProviderId, StorageId, active, {resizing, 0}) ->
    support_stage:init_unsupport(Registry, ProviderId, StorageId);
call_high_level_operation(Registry, ProviderId, StorageId, {resizing, _}, {resizing, 0}) ->
    support_stage:init_unsupport(Registry, ProviderId, StorageId);
call_high_level_operation(Registry, ProviderId, StorageId, {resizing, 0}, purging) ->
    support_stage:complete_unsupport_resize(Registry, ProviderId, StorageId);
call_high_level_operation(Registry, ProviderId, StorageId, purging, retiring) ->
    support_stage:complete_unsupport_purge(Registry, ProviderId, StorageId);
call_high_level_operation(Registry, ProviderId, StorageId, retiring, retired) ->
    support_stage:finalize_unsupport(Registry, ProviderId, StorageId).


-endif.
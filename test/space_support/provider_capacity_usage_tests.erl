%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of storage_capacity_usage module.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_capacity_usage_tests).
-author("Lukasz Opiola").

-include("space_support/provider_capacity_usage.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("space_support/support_stage.hrl").
-include("errors.hrl").

-define(PR_A, <<"provider-a">>).
-define(PR_B, <<"provider-b">>).

-define(ST_A, <<"storage-a">>).
-define(ST_B1, <<"storage-b1">>).
-define(ST_B2, <<"storage-b2">>).

-define(SIZE_A, 5078361234).
-define(SIZE_B1, 1047813692).
-define(SIZE_B2, 30929374910).

%%%===================================================================
%%% Test functions
%%%===================================================================

bad_reports_are_declined_test() ->
    new_registry(),
    register_support(?PR_A, ?ST_A, ?SIZE_A),

    ?assertException(error, badarg, consume_report(?PR_A, #{?ST_A => -1})),
    ?assertException(error, badarg, consume_report(?PR_A, #{?ST_A => <<"abcd">>})),
    ?assertException(error, badarg, consume_report(?PR_A, #{?ST_A => #{<<"key">> => <<"value">>}})).


storage_capacity_registry_lifecycle_test() ->
    new_registry(),

    register_support(?PR_A, ?ST_A, ?SIZE_A),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_A, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),


    consume_report(?PR_A, #{?ST_A => 0}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = 0, total = ?SIZE_A, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    consume_report(?PR_A, #{?ST_A => ?SIZE_A + 3}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = true, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?SIZE_A + 3, total = ?SIZE_A, overfull = true}
            }}
        },
        transitioned_to_overfull = [?PR_A],
        transitioned_from_overfull = []
    })),

    register_support(?PR_B, ?ST_B1, ?SIZE_B1),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = true, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?SIZE_A + 3, total = ?SIZE_A, overfull = true}
            }},
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_B1, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    % superfluous entries in the report should be ignored
    consume_report(?PR_B, #{?ST_B1 => 78, ?ST_B2 => 1272}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = true, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?SIZE_A + 3, total = ?SIZE_A, overfull = true}
            }},
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 78, total = ?SIZE_B1, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    % empty report should be accepted and should not change anything in the registry
    consume_report(?PR_B, #{}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = true, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?SIZE_A + 3, total = ?SIZE_A, overfull = true}
            }},
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 78, total = ?SIZE_B1, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    register_unsupport(?PR_A, ?ST_A),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 78, total = ?SIZE_B1, overfull = false}
            }}
        },
        % no transition should be reported if the provider no longer supports the space, even though it was overfull
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    register_support(?PR_B, ?ST_B2, ?SIZE_B2),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 78, total = ?SIZE_B1, overfull = false},
                ?ST_B2 => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_B2, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    register_support(?PR_A, ?ST_A, ?SIZE_A),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_A, overfull = false}
            }},
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 78, total = ?SIZE_B1, overfull = false},
                ?ST_B2 => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_B2, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    % missing entries should not cause the report to fail
    consume_report(?PR_B, #{?ST_B1 => ?SIZE_B1 - 2}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_A, overfull = false}
            }},
            ?PR_B => #provider_capacity_usage{overfull = true, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = ?SIZE_B1 - 2, total = ?SIZE_B1, overfull = true},
                ?ST_B2 => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_B2, overfull = false}
            }}
        },
        transitioned_to_overfull = [?PR_B],
        transitioned_from_overfull = []
    })),

    consume_report(?PR_A, #{?ST_A => 666}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = 666, total = ?SIZE_A, overfull = false}
            }},
            ?PR_B => #provider_capacity_usage{overfull = true, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = ?SIZE_B1 - 2, total = ?SIZE_B1, overfull = true},
                ?ST_B2 => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_B2, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    consume_report(?PR_B, #{?ST_B1 => 90909, ?ST_B2 => 1234567}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = 666, total = ?SIZE_A, overfull = false}
            }},
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 90909, total = ?SIZE_B1, overfull = false},
                ?ST_B2 => #storage_capacity_usage{used = 1234567, total = ?SIZE_B2, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = [?PR_B]
    })),

    consume_report(?PR_B, #{?ST_B1 => 80808, ?ST_B2 => ?SIZE_B2 - 190}),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = 666, total = ?SIZE_A, overfull = false}
            }},
            ?PR_B => #provider_capacity_usage{overfull = true, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 80808, total = ?SIZE_B1, overfull = false},
                ?ST_B2 => #storage_capacity_usage{used = ?SIZE_B2 - 190, total = ?SIZE_B2, overfull = true}
            }}
        },
        transitioned_to_overfull = [?PR_B],
        transitioned_from_overfull = []
    })),

    register_unsupport(?PR_B, ?ST_B2),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = 666, total = ?SIZE_A, overfull = false}
            }},
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B1 => #storage_capacity_usage{used = 80808, total = ?SIZE_B1, overfull = false}
            }}
        },
        % an overfull storage has been removed, which should cause the provider to transition
        transitioned_to_overfull = [],
        transitioned_from_overfull = [?PR_B]
    })),

    register_unsupport(?PR_B, ?ST_B1),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = 666, total = ?SIZE_A, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    register_unsupport(?PR_A, ?ST_A),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{},
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    register_support(?PR_A, ?ST_A, ?SIZE_A),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_A, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })),

    register_support(?PR_B, ?ST_B2, ?SIZE_B2),
    ?assert(check_update_result(#capacity_usage_registry_update_result{
        new_registry = #{
            ?PR_A => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_A => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_A, overfull = false}
            }},
            ?PR_B => #provider_capacity_usage{overfull = false, per_storage = #{
                ?ST_B2 => #storage_capacity_usage{used = ?UNKNOWN_USAGE_VALUE, total = ?SIZE_B2, overfull = false}
            }}
        },
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    })).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% Wrappers for the functions that modify the registry for simpler test code.
%% Previous registry state is stored in the process dictionary. Additionally,
%% the functions return the result wrapped in #registry_update_result{} record
%% including the new registry, which must be normally extracted for the
%% consecutive call. These wrappers do the extracting if needed.

new_registry() ->
    store_result(#capacity_usage_registry_update_result{new_registry = provider_capacity_usage:new_registry()}).


register_support(ProviderId, StorageId, SupportSize) ->
    store_result(provider_capacity_usage:register_support(get_current_registry(), ProviderId, StorageId, SupportSize)).


register_unsupport(ProviderId, StorageId) ->
    store_result(provider_capacity_usage:register_unsupport(get_current_registry(), ProviderId, StorageId)).


consume_report(ProviderId, BytesPerStore) ->
    Report = provider_capacity_usage:build_report(maps:keys(BytesPerStore), fun(StorageId) ->
        maps:get(StorageId, BytesPerStore)
    end),
    ?assertEqual(Report, provider_capacity_usage:report_from_json(json_utils:decode(
        json_utils:encode(provider_capacity_usage:report_to_json(Report))
    ))),
    store_result(provider_capacity_usage:consume_report(get_current_registry(), ProviderId, Report)).


check_update_result(ExpectedResult) ->
    ActualResult = get_previous_result(),

    ActualNormalized = ActualResult#capacity_usage_registry_update_result{
        transitioned_from_overfull = lists:sort(ActualResult#capacity_usage_registry_update_result.transitioned_from_overfull),
        transitioned_to_overfull = lists:sort(ActualResult#capacity_usage_registry_update_result.transitioned_to_overfull)
    },

    ExpectedNormalized = ExpectedResult#capacity_usage_registry_update_result{
        transitioned_from_overfull = lists:sort(ExpectedResult#capacity_usage_registry_update_result.transitioned_from_overfull),
        transitioned_to_overfull = lists:sort(ExpectedResult#capacity_usage_registry_update_result.transitioned_to_overfull)
    },

    ActualRegistry = ActualResult#capacity_usage_registry_update_result.new_registry,
    ?assertEqual(ActualRegistry, provider_capacity_usage:registry_from_json(json_utils:decode(
        json_utils:encode(provider_capacity_usage:registry_to_json(ActualRegistry))
    ))),

    case ActualNormalized =:= ExpectedNormalized of
        true ->
            maps:map(fun(ProviderId, ProviderUsage) ->
                % make sure the lookup function works correctly
                ?assertEqual({ok, ProviderUsage}, provider_capacity_usage:lookup_by_provider(ActualRegistry, ProviderId))
            end, ActualRegistry),
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


% macros to avoid a ton of variables - at each step we operate on the
% previously stored registry and store the new one after modification
store_result(RegistryUpdateResult) ->
    put(registry_update_result, RegistryUpdateResult).


get_previous_result() ->
    get(registry_update_result).


get_current_registry() ->
    UpdateResult = get_previous_result(),
    UpdateResult#capacity_usage_registry_update_result.new_registry.

-endif.
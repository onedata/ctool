%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of space_support module.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_sync_progress_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(PROVIDER_ALPHA, <<"provider-alpha">>).
-define(PROVIDER_BETA, <<"provider-beta">>).
-define(PROVIDER_GAMMA, <<"provider-gamma">>).


update_lookup_test() ->
    SyncProgressAlpha = #{
        ?PROVIDER_ALPHA => {21712, 1526731615},
        ?PROVIDER_BETA => {34, 1526743875}
    },
    SyncProgressBeta = #{
        ?PROVIDER_ALPHA => {18432, 1526725321},
        ?PROVIDER_BETA => {70, 1526743994}
    },
    AllProviders = [?PROVIDER_ALPHA, ?PROVIDER_BETA],
    PerProviderOne = provider_sync_progress:update_for_provider(
        #{}, AllProviders, ?PROVIDER_ALPHA, SyncProgressAlpha
    ),
    PerProviderBoth = provider_sync_progress:update_for_provider(
        PerProviderOne, AllProviders, ?PROVIDER_BETA, SyncProgressBeta
    ),

    ?assertEqual({ok, SyncProgressAlpha}, provider_sync_progress:lookup_by_provider(PerProviderBoth, ?PROVIDER_ALPHA)),
    ?assertEqual({ok, SyncProgressBeta}, provider_sync_progress:lookup_by_provider(PerProviderBoth, ?PROVIDER_BETA)).


coalesce_all_test() ->
    PerProvider = #{
        ?PROVIDER_ALPHA => #{},
        ?PROVIDER_GAMMA => #{
            ?PROVIDER_ALPHA => {342, 1536736519},
            ?PROVIDER_GAMMA => {14, 1536786502}
        }
    },
    ?assertEqual(
        #{
            ?PROVIDER_ALPHA => #{
                ?PROVIDER_ALPHA => {1, 0},
                ?PROVIDER_BETA => {1, 0},
                ?PROVIDER_GAMMA => {1, 0}
            },
            ?PROVIDER_BETA => #{
                ?PROVIDER_ALPHA => {1, 0},
                ?PROVIDER_BETA => {1, 0},
                ?PROVIDER_GAMMA => {1, 0}
            },
            ?PROVIDER_GAMMA => #{
                ?PROVIDER_ALPHA => {342, 1536736519},
                ?PROVIDER_BETA => {1, 0},
                ?PROVIDER_GAMMA => {14, 1536786502}
            }
        },
        provider_sync_progress:coalesce_all(PerProvider, [?PROVIDER_ALPHA, ?PROVIDER_BETA, ?PROVIDER_GAMMA])
    ).


coalesce_during_update_test() ->
    SyncProgress = #{
        ?PROVIDER_ALPHA => {21712, 1526731615},
        ?PROVIDER_BETA => {34, 1526743875},
        <<"superfluous">> => {6311, 1527483021}
    },
    PerProvider = #{
        ?PROVIDER_ALPHA => #{},
        ?PROVIDER_BETA => #{},
        ?PROVIDER_GAMMA => #{}
    },
    ?assertEqual(
        #{
            ?PROVIDER_ALPHA => #{
                ?PROVIDER_ALPHA => {21712, 1526731615},
                ?PROVIDER_BETA => {34, 1526743875},
                ?PROVIDER_GAMMA => {1, 0}
            },
            ?PROVIDER_BETA => #{},
            ?PROVIDER_GAMMA => #{}
        },
        provider_sync_progress:update_for_provider(
            PerProvider, [?PROVIDER_ALPHA, ?PROVIDER_BETA, ?PROVIDER_GAMMA], ?PROVIDER_ALPHA, SyncProgress
        )
    ).


inspect_progress_test() ->
    SyncProgressAlpha = #{
        ?PROVIDER_ALPHA => {1000, 1526731615},
        % It is possible that Alpha knows a higher seq for Beta due to reporting inertia
        ?PROVIDER_BETA => {795, 1526743875}
    },
    SyncProgressBeta = #{
        ?PROVIDER_ALPHA => {813, 1526725321},
        ?PROVIDER_BETA => {786, 1526743994}
    },
    AllProviders = [?PROVIDER_ALPHA, ?PROVIDER_BETA],
    PerProviderOne = provider_sync_progress:update_for_provider(
        #{}, AllProviders, ?PROVIDER_ALPHA, SyncProgressAlpha
    ),
    PerProviderBoth = provider_sync_progress:update_for_provider(
        PerProviderOne, AllProviders, ?PROVIDER_BETA, SyncProgressBeta
    ),

    ?assertEqual({1000, 1000}, provider_sync_progress:inspect_progress_between(PerProviderBoth, ?PROVIDER_ALPHA, ?PROVIDER_ALPHA)),
    ?assertEqual({795, 795}, provider_sync_progress:inspect_progress_between(PerProviderBoth, ?PROVIDER_ALPHA, ?PROVIDER_BETA)),

    ?assertEqual({813, 1000}, provider_sync_progress:inspect_progress_between(PerProviderBoth, ?PROVIDER_BETA, ?PROVIDER_ALPHA)),
    ?assertEqual({786, 786}, provider_sync_progress:inspect_progress_between(PerProviderBoth, ?PROVIDER_BETA, ?PROVIDER_BETA)).


decode_encode_test() ->
    SyncProgressAlpha = #{
        ?PROVIDER_ALPHA => {21712, 1526731615},
        ?PROVIDER_BETA => {34, 1526743875},
        ?PROVIDER_GAMMA => {6311, 1527483021}
    },
    SyncProgressBeta = #{
        ?PROVIDER_ALPHA => {18432, 1526725321},
        ?PROVIDER_BETA => {70, 1526743994},
        ?PROVIDER_GAMMA => {5612, 1527488921}
    },
    SyncProgressGamma = #{
        ?PROVIDER_ALPHA => {20654, 1526722315},
        ?PROVIDER_BETA => {70, 1526743994},
        ?PROVIDER_GAMMA => {9800, 1527494531}
    },
    AllProviders = [?PROVIDER_ALPHA, ?PROVIDER_BETA, ?PROVIDER_GAMMA],

    lists:foreach(fun(SyncProgress) ->
        ?assertEqual(SyncProgress, provider_sync_progress:from_json(json_utils:decode(
            json_utils:encode(provider_sync_progress:to_json(SyncProgress))
        )))
    end, [SyncProgressAlpha, SyncProgressBeta, SyncProgressGamma]),

    PerProvider = lists:foldl(fun({ProviderId, SyncProgress}, AccPerProvider) ->
        provider_sync_progress:update_for_provider(
            AccPerProvider, AllProviders, ProviderId, SyncProgress
        )
    end, #{}, [
        {?PROVIDER_ALPHA, SyncProgressAlpha},
        {?PROVIDER_BETA, SyncProgressBeta},
        {?PROVIDER_GAMMA, SyncProgressGamma}
    ]),
    ?assertEqual(PerProvider, provider_sync_progress:per_provider_from_json(json_utils:decode(
        json_utils:encode(provider_sync_progress:per_provider_to_json(PerProvider))
    ))).


-endif.

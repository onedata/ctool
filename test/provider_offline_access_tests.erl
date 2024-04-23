%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of provider_offline_access module.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_offline_access_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("aai/aai.hrl").
-include("graph_sync/gri.hrl").

-define(PROVIDER_ID, <<"some-provider-id">>).
-define(TTL, 123456).

-define(RAND_STR, str_utils:rand_hex(6)).
-define(RAND_OBJECTID,
    element(2, {ok, _} = file_id:guid_to_objectid(file_id:pack_guid(?RAND_STR, ?RAND_STR)))
).
-define(RAND_PATH, filename:join(
    [<<"/">>, ?RAND_STR] ++ lists_utils:random_sublist([?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR])
)).

-define(ALLOWED_OFFLINE_ACCESS_API, [
    {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_user, <<"*">>, <<"*">>, '*')},
    {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"*">>, {<<"idp_access_token">>, <<"*">>}, '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_group, <<"*">>, instance, shared)},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_space, <<"*">>, <<"*">>, '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_provider, <<"*">>, <<"*">>, '*')}
]).

build_token_caveats_test_() ->
    {foreach,
        fun() ->
            clock_freezer_mock:setup_for_eunit([?MODULE]),
            node_cache:init()
        end,
        fun(_) ->
            clock_freezer_mock:teardown_for_eunit(),
            node_cache:destroy()
        end,
        [
            {"build token caveats 1", fun build_token_caveats_1/0},
            {"build token caveats 2", fun build_token_caveats_2/0},
            {"build token caveats 3", fun build_token_caveats_3/0},
            {"build token caveats 4", fun build_token_caveats_4/0},
            {"build token caveats 5", fun build_token_caveats_5/0}
        ]
    }.

build_token_caveats_1() ->
    run_tests([
        % a token with no caveats
    ], [
        #cv_time{valid_until = clock_freezer_mock:current_time_seconds() + ?TTL},
        #cv_service{whitelist = [?SERVICE(?OP_WORKER, ?PROVIDER_ID)]},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, ?PROVIDER_ID)]},
        #cv_api{whitelist = ?ALLOWED_OFFLINE_ACCESS_API}
    ]).

build_token_caveats_2() ->
    run_tests([
        #cv_time{valid_until = 923786110239},
        #cv_ip{whitelist = [{34, 59, 102, 32}]},
        #cv_asn{whitelist = [9821, 56, 904]},
        #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
        #cv_region{type = blacklist, list = [<<"Europe">>, <<"Oceania">>]},
        #cv_interface{interface = rest},
        #cv_interface{interface = graphsync}
    ], [
        #cv_time{valid_until = clock_freezer_mock:current_time_seconds() + ?TTL},
        #cv_service{whitelist = [?SERVICE(?OP_WORKER, ?PROVIDER_ID)]},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, ?PROVIDER_ID)]},
        #cv_interface{interface = rest},
        #cv_interface{interface = graphsync},
        #cv_api{whitelist = ?ALLOWED_OFFLINE_ACCESS_API}
    ]).

build_token_caveats_3() ->
    run_tests([
        #cv_time{valid_until = 57261902434239},
        #cv_service{whitelist = [?SERVICE(?OP_WORKER, <<"*">>)]},
        #cv_time{valid_until = 57261902434238},
        #cv_time{valid_until = 57261902434237},
        #cv_interface{interface = oneclient},
        #cv_api{whitelist = [
            {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"123">>, <<"instance">>, private)}
        ]}
    ], [
        #cv_time{valid_until = clock_freezer_mock:current_time_seconds() + ?TTL},
        #cv_service{whitelist = [?SERVICE(?OP_WORKER, ?PROVIDER_ID)]},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, ?PROVIDER_ID)]},
        #cv_api{whitelist = ?ALLOWED_OFFLINE_ACCESS_API},
        #cv_interface{interface = oneclient},
        #cv_api{whitelist = [
            {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"123">>, <<"instance">>, private)}
        ]}
    ]).

build_token_caveats_4() ->
    ObjectIdCaveat1 = #cv_data_objectid{whitelist = [?RAND_OBJECTID, ?RAND_OBJECTID]},
    ObjectIdCaveat2 = #cv_data_objectid{whitelist = [?RAND_OBJECTID]},
    run_tests([
        #cv_time{valid_until = 57261902434239},
        #cv_consumer{whitelist = [?SUB(user, ?RAND_STR)]},
        #cv_interface{interface = graphsync},
        #cv_data_readonly{},
        ObjectIdCaveat1,
        ObjectIdCaveat2
    ], [
        #cv_time{valid_until = clock_freezer_mock:current_time_seconds() + ?TTL},
        #cv_service{whitelist = [?SERVICE(?OP_WORKER, ?PROVIDER_ID)]},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, ?PROVIDER_ID)]},
        #cv_api{whitelist = ?ALLOWED_OFFLINE_ACCESS_API},
        #cv_interface{interface = graphsync},
        #cv_data_readonly{},
        ObjectIdCaveat1,
        ObjectIdCaveat2
    ]).

build_token_caveats_5() ->
    DataPathCaveat = #cv_data_path{whitelist = [?RAND_PATH, ?RAND_PATH]},
    ObjectIdCaveat = #cv_data_objectid{whitelist = [?RAND_OBJECTID, ?RAND_OBJECTID]},
    run_tests([
        #cv_time{valid_until = 57261902434239},
        #cv_ip{whitelist = [{34, 59, 102, 32}]},
        #cv_asn{whitelist = [9821, 56, 904]},
        #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
        #cv_region{type = blacklist, list = [<<"Europe">>, <<"Oceania">>]},
        DataPathCaveat,
        #cv_interface{interface = oneclient},
        ObjectIdCaveat,
        #cv_api{whitelist = [
            {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"123">>, <<"instance">>, private)}
        ]}
    ], [
        #cv_time{valid_until = clock_freezer_mock:current_time_seconds() + ?TTL},
        #cv_service{whitelist = [?SERVICE(?OP_WORKER, ?PROVIDER_ID)]},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, ?PROVIDER_ID)]},
        #cv_api{whitelist = ?ALLOWED_OFFLINE_ACCESS_API},
        DataPathCaveat,
        #cv_interface{interface = oneclient},
        ObjectIdCaveat,
        #cv_api{whitelist = [
            {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"123">>, <<"instance">>, private)}
        ]}
    ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_tests(OriginalCaveats, ExpectedOfflineAccessCaveats) ->
    BuiltCaveats = provider_offline_access:build_token_caveats(OriginalCaveats, ?PROVIDER_ID, ?TTL),
    ?assert(compare_caveats(
        ExpectedOfflineAccessCaveats,
        BuiltCaveats
    )),
    % offline access token caveats based off another offline access token should be the same
    % (apart from the expiration time, but the TTL should be the same)
    TimePassed = rand:uniform(99999),
    clock_freezer_mock:simulate_seconds_passing(TimePassed),
    PreviousTimeCaveat = lists:keyfind(cv_time, 1, BuiltCaveats),
    ExpectedConsecutiveTimeCaveat = #cv_time{valid_until = PreviousTimeCaveat#cv_time.valid_until + TimePassed},
    ?assert(compare_caveats(
        lists:keyreplace(cv_time, 1, BuiltCaveats, ExpectedConsecutiveTimeCaveat),
        provider_offline_access:build_token_caveats(BuiltCaveats, ?PROVIDER_ID, ?TTL)
    )).


compare_caveats(CaveatsA, CaveatsB) ->
    lists:sort(CaveatsA) =:= lists:sort(CaveatsB).


-endif.

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of api_auth module.
%%% @end
%%%-------------------------------------------------------------------
-module(api_auth_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("aai/aai.hrl").
-include("errors.hrl").
-include("graph_sync/gri.hrl").

-define(SPACE_ALPHA, <<"d409171e7ba30b60d332d45c857745d4">>).
-define(SPACE_GAMMA, <<"bdee07d32260a56d159a980ca0d64357">>).
-define(SPACE_DELTA, <<"0d64357a778c44679dd816329e3d6527">>).

-define(RAND_STR, str_utils:rand_hex(6)).
-define(RAND_PATH_IN_SPACE(SpaceId), filename:join(
    [<<"/">>, SpaceId] ++ lists_utils:random_sublist([?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR])
)).
-define(RAND_OBJECTID_IN_SPACE(SpaceId),
    element(2, {ok, _} = file_id:guid_to_objectid(file_id:pack_guid(?RAND_STR, SpaceId)))
).

-define(DATA_ACCESS_CAVEATS_EXAMPLES, [
    #cv_interface{interface = oneclient},
    #cv_data_readonly{},
    #cv_data_path{whitelist = [?RAND_PATH_IN_SPACE(?SPACE_ALPHA), ?RAND_PATH_IN_SPACE(?SPACE_GAMMA)]},
    #cv_data_objectid{whitelist = [?RAND_OBJECTID_IN_SPACE(?SPACE_DELTA)]}
]).


% Caveats that are not relevant in the context of API (do not cause limitations)
-define(IRRELEVANT_CAVEATS_EXAMPLES, lists:flatten([
    #cv_time{valid_until = 923786110239},
    #cv_scope{scope = identity_token},
    #cv_ip{whitelist = [{34, 59, 102, 32}]},
    #cv_asn{whitelist = [9821, 56, 904]},
    #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
    #cv_region{type = blacklist, list = [<<"Europe">>, <<"Oceania">>]},
    #cv_interface{interface = rest},
    #cv_interface{interface = graphsync}
])).

-record(caveat_example, {
    should_verify :: true | false,
    caveat :: caveats:caveat()
}).

-record(testcase, {
    service :: onedata:service(),
    operation :: cv_api:operation(),
    gri :: gri:gri(),
    caveat_examples :: [#caveat_example{}]
}).


check_authorization_test() ->
    lists:foreach(fun check_authorization_test/1, testcases()).


% For each testcase, generates possible combination of caveats (based on caveat_examples)
% and checks if the result of check_authorization function is as expected.
check_authorization_test(T = #testcase{service = Service, operation = Operation, gri = GRI, caveat_examples = CaveatExamples}) ->
    TestCombinations = powerset(CaveatExamples),
    % Each combination includes a random subset of caveat examples
    lists:foreach(fun(CaveatExamplesSubset) ->
        ApiLimitingCaveats = [Example#caveat_example.caveat || Example <- CaveatExamplesSubset],
        IrrelevantCaveats = lists_utils:random_sublist(?IRRELEVANT_CAVEATS_EXAMPLES),
        ExpUnverifiedCaveats = lists:filtermap(fun
            (#caveat_example{should_verify = false, caveat = Caveat}) ->
                {true, Caveat};
            (_) ->
                false
        end, CaveatExamplesSubset),
        Auth = #auth{
            subject = ?SUB(user, <<"123">>),
            caveats = lists_utils:shuffle(ApiLimitingCaveats ++ IrrelevantCaveats)
        },
        case ExpUnverifiedCaveats of
            [] ->
                ?assertEqual(ok, api_auth:check_authorization(Auth, Service, Operation, GRI));
            _ ->
                ?assertMatch(
                    ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_)),
                    api_auth:check_authorization(Auth, Service, Operation, GRI)
                ),
                ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat)) = api_auth:check_authorization(
                    Auth, Service, Operation, GRI
                ),
                ?assert(lists:member(UnverifiedCaveat, ExpUnverifiedCaveats))
        end
    end, TestCombinations).


testcases() -> [
    #testcase{
        service = ?OZ_WORKER,
        operation = get,
        gri = ?GRI(od_user, <<"123">>, instance, private),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {all, get, ?GRI_PATTERN(od_user, '*', '*', private)}
                ]}
            },
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"123">>, instance, private)}
                ]}
            },
            gen_data_access_caveat_examples(true),
            gen_service_caveat_examples([?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER, ?OP_PANEL], true)
        ])
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = delete,
        gri = ?GRI(od_user, <<"123">>, instance, private),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {all, delete, ?GRI_PATTERN(od_user, '*', '*', private)}
                ]}
            },
            gen_data_access_caveat_examples(false),
            gen_service_caveat_examples([?OZ_WORKER], true),
            gen_service_caveat_examples([?OZ_PANEL, ?OP_WORKER, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = update,
        gri = ?GRI(od_user, <<"123">>, instance, private),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', '*', '*', '*')}
                ]}
            },
            gen_data_access_caveat_examples(false),
            gen_service_caveat_examples([?OZ_WORKER], true),
            gen_service_caveat_examples([?OZ_PANEL, ?OP_WORKER, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = get,
        gri = ?GRI(od_space, <<"ghj">>, instance, private),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_WORKER, get, ?GRI_PATTERN(od_user, <<"123">>, instance, private)}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_interface{interface = oneclient}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_data_readonly{}
            },
            % Below caveats limit the allowed spaces to a certain one, which
            % will not match the above id of <<"123">>
            #caveat_example{
                should_verify = false,
                caveat = #cv_data_path{whitelist = [?RAND_PATH_IN_SPACE(?SPACE_DELTA)]}
            },
            #caveat_example{
                should_verify = false,
                caveat = #cv_data_objectid{whitelist = [?RAND_OBJECTID_IN_SPACE(?SPACE_ALPHA)]}
            },
            gen_service_caveat_examples([?OZ_WORKER, ?OP_WORKER], true),
            gen_service_caveat_examples([?OZ_PANEL, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = get,
        gri = ?GRI(od_space, ?SPACE_ALPHA, instance, protected),
        caveat_examples = lists:flatten([
            % Only caveats that have paths / objectids including the
            % SPACE_ALPHA should be verified
            #caveat_example{
                should_verify = false,
                caveat = #cv_data_path{whitelist = [
                    <<"noncanonical-bad-path">>
                ]}
            },
            #caveat_example{
                should_verify = false,
                caveat = #cv_data_objectid{whitelist = [
                    <<"bad-objectid">>
                ]}
            },

            #caveat_example{
                should_verify = false,
                caveat = #cv_data_path{whitelist = [
                    ?RAND_PATH_IN_SPACE(?SPACE_GAMMA),
                    ?RAND_PATH_IN_SPACE(?SPACE_DELTA)
                ]}
            },
            #caveat_example{
                should_verify = false,
                caveat = #cv_data_objectid{whitelist = [
                    ?RAND_OBJECTID_IN_SPACE(?SPACE_GAMMA),
                    ?RAND_OBJECTID_IN_SPACE(?SPACE_DELTA)
                ]}
            },

            #caveat_example{
                should_verify = true,
                caveat = #cv_data_path{whitelist = [
                    ?RAND_PATH_IN_SPACE(?SPACE_ALPHA),
                    <<"noncanonical-bad-path">>
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_data_objectid{whitelist = [
                    ?RAND_OBJECTID_IN_SPACE(?SPACE_ALPHA),
                    <<"bad-objectid">>
                ]}
            },

            #caveat_example{
                should_verify = true,
                caveat = #cv_data_path{whitelist = [
                    ?RAND_PATH_IN_SPACE(?SPACE_GAMMA),
                    ?RAND_PATH_IN_SPACE(?SPACE_DELTA),
                    ?RAND_PATH_IN_SPACE(?SPACE_ALPHA)
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_data_objectid{whitelist = [
                    ?RAND_OBJECTID_IN_SPACE(?SPACE_GAMMA),
                    ?RAND_OBJECTID_IN_SPACE(?SPACE_DELTA),
                    ?RAND_OBJECTID_IN_SPACE(?SPACE_ALPHA)
                ]}
            },
            gen_service_caveat_examples([?OZ_WORKER, ?OP_WORKER], true),
            gen_service_caveat_examples([?OZ_PANEL, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = create,
        gri = ?GRI(od_user, undefined, instance, shared),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {all, all, ?GRI_PATTERN('*', '*', '*', '*')}
                ]}
            },
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_WORKER, create, ?GRI_PATTERN(od_group, undefined, instance, shared)}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OZ_WORKER, create, ?GRI_PATTERN(od_user, '*', instance, shared)}
                ]}
            },
            gen_data_access_caveat_examples(false),
            gen_service_caveat_examples([?OZ_WORKER], true),
            gen_service_caveat_examples([?OZ_PANEL, ?OP_WORKER, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OP_WORKER,
        operation = create,
        gri = ?GRI(op_file, <<"fileid">>, metadata, private),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, update, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_WORKER, delete, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_WORKER, all, ?GRI_PATTERN(od_user, '*', '*', '*')},
                    {all, all, ?GRI_PATTERN(op_file, <<"fileid">>, {'*', '*'}, '*')},
                    {?OP_WORKER, get, ?GRI_PATTERN('*', <<"badid">>, '*', protected)},
                    {?OP_WORKER, get, ?GRI_PATTERN(op_file, <<"spaceid">>, groups, protected)},
                    {?OP_WORKER, create, ?GRI_PATTERN(op_file, <<"other">>, '*', '*')}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, update, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_WORKER, delete, ?GRI_PATTERN('*', '*', '*', '*')},
                    {all, create, ?GRI_PATTERN(op_file, <<"fileid">>, '*', '*')},
                    {?OP_WORKER, all, ?GRI_PATTERN(od_user, '*', '*', '*')},
                    {all, all, ?GRI_PATTERN(op_file, <<"fileid">>, {'*', '*'}, '*')},
                    {?OP_WORKER, get, ?GRI_PATTERN('*', <<"badid">>, '*', protected)}
                ]}
            },
            gen_data_access_caveat_examples(true),
            gen_service_caveat_examples([?OP_WORKER], true),
            gen_service_caveat_examples([?OZ_WORKER, ?OZ_PANEL, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OP_WORKER,
        operation = get,
        gri = ?GRI(op_replica, <<"replicaid">>, instance, private),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, create, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, update, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_WORKER, create, ?GRI_PATTERN(op_replica, <<"replicaid">>, '*', '*')}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, get, ?GRI_PATTERN('*', '*', instance, private)},
                    {?OP_PANEL, update, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_WORKER, delete, ?GRI_PATTERN('*', '*', '*', '*')}
                ]}
            },
            gen_data_access_caveat_examples(false),
            gen_service_caveat_examples([?OP_WORKER], true),
            gen_service_caveat_examples([?OZ_WORKER, ?OZ_PANEL, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OZ_PANEL,
        operation = delete,
        gri = ?GRI(onp_host, undefined, instance, private),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, delete, ?GRI_PATTERN('*', <<"987">>, {'*', '*'}, private)},
                    {?OZ_PANEL, all, ?GRI_PATTERN(od_space, '*', '*', '*')},
                    {all, delete, ?GRI_PATTERN('*', '*', '*', protected)},
                    {?OZ_PANEL, delete, ?GRI_PATTERN('*', '*', {'*', <<"456">>}, '*')}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, all, ?GRI_PATTERN(od_space, '*', '*', '*')},
                    {all, delete, ?GRI_PATTERN('*', '*', instance, private)},
                    {?OZ_PANEL, delete, ?GRI_PATTERN('*', '*', {'*', <<"456">>}, '*')},
                    {all, all, ?GRI_PATTERN(od_space, <<"123">>, instance, private)}
                ]}
            },
            gen_data_access_caveat_examples(false),
            gen_service_caveat_examples([?OZ_PANEL], true),
            gen_service_caveat_examples([?OZ_WORKER, ?OP_WORKER, ?OP_PANEL], false)
        ])
    },

    #testcase{
        service = ?OP_PANEL,
        operation = update,
        gri = ?GRI(onp_ceph, undefined, {pool, <<"default">>}, auto),
        caveat_examples = lists:flatten([
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, update, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, delete, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', private)},
                    {?OP_PANEL, update, ?GRI_PATTERN(od_space, '*', '*', '*')},
                    {all, update, ?GRI_PATTERN('*', <<"123">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN(onp_ceph, undefined, {pool, <<"default">>}, public)}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, update, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, delete, ?GRI_PATTERN('*', '*', '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', private)},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', auto)},
                    {?OP_PANEL, update, ?GRI_PATTERN(od_space, '*', '*', '*')},
                    {all, update, ?GRI_PATTERN('*', <<"123">>, '*', '*')}
                ]}
            },
            gen_data_access_caveat_examples(false),
            gen_service_caveat_examples([?OP_PANEL], true),
            gen_service_caveat_examples([?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER], false)
        ])
    }
].


% Generates all possible sublists of all possible lengths
powerset([]) ->
    [[]];
powerset([H | T]) ->
    PT = powerset(T),
    [[H | X] || X <- PT] ++ PT.


gen_data_access_caveat_examples(ShouldVerify) ->
    lists:map(fun(Caveat) ->
        #caveat_example{should_verify = ShouldVerify, caveat = Caveat}
    end, ?DATA_ACCESS_CAVEATS_EXAMPLES).


gen_service_caveat_examples(ServiceTypes, ShouldVerify) ->
    lists:map(fun(ServiceType) ->
        TypesOnWhitelist = [ServiceType] ++ lists_utils:random_sublist(ServiceTypes),
        #caveat_example{should_verify = ShouldVerify, caveat = #cv_service{
            whitelist = [gen_service_spec(Type) || Type <- TypesOnWhitelist]
        }}
    end, ServiceTypes).


gen_service_spec(?OZ_WORKER) ->
    ?SERVICE(?OZ_WORKER, lists_utils:random_sublist([?ONEZONE_CLUSTER_ID, ?ID_WILDCARD]));
gen_service_spec(?OZ_PANEL) ->
    ?SERVICE(?OZ_PANEL, lists_utils:random_sublist([?ONEZONE_CLUSTER_ID, ?ID_WILDCARD]));
gen_service_spec(?OP_WORKER) ->
    ?SERVICE(?OP_WORKER, lists_utils:random_sublist([<<"providerId">>, ?ID_WILDCARD]));
gen_service_spec(?OP_PANEL) ->
    ?SERVICE(?OP_PANEL, lists_utils:random_sublist([<<"providerId">>, ?ID_WILDCARD])).


-endif.
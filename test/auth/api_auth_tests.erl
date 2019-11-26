%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of api_caveats module.
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
    [<<"/">>, SpaceId] ++ utils:random_sublist([?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR])
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
-define(API_LIMITING_CAVEATS_EXAMPLES, ?DATA_ACCESS_CAVEATS_EXAMPLES ++ [
    #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')}]}
]).
% Caveats that are not relevant in the context of API (do not cause limitations)
-define(IRRELEVANT_CAVEATS_EXAMPLES, [
    #cv_time{valid_until = 923786110239},
    #cv_authorization_none{},
    #cv_audience{whitelist = [?AUD(?OP_WORKER, <<"123">>)]},
    #cv_ip{whitelist = [{34, 59, 102, 32}]},
    #cv_asn{whitelist = [9821, 56, 904]},
    #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
    #cv_region{type = blacklist, list = [<<"Europe">>, <<"Oceania">>]},
    #cv_interface{interface = rest},
    #cv_interface{interface = graphsync}
]).

ensure_unlimited_test() ->
    lists:foreach(fun(_) ->
        RandApiLimiting = utils:random_sublist(?API_LIMITING_CAVEATS_EXAMPLES),
        RandIrrelevant = utils:random_sublist(?IRRELEVANT_CAVEATS_EXAMPLES),
        RandCaveats = case rand:uniform(2) of
            1 -> RandApiLimiting ++ RandIrrelevant;
            _ -> RandIrrelevant ++ RandApiLimiting
        end,
        Auth = #auth{subject = ?SUB(user, <<"123">>), caveats = RandCaveats},
        case RandApiLimiting of
            [] ->
                ?assertEqual(ok, api_auth:ensure_unlimited(Auth));
            [_ | _] ->
                ?assertMatch(
                    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(_),
                    api_auth:ensure_unlimited(Auth)
                ),
                ?ERROR_TOKEN_CAVEAT_UNVERIFIED(Cv) = api_auth:ensure_unlimited(Auth),
                ?assert(lists:member(Cv, RandApiLimiting))
        end
    end, lists:seq(1, 1000)).


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
check_authorization_test(#testcase{service = Service, operation = Operation, gri = GRI, caveat_examples = CaveatExamples}) ->
    TestCombinations = powerset(CaveatExamples),
    % Each combination includes a random subset of caveat examples
    lists:foreach(fun(CaveatExamplesSubset) ->
        ApiLimitingCaveats = [Example#caveat_example.caveat || Example <- CaveatExamplesSubset],
        IrrelevantCaveats = utils:random_sublist(?IRRELEVANT_CAVEATS_EXAMPLES),
        ExpUnverifiedCaveats = lists:filtermap(fun
            (#caveat_example{should_verify = false, caveat = Caveat}) ->
                {true, Caveat};
            (_) ->
                false
        end, CaveatExamplesSubset),
        Auth = #auth{
            subject = ?SUB(user, <<"123">>),
            caveats = utils:random_shuffle(ApiLimitingCaveats ++ IrrelevantCaveats)
        },
        case ExpUnverifiedCaveats of
            [] ->
                ?assertEqual(ok, api_auth:check_authorization(Auth, Service, Operation, GRI));
            _ ->
                ?assertMatch(
                    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(_),
                    api_auth:check_authorization(Auth, Service, Operation, GRI)
                ),
                ?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat) = api_auth:check_authorization(
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
        caveat_examples = [
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {all, get, ?GRI_PATTERN(od_user, <<"*">>, '*', private)}
                ]}
            },
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"123">>, instance, private)}
                ]}
            }
        ] ++ caveats_to_examples(?DATA_ACCESS_CAVEATS_EXAMPLES, true)
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = get,
        gri = ?GRI(od_space, <<"ghj">>, instance, private),
        caveat_examples = [
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
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
            }
        ]
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = get,
        gri = ?GRI(od_space, ?SPACE_ALPHA, instance, protected),
        caveat_examples = [
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
            }
        ]
    },

    #testcase{
        service = ?OZ_WORKER,
        operation = create,
        gri = ?GRI(od_user, undefined, instance, shared),
        caveat_examples = [
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {all, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')}
                ]}
            },
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OZ_WORKER, create, ?GRI_PATTERN(od_group, undefined, instance, shared)}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OZ_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"*">>, instance, shared)}
                ]}
            }
        ] ++ caveats_to_examples(?DATA_ACCESS_CAVEATS_EXAMPLES, false)
    },

    #testcase{
        service = ?OP_WORKER,
        operation = create,
        gri = ?GRI(op_file, <<"fileid">>, metadata, private),
        caveat_examples = [
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, update, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_WORKER, delete, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_WORKER, all, ?GRI_PATTERN(od_user, <<"*">>, '*', '*')},
                    {all, all, ?GRI_PATTERN(op_file, <<"fileid">>, {'*', <<"*">>}, '*')},
                    {?OP_WORKER, get, ?GRI_PATTERN('*', <<"badid">>, '*', protected)},
                    {?OP_WORKER, get, ?GRI_PATTERN(op_file, <<"spaceid">>, groups, protected)},
                    {?OP_WORKER, create, ?GRI_PATTERN(op_file, <<"other">>, '*', '*')}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, update, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_WORKER, delete, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {all, create, ?GRI_PATTERN(op_file, <<"fileid">>, '*', '*')},
                    {?OP_WORKER, all, ?GRI_PATTERN(od_user, <<"*">>, '*', '*')},
                    {all, all, ?GRI_PATTERN(op_file, <<"fileid">>, {'*', <<"*">>}, '*')},
                    {?OP_WORKER, get, ?GRI_PATTERN('*', <<"badid">>, '*', protected)}
                ]}
            }
        ] ++ caveats_to_examples(?DATA_ACCESS_CAVEATS_EXAMPLES, true)
    },

    #testcase{
        service = ?OP_WORKER,
        operation = get,
        gri = ?GRI(op_replica, <<"replicaid">>, instance, private),
        caveat_examples = [
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, create, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, update, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_WORKER, create, ?GRI_PATTERN(op_replica, <<"replicaid">>, '*', '*')}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OP_WORKER, get, ?GRI_PATTERN('*', <<"*">>, instance, private)},
                    {?OP_PANEL, update, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_WORKER, delete, ?GRI_PATTERN('*', <<"*">>, '*', '*')}
                ]}
            }
        ] ++ caveats_to_examples(?DATA_ACCESS_CAVEATS_EXAMPLES, false)
    },

    #testcase{
        service = ?OZ_PANEL,
        operation = delete,
        gri = ?GRI(od_cluster, <<"abc">>, {user, <<"123">>}, private),
        caveat_examples = [
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, delete, ?GRI_PATTERN('*', <<"987">>, {'*', <<"*">>}, private)},
                    {?OZ_PANEL, all, ?GRI_PATTERN(od_space, <<"*">>, '*', '*')},
                    {all, delete, ?GRI_PATTERN('*', <<"*">>, '*', protected)},
                    {?OZ_PANEL, delete, ?GRI_PATTERN('*', <<"*">>, {'*', <<"456">>}, '*')}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, all, ?GRI_PATTERN(od_space, <<"*">>, '*', '*')},
                    {all, delete, ?GRI_PATTERN('*', <<"*">>, {user, <<"*">>}, private)},
                    {?OZ_PANEL, delete, ?GRI_PATTERN('*', <<"*">>, {'*', <<"456">>}, '*')},
                    {all, all, ?GRI_PATTERN(od_space, <<"123">>, {user, <<"123">>}, private)}
                ]}
            }
        ] ++ caveats_to_examples(?DATA_ACCESS_CAVEATS_EXAMPLES, false)
    },

    #testcase{
        service = ?OP_PANEL,
        operation = update,
        gri = ?GRI(od_provider, <<"provid">>, spaces, auto),
        caveat_examples = [
            #caveat_example{
                should_verify = false,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, update, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, delete, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', private)},
                    {?OP_PANEL, update, ?GRI_PATTERN(od_space, <<"*">>, '*', '*')},
                    {all, update, ?GRI_PATTERN('*', <<"123">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN(od_provider, <<"provid">>, spaces, public)}
                ]}
            },
            #caveat_example{
                should_verify = true,
                caveat = #cv_api{whitelist = [
                    {?OZ_PANEL, update, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, delete, ?GRI_PATTERN('*', <<"*">>, '*', '*')},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', private)},
                    {?OP_PANEL, all, ?GRI_PATTERN('*', <<"*">>, '*', auto)},
                    {?OP_PANEL, update, ?GRI_PATTERN(od_space, <<"*">>, '*', '*')},
                    {all, update, ?GRI_PATTERN('*', <<"123">>, '*', '*')}
                ]}
            }
        ] ++ caveats_to_examples(?DATA_ACCESS_CAVEATS_EXAMPLES, false)
    }
].


% Generates all possible sublists of all possible lengths
powerset([]) ->
    [[]];
powerset([H | T]) ->
    PT = powerset(T),
    [[H | X] || X <- PT] ++ PT.


caveats_to_examples(Caveats, ShouldVerify) ->
    lists:map(fun(Caveat) ->
        #caveat_example{should_verify = ShouldVerify, caveat = Caveat}
    end, Caveats).


-endif.
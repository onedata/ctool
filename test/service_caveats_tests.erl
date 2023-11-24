%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of service_caveats module.
%%% @end
%%%-------------------------------------------------------------------
-module(service_caveats_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test/test_utils.hrl").
-include("aai/aai.hrl").


-define(CVS(Whitelist), #cv_service{whitelist = Whitelist}).

-record(test_spec, {
    service_caveats :: [service_caveat:cv_service()],
    service_type :: onedata:service(),
    all_service_ids :: [onedata:service_id()],
    expected_result :: [onedata:service_id()]
}).


match_allowed_service_ids_test() ->
    ?assert(run_test(#test_spec{
        service_caveats = [],
        service_type = ?OP_WORKER, all_service_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = [<<"1">>, <<"2">>, <<"3">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [?CVS([
            ?SERVICE(?OZ_PANEL, <<"1">>),
            ?SERVICE(?OZ_PANEL, <<"2">>),
            ?SERVICE(?OZ_PANEL, <<"3">>)
        ])],
        service_type = ?OZ_PANEL, all_service_ids = [<<"*">>],
        expected_result = [<<"1">>, <<"2">>, <<"3">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [?CVS([?SERVICE(?OZ_WORKER, <<"1">>)])],
        service_type = ?OP_WORKER, all_service_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = []
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [?CVS([?SERVICE(?OP_PANEL, <<"3">>)])],
        service_type = ?OP_PANEL, all_service_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = [<<"3">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [?CVS([?SERVICE(?OP_WORKER, <<"*">>)])],
        service_type = ?OP_WORKER, all_service_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = [<<"1">>, <<"2">>, <<"3">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [?CVS([
            ?SERVICE(?OZ_PANEL, <<"4">>),
            ?SERVICE(?OZ_PANEL, <<"5">>),
            ?SERVICE(?OZ_PANEL, <<"*">>)
        ])],
        service_type = ?OZ_PANEL, all_service_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = [<<"1">>, <<"2">>, <<"3">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [?CVS([
            ?SERVICE(?OZ_PANEL, <<"1">>),
            ?SERVICE(?OZ_PANEL, <<"2">>)
        ])],
        service_type = ?OZ_PANEL, all_service_ids = [<<"*">>],
        expected_result = [<<"1">>, <<"2">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [
            ?CVS([?SERVICE(?OP_WORKER, <<"1">>)]),
            ?CVS([?SERVICE(?OP_WORKER, <<"2">>)])
        ],
        service_type = ?OP_WORKER, all_service_ids = [<<"1">>, <<"2">>],
        expected_result = []
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [
            ?CVS([?SERVICE(?OZ_WORKER, <<"1">>)]),
            ?CVS([?SERVICE(?OZ_WORKER, <<"1">>), ?SERVICE(?OZ_WORKER, <<"2">>)]),
            ?CVS([?SERVICE(?OZ_WORKER, <<"1">>), ?SERVICE(?OZ_WORKER, <<"2">>), ?SERVICE(?OZ_WORKER, <<"3">>)])
        ],
        service_type = ?OZ_WORKER, all_service_ids = [<<"1">>, <<"2">>, <<"3">>, <<"4">>],
        expected_result = [<<"1">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [
            ?CVS([?SERVICE(?OP_PANEL, <<"1">>)]),
            ?CVS([?SERVICE(?OP_PANEL, <<"1">>), ?SERVICE(?OP_PANEL, <<"2">>)]),
            ?CVS([?SERVICE(?OP_PANEL, <<"1">>), ?SERVICE(?OP_PANEL, <<"2">>), ?SERVICE(?OP_PANEL, <<"3">>)]),
            ?CVS([?SERVICE(?OP_PANEL, <<"*">>), ?SERVICE(?OP_PANEL, <<"2">>), ?SERVICE(?OP_PANEL, <<"*">>)])
        ],
        service_type = ?OP_PANEL, all_service_ids = [<<"1">>, <<"2">>, <<"3">>, <<"4">>],
        expected_result = [<<"1">>]
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [
            ?CVS([?SERVICE(?OZ_PANEL, <<"1">>)]),
            ?CVS([?SERVICE(?OZ_PANEL, <<"2">>)])
        ],
        service_type = ?OZ_PANEL, all_service_ids = [<<"1">>, <<"2">>],
        expected_result = []
    })),
    ?assert(run_test(#test_spec{
        service_caveats = [
            ?CVS([?SERVICE(?OZ_PANEL, <<"1">>)]),
            ?CVS([?SERVICE(?OZ_PANEL, <<"2">>)])
        ],
        service_type = ?OZ_PANEL, all_service_ids = [<<"*">>],
        expected_result = []
    })).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
run_test(#test_spec{
    service_caveats = ServiceCaveats,
    service_type = ServiceType,
    all_service_ids = AllServiceIds,
    expected_result = ExpectedResult
}) ->
    % adding random entries for service types that are different than the queried one should
    % not impact the result
    ServiceCaveatsWithRandEntries = lists:map(fun(?CVS(Whitelist)) ->
        ?CVS(Whitelist ++ rand_service_specs_except_type(ServiceType))
    end, ServiceCaveats),

    ServiceCaveatsArg = case ServiceCaveatsWithRandEntries of
        % the API accepts either a single caveat or a list
        [Arg] -> ?RAND_CHOICE(Arg, [Arg]);
        Args -> Args
    end,

    Result = service_caveats:match_allowed_service_ids(ServiceCaveatsArg, ServiceType, AllServiceIds),
    eunit_utils:is_equal(Result, ExpectedResult, "Allowed service Ids different than expected!").


%% @private
rand_service_specs_except_type(ServiceType) ->
    lists_utils:generate(fun() ->
        ?SERVICE(
            ?RAND_ELEMENT([?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER, ?OP_PANEL] -- [ServiceType]),
            ?RAND_ELEMENT([<<"*">>] ++ lists_utils:generate(fun(I) -> integer_to_binary(I) end, 10))
        )
    end, ?RAND_INT(0, 10)).


-endif.
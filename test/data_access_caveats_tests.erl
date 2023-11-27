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
-module(data_access_caveats_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test/test_utils.hrl").
-include("aai/aai.hrl").


-define(CO(Whitelist), #cv_data_objectid{whitelist = utils:ensure_list(Whitelist)}).
-define(CP(Whitelist), #cv_data_path{whitelist = utils:ensure_list(Whitelist)}).


-record(test_spec, {
    data_access_caveats :: [service_caveat:cv_service()],
    all_space_ids :: [onedata:service_id()],
    expected_result :: [onedata:service_id()]
}).


match_allowed_service_ids_test() ->
    ?assert(run_test(#test_spec{
        data_access_caveats = [],
        all_space_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = [<<"1">>, <<"2">>, <<"3">>]
    })),
    ?assert(run_test(#test_spec{
        data_access_caveats = [
            ?CO(?RAND_OBJECTID(<<"1">>)),
            ?CP(?RAND_CANONICAL_PATH(<<"2">>))
        ],
        all_space_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = []
    })),
    ?assert(run_test(#test_spec{
        data_access_caveats = [
            ?CO(?RAND_OBJECTID(<<"3">>)),
            ?CP([?RAND_CANONICAL_PATH(<<"1">>), ?RAND_CANONICAL_PATH(<<"3">>)])
        ],
        all_space_ids = [<<"*">>],
        expected_result = [<<"3">>]
    })),
    ?assert(run_test(#test_spec{
        data_access_caveats = [
            ?CO([?RAND_OBJECTID(<<"1">>), ?RAND_OBJECTID(<<"2">>), ?RAND_OBJECTID(<<"3">>)])
        ],
        all_space_ids = [<<"1">>, <<"2">>, <<"3">>],
        expected_result = [<<"1">>, <<"2">>, <<"3">>]
    })),
    ?assert(run_test(#test_spec{
        data_access_caveats = [
            ?CO([?RAND_OBJECTID(<<"2">>), ?RAND_OBJECTID(<<"3">>)])
        ],
        all_space_ids = [],
        expected_result = []
    })),
    ?assert(run_test(#test_spec{
        data_access_caveats = [
            ?CO([?RAND_OBJECTID(<<"1">>)]),
            ?CP([?RAND_CANONICAL_PATH(<<"1">>), ?RAND_CANONICAL_PATH(<<"2">>)]),
            ?CP([?RAND_CANONICAL_PATH(<<"1">>), ?RAND_CANONICAL_PATH(<<"2">>), ?RAND_CANONICAL_PATH(<<"3">>)])
        ],
        all_space_ids = [<<"1">>, <<"2">>, <<"3">>, <<"4">>],
        expected_result = [<<"1">>]
    })),
    ?assert(run_test(#test_spec{
        data_access_caveats = [
            ?CP([?RAND_CANONICAL_PATH(<<"1">>), ?RAND_CANONICAL_PATH(<<"2">>), ?RAND_CANONICAL_PATH(<<"3">>)])
        ],
        all_space_ids = [<<"*">>],
        expected_result = [<<"1">>, <<"2">>, <<"3">>]
    })).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
run_test(#test_spec{
    data_access_caveats = DataAccessCaveats,
    all_space_ids = AllSpaceIds,
    expected_result = ExpectedResult
}) ->
    % interface=oneclient and readonly caveats should not impact the available spaces
    ExtendedDataAccessCaveats = DataAccessCaveats ++ lists_utils:generate(fun() ->
        ?RAND_CHOICE(#cv_interface{interface = oneclient}, #cv_data_readonly{})
    end, ?RAND_INT(0, 5)),

    Result = data_access_caveats:match_available_spaces(ExtendedDataAccessCaveats, AllSpaceIds),
    eunit_utils:is_equal(Result, ExpectedResult, "Available space Ids different than expected!").


-endif.
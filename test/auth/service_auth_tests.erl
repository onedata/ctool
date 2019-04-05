%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of service_auth module.
%%% @end
%%%-------------------------------------------------------------------
-module(service_auth_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").
-include("onedata.hrl").

pack_and_unpack_test() ->
    ?assertEqual(<<"opw-MaCaRoOn">>, service_auth:pack(?OP_WORKER, <<"MaCaRoOn">>)),
    ?assertEqual({?OP_WORKER, <<"MaCaRoOn">>}, service_auth:unpack(<<"opw-MaCaRoOn">>)),

    ?assertEqual(<<"opp-MaCaRoOn">>, service_auth:pack(?OP_PANEL, <<"MaCaRoOn">>)),
    ?assertEqual({?OP_PANEL, <<"MaCaRoOn">>}, service_auth:unpack(<<"opp-MaCaRoOn">>)),

    ?assertError(badarg, service_auth:pack(?OZ_WORKER, <<"MaCaRoOn">>)),
    ?assertError(badarg, service_auth:pack(?OZ_PANEL, <<"MaCaRoOn">>)),
    ?assertError(badarg, service_auth:pack(garbage, <<"MaCaRoOn">>)),

    % Auth macaroons that were not packed are assumed to be op_worker macaroons
    ?assertEqual({?OP_WORKER, <<"MaCaRoOn">>}, service_auth:unpack(<<"MaCaRoOn">>)),

    ?assertError(badarg, service_auth:unpack(<<"ozw-MaCaRoOn">>)),
    ?assertError(badarg, service_auth:unpack(<<"ozp-MaCaRoOn">>)),
    ?assertError(badarg, service_auth:unpack(<<"wth-MaCaRoOn">>)).

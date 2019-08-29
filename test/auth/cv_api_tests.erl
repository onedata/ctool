%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of cv_api module (API matchspec matching).
%%% @end
%%%-------------------------------------------------------------------
-module(cv_api_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").
-include("aai/aai.hrl").
-include("graph_sync/graph_sync.hrl").

verify_test() ->
    lists:foreach(fun({{Service, Operation, GRI}, Result, CvApiCaveat}) ->
        ?assertEqual(Result, cv_api:verify(CvApiCaveat, Service, Operation, GRI))
    end, testcases()).


testcases() -> [
    {
        {?OZ_WORKER, create, ?GRI(od_user, undefined, instance, shared)},
        true,
        #cv_api{whitelist = [
            {all, all, ?GRI('*', <<"*">>, '*', '*')}
        ]}
    },

    {
        {?OZ_WORKER, create, ?GRI(od_user, undefined, instance, shared)},
        false,
        #cv_api{whitelist = [
            {?OP_WORKER, all, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_PANEL, all, ?GRI('*', <<"*">>, '*', '*')},
            {?OZ_PANEL, all, ?GRI('*', <<"*">>, '*', '*')},
            {?OZ_WORKER, create, ?GRI(od_group, undefined, instance, shared)}
        ]}
    },
    {
        {?OZ_WORKER, create, ?GRI(od_user, undefined, instance, shared)},
        true,
        #cv_api{whitelist = [
            {?OP_WORKER, all, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_PANEL, all, ?GRI('*', <<"*">>, '*', '*')},
            {?OZ_PANEL, all, ?GRI('*', <<"*">>, '*', '*')},
            {?OZ_WORKER, create, ?GRI(od_user, <<"*">>, instance, shared)}
        ]}
    },

    {
        {?OZ_PANEL, delete, ?GRI(od_cluster, <<"abc">>, {user, <<"123">>}, private)},
        false,
        #cv_api{whitelist = [
            {?OZ_PANEL, delete, ?GRI('*', <<"987">>, {'*', <<"*">>}, private)},
            {?OZ_PANEL, all, ?GRI(od_space, <<"*">>, '*', '*')},
            {all, delete, ?GRI('*', <<"*">>, '*', protected)},
            {?OZ_PANEL, delete, ?GRI('*', <<"*">>, {'*', <<"456">>}, '*')}
        ]}
    },
    {
        {?OZ_PANEL, delete, ?GRI(od_cluster, <<"abc">>, {user, <<"123">>}, private)},
        true,
        #cv_api{whitelist = [
            {?OZ_PANEL, all, ?GRI(od_space, <<"*">>, '*', '*')},
            {all, delete, ?GRI('*', <<"*">>, {user, <<"*">>}, private)},
            {?OZ_PANEL, delete, ?GRI('*', <<"*">>, {'*', <<"456">>}, '*')},
            {all, all, ?GRI(od_space, <<"123">>, {user, <<"123">>}, private)}
        ]}
    },

    {
        {?OP_WORKER, get, ?GRI(od_space, <<"spaceid">>, instance, protected)},
        false,
        #cv_api{whitelist = [
            {?OP_WORKER, create, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_WORKER, update, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_WORKER, delete, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_WORKER, all, ?GRI(od_user, <<"*">>, '*', '*')},
            {all, get, ?GRI(od_space, <<"spaceid">>, {'*', <<"*">>}, '*')},
            {?OP_WORKER, get, ?GRI('*', <<"badid">>, '*', protected)},
            {?OP_WORKER, get, ?GRI(od_space, <<"spaceid">>, groups, protected)}
        ]}
    },
    {
        {?OP_WORKER, get, ?GRI(od_space, <<"spaceid">>, instance, protected)},
        true,
        #cv_api{whitelist = [
            {?OP_WORKER, create, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_WORKER, update, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_WORKER, delete, ?GRI('*', <<"*">>, '*', '*')},
            {all, get, ?GRI(od_space, <<"spaceid">>, '*', '*')},
            {?OP_WORKER, all, ?GRI(od_user, <<"*">>, '*', '*')},
            {all, get, ?GRI(od_space, <<"spaceid">>, {'*', <<"*">>}, '*')},
            {?OP_WORKER, get, ?GRI('*', <<"badid">>, '*', protected)}
        ]}
    },

    {
        {?OP_PANEL, update, ?GRI(od_provider, <<"provid">>, spaces, auto)},
        false,
        #cv_api{whitelist = [
            {?OZ_PANEL, update, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_PANEL, delete, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_PANEL, all, ?GRI('*', <<"*">>, '*', private)},
            {?OP_PANEL, update, ?GRI(od_space, <<"*">>, '*', '*')},
            {all, update, ?GRI('*', <<"123">>, '*', '*')},
            {?OP_PANEL, all, ?GRI(od_provider, <<"provid">>, spaces, public)}
        ]}
    },
    {
        {?OP_PANEL, update, ?GRI(od_provider, <<"provid">>, spaces, auto)},
        true,
        #cv_api{whitelist = [
            {?OZ_PANEL, update, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_PANEL, delete, ?GRI('*', <<"*">>, '*', '*')},
            {?OP_PANEL, all, ?GRI('*', <<"*">>, '*', private)},
            {?OP_PANEL, all, ?GRI('*', <<"*">>, '*', auto)},
            {?OP_PANEL, update, ?GRI(od_space, <<"*">>, '*', '*')},
            {all, update, ?GRI('*', <<"123">>, '*', '*')}
        ]}
    }
].
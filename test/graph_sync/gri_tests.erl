%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of gri module (serialization, deserialization and matching).
%%% @end
%%%-------------------------------------------------------------------
-module(gri_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").
-include("errors.hrl").
-include("graph_sync/gri.hrl").

-define(GRI_TO_PATTERN(GRI), #gri_pattern{
    type = GRI#gri.type,
    id = GRI#gri.id,
    aspect = GRI#gri.aspect,
    scope = GRI#gri.scope
}).
-define(PATTERN_TO_GRI(Pattern), #gri{
    type = Pattern#gri_pattern.type,
    id = Pattern#gri_pattern.id,
    aspect = Pattern#gri_pattern.aspect,
    scope = Pattern#gri_pattern.scope
}).


serialize_deserialize_test() ->
    lists:foreach(fun(Testcase) ->
        {Serialized, Deserialized, SerializedAgain} = case Testcase of
            {A, B} -> {A, B, A};
            {A, B, C} -> {A, B, C}
        end,
        ?assertEqual(Deserialized, gri:deserialize(Serialized)),
        ?assertEqual(SerializedAgain, gri:serialize(gri:deserialize(Serialized)))
    end, serialize_deserialize_testcases()).

serialize_deserialize_testcases() -> [
    {
        <<"user.self.instance:private">>,
        #gri{type = od_user, id = ?SELF, aspect = instance, scope = private}
    },
    {
        <<"group.gr-abc.user,u123:protected">>,
        #gri{type = od_group, id = <<"gr-abc">>, aspect = {user, <<"u123">>}, scope = protected}
    },
    {
        <<"space.null.instance:shared">>,
        #gri{type = od_space, aspect = instance, scope = shared}
    },
    {
        <<"share.sh13.instance">>,
        #gri{type = od_share, id = <<"sh13">>, aspect = instance},
        <<"share.sh13.instance:private">>
    },
    {
        <<"provider.self.spaces:auto">>,
        #gri{type = od_provider, id = ?SELF, aspect = spaces, scope = auto}
    },
    {
        <<"handleService.null.group,gr123:protected">>,
        #gri{type = od_handle_service, id = undefined, aspect = {group, <<"gr123">>}, scope = protected}
    },
    {
        <<"handle.hdle34.instance:auto">>,
        #gri{type = od_handle, id = <<"hdle34">>, aspect = instance, scope = auto}
    },
    {
        <<"cluster.null.user,abc">>,
        #gri{type = od_cluster, aspect = {user, <<"abc">>}},
        <<"cluster.null.user,abc:private">>
    },
    {
        <<"harvester.hrv13.harvest_metadata:private">>,
        #gri{type = od_harvester, id = <<"hrv13">>, aspect = harvest_metadata, scope = private}
    },
    {
        <<"token.tok9876.instance:public">>,
        #gri{type = od_token, id = <<"tok9876">>, aspect = instance, scope = public}
    },
    {
        <<"space_stats.spaceABC.provider_sync_progress,provider12345:private">>,
        #gri{type = space_stats, id = <<"spaceABC">>, aspect = {provider_sync_progress, <<"provider12345">>}, scope = private}
    },
    {
        <<"temporary_token_secret.tts123.user:private">>,
        #gri{type = temporary_token_secret, id = <<"tts123">>, aspect = user, scope = private}
    },
    {
        <<"op_archive.qwertyuiop.instance:private">>,
        #gri{type = op_archive, id = <<"qwertyuiop">>, aspect = instance, scope = private}
    },
    {
        <<"op_dataset.123345456789890.instance:private">>,
        #gri{type = op_dataset, id = <<"123345456789890">>, aspect = instance, scope = private}
    },
    {
        <<"file.891234718246113331.attrs:protected">>,
        #gri{type = op_file, id = <<"891234718246113331">>, aspect = attrs, scope = protected}
    },
    {
        <<"op_space.null.group,5456:shared">>,
        #gri{type = op_space, id = undefined, aspect = {group, <<"5456">>}, scope = shared}
    },
    {
        <<"op_user.u987.space,hr1ade:auto">>,
        #gri{type = op_user, id = <<"u987">>, aspect = {space, <<"hr1ade">>}, scope = auto}
    },
    {
        <<"op_handle.null.instance:private">>,
        #gri{type = op_handle, id = undefined, aspect = instance, scope = private}
    },
    {
        <<"op_handle_service.u987.instance:auto">>,
        #gri{type = op_handle_service, id = <<"u987">>, aspect = instance, scope = auto}
    },
    {
        <<"op_qos.null.instance:private">>,
        #gri{type = op_qos, id = undefined, aspect = instance, scope = private}
    }
].


serialize_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:serialize(GRI)) || GRI <- serialize_errors_testcases(regular)].


deserialize_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:deserialize(S)) || S <- deserialize_errors_testcases(regular)].


serialize_deserialize_pattern_test() ->
    % Include testcases for regular GRIs (also valid patterns)
    Testcases = serialize_deserialize_pattern_testcases() ++ lists:map(fun
        ({A, B}) -> {A, ?GRI_TO_PATTERN(B)};
        ({A, B, C}) -> {A, ?GRI_TO_PATTERN(B), C}
    end, serialize_deserialize_testcases()),
    lists:foreach(fun(Testcase) ->
        {Serialized, Deserialized, SerializedAgain} = case Testcase of
            {A, B} -> {A, B, A};
            {A, B, C} -> {A, B, C}
        end,
        ?assertEqual(Deserialized, gri:deserialize_pattern(Serialized)),
        ?assertEqual(SerializedAgain, gri:serialize_pattern(gri:deserialize_pattern(Serialized)))
    end, Testcases).

serialize_deserialize_pattern_testcases() -> [
    {
        <<"user.*.instance">>,
        #gri_pattern{type = od_user, id = '*', aspect = instance, scope = private},
        <<"user.*.instance:private">>
    },
    {
        <<"*.gr-abc.user,u123:protected">>,
        #gri_pattern{type = '*', id = <<"gr-abc">>, aspect = {user, <<"u123">>}, scope = protected}
    },
    {
        <<"space.null.*:shared">>,
        #gri_pattern{type = od_space, aspect = '*', scope = shared}
    },
    {
        <<"share.sh13.instance:*">>,
        #gri_pattern{type = od_share, id = <<"sh13">>, aspect = instance, scope = '*'}
    },
    {
        <<"provider.self.spaces:auto">>,
        #gri_pattern{type = od_provider, id = ?SELF, aspect = spaces, scope = auto}
    },
    {
        <<"handleService.null.*,gr123:protected">>,
        #gri_pattern{type = od_handle_service, id = undefined, aspect = {'*', <<"gr123">>}, scope = protected}
    },
    {
        <<"handle.hdle34.*,*:auto">>,
        #gri_pattern{type = od_handle, id = <<"hdle34">>, aspect = {'*', '*'}, scope = auto}
    },
    {
        <<"cluster.null.user,*">>,
        #gri_pattern{type = od_cluster, aspect = {user, '*'}},
        <<"cluster.null.user,*:private">>
    },
    {
        <<"*.*.harvest_metadata:private">>,
        #gri_pattern{type = '*', id = '*', aspect = harvest_metadata, scope = private}
    },
    {
        <<"atm_inventory.*.workflows:protected">>,
        #gri_pattern{type = od_atm_inventory, id = '*', aspect = workflows, scope = protected}
    },
    {
        <<"atm_lambda.lAmBdAiD.*">>,
        #gri_pattern{type = od_atm_lambda, id = <<"lAmBdAiD">>, aspect = '*', scope = private},
        <<"atm_lambda.lAmBdAiD.*:private">>
    },
    {
        <<"atm_workflow_schema.*.instance:*">>,
        #gri_pattern{type = od_atm_workflow_schema, id = '*', aspect = instance, scope = '*'}
    },
    {
        <<"op_atm_lambda_snapshot.123234345456.instance:*">>,
        #gri_pattern{type = op_atm_lambda_snapshot, id = <<"123234345456">>, aspect = instance, scope = '*'}
    },
    {
        <<"op_atm_inventory.*.atm_workflow_schemas:*">>,
        #gri_pattern{type = op_atm_inventory, id = '*', aspect = atm_workflow_schemas, scope = '*'}
    },
    {
        <<"op_atm_store.*.iterator:*">>,
        #gri_pattern{type = op_atm_store, id = '*', aspect = iterator, scope = '*'}
    },
    {
        <<"op_atm_task_execution.*.instance:private">>,
        #gri_pattern{type = op_atm_task_execution, id = '*', aspect = instance, scope = private}
    },
    {
        <<"op_atm_workflow_execution.*.instance:*">>,
        #gri_pattern{type = op_atm_workflow_execution, id = '*', aspect = instance, scope = '*'}
    },
    {
        <<"op_atm_workflow_schema.98776455341.instance:private">>,
        #gri_pattern{type = op_atm_workflow_schema, id = <<"98776455341">>, aspect = instance, scope = private}
    },
    {
        <<"op_atm_workflow_schema_snapshot.98776455341.instance:*">>,
        #gri_pattern{type = op_atm_workflow_schema_snapshot, id = <<"98776455341">>, aspect = instance, scope = '*'}
    },
    {
        <<"op_dataset.qwertyuiop.*:private">>,
        #gri_pattern{type = op_dataset, id = <<"qwertyuiop">>, aspect = '*', scope = private}
    },
    {
        <<"op_dataset.123345456789890.*:private">>,
        #gri_pattern{type = op_dataset, id = <<"123345456789890">>, aspect = '*', scope = private}
    },
    {
        <<"file.891234718246113331.*:*">>,
        #gri_pattern{type = op_file, id = <<"891234718246113331">>, aspect = '*', scope = '*'}
    },
    {
        <<"*.null.*,*:shared">>,
        #gri_pattern{type = '*', id = undefined, aspect = {'*', '*'}, scope = shared}
    },
    {
        <<"*.*.*">>,
        #gri_pattern{type = '*', id = '*', aspect = '*', scope = private},
        <<"*.*.*:private">>
    },
    {
        <<"*.*.*:*">>,
        #gri_pattern{type = '*', id = '*', aspect = '*', scope = '*'}
    },
    {
        <<"*.*.*,*:*">>,
        #gri_pattern{type = '*', id = '*', aspect = {'*', '*'}, scope = '*'}
    }
].


serialize_pattern_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:serialize_pattern(GRI)) || GRI <- serialize_errors_testcases(pattern)].


deserialize_pattern_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:deserialize_pattern(S)) || S <- deserialize_errors_testcases(pattern)].


serialize_errors_testcases(regular) -> [?PATTERN_TO_GRI(P) || P <- serialize_errors_testcases(pattern)] ++ [
    #gri{type = '*', id = <<"123">>, aspect = instance, scope = private},
    #gri{type = od_user, id = '*', aspect = instance, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = '*', scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = {'*', <<"bin">>}, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = {'*', '*'}, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = {aspect, '*'}, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = instance, scope = '*'}
];
serialize_errors_testcases(pattern) -> [
    #gri_pattern{type = badtype, id = <<"123">>, aspect = instance, scope = private},
    #gri_pattern{type = '', id = <<"123">>, aspect = instance, scope = private},

    #gri_pattern{type = od_user, id = <<"123">>, aspect = <<"bad-aspect">>, scope = private},
    #gri_pattern{type = od_user, id = <<"123">>, aspect = {<<"bad-aspect">>, 123}, scope = public},
    #gri_pattern{type = od_user, id = <<"123">>, aspect = {aspect, <<"bin">>, <<"bin">>}, scope = shared},
    #gri_pattern{type = od_user, id = <<"123">>, aspect = {aspect, bad_subaspect}, scope = protected},
    #gri_pattern{type = od_user, id = <<"123">>, aspect = 17892312, scope = auto}
].


deserialize_errors_testcases(regular) -> deserialize_errors_testcases(pattern) ++ [
    <<"*.123.instance:protected">>,
    <<"user.*.instance:protected">>,
    <<"user.123.*:protected">>,
    <<"user.123.*,*:protected">>,
    <<"user.123.aspect,*:protected">>,
    <<"user.123.*,bin:protected">>,
    <<"user.123.aspect:*">>
];
deserialize_errors_testcases(pattern) -> [
    <<"">>,
    <<"rubbish">>,
    <<"1.2.3.4.5.6">>,
    <<"1.2:3">>,

    <<"null.123.instance:protected">>,
    <<"unknown.123.instance:protected">>,


    <<"user.123.apect,bin,something:protected">>,

    <<"user.123.aspect.rubbish">>,
    <<"user.123.aspect:14234534.6543">>
].


gri_matching_test() ->
    [?assertEqual(Res, gri:matches(GRI, Pattern)) || {GRI, Res, Pattern} <- gri_matching_testcases()].


gri_matching_testcases() -> [
    {
        ?GRI(od_user, ?SELF, instance, private),
        true,
        ?GRI_PATTERN(od_user, ?SELF, instance, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI_PATTERN(od_group, ?SELF, instance, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI_PATTERN(od_user, undefined, instance, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI_PATTERN(od_user, ?SELF, spaces, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI_PATTERN(od_user, ?SELF, instance, auto)
    },

    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI_PATTERN('*', <<"userid">>, {space, <<"123">>}, private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI_PATTERN(od_user, '*', {space, <<"123">>}, private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI_PATTERN(od_user, <<"userid">>, '*', private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI_PATTERN(od_user, <<"userid">>, {space, <<"123">>}, '*')
    },

    {
        ?GRI(od_user, <<"userid">>, groups, private),
        false,
        ?GRI_PATTERN('*', <<"userid">>, {space, <<"123">>}, private)
    },
    {
        ?GRI(od_group, <<"userid">>, {space, <<"123">>}, private),
        false,
        ?GRI_PATTERN(od_user, '*', {space, <<"123">>}, private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, auto),
        false,
        ?GRI_PATTERN(od_user, <<"userid">>, '*', private)
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, private),
        false,
        ?GRI_PATTERN(od_user, <<"userid">>, {space, <<"123">>}, '*')
    },


    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI_PATTERN('*', '*', groups, protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI_PATTERN('*', ?SELF, '*', protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI_PATTERN('*', ?SELF, groups, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI_PATTERN(od_user, '*', '*', protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI_PATTERN(od_user, '*', groups, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI_PATTERN(od_user, ?SELF, '*', '*')
    },

    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI_PATTERN('*', '*', spaces, protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI_PATTERN('*', <<"123">>, '*', protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI_PATTERN('*', undefined, groups, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI_PATTERN(od_user, '*', '*', auto)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI_PATTERN(od_user, '*', {group, <<"123">>}, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI_PATTERN(od_space, ?SELF, '*', '*')
    },


    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI_PATTERN('*', '*', '*', auto)
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI_PATTERN('*', <<"space-id">>, '*', '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI_PATTERN('*', '*', instance, '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI_PATTERN(od_space, '*', '*', '*')
    },

    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI_PATTERN('*', '*', '*', shared)
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI_PATTERN('*', <<"123">>, '*', '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI_PATTERN('*', '*', spaces, '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI_PATTERN(od_group, '*', '*', '*')
    },


    {
        ?GRI(od_user, undefined, instance, shared),
        true,
        ?GRI_PATTERN('*', '*', '*', '*')
    },
    {
        ?GRI(od_user, undefined, instance, shared),
        false,
        ?GRI_PATTERN('*', '*', {'*', '*'}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        true,
        ?GRI_PATTERN('*', '*', {'*', '*'}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        true,
        ?GRI_PATTERN('*', '*', {space, '*'}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        true,
        ?GRI_PATTERN('*', '*', {'*', <<"123">>}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        false,
        ?GRI_PATTERN('*', '*', {'*', <<"345">>}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        false,
        ?GRI_PATTERN('*', '*', {'user', <<"123">>}, '*')
    }
].
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
-include("graph_sync/graph_sync.hrl").

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
        <<"token.tok9876.instane:public">>,
        #gri{type = od_token, id = <<"tok9876">>, aspect = instane, scope = public}
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
    }
].


serialize_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:serialize(GRI)) || GRI <- serialize_errors_testcases(regular)].


deserialize_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:deserialize(S)) || S <- deserialize_errors_testcases(regular)].


serialize_deserialize_pattern_test() ->
    lists:foreach(fun(Testcase) ->
        {Serialized, Deserialized, SerializedAgain} = case Testcase of
            {A, B} -> {A, B, A};
            {A, B, C} -> {A, B, C}
        end,
        ?assertEqual(Deserialized, gri:deserialize_pattern(Serialized)),
        ?assertEqual(SerializedAgain, gri:serialize_pattern(gri:deserialize_pattern(Serialized)))
    % Include testcases for regular GRIs (also valid patterns)
    end, serialize_deserialize_pattern_testcases() ++ serialize_deserialize_testcases()).

serialize_deserialize_pattern_testcases() -> [
    {
        <<"user.*.instance">>,
        #gri{type = od_user, id = <<"*">>, aspect = instance, scope = private},
        <<"user.*.instance:private">>
    },
    {
        <<"*.gr-abc.user,u123:protected">>,
        #gri{type = '*', id = <<"gr-abc">>, aspect = {user, <<"u123">>}, scope = protected}
    },
    {
        <<"space.null.*:shared">>,
        #gri{type = od_space, aspect = '*', scope = shared}
    },
    {
        <<"share.sh13.instance:*">>,
        #gri{type = od_share, id = <<"sh13">>, aspect = instance, scope = '*'}
    },
    {
        <<"provider.self.spaces:auto">>,
        #gri{type = od_provider, id = ?SELF, aspect = spaces, scope = auto}
    },
    {
        <<"handleService.null.*,gr123:protected">>,
        #gri{type = od_handle_service, id = undefined, aspect = {'*', <<"gr123">>}, scope = protected}
    },
    {
        <<"handle.hdle34.*,*:auto">>,
        #gri{type = od_handle, id = <<"hdle34">>, aspect = {'*', <<"*">>}, scope = auto}
    },
    {
        <<"cluster.null.user,*">>,
        #gri{type = od_cluster, aspect = {user, <<"*">>}},
        <<"cluster.null.user,*:private">>
    },
    {
        <<"*.*.harvest_metadata:private">>,
        #gri{type = '*', id = <<"*">>, aspect = harvest_metadata, scope = private}
    },
    {
        <<"file.891234718246113331.*:*">>,
        #gri{type = op_file, id = <<"891234718246113331">>, aspect = '*', scope = '*'}
    },
    {
        <<"*.null.*,*:shared">>,
        #gri{type = '*', id = undefined, aspect = {'*', <<"*">>}, scope = shared}
    },
    {
        <<"*.*.*">>,
        #gri{type = '*', id = <<"*">>, aspect = '*', scope = private},
        <<"*.*.*:private">>
    },
    {
        <<"*.*.*:*">>,
        #gri{type = '*', id = <<"*">>, aspect = '*', scope = '*'}
    },
    {
        <<"*.*.*,*:*">>,
        #gri{type = '*', id = <<"*">>, aspect = {'*', <<"*">>}, scope = '*'}
    }
].


serialize_pattern_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:serialize(GRI)) || GRI <- serialize_errors_testcases(pattern)].


deserialize_pattern_errors_test() ->
    [?assertThrow(?ERROR_BAD_GRI, gri:deserialize(S)) || S <- deserialize_errors_testcases(pattern)].


serialize_errors_testcases(regular) -> serialize_errors_testcases(pattern) ++ [
    #gri{type = '*', id = <<"123">>, aspect = instance, scope = private},
    #gri{type = od_user, id = <<"*">>, aspect = instance, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = '*', scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = {'*', <<"bin">>}, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = {'*', <<"*">>}, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = {aspect, <<"*">>}, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = instance, scope = '*'}
];
serialize_errors_testcases(pattern) -> [
    #gri{type = badtype, id = <<"123">>, aspect = instance, scope = private},
    #gri{type = '', id = <<"123">>, aspect = instance, scope = private},

    #gri{type = od_user, id = <<"123">>, aspect = <<"bad-aspect">>, scope = private},
    #gri{type = od_user, id = <<"123">>, aspect = {<<"bad-aspect">>, 123}, scope = public},
    #gri{type = od_user, id = <<"123">>, aspect = {aspect, <<"bin">>, <<"bin">>}, scope = shared},
    #gri{type = od_user, id = <<"123">>, aspect = {aspect, bad_subaspect}, scope = protected},
    #gri{type = od_user, id = <<"123">>, aspect = 17892312, scope = auto}
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
        ?GRI(od_user, ?SELF, instance, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI(od_group, ?SELF, instance, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI(od_user, undefined, instance, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI(od_user, ?SELF, spaces, private)
    },
    {
        ?GRI(od_user, ?SELF, instance, private),
        false,
        ?GRI(od_user, ?SELF, instance, auto)
    },

    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI('*', <<"userid">>, {space, <<"123">>}, private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI(od_user, <<"*">>, {space, <<"123">>}, private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI(od_user, <<"userid">>, '*', private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, private),
        true,
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, '*')
    },

    {
        ?GRI(od_user, <<"userid">>, groups, private),
        false,
        ?GRI('*', <<"userid">>, {space, <<"123">>}, private)
    },
    {
        ?GRI(od_group, <<"userid">>, {space, <<"123">>}, private),
        false,
        ?GRI(od_user, <<"*">>, {space, <<"123">>}, private)
    },
    {
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, auto),
        false,
        ?GRI(od_user, <<"userid">>, '*', private)
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, private),
        false,
        ?GRI(od_user, <<"userid">>, {space, <<"123">>}, '*')
    },


    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI('*', <<"*">>, groups, protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI('*', ?SELF, '*', protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI('*', ?SELF, groups, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI(od_user, <<"*">>, '*', protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI(od_user, <<"*">>, groups, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        true,
        ?GRI(od_user, ?SELF, '*', '*')
    },

    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI('*', <<"*">>, spaces, protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI('*', <<"123">>, '*', protected)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI('*', undefined, groups, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI(od_user, <<"*">>, '*', auto)
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI(od_user, <<"*">>, {group, <<"123">>}, '*')
    },
    {
        ?GRI(od_user, ?SELF, groups, protected),
        false,
        ?GRI(od_space, ?SELF, '*', '*')
    },


    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI('*', <<"*">>, '*', auto)
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI('*', <<"space-id">>, '*', '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI('*', <<"*">>, instance, '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        true,
        ?GRI(od_space, <<"*">>, '*', '*')
    },

    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI('*', <<"*">>, '*', shared)
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI('*', <<"123">>, '*', '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI('*', <<"*">>, spaces, '*')
    },
    {
        ?GRI(od_space, <<"space-id">>, instance, auto),
        false,
        ?GRI(od_group, <<"*">>, '*', '*')
    },


    {
        ?GRI(od_user, undefined, instance, shared),
        true,
        ?GRI('*', <<"*">>, '*', '*')
    },
    {
        ?GRI(od_user, undefined, instance, shared),
        false,
        ?GRI('*', <<"*">>, {'*', <<"*">>}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        true,
        ?GRI('*', <<"*">>, {'*', <<"*">>}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        true,
        ?GRI('*', <<"*">>, {space, <<"*">>}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        true,
        ?GRI('*', <<"*">>, {'*', <<"123">>}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        false,
        ?GRI('*', <<"*">>, {'*', <<"345">>}, '*')
    },
    {
        ?GRI(od_user, undefined, {space, <<"123">>}, shared),
        false,
        ?GRI('*', <<"*">>, {'user', <<"123">>}, '*')
    }
].
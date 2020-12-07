%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of support_parameters module.
%%% @end
%%%-------------------------------------------------------------------
-module(support_parameters_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("errors.hrl").

-define(DATA_WRITE_VALUES, [global, none]).
-define(METADATA_REPLICATION_VALUES, [eager, lazy, none]).

-define(PRV_ALPHA, <<"12345">>).
-define(PROVIDER_BETA, <<"abcde">>).


support_parameters_test() ->
    lists:foreach(fun(DataWrite) ->
        lists:foreach(fun(MetadataReplication) ->
            support_parameters_test_case(DataWrite, MetadataReplication)
        end, ?METADATA_REPLICATION_VALUES)
    end, ?DATA_WRITE_VALUES).


support_parameters_test_case(DataWrite, MetadataReplication) ->
    Params = support_parameters:build(DataWrite, MetadataReplication),
    ?assertEqual(DataWrite, support_parameters:get_data_write(Params)),
    ?assertEqual(MetadataReplication, support_parameters:get_metadata_replication(Params)),

    RegistryFirst = support_parameters:update_for_provider(#{}, ?PRV_ALPHA, Params),
    ?assertEqual({ok, Params}, support_parameters:lookup_by_provider(RegistryFirst, ?PRV_ALPHA)),
    ?assertEqual(error, support_parameters:lookup_by_provider(RegistryFirst, ?PROVIDER_BETA)),
    ?assertEqual(RegistryFirst, support_parameters:registry_from_json(json_utils:decode(
        json_utils:encode(support_parameters:registry_to_json(RegistryFirst))
    ))),

    ?assertEqual(Params, support_parameters:deserialize(
        support_parameters:serialize(Params)
    )),

    ?assertEqual(Params, support_parameters:from_json(json_utils:decode(
        json_utils:encode(support_parameters:to_json(Params))
    ))),

    OtherParams = support_parameters:build(
        lists_utils:random_element(?DATA_WRITE_VALUES -- [DataWrite]),
        lists_utils:random_element(?METADATA_REPLICATION_VALUES -- [MetadataReplication])
    ),
    RegistrySecond = support_parameters:update_for_provider(RegistryFirst, ?PROVIDER_BETA, OtherParams),
    ?assertEqual({ok, Params}, support_parameters:lookup_by_provider(RegistrySecond, ?PRV_ALPHA)),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(RegistrySecond, ?PROVIDER_BETA)),
    ?assertEqual(RegistrySecond, support_parameters:registry_from_json(json_utils:decode(
        json_utils:encode(support_parameters:registry_to_json(RegistrySecond))
    ))),

    RegistryThird = support_parameters:update_for_provider(RegistrySecond, ?PRV_ALPHA, OtherParams),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(RegistryThird, ?PRV_ALPHA)),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(RegistryThird, ?PROVIDER_BETA)),
    ?assertEqual(RegistryThird, support_parameters:registry_from_json(json_utils:decode(
        json_utils:encode(support_parameters:registry_to_json(RegistryThird))
    ))),

    RegistryLast = support_parameters:remove_for_provider(RegistryThird, ?PRV_ALPHA),
    ?assertEqual(error, support_parameters:lookup_by_provider(RegistryLast, ?PRV_ALPHA)),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(RegistryLast, ?PROVIDER_BETA)),
    ?assertEqual(RegistryLast, support_parameters:registry_from_json(json_utils:decode(
        json_utils:encode(support_parameters:registry_to_json(RegistryLast))
    ))).


-endif.

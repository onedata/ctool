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

-define(PROVIDER_ALPHA, <<"12345">>).
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

    ParamsPerProviderFirst = support_parameters:update_for_provider(#{}, ?PROVIDER_ALPHA, Params),
    ?assertEqual({ok, Params}, support_parameters:lookup_by_provider(ParamsPerProviderFirst, ?PROVIDER_ALPHA)),
    ?assertEqual(error, support_parameters:lookup_by_provider(ParamsPerProviderFirst, ?PROVIDER_BETA)),
    ?assertEqual(ParamsPerProviderFirst, support_parameters:per_provider_from_json(json_utils:decode(
        json_utils:encode(support_parameters:per_provider_to_json(ParamsPerProviderFirst))
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
    ParamsPerProviderSecond = support_parameters:update_for_provider(ParamsPerProviderFirst, ?PROVIDER_BETA, OtherParams),
    ?assertEqual({ok, Params}, support_parameters:lookup_by_provider(ParamsPerProviderSecond, ?PROVIDER_ALPHA)),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(ParamsPerProviderSecond, ?PROVIDER_BETA)),
    ?assertEqual(ParamsPerProviderSecond, support_parameters:per_provider_from_json(json_utils:decode(
        json_utils:encode(support_parameters:per_provider_to_json(ParamsPerProviderSecond))
    ))),

    ParamsPerProviderThird = support_parameters:update_for_provider(ParamsPerProviderSecond, ?PROVIDER_ALPHA, OtherParams),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(ParamsPerProviderThird, ?PROVIDER_ALPHA)),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(ParamsPerProviderThird, ?PROVIDER_BETA)),
    ?assertEqual(ParamsPerProviderThird, support_parameters:per_provider_from_json(json_utils:decode(
        json_utils:encode(support_parameters:per_provider_to_json(ParamsPerProviderThird))
    ))),

    ParamsPerProviderLast = support_parameters:remove_for_provider(ParamsPerProviderThird, ?PROVIDER_ALPHA),
    ?assertEqual(error, support_parameters:lookup_by_provider(ParamsPerProviderLast, ?PROVIDER_ALPHA)),
    ?assertEqual({ok, OtherParams}, support_parameters:lookup_by_provider(ParamsPerProviderLast, ?PROVIDER_BETA)),
    ?assertEqual(ParamsPerProviderLast, support_parameters:per_provider_from_json(json_utils:decode(
        json_utils:encode(support_parameters:per_provider_to_json(ParamsPerProviderLast))
    ))).


-endif.

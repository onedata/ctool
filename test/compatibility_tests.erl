%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of compatibility module.
%%% @end
%%%-------------------------------------------------------------------
-module(compatibility_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("onedata.hrl").

-define(SHA_ALPHA, <<"8cf038be257096d4e621ae910cac2de3a9e42879a051b0beac30276dd35bd890">>).
-define(SHA_BETA, <<"475d9b7c627d0c327d50cfda423c1671cd63ac2537a361c54fcdcbeb4006eb0f">>).
-define(SHA_GAMMA, <<"0de3bea32a22151a5875046a12bcf93a8a83b22a111f6cbb1b2652eab8e08e8c">>).
-define(SHA_DELTA, <<"5925d3a7861f184f6e6d5a0c3b3fddecde65fac243d49143a781be82030872ea">>).
-define(SHA_THETA, <<"53cc8692eb0b7c6823c9cd5022b64ba66739efe0637b0e31b134c67c6365cb0c">>).
-define(SHA_KAPPA, <<"6aad50df6ba66739efe0651046a12bcf92a111f6cbb10de05b7c6273c0276dd3">>).
-define(SHA_SIGMA, <<"0b7c68236ba6134c67c6365cb09cd5c92eb0637c022b6739efe53cc8b0e31b64">>).
-define(SHA_OMEGA, <<"8e741fa13536aad50dff4c81a29217fb7437cd5511ac7fcf8f0de05884d5768b">>).

-define(DUMMY_NODE, 'dummy_node@example.com').
-define(ANOTHER_NODE, 'another_node@example.com').

compatibility_verification_test_() ->
    {foreach, local, % 'local' runs setup/teardown and the test in the same process
        fun setup/0,
        fun teardown/1,
        [
            {"OZ:OP compatibility check", fun oz_op_compatibility_check/0},
            {"OP:OZ compatibility check", fun op_oz_compatibility_check/0},
            {"OP:OP compatibility check", fun op_op_compatibility_check/0},
            {"OP:OC compatibility check", fun op_oc_compatibility_check/0},
            {"GUI hash verification", fun gui_hash_verification/0},
            {"Caching local registry content", fun caching_local_registry_content/0},
            {"Taking default registry if newer", fun taking_default_registry_if_newer/0},
            {"Fetching newer registry", fun fetching_newer_registry/0},
            {"Fetching older registry", fun fetching_older_registry/0},
            {"Registry parsing error", fun registry_parsing_error/0},
            {"Overwriting broken or absent registry", fun overwriting_broken_or_absent_registry/0},
            {"Taking default registry if local is broken", fun taking_default_registry_if_local_is_broken/0},
            {"Resistance to node failures when overwriting registry", fun resistance_to_node_failures_when_overwriting_registry/0},
            {"Trying multiple mirrors", fun trying_multiple_mirrors/0},
            {"Forcing check for updates", fun forcing_check_for_updates/0}
        ]
    }.


%%%===================================================================
%%% Setup/teardown and functions used for mocking
%%%===================================================================

setup() ->
    clock_freezer_mock:setup_for_eunit([compatibility]),
    node_cache:init(),

    TmpPath = mochitemp:mkdtemp(),
    RegistryPath = filename:join(TmpPath, "compatibility.json"),
    DefaultRegistryPath = filename:join(TmpPath, "compatibility.default.json"),
    ctool:set_env(current_compatibility_registry_file, RegistryPath),
    ctool:set_env(default_compatibility_registry_file, DefaultRegistryPath),
    ctool:set_env(compatibility_registry_cache_ttl_secs, 900),
    ctool:set_env(compatibility_registry_fetch_backoff_secs, 900),
    compatibility:clear_registry_cache(),
    mock_current_registry(#{<<"revision">> => 2019010100}),
    mock_default_registry(#{<<"revision">> => 2019010100}),

    meck:new(rpc, [unstick, passthrough]),
    meck:expect(rpc, call, fun
        (Node, Module, Function, Args) when Node == node() ->
            meck:passthrough([Node, Module, Function, Args]);
        (Node, compatibility, overwrite_registry, [{registry, Binary, _, _}]) ->
            mock_current_registry_overwrite_on_remote_node(Node, Binary)
    end),

    meck:new(http_client, [passthrough]),
    meck:expect(http_client, get, fun(Url, _, _, [{ssl_options, [{cacerts, _}]}]) ->
        get_mocked_mirror_result(Url)
    end).

teardown(_) ->
    RegistryPath = ctool:get_env(current_compatibility_registry_file),
    TmpPath = filename:dirname(RegistryPath),
    mochitemp:rmtempdir(TmpPath),
    clear_mocked_mirrors(),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),

    ?assert(meck:validate(rpc)),
    ok = meck:unload(rpc),

    node_cache:destroy(),
    clock_freezer_mock:teardown_for_eunit().


mock_current_registry(JsonMap) when is_map(JsonMap) ->
    mock_current_registry(json_utils:encode(JsonMap));
mock_current_registry(Binary) when is_binary(Binary) ->
    RegistryPath = ctool:get_env(current_compatibility_registry_file),
    ok = file:write_file(RegistryPath, Binary),
    mock_current_registry_overwrite_on_remote_node(?DUMMY_NODE, Binary),
    mock_current_registry_overwrite_on_remote_node(?ANOTHER_NODE, Binary).


mock_default_registry(JsonMap) when is_map(JsonMap) ->
    mock_default_registry(json_utils:encode(JsonMap));
mock_default_registry(Binary) when is_binary(Binary) ->
    RegistryPath = ctool:get_env(default_compatibility_registry_file),
    ok = file:write_file(RegistryPath, Binary).


get_current_registry() ->
    RegistryPath = ctool:get_env(current_compatibility_registry_file),
    {ok, BinaryJson} = file:read_file(RegistryPath),
    json_utils:decode(BinaryJson).


-spec mock_mirror_result(Url :: binary(),
    Result :: {ok, Code :: integer(), Body :: binary() | map()} | {error, term()}) ->
    ok.
mock_mirror_result(Url, Result) ->
    node_cache:put({mocked_mirror, Url}, Result),
    Mirrors = node_cache:get(mocked_mirrors, []),
    node_cache:put(mocked_mirrors, [Url | Mirrors]).


mock_mirror_list(Mirrors) ->
    ctool:set_env(compatibility_registry_mirrors, Mirrors).


get_mocked_mirror_result(Url) ->
    case node_cache:get({mocked_mirror, Url}, {error, nxdomain}) of
        {ok, Code, JsonMap} when is_map(JsonMap) ->
            {ok, Code, #{}, json_utils:encode(JsonMap)};
        {ok, Code, Binary} when is_binary(Binary) ->
            {ok, Code, #{}, Binary};
        {error, _} = Error ->
            Error
    end.


clear_mocked_mirrors() ->
    mock_mirror_list([]),
    Mirrors = node_cache:get(mocked_mirrors, []),
    [node_cache:clear({mocked_mirror, M}) || M <- Mirrors].


mock_current_registry_overwrite_on_remote_node(Node, Binary) ->
    node_cache:put({mocked_compatibility_on_remote_node, Node}, Binary).


get_mocked_current_registry_on_remote_node(Node) ->
    json_utils:decode(node_cache:get({mocked_compatibility_on_remote_node, Node})).


is_registry_on_all_nodes_equal_to(ExpectedRegistry) ->
    is_registry_on_nodes_equal_to([node(), ?DUMMY_NODE, ?ANOTHER_NODE], ExpectedRegistry).

is_registry_on_nodes_equal_to(Nodes, ExpectedRegistry) ->
    lists:all(fun(Node) ->
        ExpectedRegistry =:= case Node of
            % catch is used as some test may break the registry on purpose
            Local when Local =:= node() -> (catch get_current_registry());
            _ -> get_mocked_current_registry_on_remote_node(Node)
        end
    end, Nodes).

%%%===================================================================
%%% Test functions
%%%===================================================================

% Macros for more concise code
-define(BuildResolver(), compatibility:build_resolver(lists_utils:shuffle([node(), ?DUMMY_NODE, ?ANOTHER_NODE]), [])).

-define(OZvsOP(VersionA, VersionB), compatibility:check_products_compatibility(
    ?BuildResolver(), ?ONEZONE, VersionA, ?ONEPROVIDER, VersionB
)).
-define(OZvsOPVersions(VersionA), compatibility:get_compatible_versions(
    ?BuildResolver(), ?ONEZONE, VersionA, ?ONEPROVIDER
)).

-define(OPvsOZ(VersionA, VersionB), compatibility:check_products_compatibility(
    ?BuildResolver(), ?ONEPROVIDER, VersionA, ?ONEZONE, VersionB
)).
-define(OPvsOZVersions(VersionA), compatibility:get_compatible_versions(
    ?BuildResolver(), ?ONEPROVIDER, VersionA, ?ONEZONE
)).

-define(OPvsOP(VersionA, VersionB), compatibility:check_products_compatibility(
    ?BuildResolver(), ?ONEPROVIDER, VersionA, ?ONEPROVIDER, VersionB
)).
-define(OPvsOPVersions(VersionA), compatibility:get_compatible_versions(
    ?BuildResolver(), ?ONEPROVIDER, VersionA, ?ONEPROVIDER
)).

-define(OPvsOC(VersionA, VersionB), compatibility:check_products_compatibility(
    ?BuildResolver(), ?ONEPROVIDER, VersionA, ?ONECLIENT, VersionB
)).
-define(OPvsOCVersions(VersionA), compatibility:get_compatible_versions(
    ?BuildResolver(), ?ONEPROVIDER, VersionA, ?ONECLIENT
)).

-define(VerifyGUI(Service, Version, Hash), compatibility:verify_gui_hash(
    ?BuildResolver(), Service, Version, Hash
)).

oz_op_compatibility_check() ->
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019010100}}}, ?OZvsOP(<<"18.02.1">>, <<"17.06.3">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.2">>, {revision, 2019010100}}}, ?OZvsOP(<<"18.02.2">>, <<"17.06.3">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.3">>, {revision, 2019010100}}}, ?OZvsOP(<<"17.06.3">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.4">>, {revision, 2019010100}}}, ?OZvsOP(<<"17.06.4">>, <<"18.02.1">>)),

    mock_current_registry(#{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"17.06.3">> => [
                    <<"17.06.1">>,
                    <<"17.06.2">>,
                    <<"17.06.3">>
                ],
                <<"18.02.1">> => [
                    <<"17.06.3">>,
                    <<"18.02.1">>
                ]
            }
        }
    }),
    compatibility:clear_registry_cache(),

    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"17.06.3">>)),
    ?assertEqual(true, ?OZvsOP(<<"17.06.3">>, <<"17.06.1">>)),
    ?assertEqual(true, ?OZvsOP(<<"17.06.3">>, <<"17.06.2">>)),

    ?assertEqual({false, [<<"17.06.1">>, <<"17.06.2">>, <<"17.06.3">>]}, ?OZvsOP(<<"17.06.3">>, <<"18.02.1">>)),
    ?assertEqual({false, [<<"17.06.3">>, <<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"17.06.1">>)),

    ?assertEqual({error, {unknown_version, <<"17.06.1">>, {revision, 2019010100}}}, ?OZvsOP(<<"17.06.1">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.2">>, {revision, 2019010100}}}, ?OZvsOP(<<"17.06.2">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.4">>, {revision, 2019010100}}}, ?OZvsOP(<<"17.06.4">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.2">>, {revision, 2019010100}}}, ?OZvsOP(<<"18.02.2">>, <<"18.02.1">>)),

    ?assertEqual({ok, [<<"17.06.1">>, <<"17.06.2">>, <<"17.06.3">>]}, ?OZvsOPVersions(<<"17.06.3">>)),
    ?assertEqual({ok, [<<"17.06.3">>, <<"18.02.1">>]}, ?OZvsOPVersions(<<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.2">>, {revision, 2019010100}}}, ?OZvsOPVersions(<<"18.02.2">>)).

op_oz_compatibility_check() ->
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019010100}}}, ?OPvsOZ(<<"18.02.1">>, <<"17.06.3">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.2">>, {revision, 2019010100}}}, ?OPvsOZ(<<"18.02.2">>, <<"17.06.3">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.3">>, {revision, 2019010100}}}, ?OPvsOZ(<<"17.06.3">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.4">>, {revision, 2019010100}}}, ?OPvsOZ(<<"17.06.4">>, <<"18.02.1">>)),

    mock_current_registry(#{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"17.06.1">> => [
                    <<"17.06.3">>
                ],
                <<"17.06.2">> => [
                    <<"17.06.3">>
                ],
                <<"17.06.3">> => [
                    <<"17.06.3">>,
                    <<"18.02.1">>
                ],
                <<"18.02.1">> => [
                    <<"18.02.1">>
                ]
            }
        }
    }),
    compatibility:clear_registry_cache(),

    ?assertEqual(true, ?OPvsOZ(<<"18.02.1">>, <<"17.06.3">>)),
    ?assertEqual(true, ?OPvsOZ(<<"17.06.3">>, <<"17.06.1">>)),
    ?assertEqual(true, ?OPvsOZ(<<"17.06.3">>, <<"17.06.2">>)),

    ?assertEqual({false, [<<"17.06.1">>, <<"17.06.2">>, <<"17.06.3">>]}, ?OPvsOZ(<<"17.06.3">>, <<"18.02.1">>)),
    ?assertEqual({false, [<<"17.06.3">>, <<"18.02.1">>]}, ?OPvsOZ(<<"18.02.1">>, <<"17.06.1">>)),

    ?assertEqual({error, {unknown_version, <<"17.06.1">>, {revision, 2019010100}}}, ?OPvsOZ(<<"17.06.1">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.2">>, {revision, 2019010100}}}, ?OPvsOZ(<<"17.06.2">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"17.06.4">>, {revision, 2019010100}}}, ?OPvsOZ(<<"17.06.4">>, <<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.2">>, {revision, 2019010100}}}, ?OPvsOZ(<<"18.02.2">>, <<"18.02.1">>)),

    ?assertEqual({ok, [<<"17.06.1">>, <<"17.06.2">>, <<"17.06.3">>]}, ?OPvsOZVersions(<<"17.06.3">>)),
    ?assertEqual({ok, [<<"17.06.3">>, <<"18.02.1">>]}, ?OPvsOZVersions(<<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.2">>, {revision, 2019010100}}}, ?OPvsOZVersions(<<"18.02.2">>)).

op_op_compatibility_check() ->
    ?assertEqual({error, {unknown_version, <<"19.02.1">>, {revision, 2019010100}}}, ?OPvsOP(<<"19.02.1">>, <<"18.02.4">>)),
    ?assertEqual({error, {unknown_version, <<"19.02.2">>, {revision, 2019010100}}}, ?OPvsOP(<<"19.02.2">>, <<"20.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"20.02.1">>, {revision, 2019010100}}}, ?OPvsOP(<<"20.02.1">>, <<"19.02.2">>)),
    ?assertEqual({error, {unknown_version, <<"20.02.2">>, {revision, 2019010100}}}, ?OPvsOP(<<"20.02.2">>, <<"18.02.4">>)),

    %% In case of compatibility between providers, the relation is symmetrical,
    %% but the registry file does not need to have symmetrical entries
    mock_current_registry(#{
        <<"revision">> => 2019011700,
        <<"compatibility">> => #{
            <<"oneprovider:oneprovider">> => #{
                <<"19.02.1">> => [
                    <<"18.02.4">>,
                    <<"19.02.1">>
                ],
                <<"19.02.2">> => [
                    <<"19.02.1">>,
                    <<"19.02.2">>
                ],
                <<"20.02.1">> => [
                    <<"19.02.1">>,
                    <<"19.02.2">>,
                    <<"20.02.1">>
                ]
            }
        }
    }),
    compatibility:clear_registry_cache(),

    ?assertEqual(true, ?OPvsOP(<<"19.02.1">>, <<"18.02.4">>)),
    ?assertEqual(true, ?OPvsOP(<<"18.02.4">>, <<"19.02.1">>)),

    ?assertEqual(true, ?OPvsOP(<<"19.02.1">>, <<"19.02.1">>)),

    ?assertEqual(true, ?OPvsOP(<<"19.02.2">>, <<"19.02.1">>)),
    ?assertEqual(true, ?OPvsOP(<<"19.02.1">>, <<"19.02.2">>)),

    ?assertEqual(true, ?OPvsOP(<<"19.02.2">>, <<"19.02.2">>)),

    ?assertEqual(true, ?OPvsOP(<<"20.02.1">>, <<"19.02.1">>)),
    ?assertEqual(true, ?OPvsOP(<<"19.02.1">>, <<"20.02.1">>)),

    ?assertEqual(true, ?OPvsOP(<<"20.02.1">>, <<"20.02.1">>)),


    ?assertEqual({false, [<<"18.02.4">>, <<"19.02.1">>, <<"19.02.2">>, <<"20.02.1">>]}, ?OPvsOP(<<"19.02.1">>, <<"18.02.1">>)),
    ?assertEqual({false, [<<"19.02.1">>, <<"19.02.2">>, <<"20.02.1">>]}, ?OPvsOP(<<"19.02.2">>, <<"19.02.3">>)),
    ?assertEqual({false, [<<"19.02.1">>, <<"19.02.2">>, <<"20.02.1">>]}, ?OPvsOP(<<"20.02.1">>, <<"18.02.4">>)),

    ?assertEqual({error, {unknown_version, <<"20.02.2">>, {revision, 2019011700}}}, ?OPvsOP(<<"20.02.2">>, <<"18.02.4">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.3">>, {revision, 2019011700}}}, ?OPvsOP(<<"18.02.3">>, <<"18.02.1">>)),

    ?assertEqual({ok, [<<"19.02.1">>]}, ?OPvsOPVersions(<<"18.02.4">>)),
    ?assertEqual({ok, [<<"18.02.4">>, <<"19.02.1">>, <<"19.02.2">>, <<"20.02.1">>]}, ?OPvsOPVersions(<<"19.02.1">>)),
    ?assertEqual({ok, [<<"19.02.1">>, <<"19.02.2">>, <<"20.02.1">>]}, ?OPvsOPVersions(<<"19.02.2">>)),
    ?assertEqual({ok, [<<"19.02.1">>, <<"19.02.2">>, <<"20.02.1">>]}, ?OPvsOPVersions(<<"20.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"20.02.2">>, {revision, 2019011700}}}, ?OPvsOPVersions(<<"20.02.2">>)).


op_oc_compatibility_check() ->
    ?assertEqual({error, {unknown_version, <<"20.08.1">>, {revision, 2019010100}}}, ?OPvsOC(<<"20.08.1">>, <<"20.08.1">>)),
    ?assertEqual({error, {unknown_version, <<"20.08.2">>, {revision, 2019010100}}}, ?OPvsOC(<<"20.08.2">>, <<"19.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"20.08.2">>, {revision, 2019010100}}}, ?OPvsOC(<<"20.08.2">>, <<"20.08.1">>)),
    ?assertEqual({error, {unknown_version, <<"20.08.2">>, {revision, 2019010100}}}, ?OPvsOC(<<"20.08.2">>, <<"20.08.2">>)),
    ?assertEqual({error, {unknown_version, <<"20.08.3">>, {revision, 2019010100}}}, ?OPvsOC(<<"20.08.3">>, <<"20.08.2">>)),

    mock_current_registry(#{
        <<"revision">> => 2019112300,
        <<"compatibility">> => #{
            <<"oneprovider:oneclient">> => #{
                <<"20.08.1">> => [
                    <<"20.08.1">>
                ],
                <<"20.08.2">> => [
                    <<"19.02.1-rc11">>,
                    <<"20.08.1">>,
                    <<"20.08.2">>
                ]
            }
        }
    }),
    compatibility:clear_registry_cache(),

    ?assertEqual(true, ?OPvsOC(<<"20.08.1">>, <<"20.08.1">>)),
    ?assertEqual(true, ?OPvsOC(<<"20.08.1">>, <<"20.08.1-10-gasdasd">>)),
    ?assertEqual(true, ?OPvsOC(<<"20.08.2">>, <<"19.02.1-rc11">>)),
    ?assertEqual(true, ?OPvsOC(<<"20.08.2">>, <<"19.02.1-rc11-10-gasjdh">>)),
    ?assertEqual(true, ?OPvsOC(<<"20.08.2">>, <<"20.08.1">>)),
    ?assertEqual(true, ?OPvsOC(<<"20.08.2">>, <<"20.08.2">>)),

    ?assertEqual({false, [<<"20.08.1">>]}, ?OPvsOC(<<"20.08.1">>, <<"20.08.2">>)),
    ?assertEqual({false, [<<"19.02.1-rc11">>, <<"20.08.1">>, <<"20.08.2">>]}, ?OPvsOC(<<"20.08.2">>, <<"19.02.0">>)),
    ?assertEqual({false, [<<"19.02.1-rc11">>, <<"20.08.1">>, <<"20.08.2">>]}, ?OPvsOC(<<"20.08.2">>, <<"19.02.1-rc1">>)),

    ?assertEqual({error, {unknown_version, <<"20.08.3">>, {revision, 2019112300}}}, ?OPvsOC(<<"20.08.3">>, <<"20.08.2">>)),
    ?assertEqual({error, {unknown_version, <<"19.02.1-rc11">>, {revision, 2019112300}}}, ?OPvsOC(<<"19.02.1-rc11">>, <<"20.08.2">>)),

    ?assertEqual({ok, [<<"20.08.1">>]}, ?OPvsOCVersions(<<"20.08.1">>)),
    ?assertEqual({ok, [<<"19.02.1-rc11">>, <<"20.08.1">>, <<"20.08.2">>]}, ?OPvsOCVersions(<<"20.08.2">>)),
    ?assertEqual({error, {unknown_version, <<"20.08.3">>, {revision, 2019112300}}}, ?OPvsOCVersions(<<"20.08.3">>)).


caching_local_registry_content() ->
    InitialRegistry = #{
        <<"revision">> => 2019041000,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>
                ]
            }
        }
    },
    NewerRegistry = #{
        <<"revision">> => 2019041100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>
                ]
            }
        }
    },
    mock_current_registry(InitialRegistry),
    compatibility:clear_registry_cache(),
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assertEqual({ok, 2019041000}, compatibility:peek_current_registry_revision(?BuildResolver())),

    % The file contents should be cached for configured time
    mock_current_registry(NewerRegistry),
    CacheTTL = ctool:get_env(compatibility_registry_cache_ttl_secs),
    clock_freezer_mock:simulate_seconds_passing(CacheTTL - 1),
    % The cache is still valid
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assertEqual({ok, 2019041000}, compatibility:peek_current_registry_revision(?BuildResolver())),

    % But not anymore
    clock_freezer_mock:simulate_seconds_passing(2),
    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assertEqual({ok, 2019041100}, compatibility:peek_current_registry_revision(?BuildResolver())).


gui_hash_verification() ->
    ?assertEqual({error, {unknown_version, <<"18.02.2">>, {revision, 2019010100}}}, ?VerifyGUI(?OP_WORKER_GUI, <<"18.02.2">>, ?SHA_BETA)),
    ?assertEqual({error, {unknown_version, <<"19.02.1">>, {revision, 2019010100}}}, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.1">>, ?SHA_DELTA)),
    ?assertEqual({error, {unknown_version, <<"19.02.2">>, {revision, 2019010100}}}, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_GAMMA)),
    ?assertEqual({error, {unknown_version, <<"19.02.2">>, {revision, 2019010100}}}, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_GAMMA)),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019010100}}}, ?VerifyGUI(?HARVESTER_GUI, <<"18.02.1">>, ?SHA_ALPHA)),

    mock_current_registry(#{
        <<"revision">> => 2019071900,
        <<"gui-sha256">> => #{
            <<"op-worker">> => #{
                <<"18.02.2">> => [
                    ?SHA_ALPHA,
                    ?SHA_BETA,
                    ?SHA_THETA
                ]
            },
            <<"onepanel">> => #{
                <<"19.02.1">> => [
                    ?SHA_DELTA,
                    ?SHA_OMEGA
                ],
                <<"19.02.2">> => [
                    ?SHA_GAMMA
                ]
            },
            %% Harvester GUI entries have another nesting level with human-readable labels
            <<"harvester">> => #{
                <<"19.02.1">> => #{
                    <<"ecrin">> => [?SHA_KAPPA],
                    <<"my-harvester">> => [?SHA_SIGMA]
                }
            }
        }
    }),
    compatibility:clear_registry_cache(),

    ?assertEqual(true, ?VerifyGUI(?OP_WORKER_GUI, <<"18.02.2">>, ?SHA_ALPHA)),
    ?assertEqual(true, ?VerifyGUI(?OP_WORKER_GUI, <<"18.02.2">>, ?SHA_BETA)),
    ?assertEqual(true, ?VerifyGUI(?OP_WORKER_GUI, <<"18.02.2">>, ?SHA_THETA)),
    ?assertEqual(true, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.1">>, ?SHA_DELTA)),
    ?assertEqual(true, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.1">>, ?SHA_DELTA)),
    ?assertEqual(true, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.1">>, ?SHA_OMEGA)),
    ?assertEqual(true, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_GAMMA)),
    ?assertEqual(true, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_GAMMA)),
    ?assertEqual(true, ?VerifyGUI(?HARVESTER_GUI, <<"19.02.1">>, ?SHA_KAPPA)),
    ?assertEqual(true, ?VerifyGUI(?HARVESTER_GUI, <<"19.02.1">>, ?SHA_SIGMA)),

    ?assertEqual({false, [?SHA_ALPHA, ?SHA_BETA, ?SHA_THETA]}, ?VerifyGUI(?OP_WORKER_GUI, <<"18.02.2">>, ?SHA_DELTA)),
    ?assertEqual({false, [?SHA_DELTA, ?SHA_OMEGA]}, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.1">>, ?SHA_THETA)),
    ?assertEqual({false, [?SHA_DELTA, ?SHA_OMEGA]}, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.1">>, ?SHA_THETA)),
    ?assertEqual({false, [?SHA_GAMMA]}, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_ALPHA)),
    ?assertEqual({false, [?SHA_GAMMA]}, ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_ALPHA)),
    ?assertEqual({false, [?SHA_KAPPA, ?SHA_SIGMA]}, ?VerifyGUI(?HARVESTER_GUI, <<"19.02.1">>, ?SHA_ALPHA)),

    ?assertEqual({error, {unknown_version, <<"18.02.3">>, {revision, 2019071900}}}, ?VerifyGUI(?OP_WORKER_GUI, <<"18.02.3">>, ?SHA_OMEGA)),
    ?assertEqual({error, {unknown_version, <<"18.02.3">>, {revision, 2019071900}}}, ?VerifyGUI(?ONEPANEL_GUI, <<"18.02.3">>, ?SHA_DELTA)),
    ?assertEqual({error, {unknown_version, <<"18.02.3">>, {revision, 2019071900}}}, ?VerifyGUI(?ONEPANEL_GUI, <<"18.02.3">>, ?SHA_GAMMA)),
    ?assertEqual({error, {unknown_version, <<"18.02.3">>, {revision, 2019071900}}}, ?VerifyGUI(?HARVESTER_GUI, <<"18.02.3">>, ?SHA_KAPPA)).


taking_default_registry_if_newer() ->
    % Older or the same revision should not be taken
    mock_current_registry(#{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>
                ]
            }
        }
    }),
    mock_default_registry(#{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"20.02.1">> => [
                    <<"20.02.1">>
                ]
            }
        }
    }),
    compatibility:clear_registry_cache(),

    ?assertEqual({ok, [<<"18.02.1">>]}, ?OZvsOPVersions(<<"18.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"20.02.1">>, {revision, 2019010100}}}, ?OZvsOPVersions(<<"20.02.1">>)),

    % Newer revision should be taken and overwrite the registry
    DefaultRegistry = #{
        <<"revision">> => 2019020304,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"20.02.1">> => [
                    <<"20.02.1">>
                ]
            }
        }
    },
    mock_default_registry(DefaultRegistry),
    compatibility:clear_registry_cache(),

    ?assertEqual({ok, [<<"20.02.1">>]}, ?OZvsOPVersions(<<"20.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019020304}}}, ?OZvsOPVersions(<<"18.02.1">>)),

    ?assert(is_registry_on_all_nodes_equal_to(DefaultRegistry)).


fetching_newer_registry() ->
    mock_current_registry(#{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>
                ]
            }
        }
    }),
    compatibility:clear_registry_cache(),
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),

    NewerMirror = "https://example.com/compatibility.json",
    NewerRegistry = #{
        <<"revision">> => 2019010200,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>
                ]
            }
        }
    },
    mock_mirror_list([NewerMirror]),
    mock_mirror_result(NewerMirror, {ok, 200, NewerRegistry}),

    % Fetching is attempted after cache expires, hence the mirror should not be
    % requested just yet
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),

    % But should be requested after clearing the cache
    compatibility:clear_registry_cache(),
    %   if a query can returns a successful result, no fetch attempt is made
    ?assertEqual({ok, [<<"18.02.1">>]}, ?OZvsOPVersions(<<"18.02.1">>)),
    %   otherwise, the registry should be fetched
    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assertEqual({ok, [<<"18.02.1">>, <<"18.02.2">>]}, ?OZvsOPVersions(<<"18.02.1">>)),

    % ... and should overwrite the old one
    clear_mocked_mirrors(),
    compatibility:clear_registry_cache(),
    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assert(is_registry_on_all_nodes_equal_to(NewerRegistry)),

    % Fetching should be attempted when the cache expires by itself
    compatibility:clear_registry_cache(),
    ?assertEqual({false, [<<"18.02.1">>, <<"18.02.2">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.3">>)),

    EvenNewerMirror = "https://mirror.com/compatibility.json",
    EvenNewerRegistry = #{
        <<"revision">> => 2019010300,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>,
                    <<"18.02.3">>
                ]
            }
        }
    },
    mock_mirror_list([EvenNewerMirror]),
    mock_mirror_result(EvenNewerMirror, {ok, 200, EvenNewerRegistry}),
    CacheTTL = ctool:get_env(compatibility_registry_cache_ttl_secs),
    clock_freezer_mock:simulate_seconds_passing(CacheTTL - 1),
    % The cache is still valid
    ?assertEqual({false, [<<"18.02.1">>, <<"18.02.2">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.3">>)),
    % But not anymore
    clock_freezer_mock:simulate_seconds_passing(2),
    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"18.02.3">>)),
    ?assert(is_registry_on_all_nodes_equal_to(EvenNewerRegistry)).


fetching_older_registry() ->
    InitialRegistry = #{
        <<"revision">> => 2019050100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>
                ]
            }
        }

    },
    mock_current_registry(InitialRegistry),

    Mirror = "https://example.com/compatibility.json",
    MirrorResult = #{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>
                ]
            }
        }
    },
    mock_mirror_list([Mirror]),
    mock_mirror_result(Mirror, {ok, 200, MirrorResult}),
    compatibility:clear_registry_cache(),

    % The compatibility registry file should not be overwritten if fetched
    % revision is lower.
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assert(is_registry_on_all_nodes_equal_to(InitialRegistry)),
    ?assertNot(is_registry_on_all_nodes_equal_to(MirrorResult)),

    % ... neither when revision is the same
    Mirror2Result = MirrorResult#{<<"revision">> => 2019050100},
    mock_mirror_result(Mirror, {ok, 200, Mirror2Result}),
    compatibility:clear_registry_cache(),
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assert(is_registry_on_all_nodes_equal_to(InitialRegistry)),
    ?assertNot(is_registry_on_all_nodes_equal_to(Mirror2Result)),

    % Make sure it is overwritten when the revision is higher
    Mirror3Result = MirrorResult#{<<"revision">> => 2019050200},
    mock_mirror_result(Mirror, {ok, 200, Mirror3Result}),
    compatibility:clear_registry_cache(),
    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assertNot(is_registry_on_all_nodes_equal_to(InitialRegistry)),
    ?assert(is_registry_on_all_nodes_equal_to(Mirror3Result)).


registry_parsing_error() ->
    % default registry would be taken if the local cannot be parsed (unless its broken too)
    mock_default_registry(<<"invalid">>),

    mock_current_registry(<<"wait:\"what'this'isnot-a-json,17">>),
    compatibility:clear_registry_cache(),
    ?assertEqual(
        {error, cannot_parse_registry},
        ?OZvsOP(<<"19.02.1">>, <<"20.08.1">>)
    ),

    mock_current_registry(#{<<"missing">> => <<"revision">>}),
    compatibility:clear_registry_cache(),
    ?assertEqual(
        {error, cannot_parse_registry},
        ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_GAMMA)
    ),

    % Inexistent compatibility file should also cause {error, cannot_parse_registry}
    RegistryPath = ctool:get_env(current_compatibility_registry_file),
    ok = file:delete(RegistryPath),

    compatibility:clear_registry_cache(),
    ?assertEqual(
        {error, cannot_parse_registry},
        ?VerifyGUI(?ONEPANEL_GUI, <<"19.02.2">>, ?SHA_GAMMA)
    ).


overwriting_broken_or_absent_registry() ->
    mock_current_registry(<<"">>),
    overwriting_broken_or_absent_registry_base(),

    mock_current_registry(<<"not a valid json">>),
    overwriting_broken_or_absent_registry_base(),

    RegistryPath = ctool:get_env(current_compatibility_registry_file),
    ok = file:delete(RegistryPath),
    overwriting_broken_or_absent_registry_base().


overwriting_broken_or_absent_registry_base() ->
    % default registry would be taken if the local cannot be parsed (unless its broken too)
    mock_default_registry(<<"invalid">>),

    Mirror = "https://example.com/compatibility.json",
    MirrorResult = #{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>
                ]
            }
        }
    },
    mock_mirror_list([Mirror]),
    mock_mirror_result(Mirror, {ok, 200, MirrorResult}),
    compatibility:clear_registry_cache(),

    % upon any query, a fetch should be attempted and overwrite the registry
    ?assertNot(is_registry_on_all_nodes_equal_to(MirrorResult)),
    ?assertEqual({ok, [<<"18.02.1">>, <<"18.02.2">>]}, ?OZvsOPVersions(<<"18.02.1">>)),
    ?assert(is_registry_on_all_nodes_equal_to(MirrorResult)).


taking_default_registry_if_local_is_broken() ->
    mock_current_registry(<<"">>),
    taking_default_registry_if_local_is_broken_base(),

    mock_current_registry(<<"not a valid json">>),
    taking_default_registry_if_local_is_broken_base(),

    RegistryPath = ctool:get_env(current_compatibility_registry_file),
    ok = file:delete(RegistryPath),
    taking_default_registry_if_local_is_broken_base().

taking_default_registry_if_local_is_broken_base() ->
    DefaultRegistry = #{
        <<"revision">> => 2019020304,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"20.02.1">> => [
                    <<"20.02.1">>
                ]
            }
        }
    },
    mock_default_registry(DefaultRegistry),
    compatibility:clear_registry_cache(),

    ?assertEqual({ok, [<<"20.02.1">>]}, ?OZvsOPVersions(<<"20.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019020304}}}, ?OZvsOPVersions(<<"18.02.1">>)),

    ?assert(is_registry_on_all_nodes_equal_to(DefaultRegistry)).


resistance_to_node_failures_when_overwriting_registry() ->
    InitialRegistry = #{
        <<"revision">> => 2019010100,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>
                ]
            }
        }
    },
    DefaultRegistry = #{
        <<"revision">> => 2019020304,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"20.02.1">> => [
                    <<"20.02.1">>
                ]
            }
        }
    },
    mock_current_registry(InitialRegistry),
    mock_default_registry(DefaultRegistry),
    compatibility:clear_registry_cache(),

    % mock a situation when the ?ANOTHER_NODE is down - the compatibility check
    % should not crash, as the registry overwriting is done in best-effort manner
    meck:expect(rpc, call, fun
        (Node, Module, Function, Args) when Node == node() ->
            meck:passthrough([Node, Module, Function, Args]);
        (?DUMMY_NODE, compatibility, overwrite_registry, [{registry, Binary, _, _}]) ->
            mock_current_registry_overwrite_on_remote_node(?DUMMY_NODE, Binary);
        (?ANOTHER_NODE, compatibility, overwrite_registry, [_Registry]) ->
            {badrpc, nodedown}
    end),

    ?assertEqual({ok, [<<"20.02.1">>]}, ?OZvsOPVersions(<<"20.02.1">>)),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019020304}}}, ?OZvsOPVersions(<<"18.02.1">>)),

    ?assert(is_registry_on_nodes_equal_to([node(), ?DUMMY_NODE], DefaultRegistry)),
    ?assert(is_registry_on_nodes_equal_to([?ANOTHER_NODE], InitialRegistry)).


trying_multiple_mirrors() ->
    ExampleRegistry = get_current_registry(),
    RegistryWithRev = fun(Rev) ->
        maps:merge(ExampleRegistry, #{<<"revision">> => Rev})
    end,

    mock_current_registry(RegistryWithRev(2019020100)),
    compatibility:clear_registry_cache(),

    Alpha = "https://alpha.com/compatibility.json",
    Beta = "https://beta.com/compatibility.json",
    Gamma = "https://gamma.com/compatibility.json",
    Delta = "https://delta.com/compatibility.json",

    mock_mirror_list([Alpha, Beta, Gamma, Delta]),
    mock_mirror_result(Alpha, {error, nxdomain}),
    mock_mirror_result(Beta, {error, econnrefused}),
    mock_mirror_result(Gamma, {ok, 200, RegistryWithRev(2019020100)}),
    mock_mirror_result(Delta, {ok, 200, RegistryWithRev(2019030100)}),
    compatibility:clear_registry_cache(),

    % Mirrors are tried in order up to the first successful hit that offered a newer revision
    % (the Gamma mirror has offers the same revision as the current compatibility)
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019030100}}}, ?OPvsOC(<<"18.02.1">>, <<"17.06.3">>)),
    ?assert(is_registry_on_all_nodes_equal_to(RegistryWithRev(2019030100))),

    % If a mirror returns an incomprehensible answer, it is ignored
    mock_current_registry(RegistryWithRev(2019010100)),
    mock_mirror_result(Alpha, {ok, 200, <<"wait:\"what'this'isnot-a-json,17">>}),
    mock_mirror_result(Beta, {ok, 200, RegistryWithRev(2019060100)}),
    compatibility:clear_registry_cache(),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019060100}}}, ?OPvsOC(<<"18.02.1">>, <<"17.06.3">>)),
    ?assert(is_registry_on_all_nodes_equal_to(RegistryWithRev(2019060100))),

    % If all mirrors fail, local compatibility file is used for the check and
    % unknown version error is returned
    mock_current_registry(RegistryWithRev(2019010100)),
    mock_mirror_result(Alpha, {ok, 307, <<"">>}),
    mock_mirror_result(Beta, {ok, 204, <<"">>}),
    mock_mirror_result(Gamma, {ok, 200, RegistryWithRev(-1)}),
    mock_mirror_result(Delta, {ok, 200, <<"wait:\"what'this'isnot-a-json,17">>}),
    compatibility:clear_registry_cache(),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019010100}}}, ?OPvsOC(<<"18.02.1">>, <<"17.06.3">>)),
    ?assert(is_registry_on_all_nodes_equal_to(RegistryWithRev(2019010100))),

    mock_current_registry(RegistryWithRev(2019010100)),
    [mock_mirror_result(M, {error, nxdomain}) || M <- [Alpha, Beta, Gamma, Delta]],
    compatibility:clear_registry_cache(),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019010100}}}, ?OPvsOC(<<"18.02.1">>, <<"17.06.3">>)),
    ?assert(is_registry_on_all_nodes_equal_to(RegistryWithRev(2019010100))),

    % The same if there are no mirrors specified at all
    mock_current_registry(RegistryWithRev(2019010100)),
    mock_mirror_list([]),
    compatibility:clear_registry_cache(),
    ?assertEqual({error, {unknown_version, <<"18.02.1">>, {revision, 2019010100}}}, ?OPvsOC(<<"18.02.1">>, <<"17.06.3">>)),
    ?assert(is_registry_on_all_nodes_equal_to(RegistryWithRev(2019010100))).


forcing_check_for_updates() ->
    InitialRegistry = #{
        <<"revision">> => 2019050500,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>
                ]
            }
        }
    },
    mock_current_registry(InitialRegistry),
    compatibility:clear_registry_cache(),
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),

    % older mirror should not overwrite the registry
    OlderMirror = "https://mirror1.example.com/compatibility.json",
    mock_mirror_result(OlderMirror, {ok, 200, #{
        <<"revision">> => 2019030300,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>
                ]
            }
        }
    }}),
    compatibility:check_for_updates(?BuildResolver(), [OlderMirror]),
    ?assertEqual({false, [<<"18.02.1">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assert(is_registry_on_all_nodes_equal_to(InitialRegistry)),

    % newer mirror should overwrite the registry
    NewerMirror = "https://mirror2.example.com/compatibility.json",
    NewerRegistry = #{
        <<"revision">> => 2019070700,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>
                ]
            }
        }
    },
    mock_mirror_result(NewerMirror, {ok, 200, NewerRegistry}),
    compatibility:check_for_updates(?BuildResolver(), [NewerMirror]),
    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"18.02.2">>)),
    ?assertEqual({ok, [<<"18.02.1">>, <<"18.02.2">>]}, ?OZvsOPVersions(<<"18.02.1">>)),
    ?assert(is_registry_on_all_nodes_equal_to(NewerRegistry)),

    % if more than one mirror is given, all should be tried unless one of them is newer
    ?assertEqual({false, [<<"18.02.1">>, <<"18.02.2">>]}, ?OZvsOP(<<"18.02.1">>, <<"18.02.3">>)),

    EvenNewerMirror = "https://mirror3.example.com/compatibility.json",
    EvenNewerRegistry = #{
        <<"revision">> => 2019090900,
        <<"compatibility">> => #{
            <<"onezone:oneprovider">> => #{
                <<"18.02.1">> => [
                    <<"18.02.1">>,
                    <<"18.02.2">>,
                    <<"18.02.3">>
                ]
            }
        }
    },
    mock_mirror_result(NewerMirror, {ok, 200, EvenNewerRegistry}),
    compatibility:check_for_updates(?BuildResolver(), [OlderMirror, NewerMirror, EvenNewerMirror]),
    ?assertEqual(true, ?OZvsOP(<<"18.02.1">>, <<"18.02.3">>)),
    ?assertEqual({ok, [<<"18.02.1">>, <<"18.02.2">>, <<"18.02.3">>]}, ?OZvsOPVersions(<<"18.02.1">>)),
    ?assert(is_registry_on_all_nodes_equal_to(EvenNewerRegistry)).

-endif.
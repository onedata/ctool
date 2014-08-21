%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module tests the functionality of gr_providers module.
%% It contains unit tests that base on eunit.
%% @end
%% ===================================================================

-module(gr_providers_tests).

-ifdef(TEST).

-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_providers.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests description
%% ===================================================================

gr_groups_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"register", fun should_register/0},
            {"unregister", fun should_unregister/0},
            {"get info", fun should_get_info/0},
            {"modify info", fun should_modify_info/0},
            {"check ip address", fun should_check_ip_address/0},
            {"check GUI port", fun should_check_gui_port/0},
            {"check REST port", fun should_check_rest_port/0},
            {"create space", fun should_create_space/0},
            {"support space", fun should_support_space/0},
            {"cancel space support", fun should_cancel_space_support/0},
            {"get spaces", fun should_get_spaces/0},
            {"get space info", fun should_get_space_info/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    meck:new(gr_endpoint),
    meck:expect(gr_endpoint, secure_request, fun
        (client, "/provider", get) -> {ok, "200", response_headers, response_body};
        (client, "/provider", delete) -> {ok, "204", response_headers, response_body};
        (client, "/provider/spaces", get) -> {ok, "200", response_headers, response_body};
        (client, "/provider/spaces/spaceId", get) -> {ok, "200", response_headers, response_body};
        (client, "/provider/spaces/spaceId", delete) -> {ok, "204", response_headers, response_body}
    end),
    meck:expect(gr_endpoint, secure_request, fun
        (client, "/provider", patch, <<"body">>) -> {ok, "200", response_headers, response_body};
        (client, "/provider/spaces", post, <<"body">>) -> {ok, "201", [{"location", "/spaces/spaceId"}], response_body};
        (client, "/provider/spaces/support", post, <<"body">>) -> {ok, "201", [{"location", "/provider/spaces/spaceId"}], response_body}
    end),
    meck:expect(gr_endpoint, insecure_request, fun
        (client, "/provider", post, <<"body">>) -> {ok, "201", response_headers, response_body};
        (client, "/provider/test/check_my_ports", get, <<"body">>) -> {ok, "200", response_headers, response_body}
    end),
    meck:expect(gr_endpoint, insecure_request, fun
        (client, "/provider/test/check_my_ip", get, [], [{connect_timeout, connect_timeout}]) -> {ok, "200", response_headers, response_body}
    end).

teardown(_) ->
    ?assert(meck:validate(gr_endpoint)),
    ok = meck:unload(gr_endpoint).

%% ===================================================================
%% Tests functions
%% ===================================================================

should_register() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [
            {<<"providerId">>, <<"providerId">>},
            {<<"certificate">>, <<"certificate">>}
        ]
    end),

    Answer = gr_providers:register(client, parameters),
    ?assertEqual({ok, <<"providerId">>, <<"certificate">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_unregister() ->
    Answer = gr_providers:unregister(client),
    ?assertEqual(ok, Answer).


should_get_info() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [
            {<<"providerId">>, <<"providerId">>},
            {<<"urls">>, <<"urls">>},
            {<<"redirectionPoint">>, <<"redirectionPoint">>}
        ]
    end),

    Answer = gr_providers:get_info(client),
    ?assertEqual({ok, #provider_info{
        id = <<"providerId">>,
        urls = <<"urls">>,
        redirectionPoint = <<"redirectionPoint">>}
    }, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_modify_info() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_providers:modify_info(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_check_ip_address() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body) -> <<"ipAddress">>
    end),

    Answer = gr_providers:check_ip_address(client, connect_timeout),
    ?assertEqual({ok, <<"ipAddress">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_check_gui_port() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun
        ([{<<"gui">>, <<"https://ipAddress:443/connection_check">>}]) -> <<"body">>
    end),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"gui">>, <<"ok">>}]
    end),

    Answer = gr_providers:check_port(client, <<"ipAddress">>, 443, <<"gui">>),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_check_rest_port() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun
        ([{<<"rest">>, <<"https://ipAddress:8443/rest/latest/connection_check">>}]) -> <<"body">>
    end),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"rest">>, <<"ok">>}]
    end),

    Answer = gr_providers:check_port(client, <<"ipAddress">>, 8443, <<"rest">>),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_create_space() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_providers:create_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_support_space() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_providers:support_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_cancel_space_support() ->
    Answer = gr_providers:cancel_space_support(client, <<"spaceId">>),
    ?assertEqual(ok, Answer).


should_get_spaces() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaces">>, <<"spaces">>}]
    end),

    Answer = gr_providers:get_spaces(client),
    ?assertEqual({ok, <<"spaces">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_space_info() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaceId">>, <<"spaceId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_providers:get_space_info(client, <<"spaceId">>),
    ?assertEqual({ok, #space_info{id = <<"spaceId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).

-endif.

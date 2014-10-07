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

gr_providers_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"register", fun should_register/0},
            {"unregister", fun should_unregister/0},
            {"get details", fun should_get_details/0},
            {"get details for given provider", fun should_get_details_for_given_provider/0},
            {"modify details", fun should_modify_details/0},
            {"check ip address", fun should_check_ip_address/0},
            {"check GUI port", fun should_check_gui_port/0},
            {"check REST port", fun should_check_rest_port/0},
            {"create space", fun should_create_space/0},
            {"support space", fun should_support_space/0},
            {"revoke space support", fun should_revoke_space_support/0},
            {"get spaces", fun should_get_spaces/0},
            {"get space details", fun should_get_space_details/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    meck:new(gr_endpoint),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/provider", get) -> {ok, "200", response_headers, response_body};
        (client, "/provider", delete) -> {ok, "202", response_headers, response_body};
        (client, "/provider/providerId", get) -> {ok, "200", response_headers, response_body};
        (client, "/provider/spaces", get) -> {ok, "200", response_headers, response_body};
        (client, "/provider/spaces/spaceId", get) -> {ok, "200", response_headers, response_body};
        (client, "/provider/spaces/spaceId", delete) -> {ok, "202", response_headers, response_body}
    end),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/provider", patch, <<"body">>) -> {ok, "204", response_headers, response_body};
        (client, "/provider/spaces", post, <<"body">>) -> {ok, "201", [{"location", "/spaces/spaceId"}], response_body};
        (client, "/provider/spaces/support", post, <<"body">>) ->
            {ok, "201", [{"location", "/provider/spaces/spaceId"}], response_body}
    end),
    meck:expect(gr_endpoint, noauth_request, fun
        (client, "/provider", post, <<"body">>) -> {ok, "200", response_headers, response_body};
        (client, "/provider/test/check_my_ports", get, <<"body">>) -> {ok, "200", response_headers, response_body};
        (client, "/provider/test/check_my_ip", get, []) -> {ok, "200", response_headers, response_body}
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


should_get_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [
            {<<"providerId">>, <<"providerId">>},
            {<<"clientName">>, <<"name">>},
            {<<"urls">>, <<"urls">>},
            {<<"redirectionPoint">>, <<"redirectionPoint">>}
        ]
    end),

    Answer = gr_providers:get_details(client),
    ?assertEqual({ok, #provider_details{
        id = <<"providerId">>,
        urls = <<"urls">>,
        name = <<"name">>,
        redirection_point = <<"redirectionPoint">>}
    }, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_details_for_given_provider() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [
            {<<"providerId">>, <<"providerId">>},
            {<<"clientName">>, <<"name">>},
            {<<"urls">>, <<"urls">>},
            {<<"redirectionPoint">>, <<"redirectionPoint">>}
        ]
    end),

    Answer = gr_providers:get_details(client, <<"providerId">>),
    ?assertEqual({ok, #provider_details{
        id = <<"providerId">>,
        name = <<"name">>,
        urls = <<"urls">>,
        redirection_point = <<"redirectionPoint">>}
    }, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_modify_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_providers:modify_details(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_check_ip_address() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body) -> <<"ipAddress">>
    end),

    Answer = gr_providers:check_ip_address(client),
    ?assertEqual({ok, <<"ipAddress">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_check_gui_port() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun
        ([{<<"gui">>, <<"https://ipAddress:443/connection_check">>}]) -> <<"body">>
    end),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"https://ipAddress:443/connection_check">>, <<"ok">>}]
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
        (response_body, [{format, proplist}]) -> [{<<"https://ipAddress:8443/rest/latest/connection_check">>, <<"ok">>}]
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


should_revoke_space_support() ->
    Answer = gr_providers:revoke_space_support(client, <<"spaceId">>),
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


should_get_space_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaceId">>, <<"spaceId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_providers:get_space_details(client, <<"spaceId">>),
    ?assertEqual({ok, #space_details{id = <<"spaceId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).

-endif.

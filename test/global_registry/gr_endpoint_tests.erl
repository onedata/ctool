%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of gr_endpoint module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_endpoint_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

% Root macaroon
-define(MACAROON, <<"macaroon">>).
% Request header with root macaroon
-define(MACAROON_HEADER,
    {<<"macaroon">>, <<"macaroon">>}
).

% List of disch macaroons
-define(DISCH_MACAROONS, [
    <<"disch_macaroon1">>,
    <<"disch_macaroon2">>,
    <<"disch_macaroon3">>
]).
% Bound disch macaroons joined into one string with spaces for request header.
-define(DISCH_MACAROONS_HEADER,
    {<<"discharge-macaroons">>,
        <<"bound_disch_macaroon1 bound_disch_macaroon2 bound_disch_macaroon3">>}
).

-define(CONTENT_TYPE_HEADER,
    {<<"content-type">>, <<"application/json">>}
).

%%%===================================================================
%%% Tests description
%%%===================================================================

gr_endpoint_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"provider request (URN, method)",
                fun should_send_provider_request_1/0},
            {"provider request (URN, method, body)",
                fun should_send_provider_request_2/0},
            {"provider request (URN, method, body, options)",
                fun should_send_provider_request_3/0},
            {"provider request (URN, method, headers, body, options)",
                fun should_send_provider_request_4/0},
            {"user request (URN, method)",
                fun should_send_user_request_1/0},
            {"user request (URN, method, body)",
                fun should_send_user_request_2/0},
            {"user request (URN, method, body, options)",
                fun should_send_user_request_3/0},
            {"user request (URN, method, headers, body, options)",
                fun should_send_user_request_4/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(gr_plugin, [non_strict]),
    meck:expect(gr_plugin, get_gr_url, fun() -> "URL/" end),
    meck:expect(gr_plugin, get_key_path, fun() -> key_path end),
    meck:expect(gr_plugin, get_cert_path, fun() -> cert_path end),

    meck:new(macaroon),
    meck:expect(macaroon, deserialize, fun(X) -> {ok, X} end),
    meck:expect(macaroon, serialize, fun(X) -> {ok, X} end),
    meck:expect(macaroon, prepare_for_request,
        fun(_Macaroon, DischMacaroon) ->
            {ok, <<"bound_", DischMacaroon/binary>>}
        end).


teardown(_) ->
    ?assert(meck:validate(gr_plugin)),
    ok = meck:unload(gr_plugin),
    ?assert(meck:validate(macaroon)),
    ok = meck:unload(macaroon).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_send_provider_request_1() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            <<>>,
            [{pool, no_auth}]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            <<>>,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        provider, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        provider, "URN", method)),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, undefined}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, undefined}, "URN", method)),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).


should_send_provider_request_2() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [{pool, no_auth}]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        provider, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        provider, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, undefined}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, undefined}, "URN", method, body)),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).


should_send_provider_request_3() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [{pool, no_auth}, options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                options
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        provider, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        provider, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, undefined}, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, undefined}, "URN", method, body, [options])),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).


should_send_provider_request_4() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, headers],
            body,
            [{pool, no_auth}, options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, headers],
            body,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                options
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        provider, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        provider, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, undefined}, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, undefined}, "URN", method, [headers], body, [options])),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).


should_send_user_request_1() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            <<>>,
            [{pool, no_auth}]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            <<>>,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).


should_send_user_request_2() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            [{pool, no_auth}]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).


should_send_user_request_3() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            mthd,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            [{pool, no_auth}, opts]
        ) -> ok;
        (
            mthd,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                opts
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).


should_send_user_request_4() ->
    meck:new(hackney_pool),
    meck:expect(hackney_pool, start_pool, fun
        (auth, _) -> ok;
        (no_auth, _) -> ok
    end),
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER,
                headers],
            body,
            [{pool, no_auth}, options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER,
                headers],
            body,
            [
                {pool, auth},
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                options
            ]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),

    ?assert(meck:validate(hackney_pool)),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),
    ok = meck:unload(hackney_pool).

-endif.
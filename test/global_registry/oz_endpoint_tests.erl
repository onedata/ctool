%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of oz_endpoint module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_endpoint_tests).

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

oz_endpoint_test_() ->
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
    meck:new(oz_plugin, [non_strict]),
    meck:expect(oz_plugin, get_oz_url, fun() -> "URL/" end),
    meck:expect(oz_plugin, get_key_path, fun() -> key_path end),
    meck:expect(oz_plugin, get_cert_path, fun() -> cert_path end),

    meck:new(macaroon),
    meck:expect(macaroon, deserialize, fun(X) -> {ok, X} end),
    meck:expect(macaroon, serialize, fun(X) -> {ok, X} end),
    meck:expect(macaroon, prepare_for_request,
        fun(_Macaroon, DischMacaroon) ->
            {ok, <<"bound_", DischMacaroon/binary>>}
        end).


teardown(_) ->
    ?assert(meck:validate(oz_plugin)),
    ok = meck:unload(oz_plugin),
    ?assert(meck:validate(macaroon)),
    ok = meck:unload(macaroon).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_send_provider_request_1() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            <<>>,
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            <<>>,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        provider, "URN", method)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        provider, "URN", method)),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, undefined}, "URN", method)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, undefined}, "URN", method)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_provider_request_2() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        provider, "URN", method, body)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        provider, "URN", method, body)),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, undefined}, "URN", method, body)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, undefined}, "URN", method, body)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_provider_request_3() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                options
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        provider, "URN", method, body, [options])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        provider, "URN", method, body, [options])),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, undefined}, "URN", method, body, [options])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, undefined}, "URN", method, body, [options])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_provider_request_4() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, headers],
            body,
            [options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, headers],
            body,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                options
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        provider, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        provider, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, undefined}, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, undefined}, "URN", method, [headers], body, [options])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_1() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            <<>>,
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            <<>>,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_2() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]}
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method, body)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_3() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            mthd,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            [opts]
        ) -> ok;
        (
            mthd,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER],
            body,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                opts
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", mthd, body, [opts])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_4() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER,
                headers],
            body,
            [options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?MACAROON_HEADER, ?DISCH_MACAROONS_HEADER,
                headers],
            body,
            [
                {ssl_options, [{keyfile, key_path}, {certfile, cert_path}]},
                options
            ]
        ) -> ok
    end),

    ?assertEqual(ok, oz_endpoint:auth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),
    ?assertEqual(ok, oz_endpoint:auth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),
    ?assertEqual(ok, oz_endpoint:noauth_request(
        {try_user, {?MACAROON, ?DISCH_MACAROONS}}, "URN", method,
        [headers], body, [options])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).

-endif.
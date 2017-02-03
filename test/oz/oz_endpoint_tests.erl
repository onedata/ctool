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
-define(MACAROON, macaroon:create("Location", "Key", "Macaroon")).
% Request header with root macaroon
macaroon_header() ->
    {ok, Bin} = token_utils:serialize62(?MACAROON),
    #{<<"macaroon">> => Bin}.

% List of disch macaroons
-define(DISCH_MACAROONS, [
    macaroon:create("Location", "Key1", "Discharge macaroon 1"),
    macaroon:create("Location", "Key2", "Discharge macaroon 2"),
    macaroon:create("Location", "Key3", "Discharge macaroon 3")
]).
% Bound disch macaroons joined into one string with spaces for request header.
disch_macaroons_header() ->
    Macaroon = ?MACAROON,
    BoundMacaroons = lists:map(fun(DM) ->
        BDM = macaroon:prepare_for_request(Macaroon, DM),
        {ok, Bin} = token_utils:serialize62(BDM),
        Bin
    end, ?DISCH_MACAROONS),
    #{<<"discharge-macaroons">> => str_utils:join_binary(BoundMacaroons, <<" ">>)}.

-define(CONTENT_TYPE_HEADER,
    #{<<"content-type">> => <<"application/json">>}
).

-define(CONTENT_TYPE_HEADER_MATCH,
    #{<<"content-type">> := <<"application/json">>}
).

% Request header with HTTP basic auth
-define(BASIC_AUTH_HEADER, <<"Basic ", (base64:encode(<<"user:password">>))/binary>>).
basic_auth_header() ->
    #{<<"Authorization">> => ?BASIC_AUTH_HEADER}.


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
    meck:new(logger_plugin, [non_strict]),
    meck:expect(logger_plugin, gather_metadata, fun() -> [] end),
    meck:new(oz_plugin, [non_strict]),
    meck:expect(oz_plugin, get_oz_url, fun() -> "OZ_URL" end),
    meck:expect(oz_plugin, get_oz_rest_port, fun() -> 9443 end),
    meck:expect(oz_plugin, get_oz_rest_api_prefix, fun() -> "/PREFIX" end),
    meck:expect(oz_plugin, get_key_file, fun() -> key_file end),
    meck:expect(oz_plugin, get_cert_file, fun() -> cert_file end),
    meck:expect(oz_plugin, get_cacerts_dir, fun() -> cacerts_dir end),
    % Function auth_to_rest_client should return REST client based on Auth term.
    % Just return the same - we will use oz_endpoint:client() terms as Auth.
    meck:expect(oz_plugin, auth_to_rest_client, fun(Auth) -> Auth end).

teardown(_) ->
    lists:foreach(fun(Module) ->
        ?assert(meck:validate(Module)),
        ok = meck:unload(Module)
    end, [oz_plugin, logger_plugin]).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_send_provider_request_1() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        ?CONTENT_TYPE_HEADER_MATCH,
        <<>>,
        [{ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}]
    ) -> ok end),
    ?assertEqual(ok, oz_endpoint:provider_request(provider, "/URN", method)),

    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        ?CONTENT_TYPE_HEADER_MATCH,
        <<>>,
        _
    ) -> ok end),
    ?assertEqual(ok, oz_endpoint:request(provider, "/URN", method)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_provider_request_2() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        ?CONTENT_TYPE_HEADER_MATCH,
        body,
        [{ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}]
    ) -> ok end),
    ?assertEqual(ok, oz_endpoint:provider_request(provider, "/URN", method, body)),

    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        ?CONTENT_TYPE_HEADER_MATCH,
        body,
        _
    ) -> ok end),
    ?assertEqual(ok, oz_endpoint:request(provider, "/URN", method, body)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_provider_request_3() ->
    meck:new(http_client),
    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        ?CONTENT_TYPE_HEADER_MATCH,
        body,
        [
            options,
            {ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}
        ]
    ) -> ok end),
    ?assertEqual(ok, oz_endpoint:provider_request(
        provider, "/URN", method, body, [options])),

    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        ?CONTENT_TYPE_HEADER_MATCH,
        body,
        [options | _]
    ) -> ok end),
    ?assertEqual(ok, oz_endpoint:request(
        provider, "/URN", method, body, [options])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_provider_request_4() ->
    ExpectedHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, #{<<"header">> => <<"abcdef">>}
    ]),

    meck:new(http_client),
    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        Headers,
        body,
        [
            options,
            {ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}
        ]
    ) -> case Headers of
        ExpectedHeaders -> ok
    end end),
    ?assertEqual(ok, oz_endpoint:provider_request(
        provider, "/URN", method, #{<<"header">> => <<"abcdef">>}, body, [options])),

    meck:expect(http_client, request, fun(
        method,
        "OZ_URL:9443/PREFIX/URN",
        Headers,
        body,
        [options | _]
    ) -> case Headers of
        ExpectedHeaders -> ok
    end end),
    ?assertEqual(ok, oz_endpoint:request(
        provider, "/URN", method, #{<<"header">> => <<"abcdef">>}, body, [options])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_1() ->
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),

    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            <<>>,
            [{ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}]
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method)),

    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            <<>>,
            _
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_2() ->
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),

    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            body,
            [{ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}]
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", method, body)),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method, body)),

    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            body,
            _
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", method, body)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method, body)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_3() ->
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),

    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            mthd,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            body,
            [
                opts,
                {ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}
            ]
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", mthd, body, [opts])),

    meck:expect(http_client, request, fun
        (
            mthd,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            body,
            [opts | _]
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", mthd, body, [opts])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_user_request_4() ->
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedMacaroonHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER,
        MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),

    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            body,
            [
                options,
                {ssl_options, [{keyfile, key_file}, {certfile, cert_file} | _]}
            ]
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", method,
        #{<<"hdr">> => <<"abcdf">>}, body, [options])),
    ?assertEqual(ok, oz_endpoint:provider_request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method,
        #{<<"hdr">> => <<"abcdf">>}, body, [options])),


    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            body,
            [options | _]
        ) ->
            case Headers of
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, {?MACAROON, ?DISCH_MACAROONS}}, "/URN", method,
        #{<<"hdr">> => <<"abcdf">>}, body, [options])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method,
        #{<<"hdr">> => <<"abcdf">>}, body, [options])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


merge_headers(Headers) ->
    merge_headers(Headers, #{}).
merge_headers([], AccMap) ->
    AccMap;
merge_headers([Header | Tail], AccMap) ->
    merge_headers(Tail, maps:merge(AccMap, Header)).


-endif.

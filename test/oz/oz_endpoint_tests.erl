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

-include("http/headers.hrl").
-include_lib("eunit/include/eunit.hrl").


token() ->
    <<"DUMMY-TOKEN">>.


token_header() ->
    #{?HDR_X_AUTH_TOKEN => token()}.


-define(CONTENT_TYPE_HEADER,
    #{?HDR_CONTENT_TYPE => <<"application/json">>}
).

-define(CONTENT_TYPE_HEADER_MATCH,
    #{?HDR_CONTENT_TYPE := <<"application/json">>}
).

% Request header with HTTP basic auth
-define(BASIC_AUTH_HEADER, <<"Basic ", (base64:encode(<<"user:password">>))/binary>>).
basic_auth_header() ->
    #{?HDR_AUTHORIZATION => ?BASIC_AUTH_HEADER}.


%%%===================================================================
%%% Tests description
%%%===================================================================

oz_endpoint_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"user request (URN, method)",
                fun should_send_request_1/0},
            {"user request (URN, method, body)",
                fun should_send_request_2/0},
            {"user request (URN, method, body, options)",
                fun should_send_request_3/0},
            {"user request (URN, method, headers, body, options)",
                fun should_send_request_4/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_plugin, [non_strict]),
    meck:new(cert_utils, []),
    meck:expect(oz_plugin, get_oz_url, fun() -> "OZ_URL" end),
    meck:expect(oz_plugin, get_oz_rest_port, fun() -> 9443 end),
    meck:expect(oz_plugin, get_oz_rest_api_prefix, fun() -> "/PREFIX" end),
    meck:expect(oz_plugin, get_cacerts_dir, fun() -> cacerts_dir end),
    % Function auth_to_rest_client should return REST client based on Auth term.
    % Just return the same - we will use oz_endpoint:client() terms as Auth.
    meck:expect(oz_plugin, auth_to_rest_client, fun(Auth) -> Auth end),
    meck:expect(cert_utils, load_ders_in_dir, fun(_) -> [] end).

teardown(_) ->
    lists:foreach(fun(Module) ->
        ?assert(meck:validate(Module)),
        ok = meck:unload(Module)
    end, [oz_plugin, cert_utils]).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_send_request_1() ->
    TokenHeader = token_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),

    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            <<>>,
            [{ssl_options, [{cacerts, _} | _]}]
        ) ->
            case Headers of
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {provider, token()}, "/URN", method)),

    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            <<>>,
            _
        ) ->
            case Headers of
                ExpectedTokenHeaders -> ok;
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_request_2() ->
    TokenHeader = token_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),

    meck:new(http_client),
    meck:expect(http_client, request, fun
        (
            method,
            "OZ_URL:9443/PREFIX/URN",
            Headers,
            body,
            [{ssl_options, [{cacerts, _} | _]}]
        ) ->
            case Headers of
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method, body)),
    ?assertEqual(ok, oz_endpoint:request(
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
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method, body)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method, body)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_request_3() ->
    TokenHeader = token_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
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
                {ssl_options, [{cacerts, _} | _]}
            ]
        ) ->
            case Headers of
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:request(
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
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", mthd, body, [opts])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_request_4() ->
    TokenHeader = token_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderTokenHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER, TokenHeader
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
                {ssl_options, [{cacerts, _} | _]}
            ]
        ) ->
            case Headers of
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method,
        #{<<"hdr">> => <<"abcdf">>}, body, [options])),
    ?assertEqual(ok, oz_endpoint:request(
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
                ExpectedTokenHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderTokenHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method,
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

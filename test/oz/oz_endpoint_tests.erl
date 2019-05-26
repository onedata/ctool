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


token() ->
    <<"DUMMY-TOKEN">>.


token_header() ->
    #{<<"X-Auth-Token">> => token()}.


% Root macaroon
macaroon() ->
    M = macaroon:create("Location", "Key", "Macaroon"),
    {ok, Bin} = onedata_macaroons:serialize(M),
    Bin.

% Request header with root macaroon
macaroon_header() ->
    #{<<"Macaroon">> => macaroon()}.


% List of disch macaroons
disch_macaroons() ->
    M1 = macaroon:create("Location", "Key1", "Discharge macaroon 1"),
    M2 = macaroon:create("Location", "Key2", "Discharge macaroon 2"),
    M3 = macaroon:create("Location", "Key3", "Discharge macaroon 3"),
    {ok, Bin1} = onedata_macaroons:serialize(M1),
    {ok, Bin2} = onedata_macaroons:serialize(M2),
    {ok, Bin3} = onedata_macaroons:serialize(M3),
    [Bin1, Bin2, Bin3].

% Bound disch macaroons joined into one string with spaces for request header.
disch_macaroons_header() ->
    MacaroonBin = macaroon(),
    {ok, Macaroon} = onedata_macaroons:deserialize(MacaroonBin),
    BoundMacaroons = lists:map(fun(DMBin) ->
        {ok, DM} = onedata_macaroons:deserialize(DMBin),
        BDM = macaroon:prepare_for_request(Macaroon, DM),
        {ok, Bin} = onedata_macaroons:serialize(BDM),
        Bin
    end, disch_macaroons()),
    #{<<"Discharge-Macaroons">> => str_utils:join_binary(BoundMacaroons, <<" ">>)}.

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
    meck:new(logger_plugin, [non_strict]),
    meck:expect(logger_plugin, gather_metadata, fun() -> [] end),
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
    end, [oz_plugin, logger_plugin, cert_utils]).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_send_request_1() ->
    TokenHeader = token_header(),
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader
    ]),
    ExpectedProviderMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader
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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {provider, macaroon()}, "/URN", method)),

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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", method)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_request_2() ->
    TokenHeader = token_header(),
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader
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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method, body)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", method, body)),
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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method, body)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", method, body)),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", method, body)),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_request_3() ->
    TokenHeader = token_header(),
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderMacaroonHeaders = merge_headers([
        ?CONTENT_TYPE_HEADER, MacaroonHeader
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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", mthd, body, [opts])),
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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", mthd, body, [opts])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, basic, ?BASIC_AUTH_HEADER}, "/URN", mthd, body, [opts])),

    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client).


should_send_request_4() ->
    TokenHeader = token_header(),
    MacaroonHeader = macaroon_header(),
    DischMacaroonsHeader = disch_macaroons_header(),
    BasicAuthHeader = basic_auth_header(),

    ExpectedTokenHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER, TokenHeader
    ]),
    ExpectedMacaroonHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER,
        MacaroonHeader, DischMacaroonsHeader
    ]),
    ExpectedBasicAuthHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER, BasicAuthHeader
    ]),
    ExpectedProviderMacaroonHeaders = merge_headers([
        #{<<"hdr">> => <<"abcdf">>}, ?CONTENT_TYPE_HEADER, MacaroonHeader
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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method,
        #{<<"hdr">> => <<"abcdf">>}, body, [options])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", method,
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
                ExpectedMacaroonHeaders -> ok;
                ExpectedBasicAuthHeaders -> ok;
                ExpectedProviderMacaroonHeaders -> ok
            end
    end),
    ?assertEqual(ok, oz_endpoint:request(
        {user, token, token()}, "/URN", method,
        #{<<"hdr">> => <<"abcdf">>}, body, [options])),
    ?assertEqual(ok, oz_endpoint:request(
        {user, macaroon, {macaroon(), disch_macaroons()}}, "/URN", method,
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

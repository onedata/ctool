%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of onedata_macaroons module.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_macaroons_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").
-include("auth/onedata_macaroons.hrl").
-include("onedata.hrl").
-include_lib("api_errors.hrl").

-define(LOCATION, <<"dummy-location">>).

serialize_and_deserialize_test() ->
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?TIME_CAVEAT(0, 100),
        ?AUTHORIZATION_NONE_CAVEAT
    ]),
    {ok, Serialized} = onedata_macaroons:serialize(M),
    ?assert(is_binary(Serialized)),
    ?assertEqual({ok, M}, onedata_macaroons:deserialize(Serialized)),
    ?assertEqual(?ERROR_BAD_MACAROON, onedata_macaroons:serialize({a, b, c, d})),
    ?assertEqual(?ERROR_BAD_MACAROON, onedata_macaroons:deserialize(<<"adsfasdfvhua89rwfg08asdf">>)).


empty_macaroon_test() ->
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, []),
    ?assertEqual(
        ok,
        onedata_macaroons:verify(M, <<"secret">>, [], [])
    ),
    ?assertEqual(
        ?ERROR_MACAROON_INVALID,
        onedata_macaroons:verify(M, <<"bad-secret">>, [], [])
    ).


macaroon_with_time_caveat_test() ->
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?TIME_CAVEAT(0, 100)
    ]),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(99, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(100, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(101, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(20), onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 20)
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"bad-secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])).


macaroon_with_infinite_time_caveat_test() ->
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?TIME_CAVEAT(0, ?TIME_INFINITY)
    ]),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(100), onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, ?TIME_INFINITY)
    ])).


macaroon_with_full_authorization_caveat_test() ->
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, []),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [])),
    % Macaroon with full authorization is also ok when none is required
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?AUTHORIZATION_NONE_CAVEAT
    ])).


macaroon_with_none_authorization_caveat_test() ->
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?AUTHORIZATION_NONE_CAVEAT
    ]),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"secret">>, [], [])).


macaroon_with_time_and_authorization_caveat_test() ->
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ]),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(101, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(20), onedata_macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 20), ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"bad-secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])).


add_caveats_test() ->
    M0 = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, []),
    ?assertEqual(ok, onedata_macaroons:verify(M0, <<"secret">>, [], [])),
    % Caveats should be verified only if they exist in the macaroon
    ?assertEqual(ok, onedata_macaroons:verify(M0, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M0, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])),

    M1 = onedata_macaroons:add_caveat(M0, ?TIME_CAVEAT(0, 100)),
    % Now we need to add caveats verifiers or verification will fail
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M1, <<"secret">>, [], [])),
    ?assertEqual(ok, onedata_macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, onedata_macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(100, 100)
    ])),

    M2 = onedata_macaroons:add_caveat(M0, ?AUTHORIZATION_NONE_CAVEAT),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M2, <<"secret">>, [], [])),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])).


further_time_confinement_test() ->
    M1 = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?TIME_CAVEAT(0, 100)
    ]),
    ?assertEqual(ok, onedata_macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, onedata_macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(100, 100)
    ])),

    M2 = onedata_macaroons:add_caveat(M1, ?TIME_CAVEAT(0, 10)),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(10), onedata_macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 10)
    ])),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(10), onedata_macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(10, 10)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, onedata_macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(10, 100)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ok.


session_id_caveat_test() ->
    CaveatSessionId = <<"sessId">>,
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?SESSION_ID_CAVEAT(CaveatSessionId)
    ]),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?SESSION_ID_VERIFIER(fun(_SessionId) -> false end)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?SESSION_ID_VERIFIER(fun(SessionId) -> SessionId == CaveatSessionId end)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?SESSION_ID_VERIFIER(fun(_SessionId) -> true end)
    ])).


cluster_type_caveat_test() ->
    MOz = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?CLUSTER_TYPE_CAVEAT(?ONEZONE)
    ]),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(MOz, <<"secret">>, [], [
        ?CLUSTER_TYPE_CAVEAT(?ONEPROVIDER)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(MOz, <<"secret">>, [], [
        ?CLUSTER_TYPE_CAVEAT(?ONEZONE)
    ])),

    MOp = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?CLUSTER_TYPE_CAVEAT(?ONEPROVIDER)
    ]),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(MOp, <<"secret">>, [], [
        ?CLUSTER_TYPE_CAVEAT(?ONEZONE)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(MOp, <<"secret">>, [], [
        ?CLUSTER_TYPE_CAVEAT(?ONEPROVIDER)
    ])).


cluster_id_caveat_test() ->
    ClusterId = <<"ClusterId">>,
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?CLUSTER_ID_CAVEAT(ClusterId)
    ]),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?CLUSTER_ID_CAVEAT(<<"badClusterId">>)
    ])),
    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?CLUSTER_ID_CAVEAT(ClusterId)
    ])).


gui_macaroon_test() ->
    CaveatSessionId = <<"sessId">>,
    ClusterId = <<"ClusterId">>,
    M = onedata_macaroons:create(?LOCATION, <<"secret">>, <<"identifier">>, [
        ?SESSION_ID_CAVEAT(CaveatSessionId),
        ?CLUSTER_TYPE_CAVEAT(?ONEPROVIDER),
        ?CLUSTER_ID_CAVEAT(ClusterId)
    ]),

    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?CLUSTER_TYPE_CAVEAT(?ONEPROVIDER),
        ?CLUSTER_ID_CAVEAT(ClusterId)
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?SESSION_ID_VERIFIER(CaveatSessionId),
        ?CLUSTER_ID_CAVEAT(ClusterId)
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?SESSION_ID_VERIFIER(CaveatSessionId),
        ?CLUSTER_TYPE_CAVEAT(?ONEPROVIDER)
    ])),

    ?assertEqual(ok, onedata_macaroons:verify(M, <<"secret">>, [], [
        ?SESSION_ID_VERIFIER(fun(SessionId) -> SessionId == CaveatSessionId end),
        ?CLUSTER_TYPE_CAVEAT(?ONEPROVIDER),
        ?CLUSTER_ID_CAVEAT(ClusterId)
    ])).

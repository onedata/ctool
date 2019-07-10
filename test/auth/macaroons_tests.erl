%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of macaroons module.
%%% @end
%%%-------------------------------------------------------------------
-module(macaroons_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").
-include("aai/aai.hrl").
-include("onedata.hrl").
-include("api_errors.hrl").

-define(LOCATION, <<"dummy-location">>).

serialize_and_deserialize_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
        ?TIME_CAVEAT(0, 100),
        ?AUTHORIZATION_NONE_CAVEAT
    ]),
    {ok, Serialized} = macaroons:serialize(M),
    ?assert(is_binary(Serialized)),
    ?assertEqual({ok, M}, macaroons:deserialize(Serialized)),
    ?assertEqual(?ERROR_BAD_MACAROON, macaroons:serialize({a, b, c, d})),
    ?assertEqual(?ERROR_BAD_MACAROON, macaroons:deserialize(<<"adsfasdfvhua89rwfg08asdf">>)).


empty_macaroon_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, []),
    ?assertEqual(
        ok,
        macaroons:verify(M, <<"secret">>, [], [])
    ),
    ?assertEqual(
        ?ERROR_MACAROON_INVALID,
        macaroons:verify(M, <<"bad-secret">>, [], [])
    ).


macaroon_with_time_caveat_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
        ?TIME_CAVEAT(0, 100)
    ]),
    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(99, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(100, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(101, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(20), macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 20)
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M, <<"bad-secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])).


macaroon_with_infinite_time_caveat_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
        ?TIME_CAVEAT(0, ?TIME_INFINITY)
    ]),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(100), macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, ?TIME_INFINITY)
    ])).


macaroon_with_full_authorization_caveat_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, []),
    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [])),
    % Macaroon with full authorization is also ok when none is required
    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [
        ?AUTHORIZATION_NONE_CAVEAT
    ])).


macaroon_with_none_authorization_caveat_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
        ?AUTHORIZATION_NONE_CAVEAT
    ]),
    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [
        ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M, <<"secret">>, [], [])).


macaroon_with_time_and_authorization_caveat_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ]),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(101, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(20), macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 20), ?AUTHORIZATION_NONE_CAVEAT
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M, <<"bad-secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])).


add_caveats_test() ->
    M0 = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, []),
    ?assertEqual(ok, macaroons:verify(M0, <<"secret">>, [], [])),
    % Caveats should be verified only if they exist in the macaroon
    ?assertEqual(ok, macaroons:verify(M0, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, macaroons:verify(M0, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])),

    M1 = macaroons:add_caveat(M0, ?TIME_CAVEAT(0, 100)),
    % Now we need to add caveats verifiers or verification will fail
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M1, <<"secret">>, [], [])),
    ?assertEqual(ok, macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(100, 100)
    ])),

    M2 = macaroons:add_caveat(M0, ?AUTHORIZATION_NONE_CAVEAT),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M2, <<"secret">>, [], [])),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(ok, macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100), ?AUTHORIZATION_NONE_CAVEAT
    ])).


further_time_confinement_test() ->
    M1 = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
        ?TIME_CAVEAT(0, 100)
    ]),
    ?assertEqual(ok, macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroons:verify(M1, <<"secret">>, [], [
        ?TIME_CAVEAT(100, 100)
    ])),

    M2 = macaroons:add_caveat(M1, ?TIME_CAVEAT(0, 10)),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(10), macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 10)
    ])),
    ?assertEqual(?ERROR_MACAROON_TTL_TO_LONG(10), macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(10, 10)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(10, 100)
    ])),
    ?assertEqual(ok, macaroons:verify(M2, <<"secret">>, [], [
        ?TIME_CAVEAT(0, 100)
    ])),
    ok.


audience_caveat_test() ->
    Audiences = [
        ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID),
        ?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
        ?AUD(?OP_WORKER, <<"provider-id">>),
        ?AUD(?OP_PANEL, <<"provider-id">>),
        ?AUD(user, <<"user-id">>)
    ],

    lists:foreach(fun(Audience) ->
        Mac = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
            ?AUDIENCE_CAVEAT(Audience)
        ]),
        ?assertEqual(ok, macaroons:verify(Mac, <<"secret">>, [], [
            ?AUDIENCE_CAVEAT(Audience)
        ])),
        lists:foreach(fun(BadAudience) ->
            ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(Mac, <<"secret">>, [], [
                ?AUDIENCE_CAVEAT(BadAudience)
            ]))
        end, Audiences -- [Audience])
    end, Audiences).


gui_macaroon_test() ->
    M = macaroons:create(?LOCATION, <<"identifier">>, <<"secret">>, [
        ?TIME_CAVEAT(0, 100),
        ?AUDIENCE_CAVEAT(?AUD(?OP_WORKER, <<"provider-id">>))
    ]),

    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M, <<"secret">>, [], [
        ?AUDIENCE_CAVEAT(?AUD(?OP_WORKER, <<"provider-id">>))
    ])),
    ?assertEqual(?ERROR_MACAROON_INVALID, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(10, 100)
    ])),
    ?assertEqual(?ERROR_MACAROON_EXPIRED, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(101, 100),
        ?AUDIENCE_CAVEAT(?AUD(?OP_WORKER, <<"provider-id">>))
    ])),

    ?assertEqual(ok, macaroons:verify(M, <<"secret">>, [], [
        ?TIME_CAVEAT(10, 100),
        ?AUDIENCE_CAVEAT(?AUD(?OP_WORKER, <<"provider-id">>))
    ])).

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of tokens module.
%%% @end
%%%-------------------------------------------------------------------
-module(tokens_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").
-include("aai/aai.hrl").
-include("onedata.hrl").
-include("api_errors.hrl").

-define(OZ_DOMAIN, <<"onezone.example.com">>).

-define(MOCK_COWBOY_REQ(Headers), #{headers => Headers}).


construct_test() ->
    Prototype = #auth_token{
        onezone_domain = ?OZ_DOMAIN,
        nonce = <<"xcvbdsfgergdfg">>,
        persistent = false,
        subject = ?SUB(?ONEPROVIDER, <<"provider-id">>),
        type = ?ACCESS_TOKEN
    },
    ?assertMatch(#auth_token{}, tokens:construct(Prototype, <<"secret-1">>, [
        ?AUTHORIZATION_NONE_CAVEAT,
        ?TIME_CAVEAT(100, 500),
        ?AUDIENCE_CAVEAT(?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID))
    ])).


verify_test() ->
    Nonce = <<"masdf8y76aftq3briuasdf97sabzdv09zsv">>,
    Secret = <<"secret-2">>,
    Subject = ?SUB(user, <<"user-id">>),
    Prototype = #auth_token{
        onezone_domain = ?OZ_DOMAIN,
        nonce = Nonce,
        persistent = true,
        subject = Subject,
        type = ?ACCESS_TOKEN
    },
    Token = tokens:construct(Prototype, Secret, [
        ?AUTHORIZATION_NONE_CAVEAT,
        ?TIME_CAVEAT(100, 500),
        ?AUDIENCE_CAVEAT(?AUD(?OP_WORKER, <<"prov-id">>))
    ]),

    CaveatVerifiers = [
        ?AUTHORIZATION_NONE_CAVEAT,
        ?TIME_CAVEAT(200, 500),
        ?AUDIENCE_CAVEAT(?AUD(?OP_WORKER, <<"prov-id">>))
    ],
    ?assertMatch({ok, Subject}, tokens:verify(Token, Secret, [], CaveatVerifiers)),

    BadVerifiersExamples = [CaveatVerifiers -- [Verifier] || Verifier <- CaveatVerifiers],
    lists:foreach(fun(BadVerifiers) ->
        ?assertMatch(?ERROR_MACAROON_INVALID, tokens:verify(Token, Secret, [], BadVerifiers))
    end, BadVerifiersExamples),

    ok.


serialize_and_deserialize_test() ->
    Nonce = <<"87ycxtv67ctxv96zvtc0z7vcysdf">>,
    Secret = <<"secret-3">>,
    Subject = ?SUB(user, <<"another-user-id">>),
    Prototype = #auth_token{
        onezone_domain = ?OZ_DOMAIN,
        nonce = Nonce,
        persistent = true,
        subject = Subject,
        type = ?ACCESS_TOKEN
    },
    Caveats = [
        ?TIME_CAVEAT(15, 20),
        ?AUDIENCE_CAVEAT(?AUD(?OP_PANEL, <<"another-prov-id">>))
    ],
    CaveatVerifiers = [
        ?TIME_CAVEAT(16, 20),
        ?AUDIENCE_CAVEAT(?AUD(?OP_PANEL, <<"another-prov-id">>))
    ],
    Token = tokens:construct(Prototype, Secret, Caveats),

    ?assertMatch({ok, _}, tokens:serialize(Token)),
    {ok, Serialized} = tokens:serialize(Token),
    ?assertEqual({ok, Token}, tokens:deserialize(Serialized)),
    ?assertMatch({ok, Subject}, tokens:verify(Token, Secret, [], CaveatVerifiers)),

    ?assertMatch(?ERROR_BAD_MACAROON, tokens:serialize({a, b, c, d})),
    ?assertMatch(?ERROR_BAD_MACAROON, tokens:deserialize(<<"rubbish-123">>)).


generate_secret_test() ->
    ?assertNotEqual(tokens:generate_secret(), tokens:generate_secret()).


serialize_and_deserialize_audience_test() ->
    Nonce = <<"z234xcvzasdfa0sd8fh7a8wesdd352a24">>,
    Secret = <<"secret-4">>,
    SessionId = <<"session00-id">>,
    Subject = ?SUB(user, <<"another-user-id">>),
    Prototype = #auth_token{
        onezone_domain = ?OZ_DOMAIN,
        nonce = Nonce,
        persistent = false,
        subject = Subject,
        type = ?GUI_TOKEN(SessionId)
    },

    Token = tokens:construct(Prototype, Secret, [?TIME_CAVEAT(1000, 3600)]),
    AudienceType = ?OZ_PANEL,
    AudienceToken = #audience_token{audience_type = AudienceType, token = Token},

    % serialize_audience_token has two versions, first takes the #audience_token{} record
    ?assertMatch({ok, _}, tokens:serialize_audience_token(AudienceToken)),
    {ok, SerializedAudience} = tokens:serialize_audience_token(AudienceToken),
    ?assertEqual({ok, AudienceToken}, tokens:deserialize_audience_token(SerializedAudience)),
    ?assertMatch(?ERROR_BAD_MACAROON, tokens:serialize_audience_token(
        AudienceToken#audience_token{token = <<"blabla">>}
    )),
    ?assertException(error, badarg, tokens:serialize_audience_token(
        AudienceToken#audience_token{audience_type = wait_what_this_is_not_a_valid_audience_type}
    )),

    % ... and the second takes audience type and serialized token
    {ok, SerializedToken} = tokens:serialize(Token),
    ?assertMatch(SerializedAudience, tokens:serialize_audience_token(AudienceType, SerializedToken)),
    ?assertException(error, badarg, tokens:serialize_audience_token(rubbish, SerializedToken)),

    % Token with no audience indicator should default to user audience
    ?assertEqual(
        {ok, #audience_token{audience_type = user, token = Token}},
        tokens:deserialize_audience_token(SerializedToken)
    ),

    BadAudienceToken = tokens:serialize_audience_token(?OZ_WORKER, <<"bad-token">>),
    ?assertEqual(?ERROR_BAD_AUDIENCE_TOKEN, tokens:deserialize_audience_token(BadAudienceToken)).


access_token_headers_manipulation_test() ->
    AccessToken = <<"gimme-access-123">>,

    ?assertEqual(undefined, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(#{}))),
    ?assertEqual(undefined, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(#{
        <<"unsupported-header">> => AccessToken
    }))),

    Headers = tokens:build_access_token_header(AccessToken),
    ?assertEqual(AccessToken, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(Headers))),

    lists:foreach(fun
        (<<"authorization">>) ->
            ?assertEqual(AccessToken, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(#{
                <<"authorization">> => <<"Bearer ", AccessToken/binary>>
            })));
        (SupportedHeader) ->
            ?assertEqual(AccessToken, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(#{
                SupportedHeader => AccessToken
            })))
    end, tokens:supported_access_token_headers()).


audience_token_headers_manipulation_test() ->
    AudienceToken = <<"my-horse-is-amazing">>,

    ?assertEqual(undefined, tokens:parse_audience_token_header(?MOCK_COWBOY_REQ(#{}))),
    ?assertEqual(undefined, tokens:parse_audience_token_header(?MOCK_COWBOY_REQ(#{
        <<"unsupported-header">> => AudienceToken
    }))),

    Headers = tokens:build_audience_token_header(AudienceToken),
    ?assertEqual(AudienceToken, tokens:parse_audience_token_header(?MOCK_COWBOY_REQ(Headers))).





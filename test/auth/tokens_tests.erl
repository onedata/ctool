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

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("onedata.hrl").
-include("aai/aai.hrl").
-include("graph_sync/graph_sync.hrl").
-include("api_errors.hrl").

-define(OZ_DOMAIN, <<"onezone.example.com">>).
-define(MOCK_COWBOY_REQ(Headers), #{headers => Headers}).
% Dummy timestamp, tokens API always depends on timestamps provided, so
% any value can be used
-define(NOW(), 15000000000).
-define(RAND_STR, hex_utils:hex(crypto:strong_rand_bytes(16))).

%%%===================================================================
%%% Test functions
%%%===================================================================

bad_token_test() ->
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:serialize({a, b, c, d})),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:serialize(<<"rubbish-123">>)),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:serialize(#auth_token{})),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:deserialize({a, b, c, d})),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:deserialize(<<"rubbish-123">>)).


invalid_subject_test() ->
    Prototype = #auth_token{
        onezone_domain = ?OZ_DOMAIN,
        nonce = ?RAND_STR,
        persistent = false,
        type = ?ACCESS_TOKEN
    },
    InvalidSubjects = [?SUB(nobody), ?SUB(root), ?SUB(invalid_sub)],

    lists:foreach(fun(Subject) ->
        ?assertThrow(
            ?ERROR_TOKEN_SUBJECT_INVALID,
            tokens:construct(Prototype#auth_token{subject = Subject}, <<"secret-1">>, [])
        )
    end, InvalidSubjects),

    % Subjects are not supported in version 1 - the value is ignored
    lists:foreach(fun(Subject) ->
        ?assertMatch(
            #auth_token{},
            tokens:construct(Prototype#auth_token{version = 1, subject = Subject}, <<"secret-1">>, [])
        )
    end, InvalidSubjects).


confine_test() ->
    Prototype = #auth_token{
        onezone_domain = ?OZ_DOMAIN,
        nonce = ?RAND_STR,
        subject = ?SUB(user, <<"uid">>),
        persistent = true,
        type = ?GUI_TOKEN(?RAND_STR)
    },
    Secret = ?RAND_STR,

    CvAud = #cv_audience{audience = ?AUD(user, <<"user-id">>)},
    CvTime = #cv_time{valid_until = 8374891234},
    CvAuthNone = #cv_authorization_none{},

    % LimitedToken has 3 above caveats, create an empty token and check if adding
    % them gradually will eventually yield the same token. Check that confining
    % serialized tokens works the same.

    LimitedToken = tokens:construct(Prototype, Secret, [CvAud, CvTime, CvAuthNone]),
    TokenAlpha = tokens:construct(Prototype, Secret, []),
    {ok, SerializedAlpha} = tokens:serialize(TokenAlpha),
    ?assertNotEqual(LimitedToken, TokenAlpha),

    TokenBeta = tokens:confine(TokenAlpha, CvAud),
    {ok, SerializedBeta} = tokens:serialize(TokenBeta),
    ?assertNotEqual(LimitedToken, TokenBeta),
    ?assertEqual({ok, TokenBeta}, tokens:deserialize(tokens:confine(SerializedAlpha, CvAud))),

    TokenGamma = tokens:confine(TokenBeta, CvTime),
    {ok, SerializedGamma} = tokens:serialize(TokenGamma),
    ?assertNotEqual(LimitedToken, TokenGamma),
    ?assertEqual({ok, TokenGamma}, tokens:deserialize(tokens:confine(SerializedBeta, CvTime))),

    TokenDelta = tokens:confine(TokenGamma, CvAuthNone),
    ?assertEqual(LimitedToken, TokenDelta),
    ?assertEqual({ok, TokenDelta}, tokens:deserialize(tokens:confine(SerializedGamma, CvAuthNone))),

    ?assertEqual(LimitedToken, tokens:confine(TokenAlpha, [CvAud, CvTime, CvAuthNone])),
    ?assertEqual({ok, LimitedToken}, tokens:deserialize(tokens:confine(SerializedAlpha, [CvAud, CvTime, CvAuthNone]))).


secret_generation_test() ->
    ?assertMatch(<<_/binary>>, tokens:generate_secret()),
    ?assertNotEqual(tokens:generate_secret(), tokens:generate_secret()).


audience_serialization_deserialization_test() ->
    lists:foreach(fun(AudienceType) ->
        audience_serialization_deserialization_test_base(AudienceType)
    end, [user, group, ?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER, ?OP_PANEL]).

audience_serialization_deserialization_test_base(AudienceType) ->
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

    Token = tokens:construct(Prototype, Secret, [
        #cv_time{valid_until = ?NOW() + 1000}
    ]),
    AudienceToken = #audience_token{audience_type = AudienceType, token = Token},

    % serialize_audience_token has two versions, first takes the #audience_token{} record
    ?assertMatch({ok, _}, tokens:serialize_audience_token(AudienceToken)),
    {ok, SerializedAudience} = tokens:serialize_audience_token(AudienceToken),
    ?assertEqual({ok, AudienceToken}, tokens:deserialize_audience_token(SerializedAudience)),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:serialize_audience_token(
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


find_caveats_test() ->
    F = fun caveats:find/2,
    ?assertEqual(
        false,
        F(cv_time, [])
    ),
    ?assertEqual(
        false,
        F(cv_time, [
            #cv_asn{whitelist = [322]},
            #cv_audience{audience = ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_api{whitelist = [{all, all, ?GRI('*', <<"*">>, '*', '*')}]}
        ])
    ),
    ?assertEqual(
        {true, [#cv_time{valid_until = 123}]},
        F(cv_time, [#cv_time{valid_until = 123}])
    ),
    ?assertEqual(
        {true, [#cv_time{valid_until = 123}, #cv_time{valid_until = 456}, #cv_time{valid_until = 789}]},
        F(cv_time, [
            #cv_time{valid_until = 123},
            #cv_asn{whitelist = [322]},
            #cv_audience{audience = ?AUD(group, <<"123">>)},
            #cv_time{valid_until = 456},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_time{valid_until = 789},
            #cv_api{whitelist = [{all, all, ?GRI('*', <<"*">>, '*', '*')}]}
        ])
    ).


filter_caveats_test() ->
    F = fun caveats:filter/2,
    ?assertEqual(
        [],
        F([cv_time], [])
    ),
    ?assertEqual(
        [],
        F([cv_time], [
            #cv_asn{whitelist = [322]},
            #cv_audience{audience = ?AUD(user, <<"567">>)},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_api{whitelist = [{all, all, ?GRI('*', <<"*">>, '*', '*')}]}
        ])
    ),
    ?assertEqual(
        [#cv_time{valid_until = 123}],
        F([cv_time], [#cv_time{valid_until = 123}])
    ),
    ?assertEqual(
        [
            #cv_time{valid_until = 123},
            #cv_asn{whitelist = [322]},
            #cv_time{valid_until = 456},
            #cv_time{valid_until = 789},
            #cv_api{whitelist = [{all, all, ?GRI('*', <<"*">>, '*', '*')}]}
        ],
        F([cv_asn, cv_api, cv_time], [
            #cv_time{valid_until = 123},
            #cv_asn{whitelist = [322]},
            #cv_audience{audience = ?AUD(?OP_PANEL, <<"provider-id">>)},
            #cv_time{valid_until = 456},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_time{valid_until = 789},
            #cv_api{whitelist = [{all, all, ?GRI('*', <<"*">>, '*', '*')}]}
        ])
    ).



%%%===================================================================
%%% Token manipulation tests (combinations)
%%%===================================================================
% These tests check token construction, serialization, deserialization and verification
% with different combinations of token data, caveats and auth_ctx.

% IP examples from different regions / countries (dummy examples)
-define(IP_LH, {127, 0, 0, 1}).      % none          / none  (localhost)
-define(IP_EG, {241, 157, 22, 3}).   % Egypt         / Africa
-define(IP_AQ, {53, 109, 199, 14}).  % Antarctica    / Antarctica
-define(IP_IN, {7, 8, 242, 211}).    % India         / Asia
-define(IP_PL, {149, 156, 16, 7}).   % Poland        / Europe + EU
-define(IP_MK, {94, 34, 116, 37}).   % Macedonia     / Europe
-define(IP_US, {89, 100, 113, 94}).  % United States / NorthAmerica
-define(IP_AU, {100, 120, 41, 157}). % Australia     / Oceania
-define(IP_BR, {213, 5, 9, 34}).     % Brazil        / SouthAmerica

% Audience examples
-define(AUD_USR, ?AUD(user, <<"user-id">>)).
-define(AUD_GRP, ?AUD(group, <<"group-id">>)).
-define(AUD_OZW, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)).
-define(AUD_OZP, ?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)).
-define(AUD_OPW, ?AUD(?OP_WORKER, <<"provider-id">>)).
-define(AUD_OPP, ?AUD(?OP_PANEL, <<"provider-id">>)).

% Mapping of IP examples to ASN, country and regions (dummy examples)
asn(?IP_LH) -> {error, not_found};
asn(?IP_EG) -> {ok, 815};
asn(?IP_AQ) -> {ok, 13};
asn(?IP_IN) -> {ok, 415};
asn(?IP_PL) -> {ok, 2904};
asn(?IP_MK) -> {ok, 1156};
asn(?IP_US) -> {ok, 79};
asn(?IP_AU) -> {ok, 951};
asn(?IP_BR) -> {ok, 314}.

country(?IP_LH) -> {error, not_found};
country(?IP_EG) -> {ok, <<"EG">>};
country(?IP_AQ) -> {ok, <<"AQ">>};
country(?IP_IN) -> {ok, <<"IN">>};
country(?IP_PL) -> {ok, <<"PL">>};
country(?IP_MK) -> {ok, <<"MK">>};
country(?IP_US) -> {ok, <<"US">>};
country(?IP_AU) -> {ok, <<"AU">>};
country(?IP_BR) -> {ok, <<"BR">>}.

region(?IP_LH) -> {error, not_found};
region(?IP_EG) -> {ok, [<<"Africa">>]};
region(?IP_AQ) -> {ok, [<<"Antarctica">>]};
region(?IP_IN) -> {ok, [<<"Asia">>]};
region(?IP_PL) -> {ok, [<<"Europe">>, <<"EU">>]};
region(?IP_MK) -> {ok, [<<"Europe">>]};
region(?IP_US) -> {ok, [<<"NorthAmerica">>]};
region(?IP_AU) -> {ok, [<<"Oceania">>]};
region(?IP_BR) -> {ok, [<<"SouthAmerica">>]}.

% Example audiences - bound to the IP for easier test code
audience(?IP_LH) -> undefined;
audience(?IP_EG) -> ?AUD_USR;
audience(?IP_AQ) -> ?AUD_OPW;
audience(?IP_IN) -> undefined;
audience(?IP_PL) -> ?AUD_OZW;
audience(?IP_MK) -> ?AUD_OPP;
audience(?IP_US) -> ?AUD_GRP;
audience(?IP_AU) -> ?AUD_USR;
audience(?IP_BR) -> ?AUD_OZP.

% Example auth_ctx's - bound to the IP for easier test code
auth_ctx(Ip) ->
    #auth_ctx{
        current_timestamp = ?NOW(),
        audience = audience(Ip),
        ip = Ip
    }.

to_asns(IpList) ->
    [element(2, {ok, _} = asn(X)) || X <- IpList, X /= ?IP_LH].

to_countries(IpList) ->
    [element(2, {ok, _} = country(X)) || X <- IpList, X /= ?IP_LH].

to_regions(IpList) ->
    lists:flatten([element(2, {ok, _} = region(X)) || X <- IpList, X /= ?IP_LH]).

% File paths must not contain the 0 (NULL) or slash characters
-define(RAND_FILE_NAME, binary:replace(
    binary:replace(crypto:strong_rand_bytes(16), <<0>>, <<"">>, [global]),
    <<$/>>, <<"">>, [global]
)).

-define(RAND_PATH, str_utils:join_binary([<<"">> | rand_sublist([
    ?RAND_FILE_NAME, ?RAND_FILE_NAME, ?RAND_FILE_NAME, ?RAND_FILE_NAME
], 1, 4)], <<"/">>)).

-define(IP_EXAMPLES, [
    ?IP_LH, ?IP_EG, ?IP_AQ, ?IP_IN, ?IP_PL, ?IP_MK, ?IP_US, ?IP_AU, ?IP_BR
]).
-define(RAND_IP, utils:random_element(?IP_EXAMPLES)).

-define(AUDIENCE_EXAMPLES, [
    ?AUD_USR, ?AUD_GRP, ?AUD_OZW, ?AUD_OZP, ?AUD_OPW, ?AUD_OPP
]).

-define(AUTH_CTX_EXAMPLES, [
    auth_ctx(Ip) || Ip <- ?IP_EXAMPLES
]).

-define(ALL_REGIONS, [
    <<"Africa">>, <<"Antarctica">>, <<"Asia">>, <<"Europe">>,
    <<"NorthAmerica">>, <<"Oceania">>, <<"SouthAmerica">>
]).

-record(caveats_testcase, {
    % Caveats in the token
    caveats = [] :: [caveats:caveat()],
    % Caveats that are expected to be unverified
    unverified = [] :: [caveats:caveat()]
}).


token_manipulation_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        {inparallel, 4, combinations()}
    }.


setup() ->
    meck:new(ip_utils, [passthrough]),
    meck:expect(ip_utils, lookup_asn, fun asn/1),
    meck:expect(ip_utils, lookup_country, fun country/1),
    meck:expect(ip_utils, lookup_region, fun region/1).


teardown(_) ->
    ?assert(meck:validate(ip_utils)),
    ?assertEqual(ok, meck:unload(ip_utils)).


combinations() ->
    % Persistence combinations
    combinations(true) ++
    combinations(false).


combinations(Persistence) ->
    % Subject combinations
    combinations(Persistence, ?SUB(user, ?RAND_STR)) ++
    combinations(Persistence, ?SUB(?ONEPROVIDER, ?RAND_STR)).

combinations(Persistence, Subject) ->
    % Token type combinations
    combinations(Persistence, Subject, ?ACCESS_TOKEN) ++
    combinations(Persistence, Subject, ?GUI_TOKEN(?RAND_STR)).

combinations(Persistence, Subject, Type) -> [
    % AuthCtx combinations
    % Placing the ?_test macro here yields (P * S * T * A) testcases
    %   P - persistence types
    %   S - subject types
    %   T - token types
    %   A - AuthCtx examples
    {timeout, 60, ?_test(combinations(Persistence, Subject, Type, AuthCtx))}
    ||
    AuthCtx <- ?AUTH_CTX_EXAMPLES
].

combinations(Persistence, Subject, Type, AuthCtx) -> [
    % Caveats combinations
    combinations(Persistence, Subject, Type, AuthCtx, CaveatsTestcase)
    ||
    CaveatsTestcase <- randomize_caveats_testcases(caveats_testcases(AuthCtx))
].

combinations(Persistent, Subject, Type, AuthCtx, #caveats_testcase{caveats = Caveats, unverified = Unverified}) ->
    Secret = ?RAND_STR,
    Prototype = #auth_token{
        onezone_domain = ?OZ_DOMAIN,
        nonce = ?RAND_STR,
        persistent = Persistent,
        subject = Subject,
        type = Type
    },
    Token = tokens:construct(Prototype, Secret, Caveats),

    check_serialize_deserialize(Token),
    check_verification_result(Token, Secret, Subject, AuthCtx, Caveats, Unverified),
    check_supported_caveats(Token, Secret, AuthCtx, Caveats, Unverified).


check_serialize_deserialize(Token) ->
    ?assertMatch({ok, _}, tokens:serialize(Token)),
    {ok, Serialized} = tokens:serialize(Token),
    ?assertEqual({ok, Token}, tokens:deserialize(Serialized)).


% If no caveats were expected to fail verification, make sure that token verification
% returns success and the token's subject, or fails when the secret is not correct.
check_verification_result(Token, Secret, Subject, AuthCtx, Caveats, [] = _Unverified) ->
    CaveatTypes = [caveats:type(C) || C <- Caveats],
    ?assertMatch(?ERROR_TOKEN_INVALID, tokens:verify(Token, <<"bad-secret">>, AuthCtx, CaveatTypes)),
    ?assertMatch(?ERROR_TOKEN_INVALID, tokens:verify(Token, ?RAND_STR, AuthCtx, CaveatTypes)),
    ?assertMatch({ok, #auth{subject = Subject}}, tokens:verify(Token, Secret, AuthCtx, CaveatTypes));
% If there are any caveats expected to fail verification, make sure that token verification
% fails and indicates one of the bad caveats.
check_verification_result(Token, Secret, _Subject, AuthCtx, Caveats, Unverified) ->
    CaveatTypes = [caveats:type(C) || C <- Caveats],
    VerifyResult = tokens:verify(Token, Secret, AuthCtx, CaveatTypes),
    ?assertMatch(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_), VerifyResult),
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat) = VerifyResult,
    ?assert(lists:member(UnverifiedCaveat, Unverified)).


% Make sures that if any of the caveats inscribed in the Token is not supported,
% the verification fails accordingly.
check_supported_caveats(Token, Secret, AuthCtx, Caveats, Unverified) ->
    CaveatTypes = [caveats:type(C) || C <- Caveats],
    % Randomize some non-full subsets of caveat types
    CaveatTypeSubsets = case length(CaveatTypes) of
        0 -> [];
        1 -> [[]];
        Len -> [rand_sublist(CaveatTypes, 0, Len - 1) || _ <- lists:seq(1, Len)]
    end,
    lists:foreach(fun(SupportedCaveats) ->
        MissingCaveats = lists:filter(fun(Caveat) ->
            not lists:member(caveats:type(Caveat), SupportedCaveats)
        end, Caveats),
        VerifyResult = tokens:verify(Token, Secret, AuthCtx, SupportedCaveats),
        ?assertMatch(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_), VerifyResult),
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(MissingCaveat) = VerifyResult,
        ?assert(lists:member(MissingCaveat, MissingCaveats ++ Unverified))
    end, CaveatTypeSubsets).


% Returns a list of #caveats_testcase{} records that include all combinations of
% caveats resulting from caveats_examples/2 function, and a list of caveats that
% are expected to be unverified based on the AuthCtx.
caveats_testcases(AuthCtx) ->
    lists:foldl(fun(CaveatType, Testcases) ->
        Examples = caveats_examples(CaveatType, AuthCtx),
        lists:flatmap(fun(Testcase = #caveats_testcase{caveats = Caveats, unverified = Unverified}) ->
            lists:map(fun({Caveat, ExpectedResult}) ->
                Testcase#caveats_testcase{
                    caveats = [Caveat | Caveats],
                    unverified = case ExpectedResult of
                        success -> Unverified;
                        failure -> [Caveat | Unverified]
                    end
                }
            end, Examples)
        end, Testcases)
    end, [#caveats_testcase{}], caveats:all_types()).


% Each example returned by caveats_testcases/1 includes all possible caveats, this
% function creates two examples from each one by randomly cutting down the number of caveats.
randomize_caveats_testcases(CaveatsTestcases) ->
    lists:flatmap(fun(Testcase = #caveats_testcase{caveats = Caveats, unverified = Unverified}) ->
        RandA = rand_sublist(Caveats, 0, length(Caveats)),
        RandB = rand_sublist(Caveats, 0, length(Caveats)),
        [
            Testcase#caveats_testcase{caveats = RandA, unverified = Unverified -- (Caveats -- RandA)},
            Testcase#caveats_testcase{caveats = RandB, unverified = Unverified -- (Caveats -- RandB)}
        ]
    end, CaveatsTestcases).


caveats_examples(cv_time, #auth_ctx{current_timestamp = Timestamp}) -> [
    {#cv_time{valid_until = Timestamp + 1}, success},
    {#cv_time{valid_until = Timestamp}, failure}
];

caveats_examples(cv_authorization_none, _AuthCtx) -> [
    {#cv_authorization_none{}, success}
];

caveats_examples(cv_audience, #auth_ctx{audience = undefined}) -> [
    {#cv_audience{audience = Audience}, failure} || Audience <- ?AUDIENCE_EXAMPLES
];
caveats_examples(cv_audience, #auth_ctx{audience = Audience}) -> [
    {#cv_audience{audience = Other}, failure} || Other <- ?AUDIENCE_EXAMPLES -- [Audience]
] ++ [{#cv_audience{audience = Audience}, success}];

caveats_examples(cv_ip, #auth_ctx{ip = Ip}) -> [
    {#cv_ip{whitelist = [{X, 32} || X <- rand_ips_without([Ip, ?IP_LH])]}, failure},
    % If the mask's ip is the same as in AuthCtx, any mask length should match
    {#cv_ip{whitelist = [{X, rand:uniform(33) - 1} || X <- rand_ips_with([Ip])]}, success}
];

caveats_examples(cv_asn, #auth_ctx{ip = ?IP_LH}) -> [
    {#cv_asn{whitelist = to_asns(rand_ips_without([?IP_LH]))}, failure}
];
caveats_examples(cv_asn, #auth_ctx{ip = Ip}) -> [
    {#cv_asn{whitelist = to_asns(rand_ips_without([Ip, ?IP_LH]))}, failure},
    {#cv_asn{whitelist = to_asns(rand_ips_with([Ip]))}, success}
];

caveats_examples(cv_country, #auth_ctx{ip = ?IP_LH}) -> [
    {#cv_country{type = whitelist, list = to_countries(rand_ips_without([?IP_LH]))}, failure},
    {#cv_country{type = blacklist, list = to_countries(rand_ips_without([?IP_LH]))}, failure}
];
caveats_examples(cv_country, #auth_ctx{ip = Ip}) -> [
    {#cv_country{type = whitelist, list = to_countries(rand_ips_without([Ip, ?IP_LH]))}, failure},
    {#cv_country{type = whitelist, list = to_countries(rand_ips_with([Ip]))}, success},
    {#cv_country{type = blacklist, list = to_countries(rand_ips_with([Ip]))}, failure},
    {#cv_country{type = blacklist, list = to_countries(rand_ips_without([Ip, ?IP_LH]))}, success}
];

caveats_examples(cv_region, #auth_ctx{ip = ?IP_LH}) -> [
    {#cv_region{type = whitelist, list = to_regions(rand_ips_without([?IP_LH]))}, failure}
];
caveats_examples(cv_region, #auth_ctx{ip = Ip}) -> [
    {#cv_region{type = whitelist, list = rand_regions_without(to_regions([Ip]))}, failure},
    {#cv_region{type = whitelist, list = to_regions(rand_ips_with([Ip]))}, success},
    {#cv_region{type = whitelist, list = rand_sublist(to_regions([Ip]), 1, 1)}, success},
    {#cv_region{type = blacklist, list = to_regions(rand_ips_with([Ip]))}, failure},
    {#cv_region{type = blacklist, list = rand_regions_without(to_regions([Ip]))}, success}
];

% Below caveats are lazy - always true when verifying a token (still, they must
% be explicitly supported). They are checked when the resulting aai:auth() object
% is consumed to perform an operation.
caveats_examples(cv_api, _AuthCtx) -> [
    {#cv_api{whitelist = rand_sublist([
        {all, create, ?GRI(od_space, <<"*">>, '*', private)},
        {?OZ_WORKER, get, ?GRI(od_user, ?RAND_STR, instance, '*')},
        {?OP_WORKER, all, ?GRI(od_user, ?RAND_STR, {group, ?RAND_STR}, private)},
        {?OP_PANEL, update, ?GRI(od_group, <<"*">>, {'*', <<"*">>})},
        {?OZ_PANEL, delete, ?GRI('*', <<"*">>, users, '*')},
        {all, all, ?GRI(od_handle, <<"*">>, '*', '*')}
    ], 1, 6)}, success}
];

caveats_examples(cv_data_space, _AuthCtx) -> [
    {#cv_data_space{whitelist = rand_sublist([
        ?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR
    ], 1, 5)}, success}
];

caveats_examples(cv_data_access, _AuthCtx) -> [
    {#cv_data_access{type = hd(rand_sublist([read, write], 1, 1))}, success}
];

caveats_examples(cv_data_path, _AuthCtx) -> [
    {#cv_data_path{whitelist = rand_sublist([
        ?RAND_PATH, ?RAND_PATH, ?RAND_PATH, ?RAND_PATH, ?RAND_PATH
    ], 1, 5)}, success}
];

caveats_examples(cv_data_objectid, _AuthCtx) -> [
    {#cv_data_objectid{whitelist = rand_sublist([
        ?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR
    ], 1, 5)}, success}
].

%%%===================================================================
%%% Helper functions
%%%===================================================================

rand_regions_without(Excludes) ->
    List = ?ALL_REGIONS -- Excludes,
    % Return at least one region
    rand_sublist(List, 1, length(List)).


rand_ips_with(Includes) ->
    % RandIps can be empty as we are adding Includes anyway
    RandIps = rand_sublist(?IP_EXAMPLES, 0, length(?IP_EXAMPLES)),
    % make sure Includes are not duplicated
    (RandIps -- Includes) ++ Includes.


rand_ips_without(Excludes) ->
    List = ?IP_EXAMPLES -- Excludes,
    % Return at least one IP
    rand_sublist(List, 1, length(List)).


rand_sublist(List, MinLength, MaxLength) ->
    Shuffled = utils:random_shuffle(List),
    lists:sublist(Shuffled, MinLength + rand:uniform(MaxLength - MinLength + 1) - 1).

-endif.

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

-include("aai/aai.hrl").
-include("errors.hrl").
-include("graph_sync/graph_sync.hrl").
-include("http/headers.hrl").
-include("onedata.hrl").

-define(OZ_DOMAIN, <<"onezone.example.com">>).
-define(MOCK_COWBOY_REQ(Headers), #{headers => Headers}).
% Dummy timestamp, tokens API always depends on timestamps provided, so
% any value can be used
-define(NOW(), 15000000000).
-define(RAND_STR, str_utils:rand_hex(16)).
-define(CUSTOM_MAX_TOKEN_SIZE, 4485764).

%%%===================================================================
%%% Test functions
%%%===================================================================

bad_token_test() ->
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:serialize({a, b, c, d})),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:serialize(<<"rubbish-123">>)),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:serialize(#token{})),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:deserialize({a, b, c, d})),
    ?assertMatch(?ERROR_BAD_TOKEN, tokens:deserialize(<<"rubbish-123">>)).


unknown_caveat_test() ->
    Secret = <<"secrettt123">>,
    Macaroon = macaroon:create(?OZ_DOMAIN, Secret, <<"id123">>),
    Macaroon2 = macaroon:add_first_party_caveat(Macaroon, <<"grant = everything">>),
    {ok, Token64} = macaroon:serialize(Macaroon2),
    Serialized = base62:from_base64(Token64),
    {ok, Token} = tokens:deserialize(Serialized),
    ?assertEqual(
        ?ERROR_TOKEN_CAVEAT_UNKNOWN(<<"grant = everything">>),
        tokens:verify(Token, Secret, #auth_ctx{current_timestamp = ?NOW()}, [])
    ).


invalid_subject_test() ->
    Prototype = #token{
        onezone_domain = ?OZ_DOMAIN,
        id = ?RAND_STR,
        persistent = false,
        type = ?ACCESS_TOKEN
    },
    InvalidSubjects = [?SUB(nobody), ?SUB(root), ?SUB(invalid_sub)],

    lists:foreach(fun(Subject) ->
        ?assertThrow(
            ?ERROR_TOKEN_SUBJECT_INVALID,
            tokens:construct(Prototype#token{subject = Subject}, <<"secret-1">>, [])
        )
    end, InvalidSubjects),

    % Subjects are not supported in version 1 - the value is ignored
    lists:foreach(fun(Subject) ->
        ?assertMatch(
            #token{},
            tokens:construct(Prototype#token{version = 1, subject = Subject}, <<"secret-1">>, [])
        )
    end, InvalidSubjects).


confine_test() ->
    Prototype = #token{
        onezone_domain = ?OZ_DOMAIN,
        id = ?RAND_STR,
        subject = ?SUB(user, <<"uid">>),
        persistent = true,
        type = ?GUI_ACCESS_TOKEN(?RAND_STR)
    },
    Secret = ?RAND_STR,

    CvAud = #cv_audience{whitelist = [?AUD(user, <<"user-id">>), ?AUD(group, <<"group-id">>)]},
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


is_token_or_invite_token_test() ->
    IsToken = fun tokens:is_token/1,
    IsInviteToken = fun tokens:is_invite_token/2,

    AccessTokenPrototype = #token{
        onezone_domain = ?OZ_DOMAIN,
        id = ?RAND_STR,
        subject = ?SUB(?ONEPROVIDER, <<"pid">>),
        persistent = true,
        type = ?ACCESS_TOKEN
    },

    GuiAccessTokenPrototype = #token{
        onezone_domain = ?OZ_DOMAIN,
        id = ?RAND_STR,
        subject = ?SUB(user, <<"uid">>),
        persistent = false,
        type = ?GUI_ACCESS_TOKEN(?RAND_STR)
    },

    InviteTokenPrototype = #token{
        onezone_domain = ?OZ_DOMAIN,
        id = ?RAND_STR,
        subject = ?SUB(user, <<"uid">>),
        persistent = false,
        type = ?INVITE_TOKEN(?USER_JOIN_SPACE, ?RAND_STR)
    },

    ?assertEqual(false, IsToken(AccessTokenPrototype)),
    ?assertEqual(false, IsInviteToken(AccessTokenPrototype, ?GROUP_JOIN_HARVESTER)),
    ?assertEqual(true, IsToken(tokens:construct(AccessTokenPrototype, <<"secret">>, []))),
    ?assertEqual(false, IsInviteToken(tokens:construct(AccessTokenPrototype, <<"secret">>, []), any)),

    ?assertEqual(false, IsToken(GuiAccessTokenPrototype)),
    ?assertEqual(false, IsInviteToken(GuiAccessTokenPrototype, any)),
    ?assertEqual(true, IsToken(tokens:construct(GuiAccessTokenPrototype, <<"secret">>, []))),
    ?assertEqual(false, IsInviteToken(tokens:construct(GuiAccessTokenPrototype, <<"secret">>, []), ?USER_JOIN_SPACE)),

    ?assertEqual(false, IsToken(InviteTokenPrototype)),
    ?assertEqual(false, IsInviteToken(InviteTokenPrototype, ?USER_JOIN_CLUSTER)),
    ?assertEqual(true, IsToken(tokens:construct(InviteTokenPrototype, <<"secret">>, []))),
    ?assertEqual(true, IsInviteToken(tokens:construct(InviteTokenPrototype, <<"secret">>, []), ?USER_JOIN_SPACE)),
    ?assertEqual(true, IsInviteToken(tokens:construct(InviteTokenPrototype, <<"secret">>, []), any)),
    ?assertEqual(false, IsInviteToken(tokens:construct(InviteTokenPrototype, <<"secret">>, []), ?GROUP_JOIN_SPACE)),

    ?assertEqual(false, IsToken(12345)),
    ?assertEqual(false, IsToken(<<"12345">>)),
    ?assertEqual(false, IsToken(#{<<"token">> => <<"12345">>})),
    ?assertEqual(false, IsToken(macaroon:create(?OZ_DOMAIN, <<"secret">>, <<"id">>))),

    ?assertEqual(false, IsInviteToken(12345, any)),
    ?assertEqual(false, IsInviteToken(<<"12345">>, ?SUPPORT_SPACE)),
    ?assertEqual(false, IsInviteToken(#{<<"token">> => <<"12345">>}, ?REGISTER_ONEPROVIDER)),
    ?assertEqual(false, IsInviteToken(macaroon:create(?OZ_DOMAIN, <<"secret">>, <<"id">>), ?SPACE_JOIN_HARVESTER)).


sanitize_type_test() ->
    S = fun tokens:sanitize_type/1,

    ?assertEqual({true, ?ACCESS_TOKEN}, S(?ACCESS_TOKEN)),
    ?assertEqual({true, ?ACCESS_TOKEN}, S(#{<<"accessToken">> => #{}})),
    ?assertEqual(false, S(<<"access">>)),

    ?assertEqual({true, ?GUI_ACCESS_TOKEN(<<"sess">>)}, S(?GUI_ACCESS_TOKEN(<<"sess">>))),
    ?assertEqual({true, ?GUI_ACCESS_TOKEN(<<"sess">>)}, S(#{<<"guiAccessToken">> => #{<<"sessionId">> => <<"sess">>}})),
    ?assertEqual(false, S(#{<<"guiAccessToken">> => #{}})),
    ?assertEqual(false, S(<<"gui">>)),
    ?assertEqual(false, S(<<"gui-">>)),

    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_GROUP, <<"id">>)}, S(?INVITE_TOKEN(?USER_JOIN_GROUP, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_GROUP, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"userJoinGroup">>, <<"groupId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, <<"id">>)}, S(?INVITE_TOKEN(?GROUP_JOIN_GROUP, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"groupJoinGroup">>, <<"groupId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_SPACE, <<"id">>)}, S(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_SPACE, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"userJoinSpace">>, <<"spaceId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, <<"id">>)}, S(?INVITE_TOKEN(?GROUP_JOIN_SPACE, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"groupJoinSpace">>, <<"spaceId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?SUPPORT_SPACE, <<"id">>)}, S(?INVITE_TOKEN(?SUPPORT_SPACE, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?SUPPORT_SPACE, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"supportSpace">>, <<"spaceId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, <<"id">>)}, S(?INVITE_TOKEN(?REGISTER_ONEPROVIDER, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"registerOneprovider">>, <<"adminUserId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, <<"id">>)}, S(?INVITE_TOKEN(?USER_JOIN_CLUSTER, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"userJoinCluster">>, <<"clusterId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, <<"id">>)}, S(?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"groupJoinCluster">>, <<"clusterId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_HARVESTER, <<"id">>)}, S(?INVITE_TOKEN(?USER_JOIN_HARVESTER, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?USER_JOIN_HARVESTER, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"userJoinHarvester">>, <<"harvesterId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, <<"id">>)}, S(?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"groupJoinHarvester">>, <<"harvesterId">> => <<"id">>
    }})),

    ?assertEqual({true, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"id">>)}, S(?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"id">>))),
    ?assertEqual({true, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"id">>)}, S(#{<<"inviteToken">> => #{
        <<"subtype">> => <<"spaceJoinHarvester">>, <<"harvesterId">> => <<"id">>
    }})),

    ?assertEqual(false, S(<<"giu-">>)),
    ?assertEqual(false, S(<<"hi">>)),
    ?assertEqual(false, S(<<"h">>)).


sanitize_invite_token_type_test() ->
    S = fun tokens:sanitize_invite_token_type/1,

    ?assertEqual({true, ?USER_JOIN_GROUP}, S(?USER_JOIN_GROUP)),
    ?assertEqual({true, ?USER_JOIN_GROUP}, S(<<"userJoinGroup">>)),
    ?assertEqual(false, S(<<"userjoingroup">>)),

    ?assertEqual({true, ?GROUP_JOIN_GROUP}, S(?GROUP_JOIN_GROUP)),
    ?assertEqual({true, ?GROUP_JOIN_GROUP}, S(<<"groupJoinGroup">>)),
    ?assertEqual(false, S(<<"gjg">>)),

    ?assertEqual({true, ?USER_JOIN_SPACE}, S(?USER_JOIN_SPACE)),
    ?assertEqual({true, ?USER_JOIN_SPACE}, S(<<"userJoinSpace">>)),
    ?assertEqual(false, S(123456)),

    ?assertEqual({true, ?GROUP_JOIN_SPACE}, S(?GROUP_JOIN_SPACE)),
    ?assertEqual({true, ?GROUP_JOIN_SPACE}, S(<<"groupJoinSpace">>)),
    ?assertEqual(false, S(<<"group_join_space">>)),

    ?assertEqual({true, ?SUPPORT_SPACE}, S(?SUPPORT_SPACE)),
    ?assertEqual({true, ?SUPPORT_SPACE}, S(<<"supportSpace">>)),
    ?assertEqual(false, S(<<"spaceSupport">>)),

    ?assertEqual({true, ?REGISTER_ONEPROVIDER}, S(?REGISTER_ONEPROVIDER)),
    ?assertEqual({true, ?REGISTER_ONEPROVIDER}, S(<<"registerOneprovider">>)),
    ?assertEqual(false, S(<<"">>)),

    ?assertEqual({true, ?USER_JOIN_CLUSTER}, S(?USER_JOIN_CLUSTER)),
    ?assertEqual({true, ?USER_JOIN_CLUSTER}, S(<<"userJoinCluster">>)),
    ?assertEqual(false, S(<<"ujc">>)),

    ?assertEqual({true, ?GROUP_JOIN_CLUSTER}, S(?GROUP_JOIN_CLUSTER)),
    ?assertEqual({true, ?GROUP_JOIN_CLUSTER}, S(<<"groupJoinCluster">>)),
    ?assertEqual(false, S(#{<<"inviteTokenType">> => <<"groupJoinCluster">>})),

    ?assertEqual({true, ?USER_JOIN_HARVESTER}, S(?USER_JOIN_HARVESTER)),
    ?assertEqual({true, ?USER_JOIN_HARVESTER}, S(<<"userJoinHarvester">>)),
    ?assertEqual(false, S(7.13)),

    ?assertEqual({true, ?GROUP_JOIN_HARVESTER}, S(?GROUP_JOIN_HARVESTER)),
    ?assertEqual({true, ?GROUP_JOIN_HARVESTER}, S(<<"groupJoinHarvester">>)),
    ?assertEqual(false, S(<<"group-join-harvester">>)),

    ?assertEqual({true, ?SPACE_JOIN_HARVESTER}, S(?SPACE_JOIN_HARVESTER)),
    ?assertEqual({true, ?SPACE_JOIN_HARVESTER}, S(<<"spaceJoinHarvester">>)),
    ?assertEqual(false, S("spaceJoinHarvester")).


secret_generation_test() ->
    ?assertMatch(<<_/binary>>, tokens:generate_secret()),
    ?assertNotEqual(tokens:generate_secret(), tokens:generate_secret()).


service_access_tokens_test() ->
    ProviderId = <<"another-provider-id">>,
    Id = <<"z234xcvzasdfa0sd8fh7a8wesdd352a24">>,
    Secret = <<"secret-4">>,
    Subject = ?SUB(?ONEPROVIDER, ProviderId),
    Prototype = #token{
        onezone_domain = ?OZ_DOMAIN,
        id = Id,
        persistent = false,
        subject = Subject,
        type = ?ACCESS_TOKEN
    },

    Verify = fun(Serialized) ->
        AuthCtx = #auth_ctx{current_timestamp = ?NOW(), audience = ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)},
        tokens:verify(element(2, {ok, _} = tokens:deserialize(Serialized)), Secret, AuthCtx, [cv_time])
    end,

    Token = tokens:construct(Prototype, Secret, [
        #cv_time{valid_until = ?NOW() + 1000}
    ]),
    {ok, Serialized} = tokens:serialize(Token),

    % Service access tokens support only ONEPROVIDER services (op-worker and op-panel)
    ?assertException(error, badarg, tokens:build_service_access_token(?OZ_WORKER, Serialized)),
    ?assertException(error, badarg, tokens:build_service_access_token(?OZ_PANEL, Serialized)),
    ?assertException(error, badarg, tokens:build_service_access_token(wait_what_this_is_not_a_valid_service, Serialized)),
    OpwServiceAccessToken = tokens:build_service_access_token(?OP_WORKER, Serialized),
    OppServiceAccessToken = tokens:build_service_access_token(?OP_PANEL, Serialized),

    ?assertMatch(
        {ok, #token{subject = ?SUB(?ONEPROVIDER, ?OP_WORKER, ProviderId)}},
        tokens:deserialize(OpwServiceAccessToken)
    ),
    ?assertMatch(
        {ok, #auth{subject = ?SUB(?ONEPROVIDER, ?OP_WORKER, ProviderId)}},
        Verify(OpwServiceAccessToken)
    ),

    ?assertMatch(
        {ok, #token{subject = ?SUB(?ONEPROVIDER, ?OP_PANEL, ProviderId)}},
        tokens:deserialize(OppServiceAccessToken)
    ),
    ?assertMatch(
        {ok, #auth{subject = ?SUB(?ONEPROVIDER, ?OP_PANEL, ProviderId)}},
        Verify(OppServiceAccessToken)
    ),

    % Service tokens that are not for op-worker or op-panel should be rejected
    ?assertEqual(?ERROR_BAD_TOKEN, tokens:deserialize(<<"ozw-", Serialized/binary>>)),
    ?assertEqual(?ERROR_BAD_TOKEN, tokens:deserialize(<<"ozp-", Serialized/binary>>)),
    ?assertEqual(?ERROR_BAD_TOKEN, tokens:deserialize(<<"usr-", Serialized/binary>>)),
    ?assertEqual(?ERROR_BAD_TOKEN, tokens:deserialize(<<"grp-", Serialized/binary>>)).



access_token_headers_manipulation_test() ->
    AccessToken = <<"gimme-access-123">>,

    ?assertEqual(undefined, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(#{}))),
    ?assertEqual(undefined, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(#{
        <<"unsupported-header">> => AccessToken
    }))),

    Headers = tokens:build_access_token_header(AccessToken),
    ?assertEqual(AccessToken, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(Headers))),

    lists:foreach(fun
        (?HDR_AUTHORIZATION) ->
            ?assertEqual(AccessToken, tokens:parse_access_token_header(?MOCK_COWBOY_REQ(#{
                ?HDR_AUTHORIZATION => <<"Bearer ", AccessToken/binary>>
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
            #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')}]}
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
            #cv_audience{whitelist = [?AUD(group, <<"123">>)]},
            #cv_time{valid_until = 456},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_time{valid_until = 789},
            #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')}]}
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
            #cv_audience{whitelist = [?AUD(user, <<"567">>)]},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')}]}
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
            #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')}]}
        ],
        F([cv_asn, cv_api, cv_time], [
            #cv_time{valid_until = 123},
            #cv_asn{whitelist = [322]},
            #cv_audience{whitelist = [?AUD(?OP_PANEL, <<"provider-id">>)]},
            #cv_time{valid_until = 456},
            #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
            #cv_time{valid_until = 789},
            #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', <<"*">>, '*', '*')}]}
        ])
    ).


too_large_token_test() ->
    ctool:set_env(max_token_size, ?CUSTOM_MAX_TOKEN_SIZE),
    % Hex converts each byte to two chars
    LargeToken = str_utils:rand_hex(?CUSTOM_MAX_TOKEN_SIZE div 2 + 1),
    ?assertEqual(?ERROR_TOKEN_TOO_LARGE(?CUSTOM_MAX_TOKEN_SIZE), tokens:deserialize(LargeToken)).


-define(BAD(Term), ?assertEqual(false, Term)).
-define(OK(Sanitized, Term), ?assertEqual({true, Sanitized}, Term)).

sanitize_caveats_test() ->
    S = fun caveats:sanitize/1,

    ?BAD(S(<<"abc">>)),
    ?BAD(S(1233454567)),
    ?BAD(S(atom)),

    ?OK(#cv_time{valid_until = 123}, S(#cv_time{valid_until = 123})),
    ?OK(#cv_time{valid_until = 123}, S(#{<<"type">> => <<"time">>, <<"validUntil">> => 123})),
    ?BAD(S(#cv_time{valid_until = dsfdsf})),
    ?BAD(S(#{<<"type">> => <<"time">>, <<"validAfter">> => 1231231})),
    ?BAD(S(#{<<"type">> => <<"time">>, <<"validUntil">> => <<"infinity">>})),

    ?OK(#cv_authorization_none{}, S(#cv_authorization_none{})),
    ?OK(#cv_authorization_none{}, S(#{<<"type">> => <<"authorizationNone">>})),
    ?BAD(S(#{<<"type">> => <<"authorizationFull">>})),


    ?OK(#cv_audience{whitelist = [?AUD(?OP_PANEL, <<"123">>)]},
        S(#cv_audience{whitelist = [?AUD(?OP_PANEL, <<"123">>)]})),
    ?OK(#cv_audience{whitelist = [?AUD(?OP_PANEL, <<"123">>)]},
        S(#{<<"type">> => <<"audience">>, <<"whitelist">> => [<<"opp-123">>]})),
    ?BAD(S(#cv_audience{whitelist = []})),
    ?BAD(S(#cv_audience{whitelist = dsfdsf})),
    ?BAD(S(#{<<"type">> => <<"audience">>, <<"whitelist">> => []})),
    ?BAD(S(#{<<"type">> => <<"audience">>, <<"whitelist">> => <<"opp-123">>})),
    ?BAD(S(#{<<"type">> => <<"audience">>, <<"whitelist">> => [<<"xcvsdjuhfsdfh">>]})),
    ?BAD(S(#{<<"type">> => <<"audience">>, <<"blacklist">> => [<<"opp-123">>]})),

    ?OK(#cv_ip{whitelist = [{{172, 98, 11, 23}, 32}]},
        S(#cv_ip{whitelist = [{{172, 98, 11, 23}, 32}]})),
    ?OK(#cv_ip{whitelist = [{{172, 98, 11, 23}, 32}]},
        S(#{<<"type">> => <<"ip">>, <<"whitelist">> => [<<"172.98.11.23/32">>]})),
    ?BAD(S(#cv_ip{whitelist = []})),
    ?BAD(S(#cv_ip{whitelist = dsfdsf})),
    ?BAD(S(#{<<"type">> => <<"ip">>, <<"whitelist">> => []})),
    ?BAD(S(#{<<"type">> => <<"ip">>, <<"whitelist">> => <<"all">>})),
    ?BAD(S(#{<<"type">> => <<"ip">>, <<"whitelist">> => [<<"all">>]})),
    ?BAD(S(#{<<"type">> => <<"ip">>, <<"whitelist">> => [<<"172.98.11.23/34">>]})),
    ?BAD(S(#{<<"type">> => <<"ip">>, <<"whitelist">> => [<<"172.9998.11.23/30">>]})),
    ?BAD(S(#{<<"type">> => <<"ip">>, <<"blacklist">> => [<<"172.98.11.23/32">>]})),

    ?OK(#cv_asn{whitelist = [45, 785, 112]},
        S(#cv_asn{whitelist = [45, 785, 112]})),
    ?OK(#cv_asn{whitelist = [45, 785, 112]},
        S(#{<<"type">> => <<"asn">>, <<"whitelist">> => [45, 785, 112]})),
    ?BAD(S(#cv_asn{whitelist = []})),
    ?BAD(S(#cv_asn{whitelist = dsfdsf})),
    ?BAD(S(#{<<"type">> => <<"asn">>, <<"whitelist">> => []})),
    ?BAD(S(#{<<"type">> => <<"asn">>, <<"whitelist">> => <<"zxcvxz">>})),
    ?BAD(S(#{<<"type">> => <<"asn">>, <<"whitelist">> => [<<"zxcvxz">>]})),
    ?BAD(S(#{<<"type">> => <<"asn">>, <<"whitelist">> => [<<"45.785.112">>]})),
    ?BAD(S(#{<<"type">> => <<"asn">>, <<"whitelist">> => [<<"76fb">>]})),
    ?BAD(S(#{<<"type">> => <<"asn">>, <<"blacklist">> => [45, 785, 112]})),

    ?OK(#cv_country{type = whitelist, list = [<<"PL">>, <<"DE">>]},
        S(#cv_country{type = whitelist, list = [<<"PL">>, <<"DE">>]})),
    ?OK(#cv_country{type = blacklist, list = [<<"PL">>, <<"DE">>]},
        S(#{<<"type">> => <<"geo.country">>, <<"filter">> => <<"blacklist">>, <<"list">> => [<<"PL">>, <<"DE">>]})),
    ?BAD(S(#cv_country{type = whitelist, list = []})),
    ?BAD(S(#cv_country{type = blacklist, list = dsfdsf})),
    ?BAD(S(#cv_country{type = all, list = [<<"PL">>, <<"DE">>]})),
    ?BAD(S(#{<<"type">> => <<"geo.country">>, <<"filter">> => <<"blacklist">>, <<"list">> => []})),
    ?BAD(S(#{<<"type">> => <<"geo.country">>, <<"filter">> => <<"whitelist">>, <<"list">> => []})),
    ?BAD(S(#{<<"type">> => <<"geo.country">>, <<"filter">> => <<"badlist">>, <<"list">> => [<<"PL">>, <<"DE">>]})),
    ?BAD(S(#{<<"type">> => <<"geo.country">>, <<"filter">> => <<"blacklist">>, <<"list">> => [1, 2, 3]})),

    ?OK(#cv_region{type = whitelist, list = [<<"Asia">>, <<"EU">>]},
        S(#cv_region{type = whitelist, list = [<<"Asia">>, <<"EU">>]})),
    ?OK(#cv_region{type = blacklist, list = [<<"Asia">>, <<"EU">>]},
        S(#{<<"type">> => <<"geo.region">>, <<"filter">> => <<"blacklist">>, <<"list">> => [<<"Asia">>, <<"EU">>]})),
    ?BAD(S(#cv_region{type = whitelist, list = []})),
    ?BAD(S(#cv_region{type = blacklist, list = 5663452})),
    ?BAD(S(#cv_region{type = all, list = [<<"Asia">>, <<"EU">>]})),
    ?BAD(S(#{<<"type">> => <<"geo.region">>, <<"filter">> => <<"blacklist">>, <<"list">> => []})),
    ?BAD(S(#{<<"type">> => <<"geo.region">>, <<"filter">> => <<"whitelist">>, <<"list">> => []})),
    ?BAD(S(#{<<"type">> => <<"geo.region">>, <<"filter">> => <<"badlist">>, <<"list">> => [<<"Asia">>, <<"EU">>]})),
    ?BAD(S(#{<<"type">> => <<"geo.region">>, <<"filter">> => <<"blacklist">>, <<"list">> => [1, 2, 3]})),

    ?OK(#cv_interface{interface = graphsync},
        S(#cv_interface{interface = graphsync})),
    ?OK(#cv_interface{interface = graphsync},
        S(#{<<"type">> => <<"interface">>, <<"interface">> => <<"graphsync">>})),
    ?OK(#cv_interface{interface = rest},
        S(#cv_interface{interface = rest})),
    ?OK(#cv_interface{interface = rest},
        S(#{<<"type">> => <<"interface">>, <<"interface">> => <<"rest">>})),
    ?OK(#cv_interface{interface = oneclient},
        S(#cv_interface{interface = oneclient})),
    ?OK(#cv_interface{interface = oneclient},
        S(#{<<"type">> => <<"interface">>, <<"interface">> => <<"oneclient">>})),
    ?BAD(S(#cv_interface{interface = []})),
    ?BAD(S(#cv_interface{interface = [rest, oneclient]})),
    ?BAD(S(#cv_interface{interface = 165})),
    ?BAD(S(#{<<"type">> => <<"interface">>, <<"interface">> => [<<"graphsync">>, <<"rest">>]})),
    ?BAD(S(#{<<"type">> => <<"interface">>, <<"interface">> => 123})),
    ?BAD(S(#{<<"type">> => <<"interface">>, <<"interface">> => #{<<"abcd">> => <<"ghij">>}})),

    ?OK(#cv_api{whitelist = [{?OZ_WORKER, all, ?GRI_PATTERN(od_user, <<"*">>, '*', auto)}]},
        S(#cv_api{whitelist = [{?OZ_WORKER, all, ?GRI_PATTERN(od_user, <<"*">>, '*', auto)}]})),
    ?OK(#cv_api{whitelist = [{?OZ_WORKER, all, ?GRI_PATTERN(od_user, <<"*">>, '*', auto)}]},
        S(#{<<"type">> => <<"api">>, <<"whitelist">> => [<<"ozw/all/user.*.*:auto">>]})),
    ?BAD(S(#cv_api{whitelist = []})),
    ?BAD(S(#cv_api{whitelist = [<<"avbb">>, 343]})),
    ?BAD(S(#{<<"type">> => <<"api">>, <<"whitelist">> => []})),
    ?BAD(S(#{<<"type">> => <<"api">>, <<"blacklist">> => [<<"ozw/all/user.*.*:auto">>]})),
    ?BAD(S(#{<<"type">> => <<"api">>, <<"whitelist">> => [<<"all/user.*.*:auto">>]})),
    ?BAD(S(#{<<"type">> => <<"api">>, <<"whitelist">> => <<"8sdafhg72aw3r">>})),

    ?OK(#cv_data_readonly{},
        S(#cv_data_readonly{})),
    ?OK(#cv_data_readonly{},
        S(#{<<"type">> => <<"data.readonly">>})),
    ?BAD(S(#{<<"type">> => <<"data.writeonly">>})),

    ?OK(#cv_data_path{whitelist = [<<"/a/b/c/d">>]},
        S(#cv_data_path{whitelist = [<<"/a/b/c/d">>]})),
    ?OK(#cv_data_path{whitelist = [<<"/space1/file1.txt">>, <<"/space2/dir/file2.txt">>]},
        S(#{<<"type">> => <<"data.path">>, <<"whitelist">> => [
            base64:encode(<<"/space1/file1.txt">>), base64:encode(<<"/space2/dir/file2.txt">>)
        ]})),
    ?BAD(S(#cv_data_path{whitelist = []})),
    ?BAD(S(#cv_data_path{whitelist = [<<"avbb">>, 343]})),
    ?BAD(S(#{<<"type">> => <<"data.path">>, <<"whitelist">> => []})),
    ?BAD(S(#{<<"type">> => <<"data.path">>, <<"whitelist">> => [<<"a/b/c/d/e$%#@">>]})),
    ?BAD(S(#{<<"type">> => <<"data.path">>, <<"blacklist">> => [base64:encode(<<"/space1/file1.txt">>)]})),
    % Make sure canonical form is checked
    ?BAD(S(#cv_data_path{whitelist = [<<"a/b/c/d/e">>]})),
    ?BAD(S(#cv_data_path{whitelist = [<<"/a/b/c/d/e/">>, <<"/space/dir">>]})),
    ?BAD(S(#cv_data_path{whitelist = [<<"/">>]})),
    ?BAD(S(#{<<"type">> => <<"data.path">>, <<"whitelist">> => [<<"a">>, <<"/b/c">>]})),
    ?BAD(S(#{<<"type">> => <<"data.path">>, <<"whitelist">> => [base64:encode(<<"/space1/">>)]})),
    ?BAD(S(#{<<"type">> => <<"data.path">>, <<"whitelist">> => [<<"/">>]})),

    ?OK(#cv_data_objectid{whitelist = [<<"01234">>, <<"7890">>]},
        S(#cv_data_objectid{whitelist = [<<"01234">>, <<"7890">>]})),
    ?OK(#cv_data_objectid{whitelist = [<<"01234">>, <<"7890">>]},
        S(#{<<"type">> => <<"data.objectid">>, <<"whitelist">> => [<<"01234">>, <<"7890">>]})),
    ?BAD(S(#cv_data_objectid{whitelist = []})),
    ?BAD(S(#cv_data_objectid{whitelist = [atom, 343]})),
    ?BAD(S(#{<<"type">> => <<"data.objectid">>, <<"whitelist">> => []})),
    ?BAD(S(#{<<"type">> => <<"data.objectid">>, <<"blacklist">> => [<<"01234">>, <<"7890">>]})).


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

% Dummy group for checking the group audience. Only ?DUMMY_USER belongs to it
% (this is covered in the group_membership_checker function).
-define(DUMMY_USER, <<"user-id">>).
-define(DUMMY_GROUP, <<"group-id">>).

% Audience examples
-define(AUD_USR, ?AUD(user, ?DUMMY_USER)).
-define(AUD_GRP, ?AUD(group, ?DUMMY_GROUP)).
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
audience(?IP_US) -> ?AUD_OPW;
audience(?IP_AU) -> ?AUD_USR;
audience(?IP_BR) -> ?AUD_OZP.

% Example auth_ctx's - bound to the IP for easier test code
auth_ctx(Ip) ->
    #auth_ctx{
        current_timestamp = ?NOW(),
        ip = Ip,
        interface = utils:random_element([undefined | cv_interface:valid_interfaces()]),
        audience = audience(Ip),
        allow_data_access_caveats = utils:random_element([true, false]),
        group_membership_checker = fun
            (?AUD_USR, ?DUMMY_GROUP) -> true;
            (_, _) -> false
        end
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

-define(RAND_PATH, str_utils:join_binary([<<"">> | utils:random_sublist([
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

-define(SUCCESS_IF_DATA_ACCESS_CAVEATS_ALLOWED(AuthCtx),
    case AuthCtx#auth_ctx.allow_data_access_caveats of
        true -> success;
        false -> failure
    end
).

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
    combinations(Persistence, Subject, ?GUI_ACCESS_TOKEN(?RAND_STR)).

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
    Prototype = #token{
        onezone_domain = ?OZ_DOMAIN,
        id = ?RAND_STR,
        persistent = Persistent,
        subject = Subject,
        type = Type
    },
    Token = tokens:construct(Prototype, Secret, Caveats),

    check_serialize_deserialize(Token),
    check_caveats_to_json_and_back(Token),
    check_verification_result(Token, Secret, Subject, AuthCtx, Caveats, Unverified),
    check_supported_caveats(Token, Secret, AuthCtx, Caveats, Unverified).


check_serialize_deserialize(Token) ->
    ?assertMatch({ok, _}, tokens:serialize(Token)),
    {ok, Serialized} = tokens:serialize(Token),
    ?assertEqual({ok, Token}, tokens:deserialize(Serialized)).


check_caveats_to_json_and_back(Token) ->
    Caveats = tokens:get_caveats(Token),
    lists:foreach(fun(Caveat) ->
        CaveatAsJson = caveats:to_json(Caveat),
        ?assert(is_map(CaveatAsJson)),
        ?assertEqual(Caveat, caveats:from_json(CaveatAsJson))
    end, Caveats).


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
        Len -> [utils:random_sublist(CaveatTypes, 0, Len - 1) || _ <- lists:seq(1, Len)]
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
        RandA = utils:random_sublist(Caveats, 0, length(Caveats)),
        RandB = utils:random_sublist(Caveats, 0, length(Caveats)),
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
    {#cv_audience{whitelist = utils:random_sublist(?AUDIENCE_EXAMPLES, 1, length(?AUDIENCE_EXAMPLES))}, failure}
];
caveats_examples(cv_audience, #auth_ctx{audience = ?AUD_USR}) -> [
    % The ?DUMMY_USER (?AUD_USR) belongs to the ?DUMMY_GROUP (?AUD_GRP), so
    % he should satisfy the group audience caveat.
    {#cv_audience{whitelist = rand_audiences_without([?AUD_USR, ?AUD_GRP])}, failure},
    {#cv_audience{whitelist = rand_audiences_with([?AUD_USR, ?AUD_GRP])}, success}
];
caveats_examples(cv_audience, #auth_ctx{audience = Audience}) -> [
    {#cv_audience{whitelist = rand_audiences_without([Audience])}, failure},
    {#cv_audience{whitelist = rand_audiences_with([Audience])}, success},
    {#cv_audience{whitelist = [Audience#audience{id = <<"*">>}]}, success}
];

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
    {#cv_region{type = whitelist, list = utils:random_sublist(to_regions([Ip]), 1, 1)}, success},
    {#cv_region{type = blacklist, list = to_regions(rand_ips_with([Ip]))}, failure},
    {#cv_region{type = blacklist, list = rand_regions_without(to_regions([Ip]))}, success}
];

caveats_examples(cv_interface, #auth_ctx{interface = undefined}) -> [
    {#cv_interface{interface = utils:random_element(cv_interface:valid_interfaces())}, failure}
];
caveats_examples(cv_interface, #auth_ctx{interface = oneclient, allow_data_access_caveats = false}) -> [
    % Oneclient interface should fail as it is allowed only when data_access_caveats are allowed.
    % Other interfaces should fail too as they do not match the oneclient interface from auth_ctx.
    {#cv_interface{interface = utils:random_element(cv_interface:valid_interfaces())}, failure}
];
caveats_examples(cv_interface, #auth_ctx{interface = Interface}) -> [
    {#cv_interface{interface = Interface}, success},
    {#cv_interface{interface = utils:random_element(cv_interface:valid_interfaces() -- [Interface])}, failure}
];

% Below caveats are lazy - always true when verifying a token (still, they must
% be explicitly supported). They are checked when the resulting aai:auth() object
% is consumed to perform an operation.
caveats_examples(cv_api, _AuthCtx) -> [
    {#cv_api{whitelist = utils:random_sublist([
        {all, create, ?GRI_PATTERN(od_space, <<"*">>, '*', private)},
        {?OZ_WORKER, get, ?GRI_PATTERN(od_user, ?RAND_STR, instance, '*')},
        {?OP_WORKER, all, ?GRI_PATTERN(od_user, ?RAND_STR, {group, ?RAND_STR}, private)},
        {?OP_PANEL, update, ?GRI_PATTERN(od_group, <<"*">>, {'*', <<"*">>})},
        {?OZ_PANEL, delete, ?GRI_PATTERN('*', <<"*">>, users, '*')},
        {all, all, ?GRI_PATTERN(od_handle, <<"*">>, '*', '*')}
    ], 1, 6)}, success}
];

caveats_examples(cv_data_readonly, AuthCtx) -> [
    {#cv_data_readonly{}, ?SUCCESS_IF_DATA_ACCESS_CAVEATS_ALLOWED(AuthCtx)}
];

caveats_examples(cv_data_path, AuthCtx) -> [
    {#cv_data_path{whitelist = utils:random_sublist([
        ?RAND_PATH, ?RAND_PATH, ?RAND_PATH, ?RAND_PATH, ?RAND_PATH
    ], 1, 5)}, ?SUCCESS_IF_DATA_ACCESS_CAVEATS_ALLOWED(AuthCtx)}
];

caveats_examples(cv_data_objectid, AuthCtx) -> [
    {#cv_data_objectid{whitelist = utils:random_sublist([
        ?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR, ?RAND_STR
    ], 1, 5)}, ?SUCCESS_IF_DATA_ACCESS_CAVEATS_ALLOWED(AuthCtx)}
].

%%%===================================================================
%%% Helper functions
%%%===================================================================

rand_regions_without(Excludes) ->
    List = ?ALL_REGIONS -- Excludes,
    % Return at least one region
    utils:random_sublist(List, 1, length(List)).


rand_ips_with(Includes) ->
    % RandIps can be empty as we are adding Includes anyway
    RandIps = utils:random_sublist(?IP_EXAMPLES, 0, length(?IP_EXAMPLES)),
    % make sure Includes are not duplicated
    (RandIps -- Includes) ++ Includes.


rand_ips_without(Excludes) ->
    List = ?IP_EXAMPLES -- Excludes,
    % Return at least one IP
    utils:random_sublist(List, 1, length(List)).


rand_audiences_without(Excludes) ->
    List = ?AUDIENCE_EXAMPLES -- Excludes,
    % Return at least one region
    utils:random_sublist(List, 1, length(List)).


rand_audiences_with(Includes) ->
    % RandIps can be empty as we are adding Includes anyway
    RandIps = utils:random_sublist(?AUDIENCE_EXAMPLES, 0, length(?AUDIENCE_EXAMPLES)),
    % make sure Includes are not duplicated
    (RandIps -- Includes) ++ Includes.

-endif.

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests that verify that the token
%%% serialization / deserialization is deterministic and does not
%%% change between system versions, despite changes in dependencies
%%% and Erlang OTP upgrades.
%%%
%%% NOTE:
%%% The hardcoded tokens in this file were generated in version 20.02.20.
%%% If any of the tests fails, it means that a change has been introduced
%%% that may cause older software versions to be unable to decode a token.
%%% Think three times before adjusting the expected serialized token form!
%%% @end
%%%-------------------------------------------------------------------
-module(tokens_compatibility_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("aai/aai.hrl").
-include("errors.hrl").
-include("onedata.hrl").
-include("graph_sync/gri.hrl").
-include("http/headers.hrl").
%@fixme co potrzebne tutaj
-define(OZ_DOMAIN, <<"onezone.example.com">>).
-define(MOCK_COWBOY_REQ(Headers), #{headers => Headers}).
% Dummy timestamp, tokens API always depends on timestamps provided, so
% any value can be used
-define(NOW(), 15000000000).
-define(RAND_STR, str_utils:rand_hex(16)).
-define(CUSTOM_MAX_TOKEN_SIZE, 4485764).

-define(ACCESS_TOKEN_CAVEATS, [
    #cv_time{valid_until = 8374891234},
    #cv_ip{whitelist = [{{172, 98, 11, 23}, 32}]},
    #cv_asn{whitelist = [322]},
    #cv_country{type = whitelist, list = [<<"PL">>, <<"FR">>]},
    #cv_region{type = whitelist, list = [<<"Asia">>, <<"EU">>]},
    #cv_scope{scope = identity_token},
    #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
    #cv_consumer{whitelist = [?SUB(user, <<"user-id">>), ?SUB(group, <<"group-id">>)]},
    #cv_interface{interface = graphsync},
    #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')}]},
    #cv_data_readonly{},
    #cv_data_path{whitelist = [<<"/space1/file1.txt">>, <<"/space2/dir/file2.txt">>]},
    #cv_data_objectid{whitelist = [element(2, {ok, _} = file_id:guid_to_objectid(file_id:pack_guid(<<"123">>, <<"abc">>)))]}
]).

%%%===================================================================
%%% Test functions
%%%===================================================================

access_token_test() ->
    Prototype = #token{
        onezone_domain = <<"domain.example.com">>,
        id = <<"1234567890example-id1234567890a">>,
        subject = ?SUB(user, <<"uid">>),
        persistence = {temporary, 7},
        type = ?ACCESS_TOKEN(<<"sessId">>)
    },
    ?assert(verify_token_serialized_form(Prototype, <<"verySecretSecret1">>, ?ACCESS_TOKEN_CAVEATS, <<
        "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDRhaWRlbnRpZmllciAyL3RtcC003L3Vzci11aWQvYWN00OnNlc3NJZC8xMjM"
        "00NTY3ODkwZXhhbXBsZS1pZDEyMzQ1Njc4OTBhCjAwMWFjaWQgdGltZSA8IDgzNzQ4OTEyMzQKMDAxZGNpZCBpcCA9IDE3Mi45OC4xMS4y"
        "My8zMgowMDEyY2lkIGFzbiA9IDMyMgowMDFjY2lkIGdlby5jb3VudHJ5ID00gUEx8RlIKMDAxZGNpZCBnZW8ucmVnaW9uID00gQXNpYXxF"
        "VQowMDFkY2lkIGF1dGhvcml6YXRpb24gPSBub25lCjAwMWVjaWQgc2VydmljZSA9IG96dy1vbmV6b25lCjAwMmNjaWQgY29uc3VtZXIgPS"
        "B1c3ItdXNlci1pZHxncnAtZ3JvdXAtaWQKMDAxZWNpZCBpbnRlcmZhY2UgPSBncmFwaHN5bmMKMDAxZWNpZCBhcGkgPSBhbGwvYWxsLyou"
        "Ki4qOioKMDAxNmNpZCBkYXRhLnJlYWRvbmx5CjAwNGFjaWQgZGF00YS5wYXRoID00gTDNOd1lXTmxNUzltYVd4bE1TNTBlSFE9fEwzTndZ"
        "V005sTWk5a2FYSXZabWxzWlRJdWRIaDAKMDA00MWNpZCBkYXRhLm9iamVjdGlkID00gMDAwMDAwMDAwMDBDREJEQTY3NzU2OTY00MjMzMT"
        "MyMzMyMzYxNjI2MwowMDJmc2lnbmF00dXJlIFzjctImjdheinIatewEE6qZxluQoX63kTHYBEbChVksCg"
    >>)).


access_token_with_session_test() ->
    Prototype = #token{
        onezone_domain = <<"domain.example.com">>,
        id = <<"1234567890example-id1234567890b">>,
        subject = ?SUB(user, <<"uid">>),
        persistence = named,
        type = ?ACCESS_TOKEN(<<"sessId">>)
    },
    ?assert(verify_token_serialized_form(Prototype, <<"verySecretSecret2">>, lists:reverse(?ACCESS_TOKEN_CAVEATS), <<
        "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDQ4aWRlbnRpZmllciAyL25tZC91c3ItdWlkL2FjdDpzZXNzSWQvMTIzNDU2N"
        "zg5MGV4YW1wbGUtaWQxMjM00NTY3ODkwYgowMDQxY2lkIGRhdGEub2JqZWN00aWQgPSAwMDAwMDAwMDAwMENEQkRBNjc3NTY5NjQyMzMxM"
        "zIzMzIzNjE2MjYzCjAwNGFjaWQgZGF00YS5wYXRoID00gTDNOd1lXTmxNUzltYVd4bE1TNTBlSFE9fEwzTndZV005sTWk5a2FYSXZabWxz"
        "WlRJdWRIaDAKMDAxNmNpZCBkYXRhLnJlYWRvbmx5CjAwMWVjaWQgYXBpID00gYWxsL2FsbC8qLiouKjoqCjAwMWVjaWQgaW500ZXJmYWNl"
        "ID00gZ3JhcGhzeW5jCjAwMmNjaWQgY29uc3VtZXIgPSB1c3ItdXNlci1pZHxncnAtZ3JvdXAtaWQKMDAxZWNpZCBzZXJ2aWNlID00gb3p3"
        "LW9uZXpvbmUKMDAxZGNpZCBhdXRob3JpemF00aW9uID00gbm9uZQowMDFkY2lkIGdlby5yZWdpb24gPSBBc2lhfEVVCjAwMWNjaWQgZ2Vv"
        "LmNvdW500cnkgPSBQTHxGUgowMDEyY2lkIGFzbiA9IDMyMgowMDFkY2lkIGlwID00gMTcyLjk4LjExLjIzLzMyCjAwMWFjaWQgdGltZSA8"
        "IDgzNzQ4OTEyMzQKMDAyZnNpZ25hdHVyZSA9cNaxGcs8TowTZAcrhE8aKlGRXqbMppFY74ciZtV9pwo"
    >>)).


identity_token_test() ->
    Prototype = #token{
        onezone_domain = <<"domain.example.com">>,
        id = <<"1234567890example-id1234567890c">>,
        subject = ?SUB(?ONEPROVIDER, <<"pid">>),
        persistence = named,
        type = ?IDENTITY_TOKEN
    },
    Caveats = [
        #cv_region{type = whitelist, list = [<<"Africa">>]},
        #cv_interface{interface = rest},
        #cv_time{valid_until = 8374891234},
        #cv_country{type = whitelist, list = [<<"DE">>, <<"PT">>]},
        #cv_asn{whitelist = [322]},
        #cv_consumer{whitelist = [?SUB(user, <<"user-id">>), ?SUB(group, <<"group-id">>)]},
        #cv_ip{whitelist = [{{172, 98, 11, 23}, 32}]}
    ],
    ?assert(verify_token_serialized_form(Prototype, <<"verySecretSecret3">>, Caveats, <<
        "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDQxaWRlbnRpZmllciAyL25tZC9wcnYtcGlkL2lkbi8xMjM00NTY3ODkwZXhh"
        "bXBsZS1pZDEyMzQ1Njc4OTBjCjAwMWNjaWQgZ2VvLnJlZ2lvbiA9IEFmcmljYQowMDE5Y2lkIGludGVyZmFjZSA9IHJlc3QKMDAxYWNpZC"
        "B00aW1lIDwgODM3NDg5MTIzNAowMDFjY2lkIGdlby5jb3VudHJ5ID00gREV8UFQKMDAxMmNpZCBhc24gPSAzMjIKMDAyY2NpZCBjb25zdW"
        "1lciA9IHVzci11c2VyLWlkfGdycC1ncm91cC1pZAowMDFkY2lkIGlwID00gMTcyLjk4LjExLjIzLzMyCjAwMmZzaWduYXR1cmUgeHgOy1l"
        "x96LmS1DmJ8VFizT6WrsALQizK1p00PQnJnYUK"
    >>)).


invite_token_with_caveats_test() ->
    Prototype = #token{
        onezone_domain = <<"domain.example.com">>,
        id = <<"1234567890example-id1234567890d">>,
        subject = ?SUB(user, <<"uid">>),
        persistence = {temporary, 49},
        type = ?INVITE_TOKEN(?USER_JOIN_SPACE, <<"spaceId">>)
    },
    Caveats = [
        #cv_country{type = whitelist, list = [<<"DE">>, <<"PT">>]},
        #cv_region{type = whitelist, list = [<<"Africa">>]},
        #cv_consumer{whitelist = [?SUB(group, <<"group-id">>), ?SUB(user, <<"user-id">>)]},
        #cv_time{valid_until = 8374891234},
        #cv_ip{whitelist = [{{11, 101, 11, 234}, 32}]},
        #cv_asn{whitelist = [112]}
    ],
    ?assert(verify_token_serialized_form(Prototype, <<"verySecretSecret4">>, Caveats, <<
        "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDRkaWRlbnRpZmllciAyL3RtcC0000OS91c3ItdWlkL3VqczpzcGFjZUlkOi8"
        "xMjM00NTY3ODkwZXhhbXBsZS1pZDEyMzQ1Njc4OTBkCjAwMWNjaWQgZ2VvLmNvdW500cnkgPSBERXxQVAowMDFjY2lkIGdlby5yZWdpb24"
        "gPSBBZnJpY2EKMDAyY2NpZCBjb25zdW1lciA9IGdycC1ncm91cC1pZHx1c3ItdXNlci1pZAowMDFhY2lkIHRpbWUgPCA4Mzc00ODkxMjM0"
        "0CjAwMWVjaWQgaXAgPSAxMS4xMDEuMTEuMjM00LzMyCjAwMTJjaWQgYXNuID00gMTEyCjAwMmZzaWduYXR1cmUgn3ofRtRE02FAEF1zeQB"
        "01t5IYpXXZO102SQSyMwvqx76ysK"
    >>)).


invite_token_of_different_types_test() ->
    lists:foreach(fun invite_token_of_different_types_test_base/1, [
        {?INVITE_TOKEN(?USER_JOIN_GROUP, <<"GroupId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDU4aWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvd"
            "WpnOkdyb3VwSWQ6LzEyMzQ1Njc4OTBleGFtcGxlLWlkMTIzNDU2Nzg5MGUKMDAyZnNpZ25hdHVyZSCH7MlKe97K7K5UHEpTU01nUb8"
            "AN77DBp7Ys20100nJBOAdwo"
        >>},

        {?INVITE_TOKEN(?GROUP_JOIN_GROUP, <<"GroupId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDU4aWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvZ"
            "2pnOkdyb3VwSWQ6LzEyMzQ1Njc4OTBleGFtcGxlLWlkMTIzNDU2Nzg5MGUKMDAyZnNpZ25hdHVyZSD00loLPXEwIFE01tXWZyMYubt"
            "XlKgPeJdTS9psZ3hwBpKQo"
        >>},

        {?INVITE_TOKEN(?USER_JOIN_SPACE, <<"SpaceId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDU4aWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvd"
            "WpzOlNwYWNlSWQ6LzEyMzQ1Njc4OTBleGFtcGxlLWlkMTIzNDU2Nzg5MGUKMDAyZnNpZ25hdHVyZSAG8T1rcWKfX8tS3nZFi61VK9f"
            "ZWSLsivWFXz1PrZNgnwo"
        >>},

        {?INVITE_TOKEN(?GROUP_JOIN_SPACE, <<"SpaceId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDU4aWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvZ"
            "2pzOlNwYWNlSWQ6LzEyMzQ1Njc4OTBleGFtcGxlLWlkMTIzNDU2Nzg5MGUKMDAyZnNpZ25hdHVyZSBkiyG39l3d5LskNbi00MKDLZm"
            "FIeZQnaOhwKcUtZoHjOAo"
        >>},

        {?INVITE_TOKEN(?SUPPORT_SPACE, <<"SpaceId">>, support_parameters:build(global, eager)), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDVhaWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvc"
            "3NwOlNwYWNlSWQ6Z2UvMTIzNDU2Nzg5MGV4YW1wbGUtaWQxMjM00NTY3ODkwZQowMDJmc2lnbmF00dXJlIICA3AFgFAXHLKDfXavXh"
            "Dw00YeJioSh5rLbBeomcltfVCg"
        >>},

        {?INVITE_TOKEN(?HARVESTER_JOIN_SPACE, <<"SpaceId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDU4aWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIva"
            "GpzOlNwYWNlSWQ6LzEyMzQ1Njc4OTBleGFtcGxlLWlkMTIzNDU2Nzg5MGUKMDAyZnNpZ25hdHVyZSCIkVahn6e3PGRcwoR757bKSoG"
            "VhzOdhjdExfIvn800DQQo"
        >>},

        {?INVITE_TOKEN(?REGISTER_ONEPROVIDER, <<"AdminUserId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDVjaWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvc"
            "m9wOkFkbWluVXNlcklkOi8xMjM00NTY3ODkwZXhhbXBsZS1pZDEyMzQ1Njc4OTBlCjAwMmZzaWduYXR1cmUgeqAZjO01V9inEh02PX"
            "keRIj2256WD71TxRyBXLrlLqG4sK"
        >>},

        {?INVITE_TOKEN(?USER_JOIN_CLUSTER, <<"ClusterId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDVhaWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvd"
            "WpjOkNsdXN00ZXJJZDovMTIzNDU2Nzg5MGV4YW1wbGUtaWQxMjM00NTY3ODkwZQowMDJmc2lnbmF00dXJlINnhiomPYiJyt2dk8IU2"
            "3PYaizhG00GtswiYeszMMN02P02Cg"
        >>},

        {?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, <<"ClusterId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDVhaWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvZ"
            "2pjOkNsdXN00ZXJJZDovMTIzNDU2Nzg5MGV4YW1wbGUtaWQxMjM00NTY3ODkwZQowMDJmc2lnbmF00dXJlIAN21J3T5t7r1va7OJzZ"
            "014Q01X8AYRC2woa1nOkjirUIUCg"
        >>},

        {?INVITE_TOKEN(?USER_JOIN_HARVESTER, <<"HarvesterId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDVjaWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvd"
            "WpoOkhhcnZlc3RlcklkOi8xMjM00NTY3ODkwZXhhbXBsZS1pZDEyMzQ1Njc4OTBlCjAwMmZzaWduYXR1cmUgy99erYZIMaNZ1682gX"
            "QAUU1AtU3KCLnpdfiKgyZN1LEK"
        >>},

        {?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, <<"HarvesterId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDVjaWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvZ"
            "2poOkhhcnZlc3RlcklkOi8xMjM00NTY3ODkwZXhhbXBsZS1pZDEyMzQ1Njc4OTBlCjAwMmZzaWduYXR1cmUgcb52Gw9VBjGdkBxV80"
            "1Tjp00UiaHQ2nMwgK14JjCUNFt4K"
        >>},

        {?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"HarvesterId">>), <<
            "MDAyMGxvY2F00aW9uIGRvbWFpbi5leGFtcGxlLmNvbQowMDVjaWRlbnRpZmllciAyL25tZC91c3ItaW52aXRhdGlvbi1zZW5kZXIvc"
            "2poOkhhcnZlc3RlcklkOi8xMjM00NTY3ODkwZXhhbXBsZS1pZDEyMzQ1Njc4OTBlCjAwMmZzaWduYXR1cmUgiaLOh53TLU5C8SwA5u"
            "ZgGtVzD02ZeZknOz1Xjz4qaL2kK"
        >>}
    ]).

invite_token_of_different_types_test_base({TokenType, ExpSerialized}) ->
    Prototype = #token{
        onezone_domain = <<"domain.example.com">>,
        id = <<"1234567890example-id1234567890e">>,
        subject = ?SUB(user, <<"invitation-sender">>),
        persistence = named,
        type = TokenType
    },
    ?assert(verify_token_serialized_form(Prototype, <<"verySecretSecret5">>, [], ExpSerialized)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
verify_token_serialized_form(Prototype, Secret, Caveats, ExpSerialized) ->
    Token = tokens:construct(Prototype, Secret, Caveats),
    case tokens:serialize(Token) of
        {ok, ExpSerialized} ->
            % make sure all the original information is retained after deserialization
            {ok, Token} == tokens:deserialize(ExpSerialized);
        {ok, DifferentSerialized} ->
            eunit_utils:debug_log(
                "Serialized token different that expected.~n"
                "Exp: ~s~n"
                "Got: ~s~n",
                [ExpSerialized, DifferentSerialized]
            ),
            false
    end.

-endif.

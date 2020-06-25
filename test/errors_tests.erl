%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of errors module.
%%% @end
%%%-------------------------------------------------------------------
-module(errors_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("errors.hrl").
-include("aai/aai.hrl").

encode_decode_error_test_() ->
    lists:map(fun(Testcase) ->
        {str_utils:to_binary(Testcase), fun() ->
            {Before, After} = case Testcase of
                {different, A, B} -> {A, B};
                Err -> {Err, Err}
            end,
            Json = errors:to_json(Before),
            ?assert(is_map(Json)),
            ?assert(size(maps:get(<<"description">>, Json)) > 0),
            % enforce description convention
            ?assert(str_utils:binary_ends_with(maps:get(<<"description">>, Json), <<".">>)),
            EncodedJSON = json_utils:encode(Json),
            DecodedJSON = json_utils:decode(EncodedJSON),
            FromJson = errors:from_json(DecodedJSON),
            ?assertMatch({error, _}, FromJson),
            ?assertEqual(After, FromJson)
        end} end, testcases()).


http_code_test_() ->
    lists:map(fun(Testcase) ->
        {str_utils:to_binary(Testcase), fun() ->
            Error = case Testcase of
                {different, A, _B} -> A;
                Err -> Err
            end,
            Code = errors:to_http_code(Error),
            ?assert(Code >= 400),
            ?assert(Code =< 503)
        end} end, testcases()).


unexpected_error_test() ->
    UnexpectedError = {error, {some_error, that_we_dont_understand, 1653}},
    ?assertMatch(
        ?ERROR_UNEXPECTED_ERROR(_),
        errors:from_json(errors:to_json(UnexpectedError))
    ).


% {different, Before, After} is used when encoding and decoding causes the error to change.
testcases() -> [
    %% -----------------------------------------------------------------------------
    %% General errors
    %% -----------------------------------------------------------------------------
    ?ERROR_BAD_MESSAGE(<<"edaml-wsesjapfs">>),
    ?ERROR_BAD_MESSAGE(#{<<"nested">> => <<"edaml-wsesjapfs">>}),
    ?ERROR_NO_CONNECTION_TO_ONEZONE,
    ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER,
    ?ERROR_UNREGISTERED_ONEPROVIDER,
    ?ERROR_INTERNAL_SERVER_ERROR,
    ?ERROR_NOT_IMPLEMENTED,
    ?ERROR_NOT_SUPPORTED,
    ?ERROR_SERVICE_UNAVAILABLE,
    ?ERROR_TIMEOUT,
    ?ERROR_TEMPORARY_FAILURE,
    ?ERROR_UNAUTHORIZED,
    ?ERROR_FORBIDDEN,
    ?ERROR_NOT_FOUND,
    ?ERROR_ALREADY_EXISTS,
    ?ERROR_FILE_ACCESS(<<"/etc/cert/web_key.pem">>, ?EROFS),
    {different, ?ERROR_FILE_ACCESS(['./', ["name"]], ?EROFS), ?ERROR_FILE_ACCESS(<<"./name">>, ?EROFS)},

    %% -----------------------------------------------------------------------------
    %% POSIX errors
    %% -----------------------------------------------------------------------------
    ?ERROR_POSIX(eacess),

    %% -----------------------------------------------------------------------------
    %% Auth errors
    %% -----------------------------------------------------------------------------
    ?ERROR_BAD_BASIC_CREDENTIALS,
    {different, ?ERROR_BAD_IDP_ACCESS_TOKEN(keycloak), ?ERROR_BAD_IDP_ACCESS_TOKEN(<<"keycloak">>)},
    ?ERROR_BAD_TOKEN,
    ?ERROR_BAD_SERVICE_TOKEN(?ERROR_BAD_TOKEN),
    ?ERROR_BAD_SERVICE_TOKEN(?ERROR_TOKEN_REVOKED),
    ?ERROR_BAD_SERVICE_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12345678})),
    ?ERROR_BAD_CONSUMER_TOKEN(?ERROR_TOKEN_INVALID),
    ?ERROR_BAD_CONSUMER_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{1, 2, 3, 4}, 32}]})),
    ?ERROR_TOKEN_INVALID,
    ?ERROR_TOKEN_REVOKED,
    ?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"123">>)),
    ?ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN),
    ?ERROR_NOT_AN_INVITE_TOKEN(?USER_JOIN_SPACE, ?ACCESS_TOKEN(<<"sess-8765">>)),
    ?ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"12345">>)),
    ?ERROR_NOT_AN_INVITE_TOKEN(any, ?ACCESS_TOKEN),
    ?ERROR_TOKEN_CAVEAT_UNKNOWN(<<"grant = everything">>),
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12323746234}),
    ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(86400),
    ?ERROR_TOKEN_SUBJECT_INVALID,
    ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, <<"kjasif2387rg7adc09jf8a0sdfg97a">>)),
    ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED,
    ?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED,
    ?ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(?ONEPROVIDER, <<"zxbcv78s0dfasdf">>)),
    ?ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(nobody)),
    ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123456">>),
    ?ERROR_TOKEN_SESSION_INVALID,

    %% -----------------------------------------------------------------------------
    %% Graph Sync errors
    %% -----------------------------------------------------------------------------
    ?ERROR_EXPECTED_HANDSHAKE_MESSAGE,
    ?ERROR_HANDSHAKE_ALREADY_DONE,
    ?ERROR_BAD_VERSION([4, 5, 6, 7, 8]),
    ?ERROR_BAD_GRI,
    ?ERROR_RPC_UNDEFINED,
    ?ERROR_NOT_SUBSCRIBABLE,

    %% -----------------------------------------------------------------------------
    %% Data validation errors
    %% -----------------------------------------------------------------------------
    ?ERROR_MALFORMED_DATA,
    ?ERROR_MISSING_REQUIRED_VALUE(<<"spaceId">>),
    ?ERROR_MISSING_AT_LEAST_ONE_VALUE([<<"name">>, <<"type">>]),
    ?ERROR_BAD_DATA(<<"spaceId">>),
    ?ERROR_BAD_VALUE_EMPTY(<<"spaceId">>),
    % @TODO VFS-5838 Placeholder to plug into existing onepanel mechanism
    ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>),
    {different, ?ERROR_BAD_VALUE_ATOM(<<"spaceId">>), ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>)},
    {different, ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>), ?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"privileges">>)},
    ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>),
    ?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"urls">>),
    ?ERROR_BAD_VALUE_INTEGER(<<"size">>),
    ?ERROR_BAD_VALUE_FLOAT(<<"latitude">>),
    ?ERROR_BAD_VALUE_JSON(<<"<xml></xml>">>),
    ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_BAD_TOKEN),
    ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_TOKEN_INVALID),
    ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_TOKEN_REVOKED),
    ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?ACCESS_TOKEN)),
    ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_scope{scope = identity_token})),
    ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>),
    ?ERROR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>),
    ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"ip">>),
    ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ip_list">>),
    ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, 500),
    ?ERROR_BAD_VALUE_TOO_HIGH(<<"size">>, 1000),
    ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"size">>, 500, 1000),
    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>]),
    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>]),
    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>),
    ?ERROR_BAD_VALUE_AMBIGUOUS_ID(<<"viewName">>),
    ?ERROR_BAD_VALUE_IDENTIFIER(<<"id">>),
    ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"spaceId">>),
    ?ERROR_BAD_VALUE_FULL_NAME,
    ?ERROR_BAD_VALUE_USERNAME,
    ?ERROR_BAD_VALUE_PASSWORD,
    ?ERROR_BAD_VALUE_EMAIL,
    ?ERROR_BAD_VALUE_NAME,
    ?ERROR_BAD_VALUE_DOMAIN,
    ?ERROR_BAD_VALUE_SUBDOMAIN,
    ?ERROR_BAD_VALUE_CAVEAT(#{<<"foo">> => <<"bar">>}),
    ?ERROR_BAD_VALUE_QOS_PARAMETERS,
    ?ERROR_BAD_GUI_PACKAGE,
    ?ERROR_GUI_PACKAGE_TOO_LARGE,
    ?ERROR_GUI_PACKAGE_UNVERIFIED(<<"5f38fb2e288be67bacc9c206e40f28ee42f9bba9c521f5d6036a4217abd146ba">>),
    ?ERROR_INVALID_QOS_EXPRESSION,
    ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(none, none),
    ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(evicting_replicas, {resizing, 0}),

    %% -----------------------------------------------------------------------------
    %% oz_worker errors
    %% -----------------------------------------------------------------------------
    ?ERROR_BASIC_AUTH_NOT_SUPPORTED,
    ?ERROR_BASIC_AUTH_DISABLED,
    ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED,
    ?ERROR_SUBDOMAIN_DELEGATION_DISABLED,
    ?ERROR_PROTECTED_GROUP,
    ?ERROR_CANNOT_DELETE_ENTITY(od_user, <<"user1">>),
    ?ERROR_CANNOT_ADD_RELATION_TO_SELF,
    ?ERROR_RELATION_DOES_NOT_EXIST(od_user, <<"user1">>, od_space, <<"space1">>),
    ?ERROR_RELATION_ALREADY_EXISTS(od_user, <<"user1">>, od_space, <<"space1">>),
    ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(<<"spaceId">>, <<"storageId">>),

    %%--------------------------------------------------------------------
    %% op_worker errors
    %%--------------------------------------------------------------------
    ?ERROR_USER_NOT_SUPPORTED,
    ?ERROR_AUTO_CLEANING_DISABLED,
    ?ERROR_FILE_POPULARITY_DISABLED,
    ?ERROR_SPACE_NOT_SUPPORTED_BY(<<"providerId">>),
    ?ERROR_STORAGE_IN_USE,
    ?ERROR_STORAGE_TEST_FAILED(read),
    ?ERROR_STORAGE_TEST_FAILED(write),
    ?ERROR_STORAGE_TEST_FAILED(remove),
    ?ERROR_REQUIRES_NON_IMPORTED_STORAGE(<<"storageId">>),
    ?ERROR_REQUIRES_IMPORTED_STORAGE(<<"storageId">>),
    ?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>]),
    ?ERROR_TRANSFER_ALREADY_ENDED,
    ?ERROR_TRANSFER_NOT_ENDED,
    ?ERROR_VIEW_NOT_EXISTS_ON(<<"providerId">>),
    ?ERROR_VIEW_QUERY_FAILED(<<"category">>, <<"description">>),
    ?ERROR_DNS_SERVERS_UNREACHABLE([default, {1, 2, 3, 4}]),
    {different, ?ERROR_DNS_SERVERS_UNREACHABLE([<<"1.1.1.1">>, <<"8.8.8.8">>]),
        ?ERROR_DNS_SERVERS_UNREACHABLE([{1, 1, 1, 1}, {8, 8, 8, 8}])},

    %%--------------------------------------------------------------------
    %% onepanel errors
    %%--------------------------------------------------------------------
    ?ERROR_ON_NODES(?ERROR_FILE_ACCESS(<<"/path">>, ?EACCES), [<<"node1.example.com">>]),
    ?ERROR_DNS_SERVERS_UNREACHABLE([default, {1, 2, 3, 4}]),
    ?ERROR_FILE_ALLOCATION(1000, 2000),
    ?ERROR_LETS_ENCRYPT_NOT_REACHABLE,
    ?ERROR_LETS_ENCRYPT_RESPONSE(undefined, <<"Bad Let's Encrypt response">>),
    ?ERROR_LETS_ENCRYPT_RESPONSE(
        #{<<"type">> => <<"urn:ietf:params:acme:error:rateLimited">>,
            <<"status">> => 429, <<"detail">> => <<"Error creating new order">>},
        <<"Error creating new order">>),
    ?ERROR_NODE_ALREADY_IN_CLUSTER(<<"onepanel@example.com">>),
    ?ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEPROVIDER),
    ?ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEZONE),
    ?ERROR_NO_CONNECTION_TO_NEW_NODE(<<"onepanel@example.com">>),
    {different, ?ERROR_NO_SERVICE_NODES(op_worker), ?ERROR_NO_SERVICE_NODES(<<"op_worker">>)},
    ?ERROR_STORAGE_IMPORT_STARTED,
    ?ERROR_USER_NOT_IN_CLUSTER,

    %% -----------------------------------------------------------------------------
    %% Unknown error
    %% -----------------------------------------------------------------------------
    ?ERROR_UNEXPECTED_ERROR(<<"deb7a8aaf82">>),
    ?ERROR_UNKNOWN_ERROR(#{
        <<"id">> => <<"someErrorThatWasNotSpecifiedInThisSoftwareVersion">>,
        <<"details">> => #{<<"key">> => <<"value">>},
        <<"description">> => <<"Human readable error description.">>
    })
].



-endif.
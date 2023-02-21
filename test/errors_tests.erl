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
-include("test/test_utils.hrl").
-include("time_series/common.hrl").
-include("graph_sync/gri.hrl").
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


cannot_translate_error_test() ->
    % in case of an error that is not specified in the errors module,
    % a proper error log is logged and an internal server error should be returned
    BadErrorTerm = {error, {some_error, that_we_dont_understand, 1653}},
    ?assertMatch(
        ?ERROR_INTERNAL_SERVER_ERROR(_),
        errors:from_json(errors:to_json(BadErrorTerm))
    ).


unrecognized_error_test() ->
    UnrecognizedErrorJson = #{
        <<"id">> => <<"someErrorThatWasNotSpecifiedInThisSoftwareVersion">>,
        <<"details">> => #{<<"key">> => <<"value">>},
        <<"description">> => <<"Human readable error description.">>
    },
    ?assertEqual(
        ?ERROR_UNRECOGNIZED_ERROR(UnrecognizedErrorJson),
        errors:from_json(UnrecognizedErrorJson)
    ),
    ?assertEqual(
       UnrecognizedErrorJson#{<<"description">> => <<"No description (unknown error).">>},
        errors:to_json(errors:from_json(maps:without([<<"description">>], UnrecognizedErrorJson)))
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
    ?ERROR_INTERNAL_SERVER_ERROR(?RAND_STR()),
    ?ERROR_NOT_IMPLEMENTED,
    ?ERROR_NOT_SUPPORTED,
    ?ERROR_SERVICE_UNAVAILABLE,
    ?ERROR_TIMEOUT,
    ?ERROR_TEMPORARY_FAILURE,
    ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(<<"Some external service">>),
    ?ERROR_UNAUTHORIZED(?ERROR_NOT_AN_ACCESS_TOKEN(?IDENTITY_TOKEN)),
    ?ERROR_UNAUTHORIZED,
    ?ERROR_FORBIDDEN,
    ?ERROR_FORBIDDEN(<<"Sausage not for the dog">>),
    ?ERROR_FORBIDDEN(<<"Honey not for the piglets.">>),
    ?ERROR_NOT_FOUND,
    ?ERROR_ALREADY_EXISTS,
    ?ERROR_FILE_ACCESS(<<"/etc/cert/web_key.pem">>, ?EROFS),
    {different, ?ERROR_FILE_ACCESS(['./', ["name"]], ?EROFS), ?ERROR_FILE_ACCESS(<<"./name">>, ?EROFS)},
    ?ERROR_LIMIT_REACHED(1000, <<"number of requests">>),

    %% -----------------------------------------------------------------------------
    %% POSIX errors
    %% -----------------------------------------------------------------------------
    ?ERROR_POSIX(eacess),

    %% -----------------------------------------------------------------------------
    %% Auth errors
    %% -----------------------------------------------------------------------------
    ?ERROR_USER_BLOCKED,
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
    ?ERROR_BAD_DATA(<<"nestedRecord">>, ?ERROR_MISSING_REQUIRED_VALUE(<<"key">>)),
    ?ERROR_BAD_DATA(<<"spaceId">>, <<"Not so readable hint">>),
    ?ERROR_BAD_VALUE_EMPTY(<<"spaceId">>),
    % @TODO VFS-5838 Placeholder to plug into existing onepanel mechanism
    ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>),
    {different, ?ERROR_BAD_VALUE_ATOM(<<"spaceId">>), ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>)},
    {different, ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>), ?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"privileges">>)},
    ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>),
    ?ERROR_BAD_VALUE_BINARY_TOO_LARGE(<<"description">>, 1000),
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
    ?ERROR_BAD_VALUE_OCTAL(<<"mode">>),
    ?ERROR_BAD_VALUE_FILE_PATH,
    ?ERROR_BAD_VALUE_FULL_NAME,
    ?ERROR_BAD_VALUE_USERNAME,
    ?ERROR_BAD_VALUE_PASSWORD,
    ?ERROR_BAD_VALUE_EMAIL,
    ?ERROR_BAD_VALUE_NAME,
    ?ERROR_BAD_VALUE_NAME(<<"key">>),
    ?ERROR_BAD_VALUE_DOMAIN,
    ?ERROR_BAD_VALUE_SUBDOMAIN,
    ?ERROR_BAD_VALUE_CAVEAT(#{<<"foo">> => <<"bar">>}),
    ?ERROR_BAD_VALUE_QOS_PARAMETERS,
    ?ERROR_TSC_MISSING_LAYOUT(#{<<"TS1">> => [<<"M1">>, <<"M2">>]}),
    ?ERROR_TSC_TOO_MANY_METRICS(10000),
    ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(
        <<"TS1">>, <<"M1">>,
        #metric_config{resolution = 60, retention = 5, aggregator = max},
        #metric_config{resolution = 3600, retention = 24, aggregator = sum}
    ),
    ?ERROR_BAD_GUI_PACKAGE,
    ?ERROR_GUI_PACKAGE_TOO_LARGE,
    ?ERROR_GUI_PACKAGE_UNVERIFIED(<<"5f38fb2e288be67bacc9c206e40f28ee42f9bba9c521f5d6036a4217abd146ba">>),
    ?ERROR_INVALID_QOS_EXPRESSION(<<"invalid \";\"">>),
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
    ?ERROR_ATM_LAMBDA_IN_USE([<<"a">>, <<"b">>, <<"c">>, <<"d">>]),
    ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, <<"space1">>),
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
    ?ERROR_SPACE_NOT_SUPPORTED_BY(<<"spaceId">>, <<"providerId">>),
    ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(<<"providerId">>, <<"storageId">>, <<"spaceId">>),
    ?ERROR_STORAGE_IN_USE,
    ?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE,
    ?ERROR_STORAGE_TEST_FAILED(read),
    ?ERROR_STORAGE_TEST_FAILED(write),
    ?ERROR_STORAGE_TEST_FAILED(remove),
    ?ERROR_REQUIRES_NON_IMPORTED_STORAGE(<<"storageId">>),
    ?ERROR_REQUIRES_IMPORTED_STORAGE(<<"storageId">>),
    ?ERROR_REQUIRES_READONLY_STORAGE(<<"storageType">>),
    ?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>]),
    ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>, <<"s3">>], [<<"s3">>]),
    ?ERROR_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"swift">>, <<"s3">>, <<"cephrados">>]),
    ?ERROR_STAT_OPERATION_NOT_SUPPORTED(<<"storageId">>),
    ?ERROR_TRANSFER_ALREADY_ENDED,
    ?ERROR_TRANSFER_NOT_ENDED,
    ?ERROR_VIEW_NOT_EXISTS_ON(<<"providerId">>),
    ?ERROR_VIEW_QUERY_FAILED(<<"category">>, <<"description">>),
    ?ERROR_DNS_SERVERS_UNREACHABLE([default, {1, 2, 3, 4}]),
    {different, ?ERROR_DNS_SERVERS_UNREACHABLE([<<"1.1.1.1">>, <<"8.8.8.8">>]),
        ?ERROR_DNS_SERVERS_UNREACHABLE([{1, 1, 1, 1}, {8, 8, 8, 8}])},
    ?ERROR_QUOTA_EXCEEDED,
    ?ERROR_DIR_STATS_DISABLED_FOR_SPACE,
    ?ERROR_DIR_STATS_NOT_READY,
    ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(ongoing, [preserved, cancelled]),
    ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(<<"archiveId">>),
    ?ERROR_RECALL_TARGET_CONFLICT,

    %%--------------------------------------------------------------------
    %% op_worker atm errors
    %%--------------------------------------------------------------------
    ?ERROR_ATM_UNSUPPORTED_DATA_TYPE(atm_string_type, [atm_number_type]),
    ?ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type),
    ?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(#{<<"fileId">> => <<"REG">>}, atm_file_type, #{<<"hasAccess">> => true}),

    ?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT,
    ?ERROR_ATM_STORE_CREATION_FAILED(<<"id">>, ?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT),
    ?ERROR_ATM_STORE_FROZEN(<<"id">>),
    ?ERROR_ATM_STORE_TYPE_DISALLOWED(<<"id">>, [single_value]),
    ?ERROR_ATM_STORE_CONTENT_NOT_SET(<<"id">>),
    ?ERROR_ATM_STORE_NOT_FOUND(<<"id">>),

    ?ERROR_ATM_WORKFLOW_EMPTY,
    ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING,
    ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED,
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED,
    ?ERROR_ATM_WORKFLOW_EXECUTION_ENDED,
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED,
    ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE,

    ?ERROR_ATM_LANE_EMPTY(<<"id">>),
    ?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(<<"id">>, ?ERROR_INTERNAL_SERVER_ERROR),
    ?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED),
    ?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED,
    ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED,

    ?ERROR_ATM_PARALLEL_BOX_EMPTY(<<"id">>),
    ?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(<<"id">>, ?ERROR_INTERNAL_SERVER_ERROR),
    ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED),

    ?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(<<"id">>, ?ERROR_INTERNAL_SERVER_ERROR),
    ?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED),
    ?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(<<"repeats">>, ?ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type)),
    ?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(<<"arg">>),
    ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(<<"arg">>),
    ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(store_credentials, [iterated_item]),
    ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED([1, 2], [0]),
    ?ERROR_ATM_TASK_ARG_MAPPING_FAILED(<<"arg">>, ?ERROR_INTERNAL_SERVER_ERROR),

    ?ERROR_ATM_TASK_RESULT_MISSING(<<"result">>),
    ?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(<<"id">>, ?ERROR_INTERNAL_SERVER_ERROR),
    ?ERROR_ATM_TASK_RESULT_MAPPING_FAILED(<<"result">>, ?ERROR_INTERNAL_SERVER_ERROR),

    ?ERROR_ATM_TASK_EXECUTION_STOPPED,

    ?ERROR_ATM_JOB_BATCH_WITHDRAWN(<<"happy">>),
    ?ERROR_ATM_JOB_BATCH_CRASHED(<<"sad">>),

    ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED,
    ?ERROR_ATM_OPENFAAS_UNREACHABLE,
    ?ERROR_ATM_OPENFAAS_UNHEALTHY,
    ?ERROR_ATM_OPENFAAS_QUERY_FAILED,
    ?ERROR_ATM_OPENFAAS_QUERY_FAILED(<<"dns resolution error...">>),
    ?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED,

    ?ERROR_ATM_INVALID_STATUS_TRANSITION(active, scheduled),

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
    ?ERROR_USER_NOT_IN_CLUSTER,

    %% -----------------------------------------------------------------------------
    %% Unknown error
    %% -----------------------------------------------------------------------------
    ?ERROR_UNRECOGNIZED_ERROR(#{
        <<"id">> => <<"someErrorThatWasNotSpecifiedInThisSoftwareVersion">>,
        <<"details">> => #{<<"key">> => <<"value">>},
        <<"description">> => <<"Human readable error description.">>
    })
].



-endif.
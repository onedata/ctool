%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019-2024 ACK CYFRONET AGH
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
-include("deprecated_errors.hrl").
-include("aai/aai.hrl").

-record(testcase, {
    error :: errors:error(),
    error_after_encoding_decoding = undefined :: undefined | errors:error(),
    deprecated_error = undefined :: undefined | deprecated_errors:error(),
    deprecated_error_after_encoding_decoding = undefined :: undefined | deprecated_errors:error()
}).


encode_decode_error_test_() ->
    lists:flatmap(fun(#testcase{
        error = Error,
        error_after_encoding_decoding = ErrorAfterEncodingDecoding,
        deprecated_error = DeprecatedError,
        deprecated_error_after_encoding_decoding = DeprecatedErrorAfterEncodingDecoding
    }) ->
        ExpError = utils:ensure_defined(ErrorAfterEncodingDecoding, Error),
        ExpDeprecatedError = utils:ensure_defined(DeprecatedErrorAfterEncodingDecoding, DeprecatedError),

        JsonEncodeDecodeFun = fun(Json) -> json_utils:decode(json_utils:encode(Json)) end,

        CompatibilityCases = case ExpDeprecatedError of
            undefined ->
                [];
            _ ->
                [
                    {str_utils:format_bin("encode-deprecated_decode: ~tp", [Error]), fun() ->
                        Json = errors:to_json(Error),
                        assert_valid_error_json(Json),
                        FromJson = deprecated_errors:from_json(JsonEncodeDecodeFun(Json)),
                        ?assertMatch({error, _}, FromJson),
                        ?assertEqual(ExpDeprecatedError, FromJson)
                    end},
                    {str_utils:format_bin("deprecated_encode-decode: ~tp", [Error]), fun() ->
                        Json = deprecated_errors:to_json(DeprecatedError),
                        assert_valid_error_json(Json),
                        FromJson = errors:from_json(JsonEncodeDecodeFun(Json)),
                        ?assertMatch(#od_error{}, FromJson),
                        ?assertEqual(ExpError, FromJson)
                    end}
                ]
        end,

        [
            {str_utils:format_bin("encode-decode: ~tp", [Error]), fun() ->
                Json = errors:to_json(Error),
                assert_valid_error_json(Json),
                FromJson = errors:from_json(JsonEncodeDecodeFun(Json)),
                ?assertMatch(#od_error{}, FromJson),
                ?assertEqual(ExpError, FromJson)
            end}
            | CompatibilityCases
        ]
    end, testcases()).


%% @private
assert_valid_error_json(Json) ->
    ?assert(is_map(Json)),
    ?assert(size(maps:get(<<"description">>, Json)) > 0),
    % enforce description convention
    ?assert(str_utils:binary_ends_with(maps:get(<<"description">>, Json), <<".">>)).


http_code_test_() ->
    lists:map(fun(#testcase{error = Error, deprecated_error = DeprecatedError}) ->
        {str_utils:to_binary(Error), fun() ->
            Code = errors:to_http_code(Error),
            ?assert(Code >= 400),
            ?assert(Code =< 503),

            case DeprecatedError of
                undefined ->
                    ok;
                _ ->
                    ?assertEqual(deprecated_errors:to_http_code(DeprecatedError), Code)
            end
        end}
    end, testcases()).


http_code_for_nonexistent_error_test() ->
    ?assertException(_, _, errors:to_http_code({error, gibberish})).


is_known_error_test_() ->
    lists:map(fun(#testcase{error = Error}) ->
        {str_utils:to_binary(Error), ?_assert(errors:is_known_error(Error))}
    end, testcases()).


is_not_known_error_test() ->
    ?assertNot(errors:is_known_error({error, gibberish})).


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
    %%--------------------------------------------------------------------
    %% Unknown / unexpected error
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_UNRECOGNIZED_ERROR(#{
            <<"id">> => <<"someErrorThatWasNotSpecifiedInThisSoftwareVersion">>,
            <<"details">> => #{<<"key">> => <<"value">>},
            <<"description">> => <<"Human readable error description.">>
        }),
        deprecated_error = ?DEPRECATED_ERROR_UNRECOGNIZED_ERROR(#{
            <<"id">> => <<"someErrorThatWasNotSpecifiedInThisSoftwareVersion">>,
            <<"details">> => #{<<"key">> => <<"value">>},
            <<"description">> => <<"Human readable error description.">>
        })
    },

    %%--------------------------------------------------------------------
    %% auth errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_BAD_BASIC_CREDENTIALS,
        deprecated_error = ?DEPRECATED_ERROR_BAD_BASIC_CREDENTIALS
    },
    % TODO
%%    #testcase{
%%        error = ?ERROR_FORBIDDEN(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN(undefined)
%%    },
    #testcase{
        error = ?ERROR_FORBIDDEN(<<"Sausage not for the dog">>),
        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN(<<"Sausage not for the dog">>)
    },
    #testcase{
        error = ?ERROR_FORBIDDEN(<<"Honey not for the piglets.">>),
        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN(<<"Honey not for the piglets.">>)
    },
    #testcase{
        error = ?ERROR_UNAUTHORIZED(?ERROR_NOT_AN_ACCESS_TOKEN(?IDENTITY_TOKEN)),
        deprecated_error = ?DEPRECATED_ERROR_UNAUTHORIZED(?DEPRECATED_ERROR_NOT_AN_ACCESS_TOKEN(?IDENTITY_TOKEN))
    },
    #testcase{
        error = ?ERROR_UNAUTHORIZED(undefined),
        deprecated_error = ?DEPRECATED_ERROR_UNAUTHORIZED(undefined)
    },
    #testcase{
        error = ?ERROR_USER_BLOCKED,
        deprecated_error = ?DEPRECATED_ERROR_USER_BLOCKED
    },

    %%--------------------------------------------------------------------
    %% auth/token errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_BAD_CONSUMER_TOKEN(?ERROR_TOKEN_INVALID),
        deprecated_error = ?DEPRECATED_ERROR_BAD_CONSUMER_TOKEN(?DEPRECATED_ERROR_TOKEN_INVALID)
    },
    #testcase{
        error = ?ERROR_BAD_CONSUMER_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{1, 2, 3, 4}, 32}]})),
        deprecated_error = ?DEPRECATED_ERROR_BAD_CONSUMER_TOKEN(?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{
            whitelist = [{{1, 2, 3, 4}, 32}]
        }))
    },
    #testcase{
        error = ?ERROR_BAD_IDP_ACCESS_TOKEN(keycloak),
        error_after_encoding_decoding = ?ERROR_BAD_IDP_ACCESS_TOKEN(<<"keycloak">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_IDP_ACCESS_TOKEN(keycloak),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_BAD_IDP_ACCESS_TOKEN(<<"keycloak">>)
    },
    #testcase{
        error = ?ERROR_BAD_SERVICE_TOKEN(?ERROR_BAD_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_BAD_SERVICE_TOKEN(?DEPRECATED_ERROR_BAD_TOKEN)
    },
    #testcase{
        error = ?ERROR_BAD_SERVICE_TOKEN(?ERROR_TOKEN_REVOKED),
        deprecated_error = ?DEPRECATED_ERROR_BAD_SERVICE_TOKEN(?DEPRECATED_ERROR_TOKEN_REVOKED)
    },
    #testcase{
        error = ?ERROR_BAD_SERVICE_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12345678})),
        deprecated_error = ?DEPRECATED_ERROR_BAD_SERVICE_TOKEN(?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12345678}))
    },
    #testcase{
        error = ?ERROR_BAD_TOKEN,
        deprecated_error = ?DEPRECATED_ERROR_BAD_TOKEN
    },
    #testcase{
        error = ?ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(?ONEPROVIDER, <<"zxbcv78s0dfasdf">>)),
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(?ONEPROVIDER, <<"zxbcv78s0dfasdf">>))
    },
    #testcase{
        error = ?ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(nobody)),
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(nobody))
    },
    #testcase{
        error = ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED,
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
    },
    #testcase{
        error = ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123456">>),
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123456">>)
    },
    #testcase{
        error = ?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED,
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED
    },
    #testcase{
        error = ?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"123">>)),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"123">>))
    },
    #testcase{
        error = ?ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN)
    },
    #testcase{
        error = ?ERROR_NOT_AN_INVITE_TOKEN(?USER_JOIN_SPACE, ?ACCESS_TOKEN(<<"sess-8765">>)),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(?USER_JOIN_SPACE, ?ACCESS_TOKEN(<<"sess-8765">>))
    },
    #testcase{
        error = ?ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"12345">>)),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"12345">>))
    },
    #testcase{
        error = ?ERROR_NOT_AN_INVITE_TOKEN(any, ?ACCESS_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(any, ?ACCESS_TOKEN)
    },
    #testcase{
        error = ?ERROR_TOKEN_CAVEAT_UNKNOWN(<<"grant = everything">>),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_CAVEAT_UNKNOWN(<<"grant = everything">>)
    },
    #testcase{
        error = ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12323746234}),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12323746234})
    },
    #testcase{
        error = ?ERROR_TOKEN_INVALID,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_INVALID
    },
    #testcase{
        error = ?ERROR_TOKEN_REVOKED,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_REVOKED
    },
    #testcase{
        error = ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, <<"kjasif2387rg7adc09jf8a0sdfg97a">>)),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, <<"kjasif2387rg7adc09jf8a0sdfg97a">>))
    },
    #testcase{
        error = ?ERROR_TOKEN_SESSION_INVALID,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_SESSION_INVALID
    },
    #testcase{
        error = ?ERROR_TOKEN_SUBJECT_INVALID,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_SUBJECT_INVALID
    },
    #testcase{
        error = ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(86400),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_TIME_CAVEAT_REQUIRED(86400)
    },
    #testcase{
        error = ?ERROR_TOKEN_TOO_LARGE(86400),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_TOO_LARGE(86400)
    },

    %%--------------------------------------------------------------------
    %% connection errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_NO_CONNECTION_TO_CLUSTER_NODE,
        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_CLUSTER_NODE
    },
    #testcase{
        error = ?ERROR_NO_CONNECTION_TO_ONEZONE,
        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_ONEZONE
    },
    #testcase{
        error = ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER,
        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER
    },

    %%--------------------------------------------------------------------
    %% data_validation errors
    %%--------------------------------------------------------------------
    % TODO
%%    #testcase{
%%        error = ?ERROR_BAD_DATA(<<"spaceId">>, undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_DATA(<<"spaceId">>, undefined)
%%    },
    #testcase{
        error = ?ERROR_BAD_DATA(<<"nestedRecord">>, ?ERROR_MISSING_REQUIRED_VALUE(<<"key">>)),
        deprecated_error = ?DEPRECATED_ERROR_BAD_DATA(<<"nestedRecord">>, ?DEPRECATED_ERROR_MISSING_REQUIRED_VALUE(<<"key">>))
    },
    #testcase{
        error = ?ERROR_BAD_DATA(<<"spaceId">>, <<"Not so readable hint">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_DATA(<<"spaceId">>, <<"Not so readable hint">>)
    },
    #testcase{
        error = ?ERROR_BAD_GUI_PACKAGE,
        deprecated_error = ?DEPRECATED_ERROR_BAD_GUI_PACKAGE
    },
    #testcase{
        error = ?ERROR_GUI_PACKAGE_TOO_LARGE,
        deprecated_error = ?DEPRECATED_ERROR_GUI_PACKAGE_TOO_LARGE
    },
    #testcase{
        error = ?ERROR_GUI_PACKAGE_UNVERIFIED(<<"5f38fb2e288be67bacc9c206e40f28ee42f9bba9c521f5d6036a4217abd146ba">>),
        deprecated_error = ?DEPRECATED_ERROR_GUI_PACKAGE_UNVERIFIED(<<"5f38fb2e288be67bacc9c206e40f28ee42f9bba9c521f5d6036a4217abd146ba">>)
    },
    #testcase{
        error = ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(none, none),
        deprecated_error = ?DEPRECATED_ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(none, none)
    },
    #testcase{
        error = ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(evicting_replicas, {resizing, 0}),
        deprecated_error = ?DEPRECATED_ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(evicting_replicas, {resizing, 0})
    },
    #testcase{
        error = ?ERROR_INVALID_QOS_EXPRESSION(<<"invalid \";\"">>),
        deprecated_error = ?DEPRECATED_ERROR_INVALID_QOS_EXPRESSION(<<"invalid \";\"">>)
    },
    #testcase{
        error = ?ERROR_MALFORMED_DATA,
        deprecated_error = ?DEPRECATED_ERROR_MALFORMED_DATA
    },
    #testcase{
        error = ?ERROR_MISSING_AT_LEAST_ONE_VALUE([<<"name">>, <<"type">>]),
        deprecated_error = ?DEPRECATED_ERROR_MISSING_AT_LEAST_ONE_VALUE([<<"name">>, <<"type">>])
    },
    #testcase{
        error = ?ERROR_MISSING_REQUIRED_VALUE(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_MISSING_REQUIRED_VALUE(<<"spaceId">>)
    },
    #testcase{
        error = ?ERROR_TSC_MISSING_LAYOUT(#{<<"TS1">> => [<<"M1">>, <<"M2">>]}),
        deprecated_error = ?DEPRECATED_ERROR_TSC_MISSING_LAYOUT(#{<<"TS1">> => [<<"M1">>, <<"M2">>]})
    },
    #testcase{
        error = ?ERROR_TSC_TOO_MANY_METRICS(10000),
        deprecated_error = ?DEPRECATED_ERROR_TSC_TOO_MANY_METRICS(10000)
    },

    %%--------------------------------------------------------------------
    %% data_validation/value errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_BAD_VALUE_AMBIGUOUS_ID(<<"viewName">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_AMBIGUOUS_ID(<<"viewName">>)
    },
    % TODO
%%    #testcase{
%%        error = ?ERROR_BAD_VALUE_ATOM(<<"spaceId">>),
%%        error_after_encoding_decoding = ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_ATOM(<<"spaceId">>),
%%        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_BAD_VALUE_BINARY(<<"spaceId">>)
%%    },
%%    #testcase{
%%        error = ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_BINARY(<<"spaceId">>)
%%    },
    #testcase{
        error = ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_CAVEAT(#{<<"foo">> => <<"bar">>}),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_CAVEAT(#{<<"foo">> => <<"bar">>})
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_DOMAIN,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_DOMAIN
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_EMAIL,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_EMAIL
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_EMPTY(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_EMPTY(<<"spaceId">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_FILE_PATH,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_FILE_PATH
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_FLOAT(<<"latitude">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_FLOAT(<<"latitude">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_FULL_NAME,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_FULL_NAME
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_IDENTIFIER(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_IDENTIFIER(<<"id">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"spaceId">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_INTEGER(<<"size">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_INTEGER(<<"size">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"ip">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_IPV4_ADDRESS(<<"ip">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_JSON(<<"<xml></xml>">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_JSON(<<"<xml></xml>">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>]),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>])
    },
    % TODO
%%    #testcase{
%%        error = ?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>),
%%        error_after_encoding_decoding = ?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"privileges">>),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>),
%%        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"privileges">>)
%%    },
%%    #testcase{
%%        error = ?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"urls">>),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"urls">>)
%%    },
    #testcase{
        error = ?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ip_list">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ip_list">>)
    },
    % TODO
%%    #testcase{
%%        error = ?ERROR_BAD_VALUE_NAME(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NAME(undefined)
%%    },
    #testcase{
        error = ?ERROR_BAD_VALUE_NAME(<<"key">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NAME(<<"key">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>]),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>])
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"size">>, 500, 1000),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NOT_IN_RANGE(<<"size">>, 500, 1000)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_OCTAL(<<"mode">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_OCTAL(<<"mode">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_PASSWORD,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_PASSWORD
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_QOS_PARAMETERS,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_QOS_PARAMETERS
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_SUBDOMAIN,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_SUBDOMAIN
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 1000),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 1000)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_BAD_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_BAD_TOKEN)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_TOKEN_INVALID),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_TOKEN_INVALID)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_TOKEN_REVOKED),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_TOKEN_REVOKED)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?ACCESS_TOKEN)),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?ACCESS_TOKEN))
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_scope{scope = identity_token})),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_scope{scope = identity_token}))
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOO_HIGH(<<"size">>, 1000),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOO_HIGH(<<"size">>, 1000)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, 500),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOO_LOW(<<"size">>, 500)
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(
            <<"TS1">>, <<"M1">>,
            #metric_config{resolution = 60, retention = 5, aggregator = max},
            #metric_config{resolution = 3600, retention = 24, aggregator = sum}
        ),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(
            <<"TS1">>, <<"M1">>,
            #metric_config{resolution = 60, retention = 5, aggregator = max},
            #metric_config{resolution = 3600, retention = 24, aggregator = sum}
        )
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_USERNAME,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_USERNAME
    },
    #testcase{
        error = ?ERROR_BAD_VALUE_XML(<<"null">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_XML(<<"null">>)
    },

    %%--------------------------------------------------------------------
    %% general errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_ALREADY_EXISTS,
        deprecated_error = ?DEPRECATED_ERROR_ALREADY_EXISTS
    },
    #testcase{
        error = ?ERROR_BAD_MESSAGE(<<"edaml-wsesjapfs">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_MESSAGE(<<"edaml-wsesjapfs">>)
    },
    #testcase{
        error = ?ERROR_BAD_MESSAGE(#{<<"nested">> => <<"edaml-wsesjapfs">>}),
        deprecated_error = ?DEPRECATED_ERROR_BAD_MESSAGE(#{<<"nested">> => <<"edaml-wsesjapfs">>})
    },
    #testcase{
        error = ?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(<<"Some external service">>),
        deprecated_error = ?DEPRECATED_ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(<<"Some external service">>)
    },
    #testcase{
        error = ?ERROR_FILE_ACCESS(<<"/etc/cert/web_key.pem">>, ?EROFS),
        deprecated_error = ?DEPRECATED_ERROR_FILE_ACCESS(<<"/etc/cert/web_key.pem">>, ?EROFS)
    },
    #testcase{
        error = ?ERROR_FILE_ACCESS(['./', ["name"]], ?EROFS),
        error_after_encoding_decoding = ?ERROR_FILE_ACCESS(<<"./name">>, ?EROFS),
        deprecated_error = ?DEPRECATED_ERROR_FILE_ACCESS(['./', ["name"]], ?EROFS),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_FILE_ACCESS(<<"./name">>, ?EROFS)
    },
    % TODO
%%    #testcase{
%%        error = ?ERROR_INTERNAL_SERVER_ERROR(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_INTERNAL_SERVER_ERROR
%%    },
    begin
        RandRef = ?RAND_STR(),

        #testcase{
            error = ?ERROR_INTERNAL_SERVER_ERROR(RandRef),
            deprecated_error = ?DEPRECATED_ERROR_INTERNAL_SERVER_ERROR(RandRef)
        }
    end,
    #testcase{
        error = ?ERROR_LIMIT_REACHED(1000, <<"number of requests">>),
        deprecated_error = ?DEPRECATED_ERROR_LIMIT_REACHED(1000, <<"number of requests">>)
    },
    #testcase{
        error = ?ERROR_NOT_FOUND,
        deprecated_error = ?DEPRECATED_ERROR_NOT_FOUND
    },
    #testcase{
        error = ?ERROR_NOT_IMPLEMENTED,
        deprecated_error = ?DEPRECATED_ERROR_NOT_IMPLEMENTED
    },
    #testcase{
        error = ?ERROR_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_NOT_SUPPORTED
    },
    #testcase{
        error = ?ERROR_SERVICE_UNAVAILABLE,
        deprecated_error = ?DEPRECATED_ERROR_SERVICE_UNAVAILABLE
    },
    #testcase{
        error = ?ERROR_TEMPORARY_FAILURE,
        deprecated_error = ?DEPRECATED_ERROR_TEMPORARY_FAILURE
    },
    #testcase{
        error = ?ERROR_TIMEOUT,
        deprecated_error = ?DEPRECATED_ERROR_TIMEOUT
    },
    #testcase{
        error = ?ERROR_UNREGISTERED_ONEPROVIDER,
        deprecated_error = ?DEPRECATED_ERROR_UNREGISTERED_ONEPROVIDER
    },

    %%--------------------------------------------------------------------
    %% graph_sync errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_BAD_GRI,
        deprecated_error = ?DEPRECATED_ERROR_BAD_GRI
    },
    #testcase{
        error = ?ERROR_BAD_VERSION([4, 5, 6, 7, 8]),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VERSION([4, 5, 6, 7, 8])
    },
    #testcase{
        error = ?ERROR_EXPECTED_HANDSHAKE_MESSAGE,
        deprecated_error = ?DEPRECATED_ERROR_EXPECTED_HANDSHAKE_MESSAGE
    },
    #testcase{
        error = ?ERROR_HANDSHAKE_ALREADY_DONE,
        deprecated_error = ?DEPRECATED_ERROR_HANDSHAKE_ALREADY_DONE
    },
    #testcase{
        error = ?ERROR_NOT_SUBSCRIBABLE,
        deprecated_error = ?DEPRECATED_ERROR_NOT_SUBSCRIBABLE
    },
    #testcase{
        error = ?ERROR_RPC_UNDEFINED,
        deprecated_error = ?DEPRECATED_ERROR_RPC_UNDEFINED
    },

    %%--------------------------------------------------------------------
    %% onepanel errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_DNS_SERVERS_UNREACHABLE([default, {1, 2, 3, 4}]),
        deprecated_error = ?DEPRECATED_ERROR_DNS_SERVERS_UNREACHABLE([default, {1, 2, 3, 4}])
    },
    #testcase{
        error = ?ERROR_DNS_SERVERS_UNREACHABLE([<<"1.1.1.1">>, <<"8.8.8.8">>]),
        error_after_encoding_decoding = ?ERROR_DNS_SERVERS_UNREACHABLE([{1, 1, 1, 1}, {8, 8, 8, 8}]),
        deprecated_error = ?DEPRECATED_ERROR_DNS_SERVERS_UNREACHABLE([<<"1.1.1.1">>, <<"8.8.8.8">>]),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_DNS_SERVERS_UNREACHABLE([{1, 1, 1, 1}, {8, 8, 8, 8}])
    },
    #testcase{
        error = ?ERROR_FILE_ALLOCATION(1000, 2000),
        deprecated_error = ?DEPRECATED_ERROR_FILE_ALLOCATION(1000, 2000)
    },
    #testcase{
        error = ?ERROR_LETS_ENCRYPT_NOT_REACHABLE,
        deprecated_error = ?DEPRECATED_ERROR_LETS_ENCRYPT_NOT_REACHABLE
    },
    #testcase{
        error = ?ERROR_LETS_ENCRYPT_RESPONSE(undefined, <<"Bad Let's Encrypt response">>),
        deprecated_error = ?DEPRECATED_ERROR_LETS_ENCRYPT_RESPONSE(undefined, <<"Bad Let's Encrypt response">>)
    },
    #testcase{
        error = ?ERROR_LETS_ENCRYPT_RESPONSE(
            #{<<"type">> => <<"urn:ietf:params:acme:error:rateLimited">>,
                <<"status">> => 429, <<"detail">> => <<"Error creating new order">>},
            <<"Error creating new order">>),
        deprecated_error = ?DEPRECATED_ERROR_LETS_ENCRYPT_RESPONSE(
            #{<<"type">> => <<"urn:ietf:params:acme:error:rateLimited">>,
                <<"status">> => 429, <<"detail">> => <<"Error creating new order">>},
            <<"Error creating new order">>)
    },
    #testcase{
        error = ?ERROR_NO_CONNECTION_TO_NEW_NODE(<<"onepanel@example.com">>),
        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_NEW_NODE(<<"onepanel@example.com">>)
    },
    #testcase{
        error = ?ERROR_NO_SERVICE_NODES(op_worker),
        error_after_encoding_decoding = ?ERROR_NO_SERVICE_NODES(<<"op_worker">>),
        deprecated_error = ?DEPRECATED_ERROR_NO_SERVICE_NODES(op_worker),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_NO_SERVICE_NODES(<<"op_worker">>)
    },
    #testcase{
        error = ?ERROR_NODE_ALREADY_IN_CLUSTER(<<"onepanel@example.com">>),
        deprecated_error = ?DEPRECATED_ERROR_NODE_ALREADY_IN_CLUSTER(<<"onepanel@example.com">>)
    },
    #testcase{
        error = ?ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEPROVIDER),
        deprecated_error = ?DEPRECATED_ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEPROVIDER)
    },
    #testcase{
        error = ?ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEZONE),
        deprecated_error = ?DEPRECATED_ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEZONE)
    },
    #testcase{
        error = ?ERROR_ON_NODES(?ERROR_FILE_ACCESS(<<"/path">>, ?EACCES), [<<"node1.example.com">>]),
        deprecated_error = ?DEPRECATED_ERROR_ON_NODES(?DEPRECATED_ERROR_FILE_ACCESS(<<"/path">>, ?EACCES), [<<"node1.example.com">>])
    },
    #testcase{
        error = ?ERROR_USER_NOT_IN_CLUSTER,
        deprecated_error = ?DEPRECATED_ERROR_USER_NOT_IN_CLUSTER
    },

    %%--------------------------------------------------------------------
    %% op_worker errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_AUTO_CLEANING_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_AUTO_CLEANING_DISABLED
    },
    #testcase{
        error = ?ERROR_FILE_POPULARITY_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_FILE_POPULARITY_DISABLED
    },
    #testcase{
        error = ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(ongoing, [preserved, cancelled]),
        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(ongoing, [preserved, cancelled])
    },
    #testcase{
        error = ?ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(<<"archiveId">>),
        deprecated_error = ?DEPRECATED_ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(<<"archiveId">>)
    },
    #testcase{
        error = ?ERROR_QUOTA_EXCEEDED,
        deprecated_error = ?DEPRECATED_ERROR_QUOTA_EXCEEDED
    },
    #testcase{
        error = ?ERROR_RECALL_TARGET_CONFLICT,
        deprecated_error = ?DEPRECATED_ERROR_RECALL_TARGET_CONFLICT
    },
    #testcase{
        error = ?ERROR_SPACE_NOT_SUPPORTED_BY(<<"spaceId">>, <<"providerId">>),
        deprecated_error = ?DEPRECATED_ERROR_SPACE_NOT_SUPPORTED_BY(<<"spaceId">>, <<"providerId">>)
    },
    #testcase{
        error = ?ERROR_STAT_OPERATION_NOT_SUPPORTED(<<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_STAT_OPERATION_NOT_SUPPORTED(<<"storageId">>)
    },
    #testcase{
        error = ?ERROR_USER_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_USER_NOT_SUPPORTED
    },

    %%--------------------------------------------------------------------
    %% op_worker/atm errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type),
        deprecated_error = ?DEPRECATED_ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type)
    },
    #testcase{
        error = ?ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(#{<<"fileId">> => <<"REG">>}, atm_file_type, #{<<"hasAccess">> => true}),
        deprecated_error = ?DEPRECATED_ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(#{<<"fileId">> => <<"REG">>}, atm_file_type, #{<<"hasAccess">> => true})
    },
    #testcase{
        error = ?ERROR_ATM_INVALID_STATUS_TRANSITION(active, scheduled),
        deprecated_error = ?DEPRECATED_ERROR_ATM_INVALID_STATUS_TRANSITION(active, scheduled)
    },
    #testcase{
        error = ?ERROR_ATM_JOB_BATCH_CRASHED(<<"sad">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_JOB_BATCH_CRASHED(<<"sad">>)
    },
    #testcase{
        error = ?ERROR_ATM_JOB_BATCH_WITHDRAWN(<<"happy">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_JOB_BATCH_WITHDRAWN(<<"happy">>)
    },
    #testcase{
        error = ?ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(<<"repeats">>, ?ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type)),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(<<"repeats">>, ?DEPRECATED_ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type))
    },
    #testcase{
        error = ?ERROR_ATM_LANE_EMPTY(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EMPTY(<<"id">>)
    },
    #testcase{
        error = ?ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(<<"id">>, ?ERROR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED)
    },
    #testcase{
        error = ?ERROR_ATM_LANE_EXECUTION_RERUN_FAILED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_RERUN_FAILED
    },
    #testcase{
        error = ?ERROR_ATM_LANE_EXECUTION_RETRY_FAILED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_RETRY_FAILED
    },
    #testcase{
        error = ?ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED
    },
    #testcase{
        error = ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED
    },
    % TODO
%%    #testcase{
%%        error = ?ERROR_ATM_OPENFAAS_QUERY_FAILED(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_QUERY_FAILED(undefined)
%%    },
    #testcase{
        error = ?ERROR_ATM_OPENFAAS_QUERY_FAILED(<<"dns resolution error...">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_QUERY_FAILED(<<"dns resolution error...">>)
    },
    #testcase{
        error = ?ERROR_ATM_OPENFAAS_UNHEALTHY,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_UNHEALTHY
    },
    #testcase{
        error = ?ERROR_ATM_OPENFAAS_UNREACHABLE,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_UNREACHABLE
    },
    #testcase{
        error = ?ERROR_ATM_PARALLEL_BOX_EMPTY(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_PARALLEL_BOX_EMPTY(<<"id">>)
    },
    #testcase{
        error = ?ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(<<"id">>, ?ERROR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED)
    },
    #testcase{
        error = ?ERROR_ATM_STORE_CONTENT_NOT_SET(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_CONTENT_NOT_SET(<<"id">>)
    },
    #testcase{
        error = ?ERROR_ATM_STORE_CREATION_FAILED(<<"id">>, ?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT)
    },
    #testcase{
        error = ?ERROR_ATM_STORE_FROZEN(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_FROZEN(<<"id">>)
    },
    #testcase{
        error = ?ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT,
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT
    },
    #testcase{
        error = ?ERROR_ATM_STORE_NOT_FOUND(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_NOT_FOUND(<<"id">>)
    },
    #testcase{
        error = ?ERROR_ATM_STORE_TYPE_DISALLOWED(<<"id">>, [single_value]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_TYPE_DISALLOWED(<<"id">>, [single_value])
    },
    #testcase{
        error = ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(<<"arg">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(<<"arg">>)
    },
    #testcase{
        error = ?ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(<<"arg">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(<<"arg">>)
    },
    #testcase{
        error = ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED([1, 2], [0]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED([1, 2], [0])
    },
    #testcase{
        error = ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(store_credentials, [iterated_item]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(store_credentials, [iterated_item])
    },
    #testcase{
        error = ?ERROR_ATM_TASK_ARG_MAPPING_FAILED(<<"arg">>, ?ERROR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPING_FAILED(<<"arg">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(<<"id">>, ?ERROR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERROR_ATM_OPENFAAS_NOT_CONFIGURED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED)
    },
    #testcase{
        error = ?ERROR_ATM_TASK_EXECUTION_STOPPED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_EXECUTION_STOPPED
    },
    #testcase{
        error = ?ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(<<"id">>, ?ERROR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERROR_ATM_TASK_RESULT_MAPPING_FAILED(<<"result">>, ?ERROR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_RESULT_MAPPING_FAILED(<<"result">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERROR_ATM_TASK_RESULT_MISSING(<<"result">>, [<<"key1">>, <<"key2">>]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_RESULT_MISSING(<<"result">>, [<<"key1">>, <<"key2">>])
    },
    #testcase{
        error = ?ERROR_ATM_UNSUPPORTED_DATA_TYPE(atm_string_type, [atm_number_type]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_UNSUPPORTED_DATA_TYPE(atm_string_type, [atm_number_type])
    },
    #testcase{
        error = ?ERROR_ATM_WORKFLOW_EMPTY,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EMPTY
    },
    #testcase{
        error = ?ERROR_ATM_WORKFLOW_EXECUTION_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_ENDED
    },
    #testcase{
        error = ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED
    },
    #testcase{
        error = ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE
    },
    #testcase{
        error = ?ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED
    },
    #testcase{
        error = ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_STOPPED
    },
    #testcase{
        error = ?ERROR_ATM_WORKFLOW_EXECUTION_STOPPING,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_STOPPING
    },

    %%--------------------------------------------------------------------
    %% op_worker/dir_stats errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_DIR_STATS_DISABLED_FOR_SPACE,
        deprecated_error = ?DEPRECATED_ERROR_DIR_STATS_DISABLED_FOR_SPACE
    },
    #testcase{
        error = ?ERROR_DIR_STATS_NOT_READY,
        deprecated_error = ?DEPRECATED_ERROR_DIR_STATS_NOT_READY
    },

    %%--------------------------------------------------------------------
    %% op_worker/storage errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>, <<"s3">>], [<<"s3">>]),
        deprecated_error = ?DEPRECATED_ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>, <<"s3">>], [<<"s3">>])
    },
    #testcase{
        error = ?ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(<<"providerId">>, <<"storageId">>, <<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(<<"providerId">>, <<"storageId">>, <<"spaceId">>)
    },
    #testcase{
        error = ?ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE,
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE
    },
    #testcase{
        error = ?ERROR_REQUIRES_IMPORTED_STORAGE(<<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_IMPORTED_STORAGE(<<"storageId">>)
    },
    #testcase{
        error = ?ERROR_REQUIRES_NON_IMPORTED_STORAGE(<<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_NON_IMPORTED_STORAGE(<<"storageId">>)
    },
    #testcase{
        error = ?ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>]),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>])
    },
    #testcase{
        error = ?ERROR_REQUIRES_READONLY_STORAGE(<<"storageType">>),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_READONLY_STORAGE(<<"storageType">>)
    },
    #testcase{
        error = ?ERROR_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"swift">>, <<"s3">>, <<"cephrados">>]),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"swift">>, <<"s3">>, <<"cephrados">>])
    },
    #testcase{
        error = ?ERROR_STORAGE_IN_USE,
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_IN_USE
    },
    #testcase{
        error = ?ERROR_STORAGE_TEST_FAILED(read),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_TEST_FAILED(read)
    },
    #testcase{
        error = ?ERROR_STORAGE_TEST_FAILED(write),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_TEST_FAILED(write)
    },
    #testcase{
        error = ?ERROR_STORAGE_TEST_FAILED(remove),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_TEST_FAILED(remove)
    },

    %%--------------------------------------------------------------------
    %% op_worker/transfer errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_TRANSFER_ALREADY_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_TRANSFER_ALREADY_ENDED
    },
    #testcase{
        error = ?ERROR_TRANSFER_NOT_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_TRANSFER_NOT_ENDED
    },

    %%--------------------------------------------------------------------
    %% op_worker/view errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_VIEW_NOT_EXISTS_ON(<<"providerId">>),
        deprecated_error = ?DEPRECATED_ERROR_VIEW_NOT_EXISTS_ON(<<"providerId">>)
    },
    #testcase{
        error = ?ERROR_VIEW_QUERY_FAILED(<<"category">>, <<"description">>),
        deprecated_error = ?DEPRECATED_ERROR_VIEW_QUERY_FAILED(<<"category">>, <<"description">>)
    },

    %%--------------------------------------------------------------------
    %% oz_worker errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_ATM_LAMBDA_IN_USE([<<"a">>, <<"b">>, <<"c">>, <<"d">>]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LAMBDA_IN_USE([<<"a">>, <<"b">>, <<"c">>, <<"d">>])
    },
    #testcase{
        error = ?ERROR_BASIC_AUTH_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_BASIC_AUTH_DISABLED
    },
    #testcase{
        error = ?ERROR_BASIC_AUTH_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_BASIC_AUTH_NOT_SUPPORTED
    },
    #testcase{
        error = ?ERROR_CANNOT_ADD_RELATION_TO_SELF,
        deprecated_error = ?DEPRECATED_ERROR_CANNOT_ADD_RELATION_TO_SELF
    },
    #testcase{
        error = ?ERROR_CANNOT_DELETE_ENTITY(od_user, <<"user1">>),
        deprecated_error = ?DEPRECATED_ERROR_CANNOT_DELETE_ENTITY(od_user, <<"user1">>)
    },
    #testcase{
        error = ?ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE,
        deprecated_error = ?DEPRECATED_ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE
    },
    #testcase{
        error = ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, <<"space1">>),
        deprecated_error = ?DEPRECATED_ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, <<"space1">>)
    },
    #testcase{
        error = ?ERROR_PROTECTED_GROUP,
        deprecated_error = ?DEPRECATED_ERROR_PROTECTED_GROUP
    },
    #testcase{
        error = ?ERROR_RELATION_ALREADY_EXISTS(od_user, <<"user1">>, od_space, <<"space1">>),
        deprecated_error = ?DEPRECATED_ERROR_RELATION_ALREADY_EXISTS(od_user, <<"user1">>, od_space, <<"space1">>)
    },
    #testcase{
        error = ?ERROR_RELATION_DOES_NOT_EXIST(od_user, <<"user1">>, od_space, <<"space1">>),
        deprecated_error = ?DEPRECATED_ERROR_RELATION_DOES_NOT_EXIST(od_user, <<"user1">>, od_space, <<"space1">>)
    },

    %%--------------------------------------------------------------------
    %% oz_worker/space errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(<<"spaceId">>, <<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(<<"spaceId">>, <<"storageId">>)
    },
    #testcase{
        error = ?ERROR_SPACE_MARKETPLACE_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_SPACE_MARKETPLACE_DISABLED
    },

    %%--------------------------------------------------------------------
    %% oz_worker/subdomain errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_SUBDOMAIN_DELEGATION_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_SUBDOMAIN_DELEGATION_DISABLED
    },
    #testcase{
        error = ?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED
    },

    %%--------------------------------------------------------------------
    %% posix errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERROR_POSIX(eacess),
        deprecated_error = ?DEPRECATED_ERROR_POSIX(eacess)
    }
].


-endif.

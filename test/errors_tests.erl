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


assert_all_errors_are_tested_test() ->
    AllModules = lists:flatmap(fun(Path) ->
        case filelib:wildcard(Path ++ "/*.beam") of
            [] -> [];
            Files -> [filename:basename(File, ".beam") || File <- Files]
        end
    end, code:get_path()),

    ErrorModules = lists:usort(lists:filtermap(fun
        (Module = "od_error_" ++ _) -> {true, list_to_atom(Module)};
        (_) -> false
    end, AllModules)),

    AllTestedErrorTypes = lists:usort(lists:map(fun(#testcase{error = ?ERR(Type)}) ->
        Type
    end, testcases())),

    case ErrorModules == AllTestedErrorTypes of
        true ->
            ok;
        false ->
            ?assertEqual(ok, {not_tested, ErrorModules -- AllTestedErrorTypes})
    end.


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
                        ?assertMatch(?ERR, FromJson),
                        ?assertEqual(ExpError, FromJson)
                    end}
                ]
        end,

        [
            {str_utils:format_bin("encode-decode: ~tp", [Error]), fun() ->
                Json = errors:to_json(Error),
                assert_valid_error_json(Json),
                FromJson = errors:from_json(JsonEncodeDecodeFun(Json)),
                ?assertMatch(?ERR, FromJson),
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
    Description = maps:get(<<"description">>, Json),
    ?assert(str_utils:binary_ends_with(Description, <<".">>)),
    ?assertEqual(size(Description), 1 + size(string:trim(Description, trailing, "."))).


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
        ?ERR_INTERNAL_SERVER_ERROR(_),
        errors:from_json(errors:to_json(BadErrorTerm))
    ).


unrecognized_error_test() ->
    UnrecognizedErrorJson = #{
        <<"id">> => <<"someErrorThatWasNotSpecifiedInThisSoftwareVersion">>,
        <<"details">> => #{<<"key">> => <<"value">>},
        <<"description">> => <<"Human readable error description.">>
    },
    ?assertEqual(
        ?ERR_UNRECOGNIZED_ERROR(UnrecognizedErrorJson),
        errors:from_json(UnrecognizedErrorJson)
    ),
    ?assertEqual(
        UnrecognizedErrorJson#{<<"description">> => <<"No description (unknown error).">>},
        errors:to_json(errors:from_json(maps:without([<<"description">>], UnrecognizedErrorJson)))
    ).


testcases() -> [
    %%--------------------------------------------------------------------
    %% Unknown / unexpected error
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_UNRECOGNIZED_ERROR(#{
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
        error = ?ERR_BAD_BASIC_CREDENTIALS,
        deprecated_error = ?DEPRECATED_ERROR_BAD_BASIC_CREDENTIALS
    },
%%    % TODO
%%    #testcase{
%%        error = ?ERR_FORBIDDEN(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN(null)
%%    },
    #testcase{
        error = ?ERR_FORBIDDEN(<<"Sausage not for the dog">>),
        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN(<<"Sausage not for the dog">>)
    },
    #testcase{
        error = ?ERR_FORBIDDEN(<<"Honey not for the piglets.">>),
        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN(<<"Honey not for the piglets.">>)
    },
    #testcase{
        error = ?ERR_UNAUTHORIZED(?ERR_NOT_AN_ACCESS_TOKEN(?IDENTITY_TOKEN)),
        deprecated_error = ?DEPRECATED_ERROR_UNAUTHORIZED(?DEPRECATED_ERROR_NOT_AN_ACCESS_TOKEN(?IDENTITY_TOKEN))
    },
    #testcase{
        error = ?ERR_UNAUTHORIZED(undefined),
        deprecated_error = ?DEPRECATED_ERROR_UNAUTHORIZED(undefined)
    },
    #testcase{
        error = ?ERR_USER_BLOCKED,
        deprecated_error = ?DEPRECATED_ERROR_USER_BLOCKED
    },

    %%--------------------------------------------------------------------
    %% auth/token errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_BAD_CONSUMER_TOKEN(?ERR_TOKEN_INVALID),
        deprecated_error = ?DEPRECATED_ERROR_BAD_CONSUMER_TOKEN(?DEPRECATED_ERROR_TOKEN_INVALID)
    },
    #testcase{
        error = ?ERR_BAD_CONSUMER_TOKEN(?ERR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{1, 2, 3, 4}, 32}]})),
        deprecated_error = ?DEPRECATED_ERROR_BAD_CONSUMER_TOKEN(?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{
            whitelist = [{{1, 2, 3, 4}, 32}]
        }))
    },
    #testcase{
        error = ?ERR_BAD_IDP_ACCESS_TOKEN(keycloak),
        error_after_encoding_decoding = ?ERR_BAD_IDP_ACCESS_TOKEN(<<"keycloak">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_IDP_ACCESS_TOKEN(keycloak),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_BAD_IDP_ACCESS_TOKEN(<<"keycloak">>)
    },
    #testcase{
        error = ?ERR_BAD_SERVICE_TOKEN(?ERR_BAD_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_BAD_SERVICE_TOKEN(?DEPRECATED_ERROR_BAD_TOKEN)
    },
    #testcase{
        error = ?ERR_BAD_SERVICE_TOKEN(?ERR_TOKEN_REVOKED),
        deprecated_error = ?DEPRECATED_ERROR_BAD_SERVICE_TOKEN(?DEPRECATED_ERROR_TOKEN_REVOKED)
    },
    #testcase{
        error = ?ERR_BAD_SERVICE_TOKEN(?ERR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12345678})),
        deprecated_error = ?DEPRECATED_ERROR_BAD_SERVICE_TOKEN(?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12345678}))
    },
    #testcase{
        error = ?ERR_BAD_TOKEN,
        deprecated_error = ?DEPRECATED_ERROR_BAD_TOKEN
    },
    #testcase{
        error = ?ERR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(?ONEPROVIDER, <<"zxbcv78s0dfasdf">>)),
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(?ONEPROVIDER, <<"zxbcv78s0dfasdf">>))
    },
    #testcase{
        error = ?ERR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(nobody)),
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_CONSUMER_INVALID(?SUB(nobody))
    },
    #testcase{
        error = ?ERR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED,
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
    },
    #testcase{
        error = ?ERR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123456">>),
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123456">>)
    },
    #testcase{
        error = ?ERR_INVITE_TOKEN_USAGE_LIMIT_REACHED,
        deprecated_error = ?DEPRECATED_ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED
    },
    #testcase{
        error = ?ERR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"123">>)),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"123">>))
    },
    #testcase{
        error = ?ERR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN)
    },
    #testcase{
        error = ?ERR_NOT_AN_INVITE_TOKEN(?USER_JOIN_SPACE, ?ACCESS_TOKEN(<<"sess-8765">>)),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(?USER_JOIN_SPACE, ?ACCESS_TOKEN(<<"sess-8765">>))
    },
    #testcase{
        error = ?ERR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"12345">>)),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"12345">>))
    },
    #testcase{
        error = ?ERR_NOT_AN_INVITE_TOKEN(any, ?ACCESS_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(any, ?ACCESS_TOKEN)
    },
    #testcase{
        error = ?ERR_TOKEN_CAVEAT_UNKNOWN(<<"grant = everything">>),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_CAVEAT_UNKNOWN(<<"grant = everything">>)
    },
    #testcase{
        error = ?ERR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12323746234}),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12323746234})
    },
    #testcase{
        error = ?ERR_TOKEN_INVALID,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_INVALID
    },
    #testcase{
        error = ?ERR_TOKEN_REVOKED,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_REVOKED
    },
    #testcase{
        error = ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, <<"kjasif2387rg7adc09jf8a0sdfg97a">>)),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, <<"kjasif2387rg7adc09jf8a0sdfg97a">>))
    },
    #testcase{
        error = ?ERR_TOKEN_SESSION_INVALID,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_SESSION_INVALID
    },
    #testcase{
        error = ?ERR_TOKEN_SUBJECT_INVALID,
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_SUBJECT_INVALID
    },
    #testcase{
        error = ?ERR_TOKEN_TIME_CAVEAT_REQUIRED(86400),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_TIME_CAVEAT_REQUIRED(86400)
    },
    #testcase{
        error = ?ERR_TOKEN_TOO_LARGE(86400),
        deprecated_error = ?DEPRECATED_ERROR_TOKEN_TOO_LARGE(86400)
    },

    %%--------------------------------------------------------------------
    %% connection errors
    %%--------------------------------------------------------------------
% TODO
%%    #testcase{
%%        error = ?ERR_NO_CONNECTION_TO_CLUSTER_NODE,
%%        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_CLUSTER_NODE
%%    },
%%    #testcase{
%%        error = ?ERR_NO_CONNECTION_TO_ONEZONE,
%%        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_ONEZONE
%%    },
%%    #testcase{
%%        error = ?ERR_NO_CONNECTION_TO_PEER_ONEPROVIDER,
%%        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER
%%    },
%%
%%    %%--------------------------------------------------------------------
%%    %% data_validation errors
%%    %%--------------------------------------------------------------------
%%    % TODO
%%    #testcase{
%%        error = ?ERR_BAD_DATA(<<"spaceId">>, undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_DATA(<<"spaceId">>, undefined)
%%    },
    #testcase{
        error = ?ERR_BAD_DATA(<<"nestedRecord">>, ?ERR_MISSING_REQUIRED_VALUE(<<"key">>)),
        deprecated_error = ?DEPRECATED_ERROR_BAD_DATA(<<"nestedRecord">>, ?DEPRECATED_ERROR_MISSING_REQUIRED_VALUE(<<"key">>))
    },
    #testcase{
        error = ?ERR_BAD_DATA(<<"spaceId">>, <<"Not so readable hint">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_DATA(<<"spaceId">>, <<"Not so readable hint">>)
    },
    #testcase{
        error = ?ERR_BAD_GUI_PACKAGE,
        deprecated_error = ?DEPRECATED_ERROR_BAD_GUI_PACKAGE
    },
    #testcase{
        error = ?ERR_GUI_PACKAGE_TOO_LARGE,
        deprecated_error = ?DEPRECATED_ERROR_GUI_PACKAGE_TOO_LARGE
    },
    #testcase{
        error = ?ERR_GUI_PACKAGE_UNVERIFIED(<<"5f38fb2e288be67bacc9c206e40f28ee42f9bba9c521f5d6036a4217abd146ba">>),
        deprecated_error = ?DEPRECATED_ERROR_GUI_PACKAGE_UNVERIFIED(<<"5f38fb2e288be67bacc9c206e40f28ee42f9bba9c521f5d6036a4217abd146ba">>)
    },
    #testcase{
        error = ?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(none, none),
        deprecated_error = ?DEPRECATED_ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(none, none)
    },
    #testcase{
        error = ?ERR_ILLEGAL_SUPPORT_STAGE_TRANSITION(evicting_replicas, {resizing, 0}),
        deprecated_error = ?DEPRECATED_ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(evicting_replicas, {resizing, 0})
    },
    #testcase{
        error = ?ERR_INVALID_QOS_EXPRESSION(<<"invalid \";\"">>),
        deprecated_error = ?DEPRECATED_ERROR_INVALID_QOS_EXPRESSION(<<"invalid \";\"">>)
    },
    #testcase{
        error = ?ERR_MALFORMED_DATA,
        deprecated_error = ?DEPRECATED_ERROR_MALFORMED_DATA
    },
    #testcase{
        error = ?ERR_MISSING_AT_LEAST_ONE_VALUE([<<"name">>, <<"type">>]),
        deprecated_error = ?DEPRECATED_ERROR_MISSING_AT_LEAST_ONE_VALUE([<<"name">>, <<"type">>])
    },
    #testcase{
        error = ?ERR_MISSING_REQUIRED_VALUE(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_MISSING_REQUIRED_VALUE(<<"spaceId">>)
    },
    #testcase{
        error = ?ERR_TSC_MISSING_LAYOUT(#{<<"TS1">> => [<<"M1">>, <<"M2">>]}),
        deprecated_error = ?DEPRECATED_ERROR_TSC_MISSING_LAYOUT(#{<<"TS1">> => [<<"M1">>, <<"M2">>]})
    },
    #testcase{
        error = ?ERR_TSC_TOO_MANY_METRICS(10000),
        deprecated_error = ?DEPRECATED_ERROR_TSC_TOO_MANY_METRICS(10000)
    },

    %%--------------------------------------------------------------------
    %% data_validation/value errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_BAD_VALUE_AMBIGUOUS_ID(<<"viewName">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_AMBIGUOUS_ID(<<"viewName">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_STRING(<<"spaceId">>),
        error_after_encoding_decoding = ?ERR_BAD_VALUE_STRING(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_ATOM(<<"spaceId">>),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_BAD_VALUE_BINARY(<<"spaceId">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_STRING(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_BINARY(<<"spaceId">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_CAVEAT(#{<<"foo">> => <<"bar">>}),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_CAVEAT(#{<<"foo">> => <<"bar">>})
    },
    #testcase{
        error = ?ERR_BAD_VALUE_DOMAIN,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_DOMAIN
    },
    #testcase{
        error = ?ERR_BAD_VALUE_EMAIL,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_EMAIL
    },
    #testcase{
        error = ?ERR_BAD_VALUE_EMPTY(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_EMPTY(<<"spaceId">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_FILE_PATH,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_FILE_PATH
    },
    #testcase{
        error = ?ERR_BAD_VALUE_FLOAT(<<"latitude">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_FLOAT(<<"latitude">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_FULL_NAME,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_FULL_NAME
    },
    #testcase{
        error = ?ERR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_ID_NOT_FOUND(<<"spaceId">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_IDENTIFIER(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_IDENTIFIER(<<"id">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"spaceId">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_INTEGER(<<"size">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_INTEGER(<<"size">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_IPV4_ADDRESS(<<"ip">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_IPV4_ADDRESS(<<"ip">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_JSON(<<"<xml></xml>">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_JSON(<<"<xml></xml>">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_LIST_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>]),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>])
    },
    #testcase{
        error = ?ERR_BAD_VALUE_LIST_OF_STRINGS(<<"privileges">>),
        error_after_encoding_decoding = ?ERR_BAD_VALUE_LIST_OF_STRINGS(<<"privileges">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"privileges">>),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"privileges">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_LIST_OF_STRINGS(<<"urls">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"urls">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ip_list">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"ip_list">>)
    },
%%    % TODO
%%    #testcase{
%%        error = ?ERR_BAD_VALUE_NAME(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NAME(undefined)
%%    },
    #testcase{
        error = ?ERR_BAD_VALUE_NAME(<<"key">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NAME(<<"key">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>]),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, [<<"a">>, <<"b">>])
    },
    #testcase{
        error = ?ERR_BAD_VALUE_NOT_IN_RANGE(<<"size">>, 500, 1000),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_NOT_IN_RANGE(<<"size">>, 500, 1000)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_OCTAL(<<"mode">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_OCTAL(<<"mode">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_PASSWORD,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_PASSWORD
    },
    #testcase{
        error = ?ERR_BAD_VALUE_QOS_PARAMETERS,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_QOS_PARAMETERS
    },
    #testcase{
        error = ?ERR_BAD_VALUE_SUBDOMAIN,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_SUBDOMAIN
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 1000),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TEXT_TOO_LARGE(<<"description">>, 1000)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERR_BAD_TOKEN),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_BAD_TOKEN)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERR_TOKEN_INVALID),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_TOKEN_INVALID)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERR_TOKEN_REVOKED),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_TOKEN_REVOKED)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?ACCESS_TOKEN)),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_GROUP, ?ACCESS_TOKEN))
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERR_TOKEN_CAVEAT_UNVERIFIED(#cv_scope{scope = identity_token})),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?DEPRECATED_ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_scope{scope = identity_token}))
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOKEN_TYPE(<<"type">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOO_HIGH(<<"size">>, 1000),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOO_HIGH(<<"size">>, 1000)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TOO_LOW(<<"size">>, 500),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_TOO_LOW(<<"size">>, 500)
    },
    #testcase{
        error = ?ERR_BAD_VALUE_TSC_CONFLICTING_METRIC_CONFIG(
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
        error = ?ERR_BAD_VALUE_USERNAME,
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_USERNAME
    },
    #testcase{
        error = ?ERR_BAD_VALUE_XML(<<"null">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VALUE_XML(<<"null">>)
    },

    %%--------------------------------------------------------------------
    %% general errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_ALREADY_EXISTS,
        deprecated_error = ?DEPRECATED_ERROR_ALREADY_EXISTS
    },
    #testcase{
        error = ?ERR_BAD_MESSAGE(<<"edaml-wsesjapfs">>),
        deprecated_error = ?DEPRECATED_ERROR_BAD_MESSAGE(<<"edaml-wsesjapfs">>)
    },
    #testcase{
        error = ?ERR_BAD_MESSAGE(#{<<"nested">> => <<"edaml-wsesjapfs">>}),
        deprecated_error = ?DEPRECATED_ERROR_BAD_MESSAGE(#{<<"nested">> => <<"edaml-wsesjapfs">>})
    },
    #testcase{
        error = ?ERR_EXTERNAL_SERVICE_OPERATION_FAILED(<<"Some external service">>),
        deprecated_error = ?DEPRECATED_ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(<<"Some external service">>)
    },
    #testcase{
        error = ?ERR_FILE_ACCESS(<<"/etc/cert/web_key.pem">>, ?EROFS),
        deprecated_error = ?DEPRECATED_ERROR_FILE_ACCESS(<<"/etc/cert/web_key.pem">>, ?EROFS)
    },
    #testcase{
        error = ?ERR_FILE_ACCESS(['./', ["name"]], ?EROFS),
        error_after_encoding_decoding = ?ERR_FILE_ACCESS(<<"./name">>, ?EROFS),
        deprecated_error = ?DEPRECATED_ERROR_FILE_ACCESS(['./', ["name"]], ?EROFS),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_FILE_ACCESS(<<"./name">>, ?EROFS)
    },
%%    % TODO
%%    #testcase{
%%        error = ?ERR_INTERNAL_SERVER_ERROR(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_INTERNAL_SERVER_ERROR
%%    },
    begin
        RandRef = ?RAND_STR(),

        #testcase{
            error = ?ERR_INTERNAL_SERVER_ERROR(RandRef),
            deprecated_error = ?DEPRECATED_ERROR_INTERNAL_SERVER_ERROR(RandRef)
        }
    end,
    #testcase{
        error = ?ERR_LIMIT_REACHED(1000, <<"number of requests">>),
        deprecated_error = ?DEPRECATED_ERROR_LIMIT_REACHED(1000, <<"number of requests">>)
    },
    #testcase{
        error = ?ERR_NOT_FOUND,
        deprecated_error = ?DEPRECATED_ERROR_NOT_FOUND
    },
    #testcase{
        error = ?ERR_NOT_IMPLEMENTED,
        deprecated_error = ?DEPRECATED_ERROR_NOT_IMPLEMENTED
    },
    #testcase{
        error = ?ERR_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_NOT_SUPPORTED
    },
    #testcase{
        error = ?ERR_SERVICE_UNAVAILABLE,
        deprecated_error = ?DEPRECATED_ERROR_SERVICE_UNAVAILABLE
    },
    #testcase{
        error = ?ERR_TEMPORARY_FAILURE,
        deprecated_error = ?DEPRECATED_ERROR_TEMPORARY_FAILURE
    },
    #testcase{
        error = ?ERR_TIMEOUT,
        deprecated_error = ?DEPRECATED_ERROR_TIMEOUT
    },
    #testcase{
        error = ?ERR_UNREGISTERED_ONEPROVIDER,
        deprecated_error = ?DEPRECATED_ERROR_UNREGISTERED_ONEPROVIDER
    },

    %%--------------------------------------------------------------------
    %% graph_sync errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_BAD_GRI,
        deprecated_error = ?DEPRECATED_ERROR_BAD_GRI
    },
    #testcase{
        error = ?ERR_BAD_VERSION([4, 5, 6, 7, 8]),
        deprecated_error = ?DEPRECATED_ERROR_BAD_VERSION([4, 5, 6, 7, 8])
    },
    #testcase{
        error = ?ERR_EXPECTED_HANDSHAKE_MESSAGE,
        deprecated_error = ?DEPRECATED_ERROR_EXPECTED_HANDSHAKE_MESSAGE
    },
    #testcase{
        error = ?ERR_HANDSHAKE_ALREADY_DONE,
        deprecated_error = ?DEPRECATED_ERROR_HANDSHAKE_ALREADY_DONE
    },
    #testcase{
        error = ?ERR_NOT_SUBSCRIBABLE,
        deprecated_error = ?DEPRECATED_ERROR_NOT_SUBSCRIBABLE
    },
    #testcase{
        error = ?ERR_RPC_UNDEFINED,
        deprecated_error = ?DEPRECATED_ERROR_RPC_UNDEFINED
    },

    %%--------------------------------------------------------------------
    %% onepanel errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_DNS_SERVERS_UNREACHABLE([default, {1, 2, 3, 4}]),
        deprecated_error = ?DEPRECATED_ERROR_DNS_SERVERS_UNREACHABLE([default, {1, 2, 3, 4}])
    },
    #testcase{
        error = ?ERR_DNS_SERVERS_UNREACHABLE([<<"1.1.1.1">>, <<"8.8.8.8">>]),
        error_after_encoding_decoding = ?ERR_DNS_SERVERS_UNREACHABLE([{1, 1, 1, 1}, {8, 8, 8, 8}]),
        deprecated_error = ?DEPRECATED_ERROR_DNS_SERVERS_UNREACHABLE([<<"1.1.1.1">>, <<"8.8.8.8">>]),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_DNS_SERVERS_UNREACHABLE([{1, 1, 1, 1}, {8, 8, 8, 8}])
    },
    #testcase{
        error = ?ERR_LETS_ENCRYPT_NOT_REACHABLE,
        deprecated_error = ?DEPRECATED_ERROR_LETS_ENCRYPT_NOT_REACHABLE
    },
    #testcase{
        error = ?ERR_LETS_ENCRYPT_RESPONSE(undefined, <<"Bad Let's Encrypt response">>),
        deprecated_error = ?DEPRECATED_ERROR_LETS_ENCRYPT_RESPONSE(undefined, <<"Bad Let's Encrypt response">>)
    },
    #testcase{
        error = ?ERR_LETS_ENCRYPT_RESPONSE(
            #{<<"type">> => <<"urn:ietf:params:acme:error:rateLimited">>,
                <<"status">> => 429, <<"detail">> => <<"Error creating new order">>},
            <<"Error creating new order">>),
        deprecated_error = ?DEPRECATED_ERROR_LETS_ENCRYPT_RESPONSE(
            #{<<"type">> => <<"urn:ietf:params:acme:error:rateLimited">>,
                <<"status">> => 429, <<"detail">> => <<"Error creating new order">>},
            <<"Error creating new order">>)
    },
    #testcase{
        error = ?ERR_NO_CONNECTION_TO_NEW_NODE(<<"onepanel@example.com">>),
        deprecated_error = ?DEPRECATED_ERROR_NO_CONNECTION_TO_NEW_NODE(<<"onepanel@example.com">>)
    },
    #testcase{
        error = ?ERR_NO_SERVICE_NODES(op_worker),
        error_after_encoding_decoding = ?ERR_NO_SERVICE_NODES(<<"op_worker">>),
        deprecated_error = ?DEPRECATED_ERROR_NO_SERVICE_NODES(op_worker),
        deprecated_error_after_encoding_decoding = ?DEPRECATED_ERROR_NO_SERVICE_NODES(<<"op_worker">>)
    },
    #testcase{
        error = ?ERR_NODE_ALREADY_IN_CLUSTER(<<"onepanel@example.com">>),
        deprecated_error = ?DEPRECATED_ERROR_NODE_ALREADY_IN_CLUSTER(<<"onepanel@example.com">>)
    },
    #testcase{
        error = ?ERR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEPROVIDER),
        deprecated_error = ?DEPRECATED_ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEPROVIDER)
    },
    #testcase{
        error = ?ERR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEZONE),
        deprecated_error = ?DEPRECATED_ERROR_NODE_NOT_COMPATIBLE(<<"onepanel@example.com">>, ?ONEZONE)
    },
    #testcase{
        error = ?ERR_ON_NODES(?ERR_FILE_ACCESS(<<"/path">>, ?EACCES), [<<"node1.example.com">>]),
        deprecated_error = ?DEPRECATED_ERROR_ON_NODES(?DEPRECATED_ERROR_FILE_ACCESS(<<"/path">>, ?EACCES), [<<"node1.example.com">>])
    },
    #testcase{
        error = ?ERR_USER_NOT_IN_CLUSTER,
        deprecated_error = ?DEPRECATED_ERROR_USER_NOT_IN_CLUSTER
    },

    %%--------------------------------------------------------------------
    %% op_worker errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_AUTO_CLEANING_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_AUTO_CLEANING_DISABLED
    },
    #testcase{
        error = ?ERR_FILE_POPULARITY_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_FILE_POPULARITY_DISABLED
    },
    #testcase{
        error = ?ERR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(ongoing, [preserved, cancelled]),
        deprecated_error = ?DEPRECATED_ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(ongoing, [preserved, cancelled])
    },
    #testcase{
        error = ?ERR_NESTED_ARCHIVE_DELETION_FORBIDDEN(<<"archiveId">>),
        deprecated_error = ?DEPRECATED_ERROR_NESTED_ARCHIVE_DELETION_FORBIDDEN(<<"archiveId">>)
    },
    #testcase{
        error = ?ERR_QUOTA_EXCEEDED,
        deprecated_error = ?DEPRECATED_ERROR_QUOTA_EXCEEDED
    },
    #testcase{
        error = ?ERR_RECALL_TARGET_CONFLICT,
        deprecated_error = ?DEPRECATED_ERROR_RECALL_TARGET_CONFLICT
    },
    #testcase{
        error = ?ERR_SPACE_NOT_SUPPORTED_BY(<<"spaceId">>, <<"providerId">>),
        deprecated_error = ?DEPRECATED_ERROR_SPACE_NOT_SUPPORTED_BY(<<"spaceId">>, <<"providerId">>)
    },
    #testcase{
        error = ?ERR_STAT_OPERATION_NOT_SUPPORTED(<<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_STAT_OPERATION_NOT_SUPPORTED(<<"storageId">>)
    },
    #testcase{
        error = ?ERR_USER_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_USER_NOT_SUPPORTED
    },

    %%--------------------------------------------------------------------
    %% op_worker/atm errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type),
        deprecated_error = ?DEPRECATED_ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type)
    },
    #testcase{
        error = ?ERR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(#{<<"fileId">> => <<"REG">>}, atm_file_type, #{<<"hasAccess">> => true}),
        deprecated_error = ?DEPRECATED_ERROR_ATM_DATA_VALUE_CONSTRAINT_UNVERIFIED(#{<<"fileId">> => <<"REG">>}, atm_file_type, #{<<"hasAccess">> => true})
    },
    #testcase{
        error = ?ERR_ATM_INVALID_STATUS_TRANSITION(active, scheduled),
        deprecated_error = ?DEPRECATED_ERROR_ATM_INVALID_STATUS_TRANSITION(active, scheduled)
    },
    #testcase{
        error = ?ERR_ATM_JOB_BATCH_CRASHED(<<"sad">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_JOB_BATCH_CRASHED(<<"sad">>)
    },
    #testcase{
        error = ?ERR_ATM_JOB_BATCH_WITHDRAWN(<<"happy">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_JOB_BATCH_WITHDRAWN(<<"happy">>)
    },
    #testcase{
        error = ?ERR_ATM_LAMBDA_CONFIG_BAD_VALUE(<<"repeats">>, ?ERR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type)),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LAMBDA_CONFIG_BAD_VALUE(<<"repeats">>, ?DEPRECATED_ERROR_ATM_DATA_TYPE_UNVERIFIED(<<"NaN">>, atm_number_type))
    },
    #testcase{
        error = ?ERR_ATM_LANE_EMPTY(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EMPTY(<<"id">>)
    },
    #testcase{
        error = ?ERR_ATM_LANE_EXECUTION_CREATION_FAILED(<<"id">>, ?ERR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERR_ATM_LANE_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERR_ATM_OPENFAAS_NOT_CONFIGURED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_INITIATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED)
    },
    #testcase{
        error = ?ERR_ATM_LANE_EXECUTION_RERUN_FAILED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_RERUN_FAILED
    },
    #testcase{
        error = ?ERR_ATM_LANE_EXECUTION_RETRY_FAILED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_LANE_EXECUTION_RETRY_FAILED
    },
    #testcase{
        error = ?ERR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_FUNCTION_REGISTRATION_FAILED
    },
    #testcase{
        error = ?ERR_ATM_OPENFAAS_NOT_CONFIGURED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED
    },
%%    % TODO
%%    #testcase{
%%        error = ?ERR_ATM_OPENFAAS_QUERY_FAILED(undefined),
%%        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_QUERY_FAILED(undefined)
%%    },
    #testcase{
        error = ?ERR_ATM_OPENFAAS_QUERY_FAILED(<<"dns resolution error...">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_QUERY_FAILED(<<"dns resolution error...">>)
    },
    #testcase{
        error = ?ERR_ATM_OPENFAAS_UNHEALTHY,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_UNHEALTHY
    },
    #testcase{
        error = ?ERR_ATM_OPENFAAS_UNREACHABLE,
        deprecated_error = ?DEPRECATED_ERROR_ATM_OPENFAAS_UNREACHABLE
    },
    #testcase{
        error = ?ERR_ATM_PARALLEL_BOX_EMPTY(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_PARALLEL_BOX_EMPTY(<<"id">>)
    },
    #testcase{
        error = ?ERR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(<<"id">>, ?ERR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_PARALLEL_BOX_EXECUTION_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERR_ATM_OPENFAAS_NOT_CONFIGURED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_PARALLEL_BOX_EXECUTION_INITIATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED)
    },
    #testcase{
        error = ?ERR_ATM_STORE_CONTENT_NOT_SET(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_CONTENT_NOT_SET(<<"id">>)
    },
    #testcase{
        error = ?ERR_ATM_STORE_CREATION_FAILED(<<"id">>, ?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT)
    },
    #testcase{
        error = ?ERR_ATM_STORE_FROZEN(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_FROZEN(<<"id">>)
    },
    #testcase{
        error = ?ERR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT,
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_MISSING_REQUIRED_INITIAL_CONTENT
    },
    #testcase{
        error = ?ERR_ATM_STORE_NOT_FOUND(<<"id">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_NOT_FOUND(<<"id">>)
    },
    #testcase{
        error = ?ERR_ATM_STORE_TYPE_DISALLOWED(<<"id">>, [single_value]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_STORE_TYPE_DISALLOWED(<<"id">>, [single_value])
    },
    #testcase{
        error = ?ERR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(<<"arg">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(<<"arg">>)
    },
    #testcase{
        error = ?ERR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(<<"arg">>),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_FOR_REQUIRED_LAMBDA_ARG_MISSING(<<"arg">>)
    },
    #testcase{
        error = ?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED([1, 2], [0]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED([1, 2], [0])
    },
    #testcase{
        error = ?ERR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(store_credentials, [iterated_item]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(store_credentials, [iterated_item])
    },
    #testcase{
        error = ?ERR_ATM_TASK_ARG_MAPPING_FAILED(<<"arg">>, ?ERR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_ARG_MAPPING_FAILED(<<"arg">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERR_ATM_TASK_EXECUTION_CREATION_FAILED(<<"id">>, ?ERR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_EXECUTION_CREATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERR_ATM_TASK_EXECUTION_INITIATION_FAILED(<<"id">>, ?ERR_ATM_OPENFAAS_NOT_CONFIGURED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_EXECUTION_INITIATION_FAILED(<<"id">>, ?DEPRECATED_ERROR_ATM_OPENFAAS_NOT_CONFIGURED)
    },
    #testcase{
        error = ?ERR_ATM_TASK_EXECUTION_STOPPED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_EXECUTION_STOPPED
    },
    #testcase{
        error = ?ERR_ATM_TASK_RESULT_DISPATCH_FAILED(<<"id">>, ?ERR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_RESULT_DISPATCH_FAILED(<<"id">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERR_ATM_TASK_RESULT_MAPPING_FAILED(<<"result">>, ?ERR_USER_NOT_SUPPORTED),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_RESULT_MAPPING_FAILED(<<"result">>, ?DEPRECATED_ERROR_USER_NOT_SUPPORTED)
    },
    #testcase{
        error = ?ERR_ATM_TASK_RESULT_MISSING(<<"result">>, [<<"key1">>, <<"key2">>]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_TASK_RESULT_MISSING(<<"result">>, [<<"key1">>, <<"key2">>])
    },
    #testcase{
        error = ?ERR_ATM_UNSUPPORTED_DATA_TYPE(atm_string_type, [atm_number_type]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_UNSUPPORTED_DATA_TYPE(atm_string_type, [atm_number_type])
    },
    #testcase{
        error = ?ERR_ATM_WORKFLOW_EMPTY,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EMPTY
    },
    #testcase{
        error = ?ERR_ATM_WORKFLOW_EXECUTION_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_ENDED
    },
    #testcase{
        error = ?ERR_ATM_WORKFLOW_EXECUTION_NOT_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_NOT_ENDED
    },
    #testcase{
        error = ?ERR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_NOT_RESUMABLE
    },
    #testcase{
        error = ?ERR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_NOT_STOPPED
    },
    #testcase{
        error = ?ERR_ATM_WORKFLOW_EXECUTION_STOPPED,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_STOPPED
    },
    #testcase{
        error = ?ERR_ATM_WORKFLOW_EXECUTION_STOPPING,
        deprecated_error = ?DEPRECATED_ERROR_ATM_WORKFLOW_EXECUTION_STOPPING
    },

    %%--------------------------------------------------------------------
    %% op_worker/dir_stats errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_DIR_STATS_DISABLED_FOR_SPACE,
        deprecated_error = ?DEPRECATED_ERROR_DIR_STATS_DISABLED_FOR_SPACE
    },
    #testcase{
        error = ?ERR_DIR_STATS_NOT_READY,
        deprecated_error = ?DEPRECATED_ERROR_DIR_STATS_NOT_READY
    },

    %%--------------------------------------------------------------------
    %% op_worker/storage errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>, <<"s3">>], [<<"s3">>]),
        deprecated_error = ?DEPRECATED_ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>, <<"s3">>], [<<"s3">>])
    },
    #testcase{
        error = ?ERR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(<<"providerId">>, <<"storageId">>, <<"spaceId">>),
        deprecated_error = ?DEPRECATED_ERROR_NOT_A_LOCAL_STORAGE_SUPPORTING_SPACE(<<"providerId">>, <<"storageId">>, <<"spaceId">>)
    },
    #testcase{
        error = ?ERR_REQUIRES_AUTO_STORAGE_IMPORT_MODE,
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_AUTO_STORAGE_IMPORT_MODE
    },
    #testcase{
        error = ?ERR_REQUIRES_IMPORTED_STORAGE(<<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_IMPORTED_STORAGE(<<"storageId">>)
    },
    #testcase{
        error = ?ERR_REQUIRES_NON_IMPORTED_STORAGE(<<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_NON_IMPORTED_STORAGE(<<"storageId">>)
    },
    #testcase{
        error = ?ERR_REQUIRES_POSIX_COMPATIBLE_STORAGE(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>]),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_POSIX_COMPATIBLE_STORAGE(<<"storageId">>, [<<"posix">>, <<"glusterfs">>, <<"nulldevice">>])
    },
    #testcase{
        error = ?ERR_REQUIRES_READONLY_STORAGE(<<"storageType">>),
        deprecated_error = ?DEPRECATED_ERROR_REQUIRES_READONLY_STORAGE(<<"storageType">>)
    },
    #testcase{
        error = ?ERR_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"swift">>, <<"s3">>, <<"cephrados">>]),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_IMPORT_NOT_SUPPORTED(<<"storageId">>, [<<"swift">>, <<"s3">>, <<"cephrados">>])
    },
    #testcase{
        error = ?ERR_STORAGE_IN_USE,
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_IN_USE
    },
    #testcase{
        error = ?ERR_STORAGE_TEST_FAILED(read),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_TEST_FAILED(read)
    },
    #testcase{
        error = ?ERR_STORAGE_TEST_FAILED(write),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_TEST_FAILED(write)
    },
    #testcase{
        error = ?ERR_STORAGE_TEST_FAILED(remove),
        deprecated_error = ?DEPRECATED_ERROR_STORAGE_TEST_FAILED(remove)
    },

    %%--------------------------------------------------------------------
    %% op_worker/transfer errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_TRANSFER_ALREADY_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_TRANSFER_ALREADY_ENDED
    },
    #testcase{
        error = ?ERR_TRANSFER_NOT_ENDED,
        deprecated_error = ?DEPRECATED_ERROR_TRANSFER_NOT_ENDED
    },

    %%--------------------------------------------------------------------
    %% op_worker/view errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_VIEW_NOT_EXISTS_ON(<<"providerId">>),
        deprecated_error = ?DEPRECATED_ERROR_VIEW_NOT_EXISTS_ON(<<"providerId">>)
    },
    #testcase{
        error = ?ERR_VIEW_QUERY_FAILED(<<"category">>, <<"description">>),
        deprecated_error = ?DEPRECATED_ERROR_VIEW_QUERY_FAILED(<<"category">>, <<"description">>)
    },

    %%--------------------------------------------------------------------
    %% oz_worker errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_ATM_LAMBDA_IN_USE([<<"a">>, <<"b">>, <<"c">>, <<"d">>]),
        deprecated_error = ?DEPRECATED_ERROR_ATM_LAMBDA_IN_USE([<<"a">>, <<"b">>, <<"c">>, <<"d">>])
    },
    #testcase{
        error = ?ERR_BASIC_AUTH_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_BASIC_AUTH_DISABLED
    },
    #testcase{
        error = ?ERR_BASIC_AUTH_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_BASIC_AUTH_NOT_SUPPORTED
    },
    #testcase{
        error = ?ERR_CANNOT_ADD_RELATION_TO_SELF,
        deprecated_error = ?DEPRECATED_ERROR_CANNOT_ADD_RELATION_TO_SELF
    },
%%    % TODO entity_type is differently encoded
%%    #testcase{
%%        error = ?ERR_CANNOT_DELETE_ENTITY(od_user, <<"user1">>),
%%        deprecated_error = ?DEPRECATED_ERROR_CANNOT_DELETE_ENTITY(od_user, <<"user1">>)
%%    }
    #testcase{
        error = ?ERR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE,
        deprecated_error = ?DEPRECATED_ERROR_CANNOT_DELETE_NON_EMPTY_HANDLE_SERVICE
    },
    #testcase{
        error = ?ERR_CANNOT_REMOVE_LAST_OWNER(od_space, <<"space1">>),
        deprecated_error = ?DEPRECATED_ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, <<"space1">>)
    },
    #testcase{
        error = ?ERR_PROTECTED_GROUP,
        deprecated_error = ?DEPRECATED_ERROR_PROTECTED_GROUP
    },
    #testcase{
        error = ?ERR_RELATION_ALREADY_EXISTS(od_user, <<"user1">>, od_space, <<"space1">>),
        deprecated_error = ?DEPRECATED_ERROR_RELATION_ALREADY_EXISTS(od_user, <<"user1">>, od_space, <<"space1">>)
    },
    #testcase{
        error = ?ERR_RELATION_DOES_NOT_EXIST(od_user, <<"user1">>, od_space, <<"space1">>),
        deprecated_error = ?DEPRECATED_ERROR_RELATION_DOES_NOT_EXIST(od_user, <<"user1">>, od_space, <<"space1">>)
    },

    %%--------------------------------------------------------------------
    %% oz_worker/space errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(<<"spaceId">>, <<"storageId">>),
        deprecated_error = ?DEPRECATED_ERROR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(<<"spaceId">>, <<"storageId">>)
    },
    #testcase{
        error = ?ERR_SPACE_MARKETPLACE_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_SPACE_MARKETPLACE_DISABLED
    },

    %%--------------------------------------------------------------------
    %% oz_worker/subdomain errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_SUBDOMAIN_DELEGATION_DISABLED,
        deprecated_error = ?DEPRECATED_ERROR_SUBDOMAIN_DELEGATION_DISABLED
    },
    #testcase{
        error = ?ERR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED,
        deprecated_error = ?DEPRECATED_ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED
    },

    %%--------------------------------------------------------------------
    %% posix errors
    %%--------------------------------------------------------------------
    #testcase{
        error = ?ERR_POSIX(eacess),
        deprecated_error = ?DEPRECATED_ERROR_POSIX(eacess)
    }
].


-endif.

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

encode_decode_error_test() ->
    lists:foreach(fun(Testcase) ->
        {Before, After} = case Testcase of
            {different, A, B} -> {A, B};
            Err -> {Err, Err}
        end,
        Json = errors:to_json(Before),
        ?assert(is_map(Json)),
        ?assert(size(maps:get(<<"description">>, Json)) > 0),
        EncodedJSON = json_utils:encode(Json),
        DecodedJSON = json_utils:decode(EncodedJSON),
        FromJson = errors:from_json(DecodedJSON),
        ?assertMatch({error, _}, FromJson),
        ?assertEqual(After, FromJson)
    end, testcases()).


http_code_test() ->
    lists:foreach(fun(Testcase) ->
        {Error, _} = case Testcase of
            {different, A, B} -> {A, B};
            Err -> {Err, Err}
        end,
        Code = errors:to_http_code(Error),
        ?assert(Code >= 400),
        ?assert(Code =< 503)
    end, testcases()).


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
    ?ERROR_TIMEOUT,
    ?ERROR_TEMPORARY_FAILURE,
    ?ERROR_UNAUTHORIZED,
    ?ERROR_FORBIDDEN,
    ?ERROR_NOT_FOUND,
    ?ERROR_ALREADY_EXISTS,

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
    ?ERROR_BAD_AUDIENCE_TOKEN(?ERROR_BAD_TOKEN),
    ?ERROR_BAD_AUDIENCE_TOKEN(?ERROR_TOKEN_INVALID),
    ?ERROR_BAD_AUDIENCE_TOKEN(?ERROR_TOKEN_REVOKED),
    ?ERROR_BAD_AUDIENCE_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12345678})),
    ?ERROR_TOKEN_INVALID,
    ?ERROR_TOKEN_REVOKED,
    ?ERROR_NOT_AN_ACCESS_TOKEN,
    ?ERROR_NOT_AN_INVITE_TOKEN(?SPACE_INVITE_USER_TOKEN),
    ?ERROR_INVITE_TOKEN_ISSUER_NOT_AUTHORIZED,
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = 12323746234}),
    ?ERROR_TOKEN_SUBJECT_INVALID,
    ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(?AUD(?OP_PANEL, <<"kjasif2387rg7adc09jf8a0sdfg97a">>)),
    ?ERROR_TOKEN_SESSION_INVALID,
    ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(86400),

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
    ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_NOT_AN_INVITE_TOKEN(?GROUP_INVITE_GROUP_TOKEN)),
    ?ERROR_BAD_VALUE_TOKEN(<<"supportToken">>, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_authorization_none{})),
    ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>),
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
    ?ERROR_BAD_VALUE_CAVEATS,
    ?ERROR_BAD_GUI_PACKAGE,
    ?ERROR_GUI_PACKAGE_TOO_LARGE,
    ?ERROR_GUI_PACKAGE_UNVERIFIED,
    ?ERROR_INVALID_QOS_EXPRESSION,

    %% -----------------------------------------------------------------------------
    %% State errors
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
    ?ERROR_SPACE_NOT_SUPPORTED_BY(<<"providerId">>),
    ?ERROR_VIEW_NOT_EXISTS_ON(<<"providerId">>),
    ?ERROR_TRANSFER_ALREADY_ENDED,
    ?ERROR_TRANSFER_NOT_ENDED,

    %% -----------------------------------------------------------------------------
    %% Unknown error
    %% -----------------------------------------------------------------------------
    ?ERROR_UNKNOWN_ERROR(#{
        <<"id">> => <<"someErrorThatWasNotSpecifiedInThisSoftwareVersion">>,
        <<"details">> => #{<<"key">> => <<"value">>},
        <<"description">> => <<"Human readable error description.">>
    })
].



-endif.
%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Errors to be used across all Onedata products.
%%% @end
%%%-------------------------------------------------------------------
-module(errors).

-include("errors.hrl").
-include("http/codes.hrl").
-include("logging.hrl").

%% API
-export([
    is_known_error/1,
    is_posix_code/1,

    to_json/1,
    from_json/1,
    to_http_code/1,
    to_errno/1
]).

-type errno() :: od_error:errno().

-type error() ::
    od_error:error() |
    % unrecognized is a special error and as such does not have its own module
    {error, #od_error{type :: od_error_unrecognized_error}}.

-type as_json() :: json_utils:json_map().

-export_type([errno/0, error/0, as_json/0]).


%%%===================================================================
%%% API
%%%===================================================================


-spec is_known_error(term()) -> boolean().
is_known_error(?ERR) -> true;
is_known_error(_) -> false.


-spec is_posix_code(term()) -> boolean().
is_posix_code(ErrorCode) ->
    ordsets:is_element(ErrorCode, ?ERROR_CODES).


-spec to_json
    (undefined) -> null;
    (error()) -> json_utils:json_map().
to_json(undefined) ->
    null;

to_json(?ERR_UNRECOGNIZED_ERROR(ErrorAsJson)) ->
    % Carries errors that have not been recognized upon decoding.
    case maps:is_key(<<"description">>, ErrorAsJson) of
        true ->
            ErrorAsJson;
        false ->
            ErrorAsJson#{<<"description">> => <<"No description (unknown error).">>}
    end;

to_json(Error = ?ERR(Type)) ->
    Type:to_json(Error);

to_json(OtherError) ->
    % Wildcard to catch all errors that might be returned by the application logic, but does
    % not match any error defined in this module. Inability to translate is treated as an
    % unexpected exception (an ?ERR_INTERNAL_SERVER_ERROR(ErrorRef) is returned).
    ReturnedError = ?catch_exceptions(error({cannot_translate_error, OtherError})),
    to_json(ReturnedError).


-spec from_json
    (null) -> undefined;
    (json_utils:json_map()) -> error().
from_json(null) ->
    undefined;

from_json(ErrorJson) ->
    try
        ErrorId = maps:get(<<"id">>, ErrorJson),
        ErrorType = maps:get(ErrorId, ?ERROR_ID_TO_TYPE_MAPPING),
        ErrorType:from_json(ErrorJson)
    catch _:_ ->
        ?ERR_UNRECOGNIZED_ERROR(?err_ctx(), ErrorJson)
    end.


-spec to_http_code(error()) -> 400 | 401 | 403 | 404 | 409 | 500 | 501 | 503.
to_http_code(?ERR_UNRECOGNIZED_ERROR(_)) -> 
    ?HTTP_500_INTERNAL_SERVER_ERROR;

to_http_code(Error = ?ERR(Type)) ->
    Type:to_http_code(Error).


-spec to_errno(error()) -> false | {true, errno()}.
to_errno(?ERR_UNRECOGNIZED_ERROR(_)) -> 
    {true, ?EAGAIN};

to_errno(Error = ?ERR(Type)) ->
    Type:to_errno(Error).

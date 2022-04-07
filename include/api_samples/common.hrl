%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions related to API samples.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(API_SAMPLES_COMMON_HRL).
-define(API_SAMPLES_COMMON_HRL, 1).


% @see rest_api_samples
-record(rest_api_samples, {
    api_root :: http_client:url(),
    samples :: [rest_api_request_sample:record()]
}).

% @see rest_api_request_sample
-record(rest_api_request_sample, {
    % human readable name and description of the operation
    name :: binary(),
    description :: binary(),
    method :: rest_api_request_sample:method(),
    % relative to the api_root from enclosing #rest_api_samples{} record
    path :: binary(),
    headers = #{} :: http_client:headers(),
    data = undefined :: undefined | json_utils:json_term(),
    requires_authorization = true :: boolean(),
    follow_redirects = false :: boolean(),
    % human readable description for each placeholder that appears in other
    % fields (path, headers, data), e.g.
    % `#{<<"$USER_ID">> => <<"Id of the user relevant for the operation">>}`
    placeholders = #{} :: #{binary() => binary()},
    swagger_operation_id :: binary()
}).


-endif.

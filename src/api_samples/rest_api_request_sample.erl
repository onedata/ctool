%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a recipe for a concrete REST API request.
%%%
%%% NOTE: In the future (probably when OpenAPI 3.0 is used) it should be
%%% possible to automatically generate the REST API samples from swagger
%%% definitions. Currently, they are composed independently.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_api_request_sample).
-author("Lukasz Opiola").

-behaviour(jsonable_record).


-include("api_samples/common.hrl").


-type method() :: 'POST' | 'PUT' | 'GET' | 'PATCH' | 'DELETE'.
-export_type([method/0]).

-type record() :: #rest_api_request_sample{}.
-export_type([record/0]).


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================


-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    #{
        <<"name">> => Record#rest_api_request_sample.name,
        <<"description">> => Record#rest_api_request_sample.description,
        <<"method">> => method_to_json(Record#rest_api_request_sample.method),
        <<"path">> => Record#rest_api_request_sample.path,
        <<"headers">> => Record#rest_api_request_sample.headers,
        <<"data">> => utils:undefined_to_null(Record#rest_api_request_sample.data),
        <<"requiresAuthorization">> => Record#rest_api_request_sample.requires_authorization,
        <<"followRedirects">> => Record#rest_api_request_sample.follow_redirects,
        <<"placeholders">> => Record#rest_api_request_sample.placeholders,
        <<"optionalParameters">> => Record#rest_api_request_sample.optional_parameters,
        <<"swaggerOperationId">> => Record#rest_api_request_sample.swagger_operation_id
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    #rest_api_request_sample{
        name = maps:get(<<"name">>, RecordJson),
        description = maps:get(<<"description">>, RecordJson),
        method = method_from_json(maps:get(<<"method">>, RecordJson)),
        path = maps:get(<<"path">>, RecordJson),
        headers = maps:get(<<"headers">>, RecordJson),
        data = utils:null_to_undefined(maps:get(<<"data">>, RecordJson)),
        requires_authorization = maps:get(<<"requiresAuthorization">>, RecordJson),
        follow_redirects = maps:get(<<"followRedirects">>, RecordJson),
        placeholders = maps:get(<<"placeholders">>, RecordJson),
        optional_parameters = maps:get(<<"optionalParameters">>, RecordJson),
        swagger_operation_id = maps:get(<<"swaggerOperationId">>, RecordJson)
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec method_to_json(method()) -> json_utils:json_term().
method_to_json('POST') -> <<"POST">>;
method_to_json('PUT') -> <<"PUT">>;
method_to_json('GET') -> <<"GET">>;
method_to_json('PATCH') -> <<"PATCH">>;
method_to_json('DELETE') -> <<"DELETE">>.


%% @private
-spec method_from_json(json_utils:json_term()) -> method().
method_from_json(<<"POST">>) -> 'POST';
method_from_json(<<"PUT">>) -> 'PUT';
method_from_json(<<"GET">>) -> 'GET';
method_from_json(<<"PATCH">>) -> 'PATCH';
method_from_json(<<"DELETE">>) -> 'DELETE'.

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a set of REST API request samples - @see rest_api_request_sample.
%%% Paths included in the request samples are relative to the api root
%%% that is common for all samples.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_api_samples).
-author("Lukasz Opiola").

-behaviour(jsonable_record).


-include("api_samples/common.hrl").


-type record() :: #rest_api_samples{}.
-export_type([record/0]).


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    #{
        <<"apiRoot">> => Record#rest_api_samples.api_root,
        <<"samples">> => jsonable_record:list_to_json(Record#rest_api_samples.samples, rest_api_request_sample)
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    #rest_api_samples{
        api_root = maps:get(<<"apiRoot">>, RecordJson),
        samples = jsonable_record:list_from_json(maps:get(<<"samples">>, RecordJson), rest_api_request_sample)
    }.

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing lambda result specification used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_result_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


% Indicates the method how results from a lambda will be transmitted to
% the orchestrating Oneprovider:
%   return_value - the result is returned in the standard output from the lambda function
%       handler under a key equal to the name in a result specs. This method allows
%       correlating results with specific items that are processed by the lambda and offers
%       a transactional way of processing - the results are consumed only upon lambda
%       execution success. Possible errors (e.g. during result data validation) are handled
%       on a per-item basis.
%   file_pipe - an arbitrary number of results can be written to a file in the lambda's
%       container, with file name equal to the result name. These files are monitored by an
%       asynchronous process and the gathered results are reported to the Oneprovider,
%       which dispatches the results to stores using the same mapping mechanisms as for the
%       'returned_value' results. With this method, results cannot be correlated with any
%       specific item. As a result, the processing is not transactional - if a lambda fails
%       part way through, the results that have already been written to the pipe might
%       have already been dispatched to the target stores. Possible errors (e.g. during
%       result data validation) are handled on the level of the whole task.
-type relay_method() :: return_value | file_pipe.
-export_type([relay_method/0]).

-type record() :: #atm_lambda_result_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(validate, RecordJson, fun jsonable_record:from_json/2).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, NestedRecordEncoder) ->
    encode_with(Record, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, NestedRecordDecoder) ->
    decode_with(skip_validation, RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"name">> => Record#atm_lambda_result_spec.name,
        <<"dataSpec">> => NestedRecordEncoder(Record#atm_lambda_result_spec.data_spec, atm_data_spec),
        <<"relayMethod">> => relay_method_to_json(Record#atm_lambda_result_spec.relay_method)
    }.


%% @private
-spec decode_with(jsonable_record:validation_strategy(), json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    #atm_lambda_result_spec{
        name = maps:get(<<"name">>, RecordJson),
        data_spec = NestedRecordDecoder(maps:get(<<"dataSpec">>, RecordJson), atm_data_spec),
        relay_method = relay_method_from_json(maps:get(<<"relayMethod">>, RecordJson, <<"returnValue">>))
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    #atm_lambda_result_spec{name = Name} = ResultSpec = decode_with(skip_validation, RecordJson, NestedRecordDecoder),
    str_utils:validate_name(Name) orelse throw(?ERROR_BAD_VALUE_NAME(<<"resultSpec.name">>)),
    ResultSpec.


%% @private
-spec relay_method_to_json(relay_method()) -> json_utils:json_term().
relay_method_to_json(return_value) -> <<"returnValue">>;
relay_method_to_json(file_pipe) -> <<"filePipe">>.


%% @private
-spec relay_method_from_json(json_utils:json_term()) -> relay_method().
relay_method_from_json(<<"returnValue">>) -> return_value;
relay_method_from_json(<<"filePipe">>) -> file_pipe.

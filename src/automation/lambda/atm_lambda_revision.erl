%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing lambda revision used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_revision).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% API
-export([calculate_checksum/1]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_lambda_revision{}.
-export_type([record/0]).

% Number assigned to a lambda revision by the user that creates the revision.
% Lambda revisions cannot be edited or overwritten - each edition requires a new revision.
% The only exception is changing the revision lifecycle state, which is permitted.
% It is recommended that new revisions get sequentially incremented number,
% but in general the numbering is up to the users.
-type revision_number() :: pos_integer().

% used to compare two lambda revisions - they are considered the same from the functional
% point of view if their checksums are the same
-type checksum() :: binary().

-export_type([revision_number/0, checksum/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec calculate_checksum(record()) -> checksum().
calculate_checksum(AtmLambdaRevision) ->
    str_utils:md5_digest([
        AtmLambdaRevision#atm_lambda_revision.name,
        AtmLambdaRevision#atm_lambda_revision.operation_spec,
        AtmLambdaRevision#atm_lambda_revision.argument_specs,
        AtmLambdaRevision#atm_lambda_revision.result_specs
    ]).

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
        <<"name">> => Record#atm_lambda_revision.name,
        <<"summary">> => Record#atm_lambda_revision.summary,
        <<"description">> => Record#atm_lambda_revision.description,
        <<"operationSpec">> => NestedRecordEncoder(Record#atm_lambda_revision.operation_spec, atm_lambda_operation_spec),
        <<"argumentSpecs">> => [NestedRecordEncoder(S, atm_lambda_argument_spec) || S <- Record#atm_lambda_revision.argument_specs],
        <<"resultSpecs">> => [NestedRecordEncoder(S, atm_lambda_result_spec) || S <- Record#atm_lambda_revision.result_specs],
        <<"preferredBatchSize">> => Record#atm_lambda_revision.preferred_batch_size,
        <<"resourceSpec">> => NestedRecordEncoder(Record#atm_lambda_revision.resource_spec, atm_resource_spec),
        <<"checksum">> => Record#atm_lambda_revision.checksum,
        <<"state">> => automation:lifecycle_state_to_json(Record#atm_lambda_revision.state)
    }.


%% @private
-spec decode_with(jsonable_record:validation_strategy(), json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(ValidationStrategy, RecordJson, NestedRecordDecoder) ->
    %% @TODO VFS-8507 Rework along with new data sanitizers for all atm models (data_spec callback?)
    InputSummary = maps:get(<<"summary">>, RecordJson, ?DEFAULT_SUMMARY),
    Summary = case ValidationStrategy of
        validate -> automation:sanitize_binary(<<"summary">>, InputSummary, ?SUMMARY_SIZE_LIMIT);
        skip_validation -> InputSummary
    end,

    %% @TODO VFS-8507 Rework along with new data sanitizers for all atm models (data_spec callback?)
    InputDescription = maps:get(<<"description">>, RecordJson, ?DEFAULT_DESCRIPTION),
    Description = case ValidationStrategy of
        validate -> automation:sanitize_binary(<<"description">>, InputDescription, ?DESCRIPTION_SIZE_LIMIT);
        skip_validation -> InputDescription
    end,

    RevisionWithoutChecksum = #atm_lambda_revision{
        name = maps:get(<<"name">>, RecordJson),
        summary = Summary,
        description = Description,
        operation_spec = NestedRecordDecoder(maps:get(<<"operationSpec">>, RecordJson), atm_lambda_operation_spec),
        argument_specs = [NestedRecordDecoder(S, atm_lambda_argument_spec) || S <- maps:get(<<"argumentSpecs">>, RecordJson)],
        result_specs = [NestedRecordDecoder(S, atm_lambda_result_spec) || S <- maps:get(<<"resultSpecs">>, RecordJson)],
        preferred_batch_size = maps:get(<<"preferredBatchSize">>, RecordJson),
        resource_spec = NestedRecordDecoder(maps:get(<<"resourceSpec">>, RecordJson), atm_resource_spec),
        checksum = <<>>,
        state = automation:lifecycle_state_from_json(maps:get(<<"state">>, RecordJson))
    },

    RevisionWithoutChecksum#atm_lambda_revision{
        checksum = case {ValidationStrategy, maps:find(<<"checksum">>, RecordJson)} of
            {skip_validation, {ok, PersistedChecksum}} ->
                PersistedChecksum;
            {_, _} ->
                calculate_checksum(RevisionWithoutChecksum)
        end
    }.

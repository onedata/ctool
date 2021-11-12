%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing lambda revision registry used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_revision_registry).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% API
-export([empty/0]).
-export([get_revision/2]).
-export([add_revision/3]).
-export([update_revision_lifecycle_state/3]).
-export([with/2]).
-export([size/1]).
-export([has_revision/2]).
-export([get_all_revision_numbers/1]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_lambda_revision_registry{}.
-export_type([record/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> record().
empty() ->
    #atm_lambda_revision_registry{}.


-spec get_revision(atm_lambda_revision:revision_number(), record()) -> atm_lambda_revision:record().
get_revision(RevisionNumber, #atm_lambda_revision_registry{registry = Registry}) ->
    case maps:find(RevisionNumber, Registry) of
        error ->
            error({badkey, RevisionNumber});
        {ok, Revision} ->
            Revision
    end.


-spec add_revision(
    atm_lambda_revision:revision_number(),
    atm_lambda_revision:record(),
    record()
) -> record().
add_revision(RevisionNumber, _Revision, #atm_lambda_revision_registry{
    registry = Registry
}) when is_map_key(RevisionNumber, Registry) ->
    error(badarg);
add_revision(RevisionNumber, Revision, Record = #atm_lambda_revision_registry{
    registry = Registry
}) when is_integer(RevisionNumber) andalso RevisionNumber > 0 ->
    Record#atm_lambda_revision_registry{registry = Registry#{
        RevisionNumber => Revision
    }}.


-spec update_revision_lifecycle_state(
    atm_lambda_revision:revision_number(),
    automation:lifecycle_state(),
    record()
) -> record().
update_revision_lifecycle_state(RevisionNumber, NewState, Record = #atm_lambda_revision_registry{
    registry = Registry
}) ->
    Record#atm_lambda_revision_registry{
        registry = maps:update_with(RevisionNumber, fun(AtmLambdaRevision) ->
            AtmLambdaRevision#atm_lambda_revision{state = NewState}
        end, Registry)
    }.


-spec with([atm_lambda_revision:revision_number()], record()) -> record().
with(RevisionNumbers, Record = #atm_lambda_revision_registry{registry = Registry}) ->
    Record#atm_lambda_revision_registry{registry = maps:with(RevisionNumbers, Registry)}.


-spec has_revision(atm_lambda_revision:revision_number(), record()) -> boolean().
has_revision(RevisionNumber, #atm_lambda_revision_registry{registry = Registry}) ->
    maps:is_key(RevisionNumber, Registry).


-spec size(record()) -> non_neg_integer().
size(#atm_lambda_revision_registry{registry = Registry}) ->
    maps:size(Registry).


-spec get_all_revision_numbers(record()) -> [atm_lambda_revision:revision_number()].
get_all_revision_numbers(#atm_lambda_revision_registry{registry = Registry}) ->
    maps:keys(Registry).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(RecordJson, fun jsonable_record:from_json/2).

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
    decode_with(RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(#atm_lambda_revision_registry{registry = RevisionRegistry}, NestedRecordEncoder) ->
    maps:fold(fun(RevisionNumber, RevisionRecord, Acc) ->
        Acc#{
            integer_to_binary(RevisionNumber) => NestedRecordEncoder(RevisionRecord, atm_lambda_revision)
        }
    end, #{}, RevisionRegistry).


-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RevisionRegistryJson, NestedRecordDecoder) ->
    #atm_lambda_revision_registry{registry = maps:fold(fun(RevisionNumberBin, RevisionRecordJson, Acc) ->
        Acc#{
            binary_to_integer(RevisionNumberBin) => NestedRecordDecoder(RevisionRecordJson, atm_lambda_revision)
        }
    end, #{}, RevisionRegistryJson)}.

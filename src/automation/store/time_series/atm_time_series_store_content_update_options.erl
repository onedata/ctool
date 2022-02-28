%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing store content update options specialization for
%%% time_series store used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_store_content_update_options).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type measurement_time_series_name_matcher() :: binary().
-export_type([measurement_time_series_name_matcher/0]).

-type record() :: #atm_time_series_store_content_update_options{}.
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
    DispatchRules = Record#atm_time_series_store_content_update_options.dispatch_rules,
    #{
        <<"dispatchRules">> => [NestedRecordEncoder(R, atm_time_series_dispatch_rule) || R <- DispatchRules]
    }.


%% @private
-spec decode_with(validate | skip_validation, json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    EncodedDispatchRules = maps:get(<<"dispatchRules">>, RecordJson),
    #atm_time_series_store_content_update_options{
        dispatch_rules = [NestedRecordDecoder(R, atm_time_series_dispatch_rule) || R <- EncodedDispatchRules]
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    Spec = decode_with(skip_validation, RecordJson, NestedRecordDecoder),
    % name matchers in different rules must be unique (there can be at most one rule for each name matcher)
    lists:foldl(fun(DispatchRule, AlreadyUsedNameMatchers) ->
        #atm_time_series_dispatch_rule{measurement_ts_name_matcher = NameMatcher} = DispatchRule,
        ordsets:is_element(NameMatcher, AlreadyUsedNameMatchers) andalso throw(
            ?ERROR_BAD_DATA(<<"dispatchRules">>, <<"There cannot be two dispatch rules with the same name matcher">>)
        ),
        ordsets:add_element(NameMatcher, AlreadyUsedNameMatchers)
    end, ordsets:new(), Spec#atm_time_series_store_content_update_options.dispatch_rules),
    Spec.

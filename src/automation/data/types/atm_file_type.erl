%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the file data type used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_file_type).
-author("Lukasz Opiola").

-behaviour(atm_data_type).

-include("automation/automation.hrl").

%% atm_data_type callbacks
-export([is_instance/1]).
-export([encode_value_constraints/2, decode_value_constraints/3]).

-type file_type() :: 'REG' | 'DIR' | 'SYMLNK' | 'ANY'.

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

%% @TODO VFS-7687 Implement all automation data types and validators
-spec is_instance(json_utils:json_term()) -> boolean().
is_instance(#{<<"file_id">> := FileId}) when is_binary(FileId) -> true;
is_instance(_Value) -> false.


-spec encode_value_constraints(atm_data_type:value_constraints(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_value_constraints(Constraints, _NestedRecordEncoder) ->
    FileType = maps:get(file_type, Constraints, 'ANY'),
    #{
        <<"fileType">> => file_type_to_json(FileType)
    }.


-spec decode_value_constraints(
    jsonable_record:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    atm_data_type:value_constraints().
decode_value_constraints(_ValidationStrategy, ConstraintsJson, _NestedRecordDecoder) ->
    FileTypeJson = maps:get(<<"fileType">>, ConstraintsJson, <<"ANY">>),
    #{
        file_type => file_type_from_json(FileTypeJson)
    }.


-spec file_type_to_json(file_type()) -> json_utils:json_term().
file_type_to_json('REG') -> <<"REG">>;
file_type_to_json('DIR') -> <<"DIR">>;
file_type_to_json('SYMLNK') -> <<"SYMLNK">>;
file_type_to_json('ANY') -> <<"ANY">>.


-spec file_type_from_json(json_utils:json_term()) -> file_type().
file_type_from_json(<<"REG">>) -> 'REG';
file_type_from_json(<<"DIR">>) -> 'DIR';
file_type_from_json(<<"SYMLNK">>) -> 'SYMLNK';
file_type_from_json(<<"ANY">>) -> 'ANY'.

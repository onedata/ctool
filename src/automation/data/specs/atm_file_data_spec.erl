%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the file data spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_file_data_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").
-include("onedata_file.hrl").

%% API
-export([allowed_file_type_specs/0]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_file_data_spec{}.
% ?LINK_TYPE is not allowed (it's allowed only upon file creation; then, the file is
% considered a regular file from the user point of view).
-type file_type_spec() :: ?REGULAR_FILE_TYPE | ?DIRECTORY_TYPE | ?SYMLINK_TYPE | 'ANY'.

-export_type([record/0, file_type_spec/0]).

%% @TODO VFS-12091 include all attrs after atm versioning is introduced
-define(AVAILABLE_ATTRS, ?API_FILE_ATTRS -- [?attr_creation_time]).

%%%===================================================================
%%% API
%%%===================================================================


-spec allowed_file_type_specs() -> [file_type_spec()].
allowed_file_type_specs() ->
    [?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE, ?SYMLINK_TYPE, 'ANY'].


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================


-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode(Record).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode(validate, RecordJson).


%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================


-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    encode(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    decode(skip_validation, RecordJson).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec encode(record()) -> json_utils:json_term().
encode(Record = #atm_file_data_spec{file_type = FileType}) ->
    #{
        <<"fileType">> => file_type_to_json(FileType),
        <<"attributes">> => case Record#atm_file_data_spec.attributes of
            undefined -> null;
            Attributes -> lists:map(fun onedata_file:attr_name_to_json/1, Attributes)
        end
    }.


%% @private
-spec decode(jsonable_record:validation_strategy(), json_utils:json_term()) -> record().
decode(ValidationStrategy, RecordJson) ->
    #atm_file_data_spec{
        file_type = file_type_from_json(maps:get(<<"fileType">>, RecordJson, <<"ANY">>)),
        attributes = try
            case maps:get(<<"attributes">>, RecordJson) of
                null ->
                    undefined;
                AttrNamesJson ->
                    lists:usort(onedata_file:sanitize_attr_names(<<"attributes">>, AttrNamesJson, current, ?AVAILABLE_ATTRS))
            end
        catch
            Class:Reason:Stacktrace when ValidationStrategy == validate ->
                erlang:raise(Class, Reason, Stacktrace);
            _:_ when ValidationStrategy == skip_validation ->
                % In case of older schemas, decoding will fail (attr names have changed),
                % so we just default to the full list to ensure that they can be loaded.
                lists:usort(?AVAILABLE_ATTRS)
        end
    }.


%% @private
-spec file_type_to_json(file_type_spec()) -> json_utils:json_term().
file_type_to_json('ANY') -> <<"ANY">>;
file_type_to_json(?LINK_TYPE) -> error(hardlink_type_not_allowed);
file_type_to_json(Atom) -> onedata_file:type_to_json(Atom).


%% @private
-spec file_type_from_json(json_utils:json_term()) -> file_type_spec().
file_type_from_json(<<"ANY">>) ->
    'ANY';
file_type_from_json(Binary) ->
    case onedata_file:type_from_json(Binary) of
        ?LINK_TYPE ->
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(
                <<"fileType">>,
                lists:map(fun file_type_to_json/1, allowed_file_type_specs()))
            );
        Other ->
            Other
    end.

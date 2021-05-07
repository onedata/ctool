%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of api_auth module.
%%% @end
%%%-------------------------------------------------------------------
-module(automation_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("automation/automation.hrl").
-include("logging.hrl").

-define(RAND_STR(), str_utils:rand_hex(16)).
-define(RAND_BOOL(), lists_utils:random_element([true, false])).

encode_decode_atm_data_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ?assert(is_equal_after_json_encode_and_decode(DataSpec))
    end, example_data_specs()).


encode_decode_operation_spec_test() ->
    OperationSpecExamples = [
        #atm_lambda_operation_spec{
            spec = #atm_onedata_function_operation_spec{
                function_id = ?RAND_STR()
            }
        },
        #atm_lambda_operation_spec{
            spec = #atm_openfaas_operation_spec{
                docker_image = ?RAND_STR(),
                docker_execution_options = #atm_docker_execution_options{
                    readonly = ?RAND_BOOL(),
                    mount_oneclient = ?RAND_BOOL(),
                    oneclient_mount_point = <<"/a/b/c/d/", (?RAND_STR())/binary>>,
                    oneclient_options = lists_utils:random_element([<<"">>, <<"--a --b">>])
                }
            }
        },
        #atm_lambda_operation_spec{
            spec = #atm_workflow_operation_spec{
                atm_workflow_id = ?RAND_STR()
            }
        },
        #atm_lambda_operation_spec{
            spec = #atm_user_form_operation_spec{
                user_form_id = ?RAND_STR()
            }
        }
    ],
    lists:foreach(fun(OperationSpec) ->
        ?assert(is_equal_after_json_encode_and_decode(OperationSpec)),
        ?assert(is_equal_after_db_encode_and_decode(OperationSpec))
    end, OperationSpecExamples).


encode_decode_docker_execution_options_test() ->
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{}
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            readonly = true
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            readonly = false
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            mount_oneclient = ?RAND_BOOL()
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            oneclient_mount_point = <<"/a/b/c/d">>
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            mount_oneclient = true,
            oneclient_mount_point = <<"/a/b/c/d">>
        }
    ),
    encode_decode_docker_execution_options_test_base(
        #atm_docker_execution_options{
            mount_oneclient = true,
            oneclient_mount_point = <<"/a/b/c/d">>,
            oneclient_options = <<"--a --b">>
        }
    ).

encode_decode_docker_execution_options_test_base(ExecutionOptions) ->
    ?assert(is_equal_after_json_encode_and_decode(ExecutionOptions)),
    ?assert(is_equal_after_db_encode_and_decode(ExecutionOptions)).


encode_decode_lambda_argument_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ArgumentSpec = #atm_lambda_argument_spec{
            name = ?RAND_STR(),
            data_spec = DataSpec,
            is_batch = ?RAND_BOOL(),
            is_optional = ?RAND_BOOL(),
            default_value = lists_utils:random_element([
                true,
                false,
                4351,
                <<"text">>,
                #{<<"object">> => 134},
                #{<<"nested">> => #{<<"object">> => <<"text">>}}
            ])
        },
        ?assert(is_equal_after_json_encode_and_decode(ArgumentSpec)),
        ?assert(is_equal_after_db_encode_and_decode(ArgumentSpec))
    end, example_data_specs()).


encode_decode_lambda_result_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ResultSpec = #atm_lambda_result_spec{
            name = ?RAND_STR(),
            data_spec = DataSpec,
            is_batch = ?RAND_BOOL()
        },
        ?assert(is_equal_after_json_encode_and_decode(ResultSpec)),
        ?assert(is_equal_after_db_encode_and_decode(ResultSpec))
    end, example_data_specs()).

%%%===================================================================
%%% Helper functions
%%%===================================================================

example_data_specs() ->
    AllTypes = [
        atm_integer_type, atm_string_type, atm_object_type,
        atm_file_type, atm_histogram_type, atm_dataset_type, atm_archive_type,
        atm_store_credentials_type, atm_onedatafs_credentials_type
    ],
    GenExampleValueConstraints = fun
        (atm_file_type) ->
            lists_utils:random_element([#{file_type => lists_utils:random_element(['REG', 'DIR', 'ANY'])}]);
        (atm_store_credentials_type) ->
            #{store_type => lists_utils:random_element([single_value, list, map, forest, range, histogram])};
        (_) ->
            #{}
    end,
    lists:map(fun(Type) ->
        #atm_data_spec{
            type = Type,
            value_constraints = GenExampleValueConstraints(Type)
        }
    end, AllTypes).


is_equal_after_json_encode_and_decode(Record) ->
    RecordType = utils:record_type(Record),
    Record =:= jsonable_record:from_json(jsonable_record:to_json(Record, RecordType), RecordType).


is_equal_after_db_encode_and_decode(Record) ->
    RecordType = utils:record_type(Record),
    Record =:= persistent_record:decode(persistent_record:encode(Record, RecordType), RecordType).

-endif.
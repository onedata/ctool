%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
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

encode_decode_atm_data_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ?assert(is_equal_after_encode_and_decode(DataSpec))
    end, example_data_specs()).


encode_decode_lambda_engine_test() ->
    encode_decode_lambda_engine_test_base(onedata_function),
    encode_decode_lambda_engine_test_base(open_faas),
    encode_decode_lambda_engine_test_base(atm_workflow),
    encode_decode_lambda_engine_test_base(user_form).

encode_decode_lambda_engine_test_base(Engine) ->
    ?assertEqual(
        Engine,
        automation:decode_lambda_engine(automation:encode_lambda_engine(Engine))
    ).


encode_decode_lambda_execution_options_test() ->
    ?assert(is_equal_after_encode_and_decode(
        #atm_lambda_execution_options{}
    )),
    ?assert(is_equal_after_encode_and_decode(
        #atm_lambda_execution_options{readonly = true}
    )),
    ?assert(is_equal_after_encode_and_decode(
        #atm_lambda_execution_options{readonly = false}
    )),
    ?assert(is_equal_after_encode_and_decode(
        #atm_lambda_execution_options{mount_space_options = #atm_mount_space_options{}}
    )),
    ?assert(is_equal_after_encode_and_decode(
        #atm_lambda_execution_options{mount_space_options = #atm_mount_space_options{
            mount_point = <<"/a/b/c/d">>
        }}
    )),
    ?assert(is_equal_after_encode_and_decode(
        #atm_lambda_execution_options{mount_space_options = #atm_mount_space_options{
            mount_oneclient = true,
            mount_point = <<"/a/b/c/d">>
        }}
    )),
    ?assert(is_equal_after_encode_and_decode(
        #atm_lambda_execution_options{mount_space_options = #atm_mount_space_options{
            mount_oneclient = true,
            mount_point = <<"/a/b/c/d">>,
            oneclient_options = <<"--a --b">>
        }}
    )).


encode_decode_lambda_argument_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ?assert(is_equal_after_encode_and_decode(#atm_lambda_argument_spec{
            name = str_utils:rand_hex(16),
            data_spec = DataSpec,
            is_array = lists_utils:random_element([true, false]),
            is_optional = lists_utils:random_element([true, false]),
            default_value = lists_utils:random_element([
                true,
                false,
                4351,
                <<"text">>,
                #{<<"object">> => 134},
                #{<<"nested">> => #{<<"object">> => <<"text">>}}
            ])
        }))
    end, example_data_specs()).


encode_decode_lambda_result_spec_test() ->
    lists:foreach(fun(DataSpec) ->
        ?assert(is_equal_after_encode_and_decode(#atm_lambda_result_spec{
            name = str_utils:rand_hex(16),
            data_spec = DataSpec,
            is_array = lists_utils:random_element([true, false]),
            is_optional = lists_utils:random_element([true, false])
        }))
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


is_equal_after_encode_and_decode(Record) ->
    Record =:= record_json_encoder:decode(record_json_encoder:encode(Record)).

-endif.
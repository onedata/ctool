%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of api_samples module.
%%% @end
%%%-------------------------------------------------------------------
-module(api_samples_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test/test_utils.hrl").
-include("api_samples/common.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================


encode_decode_rest_api_samples_test() ->
    encode_decode_test_base(example_rest_api_samples_records()).


encode_decode_rest_api_request_sample_test() ->
    encode_decode_test_base(example_rest_api_request_sample_records()).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
example_rest_api_samples_records() ->
    lists_utils:generate(fun() ->
        #rest_api_samples{
            api_root = str_utils:format_bin("https://~s/api", [?RAND_STR()]),
            samples = ?RAND_SUBLIST(example_rest_api_request_sample_records())
        }
    end, 5).


%% @private
example_rest_api_request_sample_records() ->
    lists_utils:generate(fun() ->
        #rest_api_request_sample{
            name = ?RAND_STR(),
            description = ?RAND_STR(),
            method = ?RAND_ELEMENT(['POST', 'PUT', 'GET', 'PATCH', 'DELETE']),
            path = filename:join([<<"/">>, ?RAND_STR(), ?RAND_STR(), ?RAND_STR()]),
            headers = maps_utils:generate(fun() ->
                {?RAND_STR(), ?RAND_STR()}
            end, ?RAND_INT(0, 3)),
            data = ?RAND_ELEMENT([undefined, <<"string">>, #{<<"map">> => <<"value">>}, [1, 2, 3, 4]]),
            requires_authorization = ?RAND_BOOL(),
            follow_redirects = ?RAND_BOOL(),
            placeholders = maps_utils:generate(fun() ->
                {?RAND_STR(), ?RAND_STR()}
            end, ?RAND_INT(0, 3)),
            swagger_operation_id = ?RAND_STR()
        }
    end, 10).


%% @private
encode_decode_test_base(Records) ->
    lists:foreach(fun(Example) ->
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(Example))
    end, Records).



-endif.
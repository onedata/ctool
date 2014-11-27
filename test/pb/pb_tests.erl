%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module tests the functionality of pb module.
%% It contains unit tests that base on eunit.
%% @end
%% ===================================================================
-module(pb_tests).

-include_lib("eunit/include/eunit.hrl").

-record(message, {field1, field2, field3, field4}).

%% ===================================================================
%% Tests description
%% ===================================================================

pb_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"should encode and decode record", fun should_encode_and_decode_record/0},
            {"should not encode record using invalid encoding module",
                fun should_not_encode_record_using_invalid_encoding_module/0},
            {"should not encode invalid record", fun should_not_encode_invalid_record/0},
            {"should not decode data using invalid decoding module",
                fun should_not_decode_data_using_invalid_decoding_module/0},
            {"should not decode data using invalid decoding type",
                fun should_not_decode_data_using_invalid_decoding_type/0},
            {"should not decode invalid data", fun should_not_decode_invalid_data/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    lists:foreach(fun(_) -> ok end, [test_pb, encode_message, decode_message]),
    ok = protobuffs_compile:scan_file("../test/pb/test.proto").

teardown(_) ->
    ok.

%% ===================================================================
%% Tests functions
%% ===================================================================

should_encode_and_decode_record() ->
    Record = #message{field1 = 1, field2 = 2, field3 = "field3", field4 = <<"field4">>},
    EncodeAns1 = pb:encode("test", Record),
    EncodeAns2 = pb:encode("test_pb", Record),
    EncodeAns3 = pb:encode(test_pb, Record),

    ?assertMatch({ok, _}, EncodeAns1),
    ?assertEqual(EncodeAns1, EncodeAns2),
    ?assertEqual(EncodeAns2, EncodeAns3),

    {ok, Data} = EncodeAns1,

    DecodeAns1 = pb:decode("test", "message", Data),
    DecodeAns2 = pb:decode("test_pb", "message", Data),
    DecodeAns3 = pb:decode(test_pb, "message", Data),
    DecodeAns4 = pb:decode(test_pb, "decode_message", Data),
    DecodeAns5 = pb:decode(test_pb, decode_message, Data),

    ?assertEqual({ok, Record}, DecodeAns1),
    ?assertEqual(DecodeAns1, DecodeAns2),
    ?assertEqual(DecodeAns2, DecodeAns3),
    ?assertEqual(DecodeAns3, DecodeAns4),
    ?assertEqual(DecodeAns4, DecodeAns5).

should_not_encode_record_using_invalid_encoding_module() ->
    Record = #message{field1 = 1, field2 = 2, field3 = "field3", field4 = <<"field4">>},
    EncodeAns1 = pb:encode("test2", Record),
    EncodeAns2 = pb:encode(<<"test">>, Record),

    ?assertEqual({error, unsupported_encoder}, EncodeAns1),
    ?assertEqual({error, unsupported_encoder}, EncodeAns2).

should_not_encode_invalid_record() ->
    Record1 = {message, 1, 2, "field3", <<"field4">>, field5},
    EncodeAns1 = pb:encode("test", Record1),
    Record2 = {message2, 1, 2, "field3", <<"field4">>},
    EncodeAns2 = pb:encode("test", Record2),
    Record3 = message3,
    EncodeAns3 = pb:encode("test", Record3),

    ?assertEqual({error, invalid_record}, EncodeAns1),
    ?assertEqual({error, invalid_record}, EncodeAns2),
    ?assertEqual({error, invalid_record}, EncodeAns3).

should_not_decode_data_using_invalid_decoding_module() ->
    Data = [[["\b", [1]], [[16], [2]], [[26], [6], <<"field3">>], ["\"", [6], <<"field4">>]]],
    DecodeAns1 = pb:decode("test2", "message", Data),
    DecodeAns2 = pb:decode(<<"test2">>, "message", Data),

    ?assertEqual({error, unsupported_decoder}, DecodeAns1),
    ?assertEqual({error, unsupported_decoder_or_invalid_data}, DecodeAns2).

should_not_decode_data_using_invalid_decoding_type() ->
    Data = [[["\b", [1]], [[16], [2]], [[26], [6], <<"field3">>], ["\"", [6], <<"field4">>]]],
    DecodeAns1 = pb:decode("test", "message2", Data),
    DecodeAns2 = pb:decode(<<"test">>, "message2", Data),

    ?assertEqual({error, unsupported_decoder}, DecodeAns1),
    ?assertEqual({error, unsupported_decoder}, DecodeAns2).

should_not_decode_invalid_data() ->
    Data1 = [data],
    Data2 = data,
    DecodeAns1 = pb:decode("test", "message", Data1),
    DecodeAns2 = pb:decode("test", "message", Data2),

    ?assertEqual({error, invalid_data}, DecodeAns1),
    ?assertEqual({error, unsupported_decoder_or_invalid_data}, DecodeAns2).
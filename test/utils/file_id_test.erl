%%%--------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This file tests packing and unpacking of Guids and objectids.
%%%--------------------------------------------------------------------
-module(file_id_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(ENTERPRISENUM, 0).


pack_unpack_guid_test() ->
    U = <<"uuid">>, S = <<"space_id">>,
    ?assertEqual({U, S}, file_id:unpack_guid(file_id:pack_guid(U, S))).

pack_unpack_share_guid_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    ?assertEqual({U, S, Sh}, file_id:unpack_share_guid(file_id:pack_share_guid(U, S, Sh))),
    ?assertEqual({U, <<>>, Sh}, file_id:unpack_share_guid(file_id:pack_share_guid(U, <<>>, Sh))),
    ?assertEqual({U, S, undefined}, file_id:unpack_share_guid(file_id:pack_share_guid(U, S, undefined))),
    ?assertEqual({U, S, undefined}, file_id:unpack_share_guid(file_id:pack_share_guid(U, S, <<>>))).

guid_to_share_guid_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    G1 = file_id:pack_guid(U, S),
    G2 = file_id:guid_to_share_guid(G1, Sh),
    ?assertEqual(S, file_id:guid_to_space_id(G2)),
    ?assertEqual(U, file_id:guid_to_uuid(G2)),
    ?assertEqual(Sh, file_id:guid_to_share_id(G2)).

share_guid_to_guid_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    G1 = file_id:pack_share_guid(U, S, Sh),
    G2 = file_id:share_guid_to_guid(G1),
    ?assertEqual(S, file_id:guid_to_space_id(G2)),
    ?assertEqual(U, file_id:guid_to_uuid(G2)).

guid_objectid_test() ->
    U = <<"uuid">>, S = <<"space_id">>,
    G = file_id:pack_guid(U, S),
    {ok, FileId} = file_id:guid_to_objectid(G),
    ?assertEqual({ok, G}, file_id:objectid_to_guid(FileId)).

share_guid_objectid_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    G = file_id:pack_share_guid(U, S, Sh),
    {ok, FileId} = file_id:guid_to_objectid(G),
    ?assertEqual({ok, G}, file_id:objectid_to_guid(FileId)).

guid_to_space_id_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    G1 = file_id:pack_guid(U, S),
    G2 = file_id:pack_share_guid(U, S, Sh),
    ?assertEqual(S, file_id:guid_to_space_id(G1)),
    ?assertEqual(S, file_id:guid_to_space_id(G2)).

guid_to_uuid_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    G1 = file_id:pack_guid(U, S),
    G2 = file_id:pack_share_guid(U, S, Sh),
    ?assertEqual(U, file_id:guid_to_uuid(G1)),
    ?assertEqual(U, file_id:guid_to_uuid(G2)).

guid_to_share_id_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    G1 = file_id:pack_guid(U, S),
    G2 = file_id:pack_share_guid(U, S, Sh),
    ?assertEqual(undefined, file_id:guid_to_share_id(G1)),
    ?assertEqual(Sh, file_id:guid_to_share_id(G2)).

is_share_guid_test() ->
    U = <<"uuid">>, S = <<"space_id">>, Sh = <<"share_id">>,
    G1 = file_id:pack_share_guid(U, S, Sh),
    G2 = file_id:pack_guid(U, S),
    G3 = file_id:share_guid_to_guid(G1),
    ?assertEqual(true, file_id:is_share_guid(G1)),
    ?assertEqual(false, file_id:is_share_guid(G2)),
    ?assertEqual(false, file_id:is_share_guid(G3)),
    ?assertEqual(false, file_id:is_share_guid(<<"bad_guid">>)).

crc_test() ->
    Expected = 40679,
    ?assertEqual(Expected, file_id:crc16("test string")).

build_with_enum_test() ->
    TestString = <<"data string">>,
    TestNum = 96,
    Crc = 27447,
    Length = size(TestString),
    Obj = file_id:build_objectid(TestNum, TestString),
    ?assertEqual(Obj, <<0:8, TestNum:24,
        0:8, Length:8, Crc:16, TestString/binary>>).

build_without_enum_test() ->
    TestString = <<"data string">>,
    Crc = 17183,
    Length = size(TestString),
    Obj = file_id:build_objectid(TestString),
    CmpString = TestString,
    ?assertEqual(Obj, <<0:8, ?ENTERPRISENUM:24,
        0:8, Length:8, Crc:16, CmpString/binary>>).

build_with_badarg_test() ->
    TooLong =
        <<"12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
">>,
    ?assertEqual({error, badarg},
        file_id:build_objectid(TooLong)).

build_with_badarg2_test() ->
    TooLong =
        <<"12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
12345678901234567890123456789012345:12345678901234567890123456789012345:1234567890123456789012345678
">>,
    ?assertEqual({error, badarg},
        file_id:build_objectid(?ENTERPRISENUM, TooLong)).

base16_test() ->
    TestString = <<"data string">>,
    Obj = file_id:build_objectid(TestString),
    Encode = file_id:to_base16(Obj),
    ?assertEqual(Obj, file_id:from_base16(Encode)).

-endif.

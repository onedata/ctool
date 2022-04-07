%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper functions for eunit tests.
%%% @end
%%%-------------------------------------------------------------------
-module(eunit_utils).
-author("Lukasz Opiola").

%% API
-export([debug_log/2, dump/1]).
-export([is_equal_after_json_encode_and_decode/1, is_equal_after_json_encode_and_decode/2]).
-export([is_equal_after_db_encode_and_decode/1, is_equal_after_db_encode_and_decode/2]).


%%%===================================================================
%%% API
%%%===================================================================


-spec debug_log(string(), [term()]) -> ok.
debug_log(Format, Args) ->
    FormattedOutput = lists:flatten(io_lib:format(Format, Args)),
    io:format(
        user,
        "~n"
        "----------------------------------------------------------~n"
        "~s~n"
        "----------------------------------------------------------~n",
        [FormattedOutput]
    ),
    io:put_chars(<<>>).  % force flush the log to eunit output


-spec dump(term()) -> ok.
dump(Term) ->
    debug_log("[DUMP] ~p", [Term]).


-spec is_equal_after_json_encode_and_decode(tuple()) -> boolean().
is_equal_after_json_encode_and_decode(Record) ->
    is_equal_after_json_encode_and_decode(Record, utils:record_type(Record)).

-spec is_equal_after_json_encode_and_decode(tuple(), atom()) -> boolean().
is_equal_after_json_encode_and_decode(Record, RecordType) ->
    case jsonable_record:from_json(jsonable_record:to_json(Record, RecordType), RecordType) of
        Record ->
            true;
        Other ->
            debug_log("Record different after json encode and decode!~nExpected: ~p~nGot:      ~p", [
                Record, Other
            ]),
            false
    end.


-spec is_equal_after_db_encode_and_decode(tuple()) -> boolean().
is_equal_after_db_encode_and_decode(Record) ->
    is_equal_after_db_encode_and_decode(Record, utils:record_type(Record)).

-spec is_equal_after_db_encode_and_decode(tuple(), atom()) -> boolean().
is_equal_after_db_encode_and_decode(Record, RecordType) ->
    case persistent_record:decode(persistent_record:encode(Record, RecordType), RecordType) of
        Record ->
            true;
        Other ->
            debug_log("Record different after DB encode and decode!~nExpected: ~p~nGot:      ~p", [
                Record, Other
            ]),
            false
    end.

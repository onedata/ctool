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
-export([debug_log/2, dump/1, dump/2]).
-export([is_equal/3]).
-export([is_equal_after_json_encode_and_decode/1, is_equal_after_json_encode_and_decode/2]).
-export([is_equal_after_db_encode_and_decode/1, is_equal_after_db_encode_and_decode/2]).
-export([throws_error_during_decode_from_json/2, throws_error_during_decode_from_json/3]).


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
    dump("DUMP", Term).


-spec dump(string(), term()) -> ok.
dump(Name, Term) ->
    debug_log("~s = ~p", [Name, Term]).


%% @doc gives you nice debug logs when not equal
-spec is_equal(term(), term(), string()) -> boolean().
is_equal(ExpectedValue, ExpectedValue, _) ->
    true;
is_equal(ActualValue, ExpectedValue, LogMessage) ->
    debug_log("~s~n> Exp: ~p~n> Got: ~p", [LogMessage, ExpectedValue, ActualValue]),
    false.


-spec is_equal_after_json_encode_and_decode(tuple()) -> boolean().
is_equal_after_json_encode_and_decode(Record) ->
    is_equal_after_json_encode_and_decode(Record, utils:record_type(Record)).

-spec is_equal_after_json_encode_and_decode(tuple(), atom()) -> boolean().
is_equal_after_json_encode_and_decode(Record, RecordType) ->
    is_equal(
        jsonable_record:from_json(jsonable_record:to_json(Record, RecordType), RecordType),
        Record,
        "Record different after json encode and decode!"
    ).


-spec is_equal_after_db_encode_and_decode(tuple()) -> boolean().
is_equal_after_db_encode_and_decode(Record) ->
    is_equal_after_db_encode_and_decode(Record, utils:record_type(Record)).

-spec is_equal_after_db_encode_and_decode(tuple(), atom()) -> boolean().
is_equal_after_db_encode_and_decode(Record, RecordType) ->
    is_equal(
        persistent_record:from_string(persistent_record:to_string(Record, RecordType), RecordType),
        Record,
        "Record different after DB encode and decode!"
    ).


% validation is done during decoding from json, so it is possible to encode an invalid record
% to json, but get an error when decoding it back from json
-spec throws_error_during_decode_from_json(errors:error(), tuple() | [tuple()]) -> boolean().
throws_error_during_decode_from_json(ExpError, Records) when is_list(Records) ->
    throws_error_during_decode_from_json(ExpError, utils:record_type(hd(Records)), Records);
throws_error_during_decode_from_json(ExpError, Record) ->
    throws_error_during_decode_from_json(ExpError, utils:record_type(Record), Record).

-spec throws_error_during_decode_from_json(errors:error(), atom(), tuple() | [tuple()]) -> boolean().
throws_error_during_decode_from_json(ExpError, RecordType, Records) when is_list(Records) ->
    lists:all(fun(Record) ->
        throws_error_during_decode_from_json(ExpError, RecordType, Record)
    end, Records);
throws_error_during_decode_from_json(ExpError, RecordType, Record) ->
    % validation should be done only when decoding from json (not during db decoding)
    case is_equal_after_db_encode_and_decode(Record, RecordType) of
        false ->
            false;
        true ->
            RecordJson = jsonable_record:to_json(Record, RecordType),
            ActualError = catch jsonable_record:from_json(RecordJson, RecordType),
            case ActualError of
                ExpError ->
                    true;
                _ ->
                    debug_log("Validation did not throw the expected error!~nExpected: ~p~nGot:      ~p", [
                        ExpError, ActualError
                    ]),
                    false
            end
    end.
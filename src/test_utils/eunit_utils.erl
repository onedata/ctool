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

-include("errors.hrl").

%% API
-export([debug_log/2, dump/1, dump/2]).
-export([is_equal/3]).
-export([is_equal_after_json_encode_and_decode/1, is_equal_after_json_encode_and_decode/2]).
-export([is_equal_after_db_encode_and_decode/1, is_equal_after_db_encode_and_decode/2]).
-export([throws_error_during_decode_from_json/2, throws_error_during_decode_from_json/3]).
-export([erase_ctx_if_error/1]).

-export([include_full_ctx_in_errors/1]).

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
        "~ts~n"
        "----------------------------------------------------------~n",
        [FormattedOutput]
    ),
    io:put_chars(<<>>).  % force flush the log to eunit output


-spec dump(term()) -> ok.
dump(Term) ->
    dump("DUMP", Term).


-spec dump(string(), term()) -> ok.
dump(Name, Term) ->
    debug_log("~ts = ~tp", [Name, Term]).


%% @doc gives you nice debug logs when not equal
-spec is_equal(term(), term(), string()) -> boolean().
is_equal(ExpectedValue, ExpectedValue, _) ->
    true;
is_equal(ActualValue, ExpectedValue, LogMessage) ->
    debug_log("~ts~n> Exp: ~tp~n> Got: ~tp", [LogMessage, ExpectedValue, ActualValue]),
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
            case erase_ctx_if_error(ExpError) == erase_ctx_if_error(ActualError) of
                true ->
                    true;
                false ->
                    debug_log("Validation did not throw the expected error!~nExpected: ~tp~nGot:      ~tp", [
                        ExpError, ActualError
                    ]),
                    false
            end
    end.


erase_ctx_if_error(?ERR(Type, Args, Ctx) = OriginalError) ->
    ShouldIncludeFullCtx = test_utils:should_include_full_ctx_in_errors_on_current_node(),
    ArgsWithErasedCtx = if
        ShouldIncludeFullCtx ->
            OriginalError;
        Args == undefined ->
            undefined;
        true ->
            list_to_tuple(lists:map(fun
                (Error = ?ERR) -> erase_ctx_if_error(Error);
                (Else) -> Else
            end, tuple_to_list(Args)))
    end,

    % Manually build tuple instead of using record to avoid dialyzer error
    {error, {
        od_error,
        Type,
        ArgsWithErasedCtx,
        case ShouldIncludeFullCtx of
            true -> Ctx;
            false -> undefined
        end
    }};
erase_ctx_if_error(Else) ->
    Else.


%%--------------------------------------------------------------------
%% @doc
%% The error ctx is normally mocked in all tests to be always undefined,
%% so that error equality/matching assertions always work
%% (otherwise, the ctx would always differ from the expectations).
%% This function disables this behaviour and may be useful for debug purposes.
%% @end
%%--------------------------------------------------------------------
-spec include_full_ctx_in_errors(boolean()) -> ok.
include_full_ctx_in_errors(Flag) ->
    ctool:set_env(include_full_ctx_in_errors, Flag).

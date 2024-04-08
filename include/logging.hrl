%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2013-2023 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Unified logging macros for all Onedata components, using lager behind the scenes.
%%%
%%%   Basic macros are intended for general purpose, manually formatted logs not related to exceptions.
%%%
%%%   Exception macros are intended as THE ONLY RIGHT way of logging unexpected exceptions.
%%%
%%% Use ?autoformat([TermA, TermB, ...]) for an auto-formatted string with the values
%%% of all Terms (by variable names).
%%%
%%% NOTE: always avoid using the `~tp` formatter at the end of the line to avoid large indents.
%%%
%%% NOTE: see the LOGGING.md file for usage examples.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, 1).

-include("errors.hrl").


% Macros that should be used in code for logging.

% Compilation with skip_debug flag will remove all debug messages from code.
-ifdef(skip_debug).
-define(debug(Message), ok).
-define(debug(Format, Args), ok).
-define(debug_exception(Class, Reason, Stacktrace), ok).
-define(debug_exception(DetailsMessage, Class, Reason, Stacktrace), ok).
-define(debug_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ok).
-endif.

-ifndef(skip_debug).
-define(debug(Message), ?log(7, Message, [])).
-define(debug(Format, Args), ?log(7, Format, Args)).
-define(debug_exception(Class, Reason, Stacktrace), ?log_exception(7, "", [], undefined, Class, Reason, Stacktrace)).
-define(debug_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(7, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(debug_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(7, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).
-endif.

-define(info(Message), ?log(6, Message, [])).
-define(info(Format, Args), ?log(6, Format, Args)).
-define(info_exception(Class, Reason, Stacktrace), ?log_exception(6, "", [], undefined, Class, Reason, Stacktrace)).
-define(info_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(6, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(info_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(6, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(notice(Message), ?log(5, Message, [])).
-define(notice(Format, Args), ?log(5, Format, Args)).
-define(notice_exception(Class, Reason, Stacktrace), ?log_exception(5, "", [], undefined, Class, Reason, Stacktrace)).
-define(notice_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(5, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(notice_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(5, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(warning(Message), ?log(4, Message, [])).
-define(warning(Format, Args), ?log(4, Format, Args)).
-define(warning_exception(Class, Reason, Stacktrace), ?log_exception(4, "", [], undefined, Class, Reason, Stacktrace)).
-define(warning_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(4, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(warning_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(4, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(error(Message), ?log(3, Message, [])).
-define(error(Format, Args), ?log(3, Format, Args)).
-define(error_exception(Class, Reason, Stacktrace), ?log_exception(3, "", [], undefined, Class, Reason, Stacktrace)).
-define(error_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(3, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(error_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(3, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(critical(Message), ?log(2, Message, [])).
-define(critical(Format, Args), ?log(2, Format, Args)).
-define(critical_exception(Class, Reason, Stacktrace), ?log_exception(2, "", [], undefined, Class, Reason, Stacktrace)).
-define(critical_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(2, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(critical_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(2, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(alert(Message), ?log(1, Message, [])).
-define(alert(Format, Args), ?log(1, Format, Args)).
-define(alert_exception(Class, Reason, Stacktrace), ?log_exception(1, "", [], undefined, Class, Reason, Stacktrace)).
-define(alert_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(1, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(alert_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(1, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(emergency(Message), ?log(0, Message, [])).
-define(emergency(Format, Args), ?log(0, Format, Args)).
-define(emergency_exception(Class, Reason, Stacktrace), ?log_exception(0, "", [], undefined, Class, Reason, Stacktrace)).
-define(emergency_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(0, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(emergency_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(0, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).


-define(is_printable(Str), if
    is_list(Str) -> io_lib:printable_list(Str);
    is_binary(Str) -> io_lib:printable_list(str_utils:binary_to_unicode_list(Str));
    true -> false
end).


-define(ensure_list_of_terms(TermOrTerms), case string:slice(??TermOrTerms, 0, 1) of
    "[" -> TermOrTerms;
    _ -> [TermOrTerms]
end).

% produces an auto-formatted string with the values of all Terms (by variable names)
% NOTE: the result string begins with a newline.
% NOTE: does not handle multiline strings well (i.e. when one of the Terms is a multiline string);
%       the "~p" formatter just prints an inline "\n". Thus, it's recommended to print such strings
%       using different methods, or just use binaries, which are handled well using "~s".
-define(autoformat(TermOrTerms), ?autoformat_with_msg("", [], TermOrTerms)).

% wrappers for convenience (the original macro accepts a list, but it's not 100% intuitive)
-define(autoformat(A, B), ?autoformat([A, B])).
-define(autoformat(A, B, C), ?autoformat([A, B, C])).
-define(autoformat(A, B, C, D), ?autoformat([A, B, C, D])).
-define(autoformat(A, B, C, D, E), ?autoformat([A, B, C, D, E])).
-define(autoformat(A, B, C, D, E, F), ?autoformat([A, B, C, D, E, F])).
-define(autoformat(A, B, C, D, E, F, G), ?autoformat([A, B, C, D, E, F, G])).
-define(autoformat(A, B, C, D, E, F, G, H), ?autoformat([A, B, C, D, E, F, G, H])).
-define(autoformat(A, B, C, D, E, F, G, H, I), ?autoformat([A, B, C, D, E, F, G, H, I])).
-define(autoformat(A, B, C, D, E, F, G, H, I, J), ?autoformat([A, B, C, D, E, F, G, H, I, J])).
-define(autoformat(A, B, C, D, E, F, G, H, I, J, K), ?autoformat([A, B, C, D, E, F, G, H, I, J, K])).

-define(autoformat_with_msg(Msg, FormatArgs, TermOrTerms), {autoformat, Msg, FormatArgs, string:tokens(??TermOrTerms, "[] ,"), ?ensure_list_of_terms(TermOrTerms)}).
-define(autoformat_with_msg(Msg, TermOrTerms), ?autoformat_with_msg(Msg, [], TermOrTerms)).
-define(autoformat_with_msg(Msg, FormatArgs, A, B), ?autoformat_with_msg(Msg, FormatArgs, [A, B])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D, E), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D, E])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D, E, F), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D, E, F])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D, E, F, G), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D, E, F, G])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D, E, F, G, H), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D, E, F, G, H])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D, E, F, G, H, I), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D, E, F, G, H, I])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D, E, F, G, H, I, J), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D, E, F, G, H, I, J])).
-define(autoformat_with_msg(Msg, FormatArgs, A, B, C, D, E, F, G, H, I, J, K), ?autoformat_with_msg(Msg, FormatArgs, [A, B, C, D, E, F, G, H, I, J, K])).


% DEPRECATED - use ?error_exception instead
% to be removed when occurrences of ?error_stacktrace are pruned from code
-define(error_stacktrace(DetailsMessage, Stacktrace), ?error_stacktrace(DetailsMessage, [], Stacktrace)).
-define(error_stacktrace(DetailsFormat, DetailsArgs, Stacktrace),
    ?wrap_in_loglevel_check(3,
        onedata_logger:log(3, ?gather_metadata, onedata_logger:format_deprecated_exception_log(
            ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, DetailsFormat, DetailsArgs, Stacktrace
        ))
    )
).


-define(log(LoglevelInt, Format, Args),
    ?wrap_in_loglevel_check(LoglevelInt, onedata_logger:log(
        LoglevelInt, ?gather_metadata, onedata_logger:format_generic_log(Format, Args)
    ))
).

% by default, all exceptions are logged on 'error' level
-define(log_exception(DetailsFormat, DetailsArgs, Ref, Class, Reason, Stacktrace),
    ?log_exception(3, DetailsFormat, DetailsArgs, Ref, Class, Reason, Stacktrace)
).
% A Ref (string) can optionally be passed for easier log navigation - as long
% as the Ref is then somehow identifiable, e.g. as in ?ERROR_INTERNAL_SERVER_ERROR(Ref).
-define(log_exception(LoglevelInt, DetailsFormat, DetailsArgs, Ref, Class, Reason, Stacktrace),
    ?wrap_in_loglevel_check(LoglevelInt,
        onedata_logger:log(LoglevelInt, ?gather_metadata, onedata_logger:format_exception_log(
            ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, DetailsFormat, DetailsArgs, Ref, Class, Reason, Stacktrace
        ))
    )
).


% a random string used to correlate a log with an internal server error
-define(make_error_ref(), str_utils:rand_hex(5)).


% Macro intended as a UNIVERSAL way to report internal server errors that are not caused by
% an exception, but are a result of handling anticipated errors (those that are not
% desired and should be reported as a problem and reflected in the application logs).
-define(report_internal_server_error(Message), ?report_internal_server_error(Message, [])).
-define(report_internal_server_error(DetailsFormat, DetailsArgs), begin
    ((fun(ErrorRef) ->
        ?error(onedata_logger:format_error_report(
            ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, DetailsFormat, DetailsArgs, ErrorRef
        )),
        ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef)
    end)(?make_error_ref()))
end).


% Macro used for execution flow control; extracts the result when the term indicates
% success (ok, {ok, Result}) or throws upon error.
-define(check(Expr), utils:check_result(Expr)).


% Macro intended as a UNIVERSAL way of handling exceptions, which can be classified in two ways:
%   1) All thrown errors:error() terms are treated as a control flow mechanism and simply returned.
%
%   2) Other exceptions are treated as unexpected (not anticipated during normal execution and/or
%      not part of the application logic). In such a case, the macro logs on error level with
%      a stacktrace and includes an error reference (for easier correlation of logs and errors
%      reported by clients), finally returning the ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef) error.
%
% The DetailsMessage or DetailsFormat+DetailsArgs arguments can be optionally
% passed to extend the log with some additional information.
-define(examine_exception(Class, Reason, Stacktrace),
    ?examine_exception("", Class, Reason, Stacktrace)
).
-define(examine_exception(DetailsMessage, Class, Reason, Stacktrace),
    ?examine_exception(DetailsMessage, [], Class, Reason, Stacktrace)
).
-define(examine_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), begin
    ((fun(ErrorRef) ->
        case {Class, errors:is_known_error(Reason)} of
            {throw, true} ->
                Reason;
            _ ->
                ?log_exception(DetailsFormat, DetailsArgs, ErrorRef, Class, Reason, Stacktrace),
                ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef)
        end
    end)(?make_error_ref()))
end).


% Macro intended as a UNIVERSAL way to wrap a piece of code with handling of exceptions.
% If the code finishes successfully or throws an errors:error() term, the return value
% is passed through, otherwise a standardized error (as per the 'errors' module) is returned.
-define(catch_exceptions(Expr), begin
    ((fun() ->
        try
            Expr
        catch
            Class:Reason:Stacktrace ->
                ?examine_exception(Class, Reason, Stacktrace)
        end
    end)())
end).


% Prints bad request warning (frequently used in gen_servers)
-define(log_bad_request(Request),
    % cannot use ?autoformat here as Request may be a complex term
    ?warning("~w:~B - received a bad request:~n    Request = ~tp", [?MODULE, ?LINE, Request])
).


% Prints abnormal termination warning
-define(log_terminate(Reason, State),
    case Reason of
        normal -> ok;
        shutdown -> ok;
        {shutdown, _} -> ok;
        _ -> ?warning("~w terminated in state ~tp~nReason: ~tp", [?MODULE, State, Reason])
    end
).


% Convenience macros for debug

% Prints a term or a list of terms by the name of the variable
-define(dump(TermOrTerms), io:format(user, "[DUMP]~ts~n", [
    case TermOrTerms of
        {autoformat, _, _, _, _} -> onedata_logger:format_generic_log(TermOrTerms);
        _ -> onedata_logger:format_generic_log(?autoformat(TermOrTerms))
end])).
% wrappers for convenience (the original macro accepts a list, but it's not 100% intuitive)
-define(dump(A, B), ?dump([A, B])).
-define(dump(A, B, C), ?dump([A, B, C])).
-define(dump(A, B, C, D), ?dump([A, B, C, D])).
-define(dump(A, B, C, D, E), ?dump([A, B, C, D, E])).
-define(dump(A, B, C, D, E, F), ?dump([A, B, C, D, E, F])).
-define(dump(A, B, C, D, E, F, G), ?dump([A, B, C, D, E, F, G])).
-define(dump(A, B, C, D, E, F, G, H), ?dump([A, B, C, D, E, F, G, H])).
-define(dump(A, B, C, D, E, F, G, H, I), ?dump([A, B, C, D, E, F, G, H, I])).
-define(dump(A, B, C, D, E, F, G, H, I, J), ?dump([A, B, C, D, E, F, G, H, I, J])).
-define(dump(A, B, C, D, E, F, G, H, I, J, K), ?dump([A, B, C, D, E, F, G, H, I, J, K])).


%% Macros used internally

-define(wrap_in_loglevel_check(LoglevelInt, Expression),
    case onedata_logger:should_log(LoglevelInt) of
        false ->
            ok;
        true ->
            Expression
    end
).

% Resolves current process's state and returns it as metadata proplist
% Must be called from original function where the log is,
% so that the process info makes sense
-define(gather_metadata,
    [{pid, self()}, {line, ?LINE}] ++
    onedata_logger:parse_process_info(process_info(self(), current_function))
).

% List of available loglevels in cluster
-define(CLUSTER_LOGLEVELS, [debug, info, notice, warning, error, critical, alert, emergency]).
% Available loglevels in clients
-define(CLIENT_LOGLEVELS, [debug, info, warning, error, fatal]).
% Client loglevel to discard all logs
-define(CLIENT_LOGLEVEL_NONE, none).

-endif.
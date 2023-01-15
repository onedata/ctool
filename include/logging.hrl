%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2013-2022 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains convenient logging macros.
%%% The lager application must be started for them to work.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, 1).

-include("errors.hrl").


% Macros that should be used in code for logging.
% xxx_stacktrace macros will automatically include the stack trace in the log,
% provided the env variable 'include_stacktrace' is set to true.

% Compilation with skip_debug flag will remove all debug messages from code.

-ifdef(skip_debug).
-define(debug(Message), ok).
-define(debug(Format, Args), ok).
-define(debug_stacktrace(Message), ok).
-define(debug_stacktrace(Format, Args), ok).
-endif.

-ifndef(skip_debug).
-define(debug(Message), ?do_log(0, Message, undefined)).
-define(debug(Format, Args), ?do_log(0, Format, Args, undefined)).
-define(debug_stacktrace(Message, Stacktrace), ?do_log(0, Message, Stacktrace)).
-define(debug_stacktrace(Format, Args, Stacktrace), ?do_log(0, Format, Args, Stacktrace)).
-endif.

-define(info(Message), ?do_log(1, Message, undefined)).
-define(info(Format, Args), ?do_log(1, Format, Args, undefined)).
-define(info_stacktrace(Message, Stacktrace), ?do_log(1, Message, Stacktrace)).
-define(info_stacktrace(Format, Args, Stacktrace), ?do_log(1, Format, Args, Stacktrace)).

-define(notice(Message), ?do_log(2, Message, undefined)).
-define(notice(Format, Args), ?do_log(2, Format, Args, undefined)).
-define(notice_stacktrace(Message, Stacktrace), ?do_log(2, Message, Stacktrace)).
-define(notice_stacktrace(Format, Args, Stacktrace), ?do_log(2, Format, Args, Stacktrace)).

-define(warning(Message), ?do_log(3, Message, undefined)).
-define(warning(Format, Args), ?do_log(3, Format, Args, undefined)).
-define(warning_stacktrace(Message, Stacktrace), ?do_log(3, Message, Stacktrace)).
-define(warning_stacktrace(Format, Args, Stacktrace), ?do_log(3, Format, Args, Stacktrace)).

-define(error(Message), ?do_log(4, Message, undefined)).
-define(error(Format, Args), ?do_log(4, Format, Args, undefined)).
-define(error_stacktrace(Message, Stacktrace), ?do_log(4, Message, Stacktrace)).
-define(error_stacktrace(Format, Args, Stacktrace), ?do_log(4, Format, Args, Stacktrace)).

-define(critical(Message), ?do_log(5, Message, undefined)).
-define(critical(Format, Args), ?do_log(5, Format, Args, undefined)).
-define(critical_stacktrace(Message, Stacktrace), ?do_log(5, Message, Stacktrace)).
-define(critical_stacktrace(Format, Args, Stacktrace), ?do_log(5, Format, Args, Stacktrace)).

-define(alert(Message), ?do_log(6, Message, undefined)).
-define(alert(Format, Args), ?do_log(6, Format, Args, undefined)).
-define(alert_stacktrace(Message, Stacktrace), ?do_log(6, Message, Stacktrace)).
-define(alert_stacktrace(Format, Args, Stacktrace), ?do_log(6, Format, Args, Stacktrace)).

-define(emergency(Message), ?do_log(7, Message, undefined)).
-define(emergency(Format, Args), ?do_log(7, Format, Args, undefined)).
-define(emergency_stacktrace(Message, Stacktrace), ?do_log(7, Message, Stacktrace)).
-define(emergency_stacktrace(Format, Args, Stacktrace), ?do_log(7, Format, Args, Stacktrace)).


% Macro used for execution flow control; extracts the result when the term indicates
% success (ok, {ok, Result}) or throws upon error.
-define(check(Expr), utils:check_result(Expr)).

% Macro intended as a UNIVERSAL way of handling exceptions, which can be classified in two ways:
%   1) All thrown error-like terms are treated as a control flow mechanism and simply returned.
%
%   2) Other exceptions are treated as unexpected (not anticipated during normal execution and/or
%      not part of the application logic). In such a case, the macro logs on error level with
%      a stacktrace and includes an error reference (for easier correlation of logs and errors
%      reported by clients), finally returning the ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef) error.
%
% The ExtraInfoFmt, ExtraInfoArgs arguments can be optionally passed to
% extend the log with arbitrary additional information.
-define(examine_exception(Class, Reason, Stacktrace),
    ?examine_exception(Class, Reason, Stacktrace, [])
).
-define(examine_exception(Class, Reason, Stacktrace, ExtraTermsToPrint),
    ?examine_exception(Class, Reason, Stacktrace,
        lists:flatten(lists:join("~n", lists:map(fun(TermName) ->
            "> " ++ TermName ++ ": ~p"
        end, string:tokens(??ExtraTermsToPrint, "[] ,")))),
        ExtraTermsToPrint
    )
).
-define(examine_exception(Class, Reason, Stacktrace, ExtraInfoFmt, ExtraInfoArgs), begin
    ((fun(ErrorRef) ->
        case {Class, Reason} of
            {throw, {error, _}} ->
                Reason;
            _ ->
                ?error_stacktrace(
                    "An unexpected exception (ref: ~s) ocurred in ~w:~w/~B, line ~B~n"
                    "~s"
                    "Caught: ~w:~p", [
                        ErrorRef, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                        case ExtraInfoFmt of
                            "" ->
                                "";
                            _ ->
                                str_utils:format(
                                    "----------------~n" ++ ExtraInfoFmt ++ "~n----------------~n",
                                    ExtraInfoArgs
                                )
                        end,
                        Class, Reason
                    ],
                    Stacktrace
                ),
                ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef)
        end
    end)(str_utils:rand_hex(5)))
end).

% Macro intended as a UNIVERSAL way to wrap a piece of code with handling of exceptions.
% If the code finishes successfully or throws an error-like term, the return value
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

% Macro intended as a UNIVERSAL way to report internal server errors that are not caused by
% an exception, but are a result of handling anticipated errors (those that are not
% desired and should be reported as a problem and reflected in the application logs).
-define(report_internal_server_error(Format, Args), begin
    ((fun(ErrorRef) ->
        ?error(
            "An unexpected error (ref: ~s) ocurred in ~w:~w/~B, line ~B~nDetailed log: " ++ Format,
            [ErrorRef, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE | Args]
        ),
        ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef)
    end)(str_utils:rand_hex(5)))
end).


% Convenience macros for development purposes

% Prints bad request warning (frequently used in gen_servers)
-define(log_bad_request(Request),
    ?do_log(3, "~p:~p - bad request ~p", [?MODULE, ?LINE, Request], undefined)
).

% Prints abnormal termination warning
-define(log_terminate(Reason, State),
    case Reason of
        normal -> ok;
        shutdown -> ok;
        {shutdown, _} -> ok;
        _ ->
            ?do_log(3, "~p terminated in state ~p due to: ~p", [?MODULE, State, Reason], undefined)
    end
).

% Prints a single variable
-define(dump(Arg), io:format(user, "[DUMP] ~s: ~p~n~n", [??Arg, Arg])).

% Prints a list of variables
-define(dump_all(ListOfVariables), lists:foreach(fun({_Name, _Value}) ->
    io:format(user, "[DUMP] ~s: ~p~n~n", [_Name, _Value])
end, lists:zip(string:tokens(??ListOfVariables, "[] ,"), ListOfVariables))).

%% Macros used internally

-define(do_log(LoglevelAsInt, Message, Stacktrace),
    ?do_log(LoglevelAsInt, Message, [], Stacktrace)
).

-define(do_log(LoglevelAsInt, Format, Args, Stacktrace),
    case onedata_logger:should_log(LoglevelAsInt) of
        false -> ok;
        true ->
            onedata_logger:dispatch_log(LoglevelAsInt, ?gather_metadata, Format, Args, Stacktrace)
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
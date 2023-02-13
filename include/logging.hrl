%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2013-2023 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains unified logging macros for all Onedata components.
%%% The lager application must be started for them to work.
%%%
%%% NOTE: see the LOGGING.md file for usage examples.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, 1).

-include("errors.hrl").


% Macros that should be used in code for logging.
%
%   Basic macros are intended for general purpose, manually formatted logs not related to exceptions.
%
%   Exception macros are intended as a THE ONLY RIGHT way of logging unexpected exceptions.
%       Use 'autoformat' atom as DetailsFormat for a generic log with automatic formatting of DetailsArgs.
%
% NOTE: always avoid using the `~p` formatter at the end of the line to avoid large indents.

% Compilation with skip_debug flag will remove all debug messages from code.
-ifdef(skip_debug).
-define(debug(Message), ok).
-define(debug(Format, Args), ok).
-define(debug_exception(Class, Reason, Stacktrace), ok).
-define(debug_exception(DetailsMessage, Class, Reason, Stacktrace), ok).
-define(debug_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ok).
-endif.

-ifndef(skip_debug).
-define(debug(Message), ?log(0, Message, [])).
-define(debug(Format, Args), ?log(0, Format, Args)).
-define(debug_exception(Class, Reason, Stacktrace), ?log_exception(0, "", [], undefined, Class, Reason, Stacktrace)).
-define(debug_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(0, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(debug_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(0, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).
-endif.

-define(info(Message), ?log(1, Message, [])).
-define(info(Format, Args), ?log(1, Format, Args)).
-define(info_exception(Class, Reason, Stacktrace), ?log_exception(1, "", [], undefined, Class, Reason, Stacktrace)).
-define(info_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(1, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(info_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(1, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(notice(Message), ?log(2, Message, [])).
-define(notice(Format, Args), ?log(2, Format, Args)).
-define(notice_exception(Class, Reason, Stacktrace), ?log_exception(2, "", [], undefined, Class, Reason, Stacktrace)).
-define(notice_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(2, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(notice_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(2, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(warning(Message), ?log(3, Message, [])).
-define(warning(Format, Args), ?log(3, Format, Args)).
-define(warning_exception(Class, Reason, Stacktrace), ?log_exception(3, "", [], undefined, Class, Reason, Stacktrace)).
-define(warning_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(3, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(warning_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(3, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(error(Message), ?log(4, Message, [])).
-define(error(Format, Args), ?log(4, Format, Args)).
-define(error_exception(Class, Reason, Stacktrace), ?log_exception(4, "", [], undefined, Class, Reason, Stacktrace)).
-define(error_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(4, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(error_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(4, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(critical(Message), ?log(5, Message, [])).
-define(critical(Format, Args), ?log(5, Format, Args)).
-define(critical_exception(Class, Reason, Stacktrace), ?log_exception(5, "", [], undefined, Class, Reason, Stacktrace)).
-define(critical_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(5, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(critical_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(5, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(alert(Message), ?log(6, Message, [])).
-define(alert(Format, Args), ?log(6, Format, Args)).
-define(alert_exception(Class, Reason, Stacktrace), ?log_exception(6, "", [], undefined, Class, Reason, Stacktrace)).
-define(alert_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(6, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(alert_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(6, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).

-define(emergency(Message), ?log(7, Message, [])).
-define(emergency(Format, Args), ?log(7, Format, Args)).
-define(emergency_exception(Class, Reason, Stacktrace), ?log_exception(7, "", [], undefined, Class, Reason, Stacktrace)).
-define(emergency_exception(DetailsMessage, Class, Reason, Stacktrace), ?log_exception(7, DetailsMessage, [], undefined, Class, Reason, Stacktrace)).
-define(emergency_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace), ?log_exception(7, DetailsFormat, DetailsArgs, undefined, Class, Reason, Stacktrace)).


% DEPRECATED - use ?error_exception instead
% to be removed when occurrences of ?error_stacktrace are pruned from code
-define(error_stacktrace(DetailsMessage, Stacktrace), ?error_stacktrace(DetailsMessage, [], Stacktrace)).
-define(error_stacktrace(DetailsFormat, DetailsArgs, Stacktrace),
    ?wrap_in_loglevel_check(4, onedata_logger:dispatch_log(
        4,
        ?gather_metadata,
        "An unexpected exception occurred in ~w:~w/~B line ~B~n"
        "> Stacktrace:~s"
        "~s",
        [
            ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
            lager:pr_stacktrace(Stacktrace),
            ?print_details_log_suffix(DetailsFormat, DetailsArgs)
        ]
    ))
).


-define(log(LoglevelInt, Format, Args),
    ?wrap_in_loglevel_check(LoglevelInt, onedata_logger:dispatch_log(LoglevelInt, ?gather_metadata, Format, Args))
).

% by default, all exceptions are logged on 'error' level
-define(log_exception(DetailsFormat, DetailsArgs, Ref, Class, Reason, Stacktrace),
    ?log_exception(4, DetailsFormat, DetailsArgs, Ref, Class, Reason, Stacktrace)
).
% A Ref (string) can optionally be passed for easier log navigation - as long
% as the Ref is then somehow identifiable, e.g. as in ?ERROR_INTERNAL_SERVER_ERROR(Ref).
-define(log_exception(LoglevelInt, DetailsFormat, DetailsArgs, Ref, Class, Reason, Stacktrace),
    ?wrap_in_loglevel_check(LoglevelInt, onedata_logger:dispatch_log(
        LoglevelInt,
        ?gather_metadata,
        "An unexpected exception~s occurred in ~w:~w/~B line ~B~n"
        "> Caught: ~s:~p~n"
        "> Stacktrace:~s"
        "~s",
        [
            case Ref of
                undefined -> "";
                _ -> str_utils:format(" (ref: ~s)", [Ref])
            end,
            ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
            Class, Reason,
            lager:pr_stacktrace(Stacktrace),
            ?print_details_log_suffix(DetailsFormat, DetailsArgs)
        ]
    ))
).


% Macro intended as a UNIVERSAL way to report internal server errors that are not caused by
% an exception, but are a result of handling anticipated errors (those that are not
% desired and should be reported as a problem and reflected in the application logs).
-define(report_internal_server_error(Message), ?report_internal_server_error(Message, [])).
-define(report_internal_server_error(DetailsFmt, DetailsArgs), begin
    ((fun(ErrorRef) ->
        ?error(
            "An error (ref: ~s) occurred in ~w:~w/~B line ~B"
            "~s",
            [
                ErrorRef, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
                ?print_details_log_suffix(DetailsFmt, DetailsArgs)
            ]
        ),
        ?ERROR_INTERNAL_SERVER_ERROR(ErrorRef)
    end)(str_utils:rand_hex(5)))
end).


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
% The DetailsMessage or DetailsFmt+DetailsArgs arguments can be optionally
% passed to extend the log with some additional information.
-define(examine_exception(Class, Reason, Stacktrace),
    ?examine_exception("", Class, Reason, Stacktrace)
).
-define(examine_exception(DetailsMessage, Class, Reason, Stacktrace),
    ?examine_exception(DetailsMessage, [], Class, Reason, Stacktrace)
).
% use 'autoformat' atom as DetailsFmt for automatic formatting of DetailsArgs
-define(examine_exception(DetailsFmt, DetailsArgs, Class, Reason, Stacktrace), begin
    ((fun(ErrorRef) ->
        case {Class, Reason} of
            {throw, {error, _}} ->
                Reason;
            _ ->
                ?log_exception(DetailsFmt, DetailsArgs, ErrorRef, Class, Reason, Stacktrace),
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


% Prints bad request warning (frequently used in gen_servers)
-define(log_bad_request(Request),
    ?warning("~w:~B - bad request ~p", [?MODULE, ?LINE, Request])
).


% Prints abnormal termination warning
-define(log_terminate(Reason, State),
    case Reason of
        normal -> ok;
        shutdown -> ok;
        {shutdown, _} -> ok;
        _ -> ?warning("~w terminated in state ~p~nReason: ~p", [?MODULE, State, Reason])
    end
).


% Convenience macros for debug

% Prints a single term by the name of the variable
-define(dump(Term), io:format(user, "[DUMP] ~s: ~p~n~n", [??Term, Term])).

% Prints a list of terms
-define(dump_all(Terms), io:format(user, "[DUMP ALL]~n" ++ ?make_autoformat_string(Terms) ++ "~n~n", Terms)).


%% Macros used internally

-define(make_autoformat_string(Terms),
    lists:flatten(lists:join("~n", lists:map(fun(TermName) ->
        "    " ++ TermName ++ " = ~p"
    end, string:tokens(??Terms, "[] ,"))))
).

-define(print_details_log_suffix(Format, Args),
    case Format of
        "" -> "";
        autoformat -> str_utils:format("~n> Details:~n" ++ ?make_autoformat_string(Args), Args);
        _ -> str_utils:format("~n> Details: " ++ Format, Args)
    end
).

-define(wrap_in_loglevel_check(LoglevelInt, Term),
    case onedata_logger:should_log(LoglevelInt) of
        false ->
            ok;
        true ->
            Term
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
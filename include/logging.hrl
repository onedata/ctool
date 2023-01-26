%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains convenient logging macros.
%%% lager application must be started for them to work.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, 1).

% Convienience macros for development purposes

% Prints bad request warning (frequently used in gen_servers)
-define(log_bad_request(Request),
    ?do_log(3, "~p:~p - bad request ~p", [?MODULE, ?LINE, Request], false)
).

% Prints abnormal termination warning
-define(log_terminate(Reason, State),
    case Reason of
        normal -> ok;
        shutdown -> ok;
        {shutdown, _} -> ok;
        _ ->
            ?do_log(3, "~p terminated in state ~p due to: ~p", [?MODULE, State, Reason], false)
    end
).

% Prints a single variable
-define(dump(_Arg), io:format(user, "[DUMP] ~s: ~p~n~n", [??_Arg, _Arg])).

% Prints a list of variables
-define(dump_all(_ListOfVariables),
    lists:foreach(
        fun({_Name, _Value}) ->
            io:format(user, "[DUMP] ~s: ~p~n~n", [_Name, _Value])
        end, lists:zip(string:tokens(??_ListOfVariables, "[] ,"), _ListOfVariables))
).

%% Macros used internally

-define(do_log(_LoglevelAsInt, _Message, _IncludeStackTrace),
    ?do_log(_LoglevelAsInt, _Message, [], _IncludeStackTrace)
).

-define(do_log(_LoglevelAsInt, _Format, _Args, _IncludeStackTrace),
    case logger:should_log(_LoglevelAsInt) of
        false -> ok;
        true ->
            logger:dispatch_log(_LoglevelAsInt, ?gather_metadata, _Format, _Args, _IncludeStackTrace)
    end
).

-define(do_log(_LoglevelAsInt, _Message, _Class, _Reason, _IncludeStackTrace),
    ?do_log(_LoglevelAsInt, _Message ++ "~nCaught: ~w:~p", [_Class, _Reason], _IncludeStackTrace)
).

% Resolves current process's state and returns it as metadata proplist
% Must be called from original function where the log is,
% so that the process info makes sense
-define(gather_metadata,
    [{pid, self()}, {line, ?LINE}] ++
    logger:parse_process_info(process_info(self(), current_function))
).

% List of available loglevels in cluster
-define(CLUSTER_LOGLEVELS, [debug, info, notice, warning, error, critical, alert, emergency]).
% Available loglevels in clients
-define(CLIENT_LOGLEVELS, [debug, info, warning, error, fatal]).
% Client loglevel to discard all logs
-define(CLIENT_LOGLEVEL_NONE, none).

% Macros that should be used in code for logging.
% Developers are encouraged to use the variants that accept
% exception class and reason and format it in a standard way, unless
% a custom log format is required. In such a case, they must avoid
% using the `~p` formatter at the end of the line to avoid large indents.

% xxx_stacktrace logs will automatically include stack trace,
% provided the env variable 'include_stacktrace' is set to true

% Compilation with skip_debug flag will remove all debug messages from code

-ifdef(skip_debug).
-define(debug(_Message), ok).
-define(debug(_Format, _Args), ok).
-define(debug_stacktrace(_Message), ok).
-define(debug_stacktrace(_Format, _Args), ok).
-endif.

-ifndef(skip_debug).
-define(debug(_Message), ?do_log(0, _Message, false)).
-define(debug(_Format, _Args), ?do_log(0, _Format, _Args, false)).
-define(debug(_Message, _Class, _Reason), ?do_log(0, _Message, _Class, _Reason, false)).
-define(debug_stacktrace(_Message), ?do_log(0, _Message, true)).
-define(debug_stacktrace(_Format, _Args), ?do_log(0, _Format, _Args, true)).
-define(debug_stacktrace(_Message, _Class, _Reason), ?do_log(0, _Message, _Class, _Reason, true)).
-endif.

-define(info(_Message), ?do_log(1, _Message, false)).
-define(info(_Format, _Args), ?do_log(1, _Format, _Args, false)).
-define(info(_Message, _Class, _Reason), ?do_log(1, _Message, _Class, _Reason, false)).
-define(info_stacktrace(_Message), ?do_log(1, _Message, true)).
-define(info_stacktrace(_Format, _Args), ?do_log(1, _Format, _Args, true)).
-define(info_stacktrace(_Message, _Class, _Reason), ?do_log(1, _Message, _Class, _Reason, true)).

-define(notice(_Message), ?do_log(2, _Message, false)).
-define(notice(_Format, _Args), ?do_log(2, _Format, _Args, false)).
-define(notice(_Message, _Class, _Reason), ?do_log(2, _Message, _Class, _Reason, false)).
-define(notice_stacktrace(_Message), ?do_log(2, _Message, true)).
-define(notice_stacktrace(_Format, _Args), ?do_log(2, _Format, _Args, true)).
-define(notice_stacktrace(_Message, _Class, _Reason), ?do_log(2, _Message, _Class, _Reason, true)).

-define(warning(_Message), ?do_log(3, _Message, false)).
-define(warning(_Format, _Args), ?do_log(3, _Format, _Args, false)).
-define(warning(_Message, _Class, _Reason), ?do_log(3, _Message, _Class, _Reason, false)).
-define(warning_stacktrace(_Message), ?do_log(3, _Message, true)).
-define(warning_stacktrace(_Format, _Args), ?do_log(3, _Format, _Args, true)).
-define(warning_stacktrace(_Message, _Class, _Reason), ?do_log(3, _Message, _Class, _Reason, true)).

-define(error(_Message), ?do_log(4, _Message, false)).
-define(error(_Format, _Args), ?do_log(4, _Format, _Args, false)).
-define(error(_Message, _Class, _Reason), ?do_log(4, _Message, _Class, _Reason, false)).
-define(error_stacktrace(_Message), ?do_log(4, _Message, true)).
-define(error_stacktrace(_Format, _Args), ?do_log(4, _Format, _Args, true)).
-define(error_stacktrace(_Message, _Class, _Reason), ?do_log(4, _Message, _Class, _Reason, true)).

-define(critical(_Message), ?do_log(5, _Message, false)).
-define(critical(_Format, _Args), ?do_log(5, _Format, _Args, false)).
-define(critical(_Message, _Class, _Reason), ?do_log(5, _Message, _Class, _Reason, false)).
-define(critical_stacktrace(_Message), ?do_log(5, _Message, true)).
-define(critical_stacktrace(_Format, _Args), ?do_log(5, _Format, _Args, true)).
-define(critical_stacktrace(_Message, _Class, _Reason), ?do_log(5, _Message, _Class, _Reason, true)).

-define(alert(_Message), ?do_log(6, _Message, false)).
-define(alert(_Format, _Args), ?do_log(6, _Format, _Args, false)).
-define(alert(_Message, _Class, _Reason), ?do_log(6, _Message, _Class, _Reason, false)).
-define(alert_stacktrace(_Message), ?do_log(6, _Message, true)).
-define(alert_stacktrace(_Format, _Args), ?do_log(6, _Format, _Args, true)).
-define(alert_stacktrace(_Message, _Class, _Reason), ?do_log(6, _Message, _Class, _Reason, true)).

-define(emergency(_Message), ?do_log(7, _Message, false)).
-define(emergency(_Format, _Args), ?do_log(7, _Format, _Args, false)).
-define(emergency(_Message, _Class, _Reason), ?do_log(7, _Message, _Class, _Reason, false)).
-define(emergency_stacktrace(_Message), ?do_log(7, _Message, true)).
-define(emergency_stacktrace(_Format, _Args), ?do_log(7, _Format, _Args, true)).
-define(emergency_stacktrace(_Message, _Class, _Reason), ?do_log(7, _Message, _Class, _Reason, true)).

-endif.
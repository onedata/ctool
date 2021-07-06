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
-define(dump(_Arg), io:format(user, "[DUMP] ~s: ~p~n~n", [??_Arg, _Arg])).

% Prints a list of variables
-define(dump_all(_ListOfVariables),
    lists:foreach(
        fun({_Name, _Value}) ->
            io:format(user, "[DUMP] ~s: ~p~n~n", [_Name, _Value])
        end, lists:zip(string:tokens(??_ListOfVariables, "[] ,"), _ListOfVariables))
).

%% Macros used internally

-define(do_log(_LoglevelAsInt, _Message, _Stacktrace),
    ?do_log(_LoglevelAsInt, _Message, [], _Stacktrace)
).

-define(do_log(_LoglevelAsInt, _Format, _Args, _Stacktrace),
    case onedata_logger:should_log(_LoglevelAsInt) of
        false -> ok;
        true ->
            onedata_logger:dispatch_log(_LoglevelAsInt, ?gather_metadata, _Format, _Args, _Stacktrace)
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

% Macros that should be used in code for logging
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
-define(debug(_Message), ?do_log(0, _Message, undefined)).
-define(debug(_Format, _Args), ?do_log(0, _Format, _Args, undefined)).
-define(debug_stacktrace(_Message, _Stacktrace), ?do_log(0, _Message, _Stacktrace)).
-define(debug_stacktrace(_Format, _Args, _Stacktrace), ?do_log(0, _Format, _Args, _Stacktrace)).
-endif.

-define(info(_Message), ?do_log(1, _Message, undefined)).
-define(info(_Format, _Args), ?do_log(1, _Format, _Args, undefined)).
-define(info_stacktrace(_Message, _Stacktrace), ?do_log(1, _Message, _Stacktrace)).
-define(info_stacktrace(_Format, _Args, _Stacktrace), ?do_log(1, _Format, _Args, _Stacktrace)).

-define(notice(_Message), ?do_log(2, _Message, undefined)).
-define(notice(_Format, _Args), ?do_log(2, _Format, _Args, undefined)).
-define(notice_stacktrace(_Message, _Stacktrace), ?do_log(2, _Message, _Stacktrace)).
-define(notice_stacktrace(_Format, _Args, _Stacktrace), ?do_log(2, _Format, _Args, _Stacktrace)).

-define(warning(_Message), ?do_log(3, _Message, undefined)).
-define(warning(_Format, _Args), ?do_log(3, _Format, _Args, undefined)).
-define(warning_stacktrace(_Message, _Stacktrace), ?do_log(3, _Message, _Stacktrace)).
-define(warning_stacktrace(_Format, _Args, _Stacktrace), ?do_log(3, _Format, _Args, _Stacktrace)).

-define(error(_Message), ?do_log(4, _Message, undefined)).
-define(error(_Format, _Args), ?do_log(4, _Format, _Args, undefined)).
-define(error_stacktrace(_Message, _Stacktrace), ?do_log(4, _Message, _Stacktrace)).
-define(error_stacktrace(_Format, _Args, _Stacktrace), ?do_log(4, _Format, _Args, _Stacktrace)).

-define(critical(_Message), ?do_log(5, _Message, undefined)).
-define(critical(_Format, _Args), ?do_log(5, _Format, _Args, undefined)).
-define(critical_stacktrace(_Message, _Stacktrace), ?do_log(5, _Message, _Stacktrace)).
-define(critical_stacktrace(_Format, _Args, _Stacktrace), ?do_log(5, _Format, _Args, _Stacktrace)).

-define(alert(_Message), ?do_log(6, _Message, undefined)).
-define(alert(_Format, _Args), ?do_log(6, _Format, _Args, undefined)).
-define(alert_stacktrace(_Message, _Stacktrace), ?do_log(6, _Message, _Stacktrace)).
-define(alert_stacktrace(_Format, _Args, _Stacktrace), ?do_log(6, _Format, _Args, _Stacktrace)).

-define(emergency(_Message), ?do_log(7, _Message, undefined)).
-define(emergency(_Format, _Args), ?do_log(7, _Format, _Args, undefined)).
-define(emergency_stacktrace(_Message, _Stacktrace), ?do_log(7, _Message, _Stacktrace)).
-define(emergency_stacktrace(_Format, _Args, _Stacktrace), ?do_log(7, _Format, _Args, _Stacktrace)).

-endif.
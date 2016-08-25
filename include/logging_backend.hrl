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

-ifndef(LOGGING_BACKEND_HRL).
-define(LOGGING_BACKEND_HRL, 1).

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

% Resolves current process's state and returns it as metadata proplist
% Must be called from original function where the log is,
% so that the process info makes sense
-define(gather_metadata,
    [{pid, self()}, {line, ?LINE}] ++
    logger:parse_process_info(process_info(self(), current_function))
).

-endif.
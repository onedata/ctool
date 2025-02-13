%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2013-2023 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module covers logging utilities and management of loglevel.
%%% @end
%%%-------------------------------------------------------------------

-module(onedata_logger).

-include("global_definitions.hrl").
-include("logging.hrl").
-include_lib("kernel/include/logger.hrl").

-export([format_generic_log/2, format_exception_log/10,
    format_deprecated_exception_log/7, format_error_report/7]).
-export([should_log/1, log/3, log_with_rotation/4]).
-export([set_loglevel/1, set_console_loglevel/1]).
-export([get_current_loglevel/0, get_default_loglevel/0, get_console_loglevel/0]).
-export([loglevel_int_to_atom/1, loglevel_atom_to_int/1]).
-export([is_printable/1]).
-export([pr_stacktrace/1, pr_stacktrace/2]).
-export([configure_logger/0]).

-type autoformat_spec() :: #autoformat_spec{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec format_generic_log(string() | autoformat_spec(), list()) -> string().
format_generic_log(#autoformat_spec{} = AutoformatSpec, []) ->
    {DetailsFormat, DetailsArgs} = autoformat_spec_to_format_and_args(AutoformatSpec),
    format_generic_log(DetailsFormat, DetailsArgs);
format_generic_log(#autoformat_spec{} = AutoformatSpec, _List) ->
    ?warning(
        "Bad usage of the ?autoformat logging macro - ignoring superfluous format arguments. The log format was: ~ts",
        [AutoformatSpec#autoformat_spec.format]
    ),
    format_generic_log(AutoformatSpec, []);
format_generic_log(Format, Args) ->
    str_utils:format(Format, Args).


-spec format_exception_log(
    module(), atom(), non_neg_integer(), non_neg_integer(),
    string() | autoformat_spec(), list(), undefined | string() | binary(),
    atom(), term(), stacktrace()
) -> string().
format_exception_log(
    Module, Function, Arity, Line,
    DetailsFormat, DetailsArgs, Ref,
    Class, Reason, Stacktrace
) ->
    format_generic_log(
        "An unexpected exception~ts occurred in ~w:~w/~B line ~B~n"
        "> Stacktrace:~ts~n"
        "> Caught: ~ts:~tp"
        "~ts",
        [
            case Ref of
                undefined -> "";
                _ -> str_utils:format(" (ref: ~ts)", [Ref])
            end,
            Module, Function, Arity, Line,
            pr_stacktrace(Stacktrace),
            Class, Reason,
            format_details_suffix(DetailsFormat, DetailsArgs)
        ]
    ).


-spec format_deprecated_exception_log(
    module(), atom(), non_neg_integer(), non_neg_integer(),
    string() | autoformat_spec(), list(), stacktrace()
) -> string().
format_deprecated_exception_log(
    Module, Function, Arity, Line,
    DetailsFormat, DetailsArgs, Stacktrace
) ->
    format_generic_log(
        "An unexpected exception occurred in ~w:~w/~B line ~B~n"
        "> Stacktrace:~ts"
        "~ts",
        [
            Module, Function, Arity, Line,
            pr_stacktrace(Stacktrace),
            format_details_suffix(DetailsFormat, DetailsArgs)
        ]
    ).


-spec format_error_report(
    module(), atom(), non_neg_integer(), non_neg_integer(),
    string() | autoformat_spec(), list(), undefined | string() | binary()
) -> string().
format_error_report(
    Module, Function, Arity, Line,
    DetailsFormat, DetailsArgs, Ref
) ->
    format_generic_log(
        "An error (ref: ~ts) occurred in ~w:~w/~B line ~B"
        "~ts",
        [
            Ref, Module, Function, Arity, Line,
            format_details_suffix(DetailsFormat, DetailsArgs)
        ]
    ).

%%--------------------------------------------------------------------
%% @doc Determines if logs with provided loglevel should be logged or discarded.
%%--------------------------------------------------------------------
-spec should_log(LoglevelAsInt :: integer()) -> boolean().
should_log(LevelAsInt) ->
    case get_current_loglevel() of
        Int when LevelAsInt =< Int -> true;
        _ -> false
    end.


-spec log(LoglevelAsInt :: integer(), Metadata :: map(), FormattedLog :: string()) -> ok.
log(LoglevelAsInt, Metadata, FormattedLog) ->
    Severity = loglevel_int_to_atom(LoglevelAsInt),

    % the reformatting with 't' modifier ensures that special characters are properly handled
    logger:log(Severity, "~ts", [FormattedLog], Metadata#{
        color => severity_to_color(Severity), reset => "\e[0m"
    }).


%%--------------------------------------------------------------------
%% @doc Changes current global loglevel to desired. Argument can be loglevel as int or atom
%% 'default' atom can be used to set it back to default
%% @end
%%--------------------------------------------------------------------
-spec set_loglevel(Loglevel :: integer() | atom()) -> ok | {error, badarg}.
set_loglevel(Loglevel) when is_atom(Loglevel) ->
    try
        LevelAsInt = case Loglevel of
            default -> get_default_loglevel();
            Atom -> loglevel_atom_to_int(Atom)
        end,
        set_loglevel(LevelAsInt)
    catch _:_ ->
        {error, badarg}
    end;

set_loglevel(Loglevel) when is_integer(Loglevel) andalso (Loglevel >= 0) andalso (Loglevel =< 7) ->
    ctool:set_env(current_loglevel, Loglevel);

set_loglevel(_) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc Changes current console loglevel to desired. Argument can be loglevel as int or atom
%% 'default' atom can be used to set it back to default - default is what is defined in sys.config
%% @end
%%--------------------------------------------------------------------
-spec set_console_loglevel(Loglevel :: integer() | atom()) -> ok | {error, badarg}.
set_console_loglevel(Loglevel) when is_integer(Loglevel) andalso (Loglevel >= 0) andalso (Loglevel =< 7) ->
    set_console_loglevel(loglevel_int_to_atom(Loglevel));

set_console_loglevel(Loglevel) when is_atom(Loglevel) ->
    try
        LevelAsAtom = case Loglevel of
            default ->
                {ok, Config} = logger:get_handler_config(logger_console_backend),
                maps:get(level, Config);
            Atom ->
                % Makes sure that the atom is recognizable as loglevel
                loglevel_int_to_atom(loglevel_atom_to_int(Atom))
        end,
        logger:set_handler_config(logger_console_backend, #{level => LevelAsAtom}),
        ok
    catch _:_ ->
        {error, badarg}
    end;

set_console_loglevel(_) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc Returns current loglevel as set in application's env
%% @end
%%--------------------------------------------------------------------
-spec get_current_loglevel() -> integer().
get_current_loglevel() ->
    ctool:get_env(current_loglevel, 6).

%%--------------------------------------------------------------------
%% @doc Returns default loglevel as set in application's env
%% @end
%%--------------------------------------------------------------------
-spec get_default_loglevel() -> integer().
get_default_loglevel() ->
    ctool:get_env(default_loglevel, 6).

%%--------------------------------------------------------------------
%% @doc Returns current console loglevel
%% @end
%%--------------------------------------------------------------------
-spec get_console_loglevel() -> integer().
get_console_loglevel() ->
    {ok, Config} = logger:get_handler_config(logger_console_backend),
    loglevel_atom_to_int(maps:get(level, Config)).

%%--------------------------------------------------------------------
%% @doc Returns loglevel name associated with loglevel number
%% @end
%%--------------------------------------------------------------------
-spec loglevel_int_to_atom(LoglevelAsInt :: integer()) -> atom().
loglevel_int_to_atom(7) -> debug;
loglevel_int_to_atom(6) -> info;
loglevel_int_to_atom(5) -> notice;
loglevel_int_to_atom(4) -> warning;
loglevel_int_to_atom(3) -> error;
loglevel_int_to_atom(2) -> critical;
loglevel_int_to_atom(1) -> alert;
loglevel_int_to_atom(0) -> emergency.

%%--------------------------------------------------------------------
%% @doc Returns loglevel number associated with loglevel name
%% @end
%%--------------------------------------------------------------------
-spec loglevel_atom_to_int(LoglevelAsAtom :: atom()) -> integer().
loglevel_atom_to_int(debug) -> 7;
loglevel_atom_to_int(info) -> 6;
loglevel_atom_to_int(notice) -> 5;
loglevel_atom_to_int(warning) -> 4;
loglevel_atom_to_int(error) -> 3;
loglevel_atom_to_int(critical) -> 2;
loglevel_atom_to_int(alert) -> 1;
loglevel_atom_to_int(emergency) -> 0.

%%--------------------------------------------------------------------
%% @doc Logs given message to LogFile.
%% If size of LogFile exceeds MaxSize, its name will be appended with
%% suffix ".1". Previous suffixed LogFile will be deleted, if it exists.
%% @end
%%--------------------------------------------------------------------
-spec log_with_rotation(LogFile :: string(),
    Format :: io:format(), Args :: [term()], MaxSize :: non_neg_integer()) -> ok.
log_with_rotation(LogFile, Format, Args, MaxSize) ->
    DateTime = calendar:system_time_to_rfc3339(logger:timestamp(), [{unit, microsecond}]),

    case filelib:file_size(LogFile) > MaxSize of
        true ->
            LogFile2 = LogFile ++ ".1",
            file:delete(LogFile2),
            file:rename(LogFile, LogFile2),
            ok;
        _ ->
            ok
    end,
    file:write_file(LogFile,
        io_lib:format("~n~ts: " ++ Format, [DateTime | Args]), [append]),
    ok.


-spec pr_stacktrace(stacktrace()) -> stacktrace().
pr_stacktrace(Stacktrace) ->
    Indent = "\n    ",
    lists:foldl(fun(Entry, Acc) ->
        case Entry of
            {Module, Function, Args, [{file, _File}, {line, Line}]} ->
                Acc ++ Indent ++ io_lib:format("~ts", [format_mfa({Module, Function, Args})]) ++
                    " line " ++ integer_to_list(Line);
            {Module, Function, Args, _} ->
                Acc ++ Indent ++ io_lib:format("~ts", [format_mfa({Module, Function, Args})]);
            _ ->
                Acc ++ Indent ++ io_lib:format("~tp", [Entry])
        end
    end, [], lists:reverse(Stacktrace)).


-spec pr_stacktrace(stacktrace(), {atom(), term()}) -> stacktrace().
pr_stacktrace(Stacktrace, {Class, Reason}) ->
    pr_stacktrace(Stacktrace) ++  "\n" ++ io_lib:format("~ts:~tp", [Class, Reason]).


-spec configure_logger() -> ok.
configure_logger() ->
    logger:set_primary_config(level, debug),
    logger:add_primary_filter(progress, {fun logger_filters:progress/2, stop}),

    LogDir = ctool:get_env(log_dir),
    Config = #{
        % Maximum events to handle in 1000ms. Exceeding this limit pauses event processing.
        burst_limit_max_count => 200,

        % Threshold for switching to synchronous mode when the log queue exceeds this length.
        % Returns to asynchronous mode when the queue shrinks below this threshold.
        sync_mode_qlen => 500,

        % Logs are ignored when the queue exceeds this length.
        % Normal logging resumes when it shrinks.
        drop_mode_qlen => 1000,

        % When the queue exceeds this threshold, events are discarded in a flush loop.
        % The handler's priority is increased to prevent new events during flush.
        flush_qlen => 2000
    },

    FileFormat = {onedata_logger_formatter, #{
        max_size => 52428800,
        depth => 10,
        template =>  ["[", level, " ", time, " ", pid, "] ", msg, "\n"]
    }},

    Filters = [{file_access_audit_log_disabled, {
        fun onedata_logger_filters:file_access_audit_log_filter/2, stop
    }}],

    case lists:member(debug, logger:get_handler_ids()) of
        true ->
            ok;
        false ->
            logger:add_handler(debug, logger_std_h, #{
                level => debug,
                config => Config#{file => LogDir ++ "/debug.log"},
                filter_default => stop,
                filters => Filters,
                formatter => FileFormat
            })
    end,

    logger:add_handler(console_backend, logger_std_h, #{
        level => info,
        config => Config,
        filter_default => stop,
        filters => Filters,
        formatter => {onedata_logger_formatter, #{
            legacy_header => false,
            single_line => false,
            no_date => true,
            template => [color, "[", level, " ", time, " pid ", pid, "] ", msg, reset, "\n"]
        }}
    }),

    logger:add_handler(error, logger_std_h, #{
        level => error,
        config => Config#{file => LogDir ++ "/error.log"},
        filter_default => stop,
        filters => Filters,
        formatter => FileFormat
    }),

    logger:add_handler(info, logger_std_h, #{
        level => info,
        config => Config#{file => LogDir ++ "/info.log"},
        filter_default => stop,
        filters => Filters,
        formatter => FileFormat
    }).


%%%===================================================================
%%% API
%%%===================================================================


%% @private
-spec severity_to_color(atom()) -> list().
severity_to_color(debug) -> "\e[0;38m";
severity_to_color(info) -> "\e[1;37m";
severity_to_color(notice) -> "\e[1;36m";
severity_to_color(warning) -> "\e[1;33m";
severity_to_color(error) -> "\e[1;31m";
severity_to_color(critical) -> "\e[1;35m";
severity_to_color(alert) -> "\e[1;44m";
severity_to_color(emergency) -> "\e[1;41m".


%% @private
-spec format_mfa({atom(), atom(), list() | integer()} | any()) -> list().
format_mfa({Module, Function, Args}) when is_list(Args) ->
    io_lib:format("~tp:~tp/~tp", [Module, Function, length(Args)]);
format_mfa({Module, Function, Arity}) when is_integer(Arity) ->
    io_lib:format("~tp:~tp/~tp", [Module, Function, Arity]);
format_mfa(Unknown) ->
    io_lib:format("~tp", [Unknown]).


%% @private
-spec format_details_suffix(string() | autoformat_spec(), list()) -> string().
format_details_suffix("", _) ->
    "";
format_details_suffix(#autoformat_spec{} = AutoformatSpec, _DetailsArgs) ->
    {DetailsFormat, DetailsArgs} = autoformat_spec_to_format_and_args(AutoformatSpec),
    format_details_suffix(DetailsFormat, DetailsArgs);
format_details_suffix(DetailsFormat, DetailsArgs) ->
    str_utils:format("~n> Details: " ++ DetailsFormat, DetailsArgs).


-spec is_printable(term()) ->  boolean().
is_printable(Str) when is_list(Str) -> io_lib:printable_list(Str);
is_printable(Str) when is_binary(Str) -> io_lib:printable_list(str_utils:binary_to_unicode_list(Str));
is_printable(_Str) -> false.


%% @private
-spec autoformat_spec_to_format_and_args(autoformat_spec()) -> {string(), list()}.
autoformat_spec_to_format_and_args(#autoformat_spec{
    format = Format,
    args = Args,
    term_names = TermNames,
    term_values = TermValues
}) ->
    DetailsFormat = Format ++ lists:flatten(lists:map(fun({TermName, Term}) ->
        ControlSequence = case is_printable(Term) of
            true -> "~ts";
            false -> "~tp"
        end,
        "~n    " ++ TermName ++  " = " ++ ControlSequence
    end, lists:zip(TermNames, TermValues))),
    {DetailsFormat, Args ++ TermValues}.

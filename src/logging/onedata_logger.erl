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

-export([format_generic_log/2, format_exception_log/10, format_deprecated_exception_log/7]).
-export([format_internal_server_error_report/7]).
-export([should_log/1]).
-export([log/3, log_with_rotation/4]).
-export([set_loglevel/1, set_console_loglevel/1]).
-export([get_current_loglevel/0, get_default_loglevel/0, get_console_loglevel/0]).
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
        "> Class: ~ts~n"
        "> Reason: ~tp"
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


-spec format_internal_server_error_report(
    module(), atom(), non_neg_integer(), non_neg_integer(),
    string() | autoformat_spec(), list(), undefined | string() | binary()
) -> string().
format_internal_server_error_report(
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


-spec should_log(logger:level()) -> boolean().
should_log(LogLevel) ->
    not (lt == logger:compare_levels(LogLevel, get_current_loglevel())).


-spec log(Loglevel :: logger:level(), Metadata :: map(), FormattedLog :: string()) -> ok.
log(Loglevel, Metadata, FormattedLog) ->
    % the reformatting with 't' modifier ensures that special characters are properly handled
    logger:log(Loglevel, "~ts", [FormattedLog], Metadata#{
        color => severity_to_color(Loglevel),
        reset => "\e[0m"
    }).


-spec set_loglevel(logger:level() | default) -> ok | {error, badarg}.
set_loglevel(Loglevel) when is_atom(Loglevel) ->
    set_loglevel_internal(Loglevel, fun(L) -> logger:set_primary_config(level, L) end).


-spec set_console_loglevel(logger:level() | default) -> ok | {error, badarg}.
set_console_loglevel(Loglevel) ->
    set_loglevel_internal(Loglevel, fun(L) -> logger:set_handler_config(console_backend, level, L) end).


-spec get_current_loglevel() -> logger:level().
get_current_loglevel() ->
    maps:get(level, logger:get_primary_config()).


-spec get_default_loglevel() -> logger:level().
get_default_loglevel() ->
    ctool:get_env(default_loglevel, info).


-spec get_console_loglevel() -> logger:level().
get_console_loglevel() ->
    {ok, Config} = logger:get_handler_config(console_backend),
    maps:get(level, Config).


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


-spec pr_stacktrace(stacktrace()) -> string().
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
    % logger_proxy is a process responsible for forwarding logs to remote node based on process group leader.
    % This results in logs being logged both on node executing code and making an rpc call.
    % We do not want this so it is disabled.
    unregister(logger_proxy),

    ok = set_loglevel(default),

    % Progress filter is explained here: https://www.erlang.org/doc/apps/kernel/logger_filters.html#progress/2
    ok = logger:add_primary_filter(progress, {fun logger_filters:progress/2, stop}),

    % Logs originating internally from onedata outside of onedata_logger (which has a domain of []) should
    % be stopped in generic handlers - such logs should have their own handlers.
    % Domain filter is explained here: https://www.erlang.org/doc/apps/kernel/logger_filters.html#domain/2
    HandlerFilters = [{onedata_domain, {fun logger_filters:domain/2, {stop, sub, [onedata]}}}],

    LogDir = ctool:get_env(log_dir),
    Config = ctool:get_env(logger_base_config),
    BaseFileConfig = Config#{
        max_no_bytes => ctool:get_env(logger_max_file_size),
        max_no_files => ctool:get_env(logger_max_file_no)
    },
    
    FileHandler = fun(Level) -> 
        LogFile = "/" ++ atom_to_list(Level) ++ ".log",
        #{
            level => Level,
            filters => HandlerFilters,
            formatter => onedata_logger_formatter:get_config_spec(file),
            config => BaseFileConfig#{file => LogDir ++ LogFile}
        }
    end,

    ok = logger:add_handler(debug, logger_std_h, FileHandler(debug)),
    ok = logger:add_handler(info, logger_std_h, FileHandler(info)),
    ok = logger:add_handler(error, logger_std_h, FileHandler(error)),

    ok = logger:add_handler(console_backend, logger_std_h, #{
        level => get_default_loglevel(),
        config => Config,
        filters => HandlerFilters,
        formatter => onedata_logger_formatter:get_config_spec(console)
    }),
    ok = logger:remove_handler(default).


%%%===================================================================
%%% Internal functions
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


%% @private
-spec set_loglevel_internal(logger:level() | default, fun((logger:level()) -> ok)) -> ok | {error, term()}.
set_loglevel_internal(default, SetFun) ->
    set_loglevel_internal(get_default_loglevel(), SetFun);
set_loglevel_internal(Loglevel, SetFun) ->
    SetFun(Loglevel).

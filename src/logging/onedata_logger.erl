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

-export([format_generic_log/2, format_exception_log/10,
    format_deprecated_exception_log/7, format_error_report/7]).
-export([should_log/1, log/3, parse_process_info/1, log_with_rotation/4]).
-export([set_loglevel/1, set_console_loglevel/1]).
-export([get_current_loglevel/0, get_default_loglevel/0, get_console_loglevel/0]).
-export([loglevel_int_to_atom/1, loglevel_atom_to_int/1]).

-type autoformat_spec() :: #autoformat_spec{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec format_generic_log(string() | autoformat_spec(), list()) -> string().
format_generic_log(#autoformat_spec{
    format = Format,
    args = Args,
    term_names = TermNames,
    term_values = TermValues
}, _List) ->
    format_generic_log(
        Format ++ lists:flatten(lists:map(fun({TermName, Term}) ->
            "~n    " ++ TermName ++  " = " ++
                case ?is_printable(Term) of true -> "~ts"; false -> "~tp" end
        end, lists:zip(TermNames, TermValues))),
        Args ++ TermValues
    );
format_generic_log(Format, Args) ->
    str_utils:format(Format, Args).


-spec format_exception_log(
    module(), atom(), non_neg_integer(), non_neg_integer(),
    string(), list(), undefined | string() | binary(),
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
            lager:pr_stacktrace(Stacktrace),
            Class, Reason,
            format_details_suffix(DetailsFormat, DetailsArgs)
        ]
    ).


-spec format_deprecated_exception_log(
    module(), atom(), non_neg_integer(), non_neg_integer(),
    string(), list(), stacktrace()
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
            lager:pr_stacktrace(Stacktrace),
            format_details_suffix(DetailsFormat, DetailsArgs)
        ]
    ).


-spec format_error_report(
    module(), atom(), non_neg_integer(), non_neg_integer(),
    string(), list(), undefined | string() | binary()
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

-spec log(LoglevelAsInt :: integer(), Metadata :: [tuple()], FormattedLog :: string()) ->
    ok | {error, lager_not_running}.
log(LoglevelAsInt, Metadata, FormattedLog) ->
    Severity = loglevel_int_to_atom(LoglevelAsInt),
    % the reformatting with 't' modifier ensures that special characters are properly handled
    lager:log(Severity, Metadata, "~ts", [FormattedLog]).

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
                {ok, Proplist} = application:get_env(lager, handlers),
                proplists:get_value(lager_console_backend, Proplist);
            Atom ->
                % Makes sure that the atom is recognizable as loglevel
                loglevel_int_to_atom(loglevel_atom_to_int(Atom))
        end,
        gen_event:call(lager_event, lager_console_backend, {set_loglevel, LevelAsAtom}),
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
    {mask, Mask} = gen_event:call(lager_event, lager_console_backend, get_loglevel),
    % lager_util:mask_to_levels(Mask) returns list of allowed log level, first of
    % which is the lowest loglevel
    loglevel_atom_to_int(lists:nth(1, lager_util:mask_to_levels(Mask))).

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
%% @doc Changes standard 'process_info' tuple into metadata proplist
%% @end
%%--------------------------------------------------------------------
-spec parse_process_info(ProcessInfo :: tuple()) -> [tuple()].
parse_process_info({_, {Module, Function, Arity}}) ->
    [{module, Module}, {function, Function}, {arity, Arity}].


%%--------------------------------------------------------------------
%% @doc Logs given message to LogFile.
%% If size of LogFile exceeds MaxSize, its name will be appended with
%% suffix ".1". Previous suffixed LogFile will be deleted, if it exists.
%% @end
%%--------------------------------------------------------------------
-spec log_with_rotation(LogFile :: string(),
    Format :: io:format(), Args :: [term()], MaxSize :: non_neg_integer()) -> ok.
log_with_rotation(LogFile, Format, Args, MaxSize) ->
    {Date, Time} = lager_util:format_time(lager_util:maybe_utc(
        lager_util:localtime_ms())),

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
        io_lib:format("~n~ts, ~ts: " ++ Format, [Date, Time | Args]), [append]),
    ok.

%%%===================================================================
%%% API
%%%===================================================================

%% @private
-spec format_details_suffix(string(), list()) -> string().
format_details_suffix("", _) ->
    "";
format_details_suffix(DetailsFormat, DetailsArgs) ->
    str_utils:format("~n> Details: " ++ DetailsFormat, DetailsArgs).

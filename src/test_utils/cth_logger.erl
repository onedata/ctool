%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% CT hook module implementing logging functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(cth_logger).
-author("Jakub Kudzia").

-include("global_definitions.hrl").
-include("test/test_utils.hrl").

%% API
%% CTH callbacks

%% initialization
-export([init/2]).
%% prehooks
-export([pre_init_per_suite/3, pre_init_per_testcase/3]).
%% posthooks
-export([post_init_per_group/5, post_end_per_group/5]).
-export([post_init_per_testcase/4, post_end_per_testcase/4]).
-export([post_end_per_suite/4]).

-record(logger_state, {
    suite :: atom(),
    suite_stopwatch :: undefined | stopwatch:instance(),
    stopwatch_by_testcase = #{} :: #{atom() => stopwatch:instance()}
}).
-type logger_state() :: #logger_state{}.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called when hook is being installed.
%% Initializes logger state.
%% @end
%%--------------------------------------------------------------------
-spec init(_Id :: term(), _Opts :: term()) -> {ok, logger_state(), non_neg_integer()}.
init(_Id, _Opts) ->
    {ok, #logger_state{}, ?CTH_LOGGER_PRIORITY}.


-spec pre_init_per_suite(Suite :: term(), Config :: [term()],
    State :: logger_state()) -> {ok, logger_state()}.
pre_init_per_suite(Suite, Config, State) ->
    {Config, State#logger_state{suite = Suite, suite_stopwatch = stopwatch:start()}}.


-spec pre_init_per_testcase(TestCase :: atom(), Config :: [term()],
    State :: logger_state()) -> {[term()], logger_state()}.
pre_init_per_testcase(TestCase, Config, State) ->
    ct_pal_report(State, TestCase, "STARTED"),
    {Config, State}.


-spec post_init_per_group(SuiteName :: atom(), TestCase :: atom(), Config :: [term()],
    Return :: ok | {error | skip, term()}, State :: logger_state()) ->
    {ok | {error | skip, term()}, logger_state()}.
post_init_per_group(_SuiteName, _GroupName, _Config, [_ | _] = Return, State) ->
    % if everything is ok, the Return is the modified Config return by the callback
    {Return, State};

post_init_per_group(_SuiteName, GroupName, _Config, Return, State) ->
    Msg = case Return of
        {'EXIT', {Reason, Stacktrace}} ->
            fmt_log_exception('EXIT', Reason, Stacktrace);
        {failed, {thrown, ThrownTerm}} ->
            fmt_log_throw(ThrownTerm);
        _ ->
            fmt_log_unexpected_return(?MODULE, ?FUNCTION_NAME, Return)
    end,
    ct_pal_report(State, GroupName, "group's init_per_group CRASHED due to:~n~n~ts", [Msg]),
    {Return, State}.


-spec post_end_per_group(SuiteName :: atom(), TestCase :: atom(), Config :: [term()],
    Return :: ok | {error | skip, term()}, State :: logger_state()) ->
    {ok | {error | skip, term()}, logger_state()}.
post_end_per_group(_SuiteName, _GroupName, _Config, ok, State) ->
    {ok, State};

post_end_per_group(_SuiteName, GroupName, _Config, Return = {failed, {_, _, FailureSummary}}, State) ->
    Msg = case FailureSummary of
        {Class, {Reason, Stacktrace}} ->
            fmt_log_exception(Class, Reason, Stacktrace);
        ThrownTerm ->
            fmt_log_throw(ThrownTerm)
    end,
    ct_pal_report(State, GroupName, "group's end_per_group CRASHED due to:~n~n~ts", [Msg]),
    {Return, State};

post_end_per_group(_SuiteName, GroupName, _Config, Return = {error, _}, State) ->
    Msg = case Return of
        {error, {thrown, ThrownTerm}} ->
            fmt_log_throw(ThrownTerm);
        {error, {Reason, Stacktrace}} ->
            fmt_log_exception(unknown, Reason, Stacktrace);
        {error, Reason} ->
            fmt_log_exception(unknown, Reason, undefined)
    end,
    ct_pal_report(State, GroupName, "group's end_per_group CRASHED due to:~n~n~ts", [Msg]),
    {Return, State};

post_end_per_group(_SuiteName, GroupName, _Config, Return, State) ->
    ct_pal_report(State, GroupName, "group's end_per_group DID SOMETHING WE HADN'T FORESEEN:~n~n~ts", [
        fmt_log_unexpected_return(?MODULE, ?FUNCTION_NAME, Return)
    ]),
    {Return, State}.


-spec post_init_per_testcase(TestCase :: atom(), Config :: [term()],
    Return :: ok | {error | skip, term()}, State :: logger_state()) ->
    {ok | {error | skip, term()}, logger_state()}.
post_init_per_testcase(TestCase, _Config, ok, State) ->
    {ok, State#logger_state{
        stopwatch_by_testcase = maps:put(TestCase, stopwatch:start(), State#logger_state.stopwatch_by_testcase)
    }};

post_init_per_testcase(TestCase, _Config, Return, State) ->
    Msg = case Return of
        {skip, {failed, {_, _, {Reason, Stacktrace}}}} ->
            fmt_log_exception(unknown, Reason, Stacktrace);
        {skip, {failed, {_, _, ThrownTerm}}} ->
            fmt_log_throw(ThrownTerm);
        _ ->
            fmt_log_unexpected_return(?MODULE, ?FUNCTION_NAME, Return)
    end,
    ct_pal_report(State, TestCase, "init_per_testcase CRASHED due to:~n~n~ts", [Msg]),
    {Return, State}.


-spec post_end_per_testcase(TestCase :: atom(), Config :: [term()],
    Return :: ok | {error | skip, term()}, State :: logger_state()) ->
    {ok | {error | skip, term()}, logger_state()}.
post_end_per_testcase(TestCase, _Config, ok, State) ->
    Stopwatch = maps:get(TestCase, State#logger_state.stopwatch_by_testcase),
    ct_pal_report(State, TestCase, "PASSED (in ~ts)", [
        fmt_time(stopwatch:read_millis(Stopwatch))
    ]),
    {ok, State};

post_end_per_testcase(TestCase, _Config, Return = {failed, {_, _, FailureSummary}}, State) ->
    Msg = case FailureSummary of
        {Class, {Reason, Stacktrace}} ->
            fmt_log_exception(Class, Reason, Stacktrace);
        ThrownTerm ->
            fmt_log_throw(ThrownTerm)
    end,
    ct_pal_report(State, TestCase, "end_per_testcase CRASHED due to:~n~n~ts", [Msg]),
    {Return, State};

post_end_per_testcase(TestCase, _Config, Return = {error, _}, State) ->
    Msg = case Return of
        {error, {thrown, ThrownTerm}} ->
            fmt_log_throw(ThrownTerm);
        {error, {Reason, Stacktrace}} ->
            fmt_log_exception(unknown, Reason, Stacktrace);
        {error, Reason} ->
            fmt_log_exception(unknown, Reason, undefined)
    end,
    Stopwatch = maps:get(TestCase, State#logger_state.stopwatch_by_testcase),
    ct_pal_report(State, TestCase, "FAILED (in ~ts) due to:~n~n~ts", [
        fmt_time(stopwatch:read_millis(Stopwatch)),
        Msg
    ]),
    {Return, State};

post_end_per_testcase(TestCase, _Config, Return, State) ->
    ct_pal_report(State, TestCase, "DID SOMETHING WE HADN'T FORESEEN:~n~n~ts", [
        fmt_log_unexpected_return(?MODULE, ?FUNCTION_NAME, Return)
    ]),
    {Return, State}.

-spec post_end_per_suite(Suite :: term(), Config :: [term()],
    Return, State :: logger_state()) -> {Return, logger_state()}.
post_end_per_suite(_Suite, _Config, Return, State) ->
    ct_pal_report(State, "total duration:", fmt_time(stopwatch:read_millis(State#logger_state.suite_stopwatch))),
    {Return, State}.


%%%===================================================================
%%% Helpers
%%%===================================================================


%% @private
-spec ct_pal_report(logger_state(), atom() | string(), string()) -> ok.
ct_pal_report(State, TestCaseOrGroupNameOrSubtitle, Msg) ->
    ct_pal_report(State, TestCaseOrGroupNameOrSubtitle, "~ts", [Msg]).

%% @private
-spec ct_pal_report(logger_state(), atom() | string(), string(), [term()]) -> ok.
ct_pal_report(#logger_state{suite = Suite}, TestCaseOrGroupNameOrSubtitle, Format, Args) ->
    ct:pal("[~tp] ~ts " ++ Format, [Suite, TestCaseOrGroupNameOrSubtitle] ++ Args).


%% @private
-spec fmt_log_exception(atom(), term(), stacktrace() | undefined) -> string().
fmt_log_exception(Class, Reason, Stacktrace) ->
    str_utils:format(
        "An unexpected exception occurred:~n"
        "> Stacktrace: ~ts~n"
        "> Class: ~tp~n"
        "> Reason: ~tp", [
            case Stacktrace of
                undefined -> "unknown";
                _ -> lager:pr_stacktrace(Stacktrace)
            end,
            Class,
            Reason
        ]
    ).


%% @private
-spec fmt_log_throw(term()) -> string().
fmt_log_throw(ThrownTerm) ->
    str_utils:format(
        "An uncaught throw occurred:~n"
        "> Thrown: ~tp~n", [
            ThrownTerm
        ]
    ).


%% @private
-spec fmt_log_unexpected_return(module(), atom(), term()) -> string().
fmt_log_unexpected_return(Module, Function, Return) ->
    str_utils:format(
        "Got an unexpected return in ~tp:~tp - consider adding a more specific log here!~n"
        "> Return: ~tp~n", [
            Module, Function,
            Return
        ]
    ).


%% @private
-spec fmt_time(time:millis()) -> string().
fmt_time(MillisTotal) ->
    Minutes = MillisTotal div timer:minutes(1),
    Seconds = (MillisTotal - timer:minutes(Minutes)) div timer:seconds(1),
    MillisRemainder = MillisTotal - timer:minutes(Minutes) - timer:seconds(Seconds),
    str_utils:format("~2..0B:~2..0B.~3..0B", [Minutes, Seconds, MillisRemainder]).
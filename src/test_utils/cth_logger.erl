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

-include("test/test_utils.hrl").

%% API
%% CTH callbacks

%% initialization
-export([init/2]).
%% prehooks
-export([pre_init_per_suite/3, pre_init_per_testcase/3]).
%% posthooks
-export([post_end_per_testcase/4]).

-record(logger_state, {suite}).
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


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called before init_per_suite.
%% Saves current suite name in logger state.
%% @end
%%--------------------------------------------------------------------
-spec pre_init_per_suite(Suite :: term(), Config :: [term()],
    State :: logger_state()) -> {ok, logger_state()}.
pre_init_per_suite(Suite, Config, State) ->
    {Config, State#logger_state{suite = Suite}}.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called before init_per_testcase.
%% Logs testcase name that will be started.
%% @end
%%--------------------------------------------------------------------
-spec pre_init_per_testcase(TestCase :: atom(), Config :: [term()],
    State :: logger_state()) -> {[term()], logger_state()}.
pre_init_per_testcase(TestCase, Config, State = #logger_state{suite = Suite}) ->
    ct:pal("Testcase ~p in suite: ~p STARTED", [TestCase, Suite]),
    {Config, State}.

%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after end_per_testcase.
%% Logs testcase name that was executed.
%% @end
%%--------------------------------------------------------------------
-spec post_end_per_testcase(TestCase :: atom(), Config :: [term()],
    Return :: ok | {error | skip, term()}, State :: logger_state()) ->
    {ok | {error | skip, term()}, logger_state()}.
post_end_per_testcase(TestCase, _Config, ok, State) ->
    ct:pal("Testcase ~p in suite: ~p PASSED", [TestCase, State#logger_state.suite]),
    {ok, State};

post_end_per_testcase(TestCase, _Config, Return = {skip, _}, State) ->
    ct:pal("Testcase ~p in suite: ~p SKIPPED", [TestCase, State#logger_state.suite]),
    {Return, State};

post_end_per_testcase(TestCase, _Config, Return = {error, _}, State) ->
    Msg = case Return of
        {error, {thrown, Reason}} ->
            onedata_logger:format_generic_log("An uncaught throw occurred: ~p", [Reason]);
        {error, {Reason, Stacktrace}} ->
            onedata_logger:format_generic_log(
                "An unexpected error occurred~n"
                "> Stacktrace:~s~n"
                "> Caught: ~p",
                [lager:pr_stacktrace(Stacktrace), Reason]
            );
        {error, Reason} ->
            onedata_logger:format_generic_log(
                "An unexpected exception occurred~n~n~p",
                [Reason]
            )
    end,
    ct:pal("Testcase ~p in suite: ~p FAILED~n~n~s", [TestCase, State#logger_state.suite, Msg]),
    {Return, State};

post_end_per_testcase(TestCase, _Config, Return, State) ->
    ct:pal("Testcase ~p in suite: ~p RETURNED: ~p", [
        TestCase, State#logger_state.suite, Return
    ]),
    {Return, State}.

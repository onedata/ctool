%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% CT hook responsible for running posthooks on started environment
%%% with running mock_managers on each node. This hook must be started
%%% after cth_env_up and cth_mock_manager
%%% @end
%%%-------------------------------------------------------------------
-module(cth_posthook).
-author("Jakub Kudzia").

%% API
%% CTH callback
%% initialization
-export([init/2]).
%% posthooks
-export([post_init_per_suite/4]).

-include("test/test_utils.hrl").

-record(state, {disabled=false}).
-type state() :: #state{}.

%%--------------------------------------------------------------------
%% @doc
%% CTH callback called when hook is being installed.
%% Initializes logger state.
%% @end
%%--------------------------------------------------------------------
-spec init(_Id :: term(), _Opts :: term()) -> {ok, state(), non_neg_integer()}.
init(_Id, _Opts) ->
    {ok, #state{}, ?CTH_POSTHOOK_PRIORITY}.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after init_per_suite.
%% Runs posthooks on started environment with running mock_managers
%% on each node.
%% @end
%%--------------------------------------------------------------------
-spec post_init_per_suite(Suite :: atom(), _Config :: [term()], Return :: [term()],
    State :: state()) -> {[term()], state()}.
post_init_per_suite(_Suite, _Config, Return, State) ->
    case ?config(?ENV_UP_POSTHOOK, Return) of
        undefined ->
            {Return, State};
        Posthook ->
            try
                NewConfig = Posthook(Return),
                {NewConfig, State}
            catch
                Class:Reason:Stacktrace ->
                    ?ct_pal_exception("CTH posthook failed", Class, Reason, Stacktrace),
                    erlang:Class(Reason)
            end
    end.

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module executes generic requests and handles possible
%% errors.
%% @end
%% ===================================================================

-module(gr_runner).

-include("logging.hrl").

%% API
-export([run/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% run/2
%% ====================================================================
%% @doc Executes requests and handles possible errors.
-spec run({Module :: atom(), Function :: function(), Arity :: integer()}, RequestBody :: function()) -> Result when
    Result :: term().
%% ====================================================================
run({Module, Function, Arity}, RequestBody) ->
    try
        RequestBody()
    catch
        Reason ->
            %% Manually thrown error, normal interrupt case.
            ?debug_stacktrace("Error in function ~p:~p/~p: ~p", [Module, Function, Arity, Reason]),
            {error, Reason};
        error:{badmatch, {error, Reason}} ->
            %% Bad Match assertion - something went wrong, but it could be expected.
            ?warning("Error in function ~p:~p/~p: ~p", [Module, Function, Arity, Reason]),
            ?debug_stacktrace("Error in function ~p:~p/~p: ~p", [Module, Function, Arity, Reason]),
            {error, Reason};
        error:{case_clause, {error, Reason}} ->
            %% Bad Match assertion - something went seriously wrong and we should know about it.
            ?error_stacktrace("Error in function ~p:~p/~p: ~p", [Module, Function, Arity, Reason]),
            {error, Reason};
        error:UnkError ->
            %% Bad Match assertion - something went horribly wrong. This should not happen.
            ?error_stacktrace("Error in function ~p:~p/~p: ~p", [Module, Function, Arity, UnkError]),
            {error, UnkError}
    end.
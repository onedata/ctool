%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module executes generic requests and handles possible
%%% errors.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_runner).

-include("logging.hrl").

%% API
-export([run/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Executes requests and handles possible errors.
%% @end
%%--------------------------------------------------------------------
-spec run({Module :: atom(), Function :: fun(), Arity :: integer()},
    RequestBody :: function()) ->
    term().
run({Module, Function, Arity}, RequestBody) ->
    try
        RequestBody()
    catch 
        error:{badmatch, Reason}:Stacktrace ->
            %% Bad Match assertion - something went wrong,
            %% but it could be expected.
            ErrorDetails = get_error_details(Reason),
            ?warning("Error in function ~p:~p/~p: ~p",
                [Module, Function, Arity, ErrorDetails]),
            ?debug_stacktrace("Error in function ~p:~p/~p: ~p",
                [Module, Function, Arity, ErrorDetails], Stacktrace),
            {error, ErrorDetails};
        error:{case_clause, Reason}:Stacktrace ->
            %% Case clause assertion - something went seriously wrong
            %% and we should know about it.
            ErrorDetails = get_error_details(Reason),
            ?error_stacktrace("Error in function ~p:~p/~p: ~p",
                [Module, Function, Arity, ErrorDetails], Stacktrace),
            {error, ErrorDetails};
        error:UnknownError:Stacktrace ->
            %% Unknown error - something went horribly wrong.
            %% This should not happen.
            ?error_stacktrace("Error in function ~p:~p/~p: ~p",
                [Module, Function, Arity, UnknownError], Stacktrace),
            {error, UnknownError};
        _:Reason:Stacktrace ->
            %% Manually thrown error, normal interrupt case.
            ErrorDetails = get_error_details(Reason),
            ?debug_stacktrace("Error in function ~p:~p/~p: ~p",
                [Module, Function, Arity, ErrorDetails], Stacktrace),
            {error, ErrorDetails}
    end.

%%--------------------------------------------------------------------
%% @doc Tries to extract details of given error.
%% @end
%%--------------------------------------------------------------------
-spec get_error_details(Reason :: term()) ->
    term().
get_error_details({error, Reason}) ->
    get_error_details(Reason);

get_error_details({ok, Status, _ResponseHeaders, ResponseBody}) ->
    try
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Error = proplists:get_value(<<"error">>, Proplist, <<"">>),
        ErrorDescription = proplists:get_value(<<"error_description">>,
            Proplist, <<"">>),
        {Status, Error, ErrorDescription}
    catch
        _:_ -> {Status, <<"">>, <<"">>}
    end;

get_error_details(Reason) ->
    Reason.

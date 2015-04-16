%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a rebar plugin responsible for including git metadata in
%%% application's environment variables.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar_git_plugin).
-author("Krzysztof Trzepla").

%% API
-export([post_compile/2, get_git_metadata/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Implementation of rebar post compilation callback.
%% @end
%%--------------------------------------------------------------------
-spec post_compile(Config :: term(), AppFile :: string()) ->
    ok | {error, Reason :: term()}.
post_compile(Config, AppFile) ->
    try
        case rebar_app_utils:is_app_dir() of
            {true, AppFile} ->
                inject_git_metadata(Config, AppFile);
            _ ->
                ok
        end
    catch
        Error:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            rebar_log:log(error, "~s - Failed to set git metadata due to: "
            "~p:~p~nStacktrace:~n~p~n", [?MODULE_STRING, Error, Reason, Stacktrace]),
            {Error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns proplist of repository URL, project name, branch name, tag name and
%% commit hash for git project.
%% @end
%%--------------------------------------------------------------------
-spec get_git_metadata() -> [{Key :: atom(), Value :: string()}].
get_git_metadata() ->
    Url = os:cmd("git config --get remote.origin.url") -- "\n",
    Repository = case lists:reverse(string:tokens(Url, "/.")) of
                     [_, Name | _] -> Name;
                     _ -> ""
                 end,
    [
        {git_repository, Repository},
        {git_branch, os:cmd("git rev-parse --abbrev-ref HEAD") -- "\n"},
        {git_tag, os:cmd("git describe --always --tags") -- "\n"},
        {git_commit, os:cmd("git rev-parse HEAD") -- "\n"}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Injects git metadata to application file.
%% @end
%%--------------------------------------------------------------------
-spec inject_git_metadata(Config :: term(), AppFile :: string()) -> ok.
inject_git_metadata(Config, AppFile) ->
    {_, AppName} = rebar_app_utils:app_name(Config, AppFile),
    EbinAppFile = filename:join("ebin", erlang:atom_to_list(AppName) ++ ".app"),
    {ok, [{application, AppName, Metadata}]} = file:consult(EbinAppFile),
    Env = proplists:get_value(env, Metadata, []),
    GitMetadata = get_git_metadata(),
    NewEnv = Env ++ GitMetadata,
    NewMetadata = lists:keyreplace(env, 1, Metadata, {env, NewEnv}),
    NewAppFile = io_lib:fwrite("~p.\n", [{application, AppName, NewMetadata}]),
    ok = file:write_file(EbinAppFile, NewAppFile).
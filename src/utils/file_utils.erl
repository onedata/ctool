%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for files operations.
%%% @end
%%%--------------------------------------------------------------------
-module(file_utils).
-author("Krzysztof Trzepla").

%% API
-export([read_files/1, recursive_del/1, move/2]).
-export([seconds_since_modification/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of file contents for files located in the provided
%% directory or given as a list of file paths.
%% @end
%%--------------------------------------------------------------------
-spec read_files({dir, Path} | {files, [Path]}) ->
    {ok, Files :: [binary()]} | {error, Reason :: term()} when
    Path :: file:name_all().
read_files({dir, DirPath}) ->
    case file:list_dir(DirPath) of
        {ok, FileNames} ->
            FilePaths = lists:filtermap(fun(FileName) ->
                FilePath = filename:join(DirPath, FileName),
                case filelib:is_regular(FilePath) of
                    true -> {true, FilePath};
                    false -> false
                end
            end, FileNames),
            read_files({files, FilePaths});
        {error, Reason} ->
            {error, Reason}
    end;

read_files({files, FilePaths}) ->
    lists:foldl(fun
        (FilePath, {ok, Files}) ->
            case file:read_file(FilePath) of
                {ok, File} -> {ok, [File | Files]};
                {error, Reason} -> {error, Reason}
            end;
        (_, {error, Reason}) ->
            {error, Reason}
    end, {ok, []}, FilePaths).


%%--------------------------------------------------------------------
%% @doc Removes file or directory at given path,
%% even if the directory is nonempty.
%% @end
%%--------------------------------------------------------------------
-spec recursive_del(Path :: file:name_all()) -> ok | no_return().
recursive_del(Path) ->
    case {filelib:is_file(Path), filelib:is_dir(Path)} of
        {false, _} -> ok;
        {true, false} -> ok = file:delete(Path);
        {true, true} ->
            % contrary to the name, can work on any directory
            ok = mochitemp:rmtempdir(Path)
    end.


%%--------------------------------------------------------------------
%% @doc Moves a file or directory.
%% Full target path (including filename) must be specified,
%% as in file:rename/2. Improves on file:rename by allowing
%% cross-filesystem moves.
%% Source must exist, otherwise enoent is returned.
%% Target must not exist, otherwise eexist is returned.
%% @end
%%--------------------------------------------------------------------
-spec move(From :: Filename, To :: Filename) -> ok | {error, Reason} when
    Filename :: binary() | string() | atom(),
    Reason :: enoent | eexist | string().
move(From, To) ->
    case {filelib:is_file(From), filelib:is_file(To)} of
        {true, false} ->
            Cmd = str_utils:format("mv -T '~s' '~s'", [From, To]),
            case os:cmd(Cmd) of
                "" -> ok;
                Error -> {error, Error}
            end;
        {false, _} -> {error, enoent};
        {_, true} -> {error, eexist}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the number of seconds elapsed since the last modification of
%% given file.
%% @end
%%--------------------------------------------------------------------
-spec seconds_since_modification(file:name_all()) -> {ok, clock:seconds()} | {error, enoent}.
seconds_since_modification(Path) ->
    case filelib:last_modified(Path) of
        0 ->
            {error, enoent};
        LastModified ->
            {ok, clock:timestamp_seconds() - time_format:datetime_to_seconds(LastModified)}
    end.

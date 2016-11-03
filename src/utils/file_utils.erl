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
-export([read_files/1]).

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
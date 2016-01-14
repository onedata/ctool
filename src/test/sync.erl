%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Developer module that allows for easy recompilation and reload of
%%% erlang modules and static GUI files.
%%% @end
%%%-------------------------------------------------------------------
-module(sync).
-author("Lukasz Opiola").

%% ETS name that holds md5 checksums of files
-define(MD5_ETS, md5_ets).

%% Predefined file with config (location relative to including project root).
-define(GUI_CONFIG_LOCATION, "rel/gui.config").

%% API
-export([start/1, ensure_started/1, reset/0]).
-export([track_gui/0, dont_track_gui/0]).
-export([track_dep/1, dont_track_dep/1]).
-export([track_dir/1, dont_track_dir/1]).
-export([track_module/1, dont_track_module/1]).
-export([add_includes/1]).
-export([sync/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the sync service. Before sync can discover any changes,
%% sync:track_* must be used.
%% ProjectSourceDir is the path to project's SOURCE files.
%% @end
%%--------------------------------------------------------------------
-spec start(ProjectSourceDir :: string()) -> ok.
start(ProjectSourceDir) ->
    start_ets(),
    ets_insert(project_dir, str_utils:to_list(ProjectSourceDir)),
    % Resolve all paths to includes
    ProjIncludes = [filename:join(ProjectSourceDir, "include")],
    Deps = find_all_dirs(filename:join(ProjectSourceDir, "deps")),
    DepsIncludes = lists:map(
        fun(DepPath) ->
            filename:join(DepPath, "include")
        end, Deps),
    ets_insert(includes, ProjIncludes ++ DepsIncludes),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Ensures that the sync service is started.
%% @end
%%--------------------------------------------------------------------
-spec ensure_started(ProjectSourceDir :: string()) -> ok.
ensure_started(ProjectSourceDir) ->
    case ets:info(?MD5_ETS) of
        undefined ->
            start(ProjectSourceDir);
        _ ->
            case ets_lookup(project_dir, undefined) of
                ProjectSourceDir ->
                    ok;
                _ ->
                    start(ProjectSourceDir)
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Clears the list of tracked files and cached versions of files.
%% @end
%%--------------------------------------------------------------------
-spec reset() -> ok.
reset() ->
    ProjectDir = ets_lookup(project_dir),
    start(ProjectDir).


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to track all GUI files. Their location is resolved
%% according to gui.config found in the including project.
%% @end
%%--------------------------------------------------------------------
-spec track_gui() -> boolean().
track_gui() ->
    % Make sure ets exists.
    case ensure_ets() of
        false -> false;
        true -> toggle_track_gui(true)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to stop tracking GUI files. Their location is resolved
%% according to gui.config found in the including project.
%% @end
%%--------------------------------------------------------------------
-spec dont_track_gui() -> boolean().
dont_track_gui() ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            toggle_track_gui(false)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to track all erl files of given dependency application (by name).
%% Multiple deps can be given at once.
%% @end
%%--------------------------------------------------------------------
-spec track_dep(DepOrDeps :: atom() | [atom()]) -> boolean().
track_dep(DepOrDeps) ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            Deps = ensure_list_of_strings(DepOrDeps),
            Results = lists:map(
                fun(Dep) ->
                    toggle_track_dir(filename:join(["deps", Dep, "src"]),
                        true)
                end, Deps),
            lists:all(fun(Res) -> Res end, Results)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to stop tracking erl files of given
%% dependency application (by name).
%% Multiple deps can be given at once.
%% @end
%%--------------------------------------------------------------------
-spec dont_track_dep(DepOrDeps :: atom() | [atom()]) -> boolean().
dont_track_dep(DepOrDeps) ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            Deps = ensure_list_of_strings(DepOrDeps),
            Results = lists:map(
                fun(Dep) ->
                    toggle_track_dir(filename:join(["deps", Dep, "src"]),
                        false)
                end, Deps),
            lists:all(fun(Res) -> Res end, Results)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to track all erl files in given directory (by path).
%% Path can be relative or absolute.
%% Multiple paths can be given at once.
%% @end
%%--------------------------------------------------------------------
-spec track_dir(DirOrDirs :: string() | [string()]) -> boolean().
track_dir(DirOrDirs) ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            Dirs = ensure_list_of_strings(DirOrDirs),
            Results = lists:map(
                fun(Dir) ->
                    toggle_track_dir(Dir, true)
                end, Dirs),
            lists:all(fun(Res) -> Res end, Results)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to stop tracking erl files in given directory (by path).
%% Path can be relative or absolute.
%% Multiple paths can be given at once.
%% @end
%%--------------------------------------------------------------------
-spec dont_track_dir(DirOrDirs :: string() | [string()]) -> boolean().
dont_track_dir(DirOrDirs) ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            Dirs = ensure_list_of_strings(DirOrDirs),
            Results = lists:map(
                fun(Dir) ->
                    toggle_track_dir(Dir, false)
                end, Dirs),
            lists:all(fun(Res) -> Res end, Results)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to track given erl module (by name or path).
%% When given by name, the path to the module is resolved automatically.
%% Multiple modules can be given at once.
%% @end
%%--------------------------------------------------------------------
-spec track_module(ModuleOrModules :: string() | [string()]) -> boolean().
track_module(ModuleOrModules) ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            Modules = ensure_list_of_strings(ModuleOrModules),
            Results = lists:map(
                fun(Module) ->
                    toggle_track_module(Module, true)
                end, Modules),
            lists:all(fun(Res) -> Res end, Results)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Causes sync to stop tracking given erl module (by name or path).
%% When given by name, the path to the module is resolved automatically.
%% Multiple modules can be given at once.
%% @end
%%--------------------------------------------------------------------
-spec dont_track_module(ModuleOrModules :: atom() | [atom()]) -> boolean().
dont_track_module(ModuleOrModules) ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            Modules = ensure_list_of_strings(ModuleOrModules),
            Results = lists:map(
                fun(Module) ->
                    toggle_track_module(Module, false)
                end, Modules),
            lists:all(fun(Res) -> Res end, Results)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds a path to include to erl compile opts. Paths to main project includes
%% and deps includes are added automatically, this is useful for instance when
%% adding some includes from native erlang libs.
%% Path can be relative or absolute.
%% Multiple paths can be given at once.
%% @end
%%--------------------------------------------------------------------
-spec add_includes(IncludeOrIncludes :: string() | [string()]) -> boolean().
add_includes(IncludeOrIncludes) ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            IncludesToAdd = ensure_list_of_strings(IncludeOrIncludes),
            Includes = ets_lookup(includes, []),
            ets_insert(includes, Includes ++ IncludesToAdd),
            true
    end.


%%--------------------------------------------------------------------
%% @doc
%% The main part of sync service. When called, sync will scan all tracked files
%% and update the files that have changed.
%% The files are updated in runtime and loaded to the erlang VM.
%% Static files are compiled if needed and copied to release package.
%% The first call to sync will always update all the tracked files.
%% @end
%%--------------------------------------------------------------------
-spec sync() -> boolean().
sync() ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            info_msg("Running sync..."),
            msg("-------------------------------------------", [], ""),
            ProjectDir = ets_lookup(project_dir),
            DirsToRecompile = ets_lookup(dirs, []),
            Includes = ets_lookup(includes, []),

            % Check if GUI is tracked.
            GuiTracked = ets_lookup(track_gui, false),
            UpdateGUIFilesRes =
                case GuiTracked of
                    true ->
                        {ok, GuiCfg} = get_gui_config(),
                        SrcGuiDir = proplists:get_value(source_gui_dir, GuiCfg),
                        {EOK, EUTD, EErr} =
                            update_erl_files(ProjectDir, [SrcGuiDir], Includes),
                        {SOK, SUTD, SErr} =
                            update_gui_static_files(ProjectDir, GuiCfg),
                        {EOK + SOK, EUTD + SUTD, EErr + SErr};
                    false ->
                        {0, 0, 0}
                end,

            % Recompile erl files. If gui.config exists, GUI erl files will
            % be recompiled automatically.
            UpdateErlFilesRes = update_erl_files(
                ProjectDir, DirsToRecompile, Includes),

            % Check the results.
            {GUIOK, GUIUpToDate, GUIError} = UpdateGUIFilesRes,
            {ErlOK, ErlUpToDate, ErlError} = UpdateErlFilesRes,
            OK = GUIOK + ErlOK,
            UpToDate = GUIUpToDate + ErlUpToDate,
            Error = GUIError + ErlError,
            case OK + UpToDate + Error of
                0 ->
                    info_msg("No files are tracked. Use sync:track_* first.");
                _ ->
                    case OK of
                        0 ->
                            ok;
                        _ ->
                            msg(
                                "-------------------------------------------",
                                [], "")
                    end,
                    info_msg("~4.b file(s) were updated", [OK]),
                    info_msg("~4.b file(s) were already up to date",
                        [UpToDate]),
                    info_msg("~4.b file(s) could not be updated", [Error]),
                    case Error of
                        0 ->
                            info_msg("Success!"),
                            true;
                        _ ->
                            error_msg("There were errors."),
                            false
                    end
            end
    end.


%%%===================================================================
%%% Internal funtions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Toggles if GUI files are tracked by sync.
%% @end
%%--------------------------------------------------------------------
-spec toggle_track_gui(Flag :: boolean()) -> boolean().
toggle_track_gui(Flag) ->
    case get_gui_config() of
        {error, enoent} ->
            error_msg("Cannot track GUI: gui.config not found in `~s`.",
                [?GUI_CONFIG_LOCATION]),
            false;
        {ok, _} ->
            case Flag of
                true ->
                    info_msg("Tracking all GUI files");
                false ->
                    info_msg("Untracked GUI files")
            end,
            ets_insert(track_gui, Flag),
            true
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Toggles if given dir is tracked by sync.
%% @end
%%--------------------------------------------------------------------
-spec toggle_track_dir(Path :: string(), Flag :: boolean()) -> boolean().
toggle_track_dir(Path, Flag) ->
    ProjectDir = ets_lookup(project_dir),
    Dirs = ets_lookup(dirs, []),
    DirsWithout = Dirs -- [Path],
    case filelib:is_dir(filename:join([ProjectDir, Path])) of
        true ->
            NewDirs = case Flag of
                          true -> DirsWithout ++ [Path];
                          false -> DirsWithout
                      end,
            ets_insert(dirs, NewDirs),
            case Flag of
                true ->
                    info_msg("Tracking all files in directory `~s`", [Path]);
                false ->
                    info_msg("Untracked files in directory `~s`", [Path])
            end,
            true;
        false ->
            error_msg("Cannot track directory `~s` - it was not found in "
            "the source project dir.", [Path]),
            false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Toggles if given module is tracked by sync.
%% @end
%%--------------------------------------------------------------------
-spec toggle_track_module(PathOrName :: string(), Flag :: boolean()) ->
    boolean().
toggle_track_module(PathOrName, Flag) ->
    ProjectDir = ets_lookup(project_dir),
    Path = case filelib:is_file(PathOrName) of
               true ->
                   PathOrName;
               _ ->
                   case find_all_files(ProjectDir, PathOrName ++ "'.erl'",
                       true) of
                       [] ->
                           error_msg("Cannot track module `~s` - it was not "
                           "found in the source project dir.", [PathOrName]),
                           undefined;
                       [FilePath] ->
                           FilePath
                   end
           end,
    case Path of
        undefined ->
            false;
        _ ->
            FullPath = filename:join([ProjectDir, Path]),
            Files = ets_lookup(files, []),
            FilesWithout = Files -- [FullPath],
            NewFiles = case Flag of
                           true -> [FullPath | FilesWithout];
                           false -> FilesWithout
                       end,
            ets_insert(files, NewFiles),
            case Flag of
                true ->
                    info_msg("Tracking file `~s`", [FullPath]);
                false ->
                    info_msg("Untracked file `~s`", [FullPath])
            end,
            true
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates all erl files that have changed.
%% Returns a triple meaning how many files were updated, did not need updating
%% or could not be updated.
%% @end
%%--------------------------------------------------------------------
-spec update_erl_files(ProjectDir :: string(),
    DirsToRecompile :: [string()], Includes :: [string()]) ->
    {OK :: integer(), UpToDate :: integer(), Error :: integer()}.
update_erl_files(ProjectDir, DirsToRecompile, Includes) ->
    AllIncludes = lists:map(
        fun(DepPath) ->
            {i, DepPath}
        end, Includes),
    % Resolve list of files to recompile
    FilesInDirs = lists:foldl(
        fun(DirPath, Acc) ->
            Files = find_all_files(
                filename:join(ProjectDir, DirPath), "*.erl", false),
            Files ++ Acc
        end, [], DirsToRecompile),

    FilesToCheck = FilesInDirs ++ ets_lookup(files, []),

    % Do the recompilation
    CompilationResults = utils:pmap(
        fun(File) ->
            update_erl_file(File, AllIncludes ++ [report])
        end, FilesToCheck),

    % Count number of successful updates, files that were up to data and
    % fiels that were failed to update.
    lists:foldl(fun(Res, {AccOK, AccUpToDate, AccError}) ->
        case Res of
            true -> {AccOK + 1, AccUpToDate, AccError};
            up_to_date -> {AccOK, AccUpToDate + 1, AccError};
            false -> {AccOK, AccUpToDate, AccError + 1}
        end
    end, {0, 0, 0}, CompilationResults).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates all static GUI files that have changes
%% (including coffee scripts and handlebars templates, which require compiling).
%% Returns a triple meaning how many files were updated, did not need updating
%% or could not be updated.
%% @end
%%--------------------------------------------------------------------
-spec update_gui_static_files(ProjectDir :: string(),
    GuiConfig :: proplists:proplist()) ->
    {OK :: integer(), UpToDate :: integer(), Error :: integer()}.
update_gui_static_files(ProjectDir, GuiConfig) ->
    RelaseStaticFilesDir = proplists:get_value(
        release_static_files_dir, GuiConfig),
    SourceGuiDir = filename:join(
        [ProjectDir, proplists:get_value(source_gui_dir, GuiConfig)]),

    % Returns tuples with source file path, and target file path but relative to
    % RelaseStaticFilesDir.
    SourceFileMappings = lists:map(
        fun(File) ->
            {filename:join([SourceGuiDir, File]), File}
        end, find_all_files(SourceGuiDir, "*", true)),

    % Do the updating
    CompilationResults = utils:pmap(
        fun({SourceFilePath, FileName}) ->
            case filename:extension(FileName) of
                ".erl" ->
                    % Do not copy erl files
                    skip;
                ".coffee" ->
                    % Compile coffee files, place js in release
                    update_coffee_script(SourceFilePath,
                        RelaseStaticFilesDir, FileName);
                ".hbs" ->
                    % Precompile handlebars files, place js in release
                    update_handlebars_template(SourceFilePath,
                        RelaseStaticFilesDir, FileName);
                ".html" ->
                    % Copy html files to static files root
                    update_static_file(SourceFilePath,
                        RelaseStaticFilesDir,
                        filename:basename(SourceFilePath));
                _ ->
                    % Copy all other files 1:1 (path-wise)
                    update_static_file(SourceFilePath,
                        RelaseStaticFilesDir, FileName)
            end
        end, SourceFileMappings),

    % Count number of successful updates, files that were up to data and
    % fiels that were failed to update.
    lists:foldl(fun(Res, {AccOK, AccUpToDate, AccError}) ->
        case Res of
            true -> {AccOK + 1, AccUpToDate, AccError};
            up_to_date -> {AccOK, AccUpToDate + 1, AccError};
            false -> {AccOK, AccUpToDate, AccError + 1};
            skip -> {AccOK, AccUpToDate, AccError}
        end
    end, {0, 0, 0}, CompilationResults).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates an erl file, if needed - compiles it and loads into erlang VM.
%% @end
%%--------------------------------------------------------------------
-spec update_erl_file(File :: string(), CompileOpts :: [term()]) -> boolean().
update_erl_file(File, CompileOpts) ->
    CurrentMD5 = file_md5(File),
    case should_update(File, CurrentMD5) of
        false ->
            up_to_date;
        true ->
            case compile:file(File, CompileOpts) of
                {ok, ModuleName} ->
                    code:purge(ModuleName),
                    code:load_file(ModuleName),
                    update_file_md5(File, CurrentMD5),
                    info_msg("Compiled:  ~s", [filename:basename(File)]),
                    true;
                _ ->
                    false
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a static GUI file, if needed - copies it to release dir.
%% @end
%%--------------------------------------------------------------------
-spec update_static_file(SourceFile :: string(),
    RelaseStaticFilesDir :: string(), FileName :: string()) -> boolean().
update_static_file(SourceFile, RelaseStaticFilesDir, FileName) ->
    TargetPath = filename:join(RelaseStaticFilesDir, FileName),
    CurrentMD5 = file_md5(SourceFile),
    case should_update(SourceFile, CurrentMD5) of
        false ->
            up_to_date;
        true ->
            case shell_cmd(["mkdir -p", filename:dirname(TargetPath)]) of
                [] ->
                    case shell_cmd(["cp -f", SourceFile, TargetPath]) of
                        [] ->
                            info_msg("Updated:   ~s", [abs_path(FileName)]),
                            update_file_md5(SourceFile, CurrentMD5),
                            true;
                        Other1 ->
                            error_msg("Cannot copy ~s: ~s", [FileName, Other1]),
                            false
                    end;
                Other2 ->
                    error_msg("Cannot create dir ~s: ~s",
                        [filename:dirname(FileName), Other2]),
                    false
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a coffee script file, if needed. Compiles it to .js before copying
%% to release dir.
%% @end
%%--------------------------------------------------------------------
-spec update_coffee_script(SourceFile :: string(),
    RelaseStaticFilesDir :: string(), FileName :: string()) -> boolean().
update_coffee_script(SourceFile, RelaseStaticFilesDir, FileName) ->
    TargetPath = filename:join(RelaseStaticFilesDir, FileName),
    TargetDir = filename:dirname(TargetPath),
    CurrentMD5 = file_md5(SourceFile),
    case should_update(SourceFile, CurrentMD5) of
        false ->
            up_to_date;
        true ->
            case shell_cmd(["mkdir -p", TargetDir]) of
                [] ->
                    case shell_cmd(
                        ["coffee", "-o", TargetDir, "-c", SourceFile]) of
                        [] ->
                            JSFile = filename:rootname(FileName) ++ ".js",
                            update_file_md5(SourceFile, CurrentMD5),
                            info_msg("Compiled:  ~s -> ~s",
                                [abs_path(FileName), abs_path(JSFile)]),
                            true;
                        Other ->
                            error_msg("Cannot compile ~s: ~s",
                                [SourceFile, Other]),
                            false
                    end;
                Other2 ->
                    error_msg("Cannot create dir ~s: ~s", [TargetDir, Other2]),
                    false
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a handlebars template, if needed. Compiles it to .js before copying
%% to release dir.
%% @end
%%--------------------------------------------------------------------
-spec update_handlebars_template(SourceFile :: string(),
    RelaseStaticFilesDir :: string(), FileName :: string()) -> boolean().
update_handlebars_template(SourceFile, RelaseStaticFilesDir, FileName) ->
    TargetFileName = filename:rootname(FileName) ++ ".js",
    TargetPath = filename:join([RelaseStaticFilesDir, TargetFileName]),
    TargetDir = filename:dirname(TargetPath),
    CurrentMD5 = file_md5(SourceFile),
    case should_update(SourceFile, CurrentMD5) of
        false ->
            up_to_date;
        true ->
            case shell_cmd(["mkdir -p", TargetDir]) of
                [] ->
                    case shell_cmd(
                        ["ember-precompile", SourceFile, "-f", TargetPath]) of
                        [] ->
                            update_file_md5(SourceFile, CurrentMD5),
                            info_msg("Compiled:  ~s -> ~s",
                                [abs_path(FileName), abs_path(TargetFileName)]),
                            true;
                        Other ->
                            error_msg("Cannot compile ~s: ~s",
                                [SourceFile, Other]),
                            false
                    end;
                Other2 ->
                    error_msg("Cannot create dir ~s: ~s", [TargetDir, Other2]),
                    false
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns parsed GUI config, or error if it does not exist.
%% @end
%%--------------------------------------------------------------------
-spec get_gui_config() -> {ok, proplists:proplist()} | {error, enoent}.
get_gui_config() ->
    ProjectDir = ets_lookup(project_dir),
    GuiConfigPath = filename:join([ProjectDir, ?GUI_CONFIG_LOCATION]),
    case file:consult(GuiConfigPath) of
        {ok, GuiConfig} -> {ok, GuiConfig};
        {error, enoent} -> {error, enoent}
    end.


%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs a shell call given a list of arguments and returns the output.
%% @end
%%--------------------------------------------------------------------
-spec shell_cmd([string()]) -> string().
shell_cmd(List) ->
    os:cmd(string:join(List, " ")).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns absolute path to a file.
%% @end
%%--------------------------------------------------------------------
-spec abs_path(FilePath :: string()) -> string().
abs_path(FilePath) ->
    str_utils:to_list(filename:absname_join("/", FilePath)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts ETS cache.
%% @end
%%--------------------------------------------------------------------
-spec start_ets() -> ok.
start_ets() ->
    case ets:info(?MD5_ETS) of
        undefined ->
            % Start ETS in another process so it won't be deleted
            % if calling process crashes
            spawn(
                fun() ->
                    ets:new(?MD5_ETS, [public, set, protected, named_table,
                        {read_concurrency, true}
                    ]),
                    receive kill -> ok end
                end),
            info_msg("Started new ETS table to track changes in files.");
        _ ->
            ets:delete_all_objects(?MD5_ETS),
            info_msg("Cleared the ETS cache.")
    end,
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if cache ETS exists, if not logs displays an error message.
%% @end
%%--------------------------------------------------------------------
-spec ensure_ets() -> boolean().
ensure_ets() ->
    case ets:info(?MD5_ETS) of
        undefined ->
            error_msg("You must do sync:start/1 before using sync."),
            error_msg("Then, use sync:track_* to specify files to be tracked."),
            false;
        _ ->
            true
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Lookups a Key in cache ETS.
%% @end
%%--------------------------------------------------------------------
-spec ets_lookup(Key :: term()) -> term().
ets_lookup(Key) ->
    ets_lookup(Key, undefined).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Lookups a Key in cache ETS. Returns default value if Key is not found.
%% @end
%%--------------------------------------------------------------------
-spec ets_lookup(Key :: term(), Default :: term()) -> term().
ets_lookup(Key, Default) ->
    case ets:lookup(?MD5_ETS, Key) of
        [{Key, Val}] -> Val;
        _ -> Default
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Inserts a Key - Value pair into cache ETS.
%% @end
%%--------------------------------------------------------------------
-spec ets_insert(Key :: term(), Val :: term()) -> true.
ets_insert(Key, Val) ->
    ets:insert(?MD5_ETS, {Key, Val}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates md5 checksum of given file.
%% @end
%%--------------------------------------------------------------------
-spec file_md5(FilePath :: string()) -> binary().
file_md5(FilePath) ->
    {ok, Bin} = file:read_file(FilePath),
    erlang:md5(Bin).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates MD5 of given file in cache.
%% @end
%%--------------------------------------------------------------------
-spec update_file_md5(FilePath :: string(), CurrentMD5 :: binary()) -> true.
update_file_md5(FilePath, CurrentMD5) ->
    ets_insert(FilePath, CurrentMD5).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Indicates if a file should be updated based on its current MD5
%% (compares it to its last MD5, if it exists in cache).
%% @end
%%--------------------------------------------------------------------
-spec should_update(FilePath :: string(), CurrentMD5 :: binary()) -> boolean().
should_update(FilePath, CurrentMD5) ->
    case ets_lookup(FilePath) of
        CurrentMD5 ->
            false;
        _ ->
            true
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds all files in given directory. Can return paths relative
%% to current directory or not (just names of files).
%% @end
%%--------------------------------------------------------------------
-spec find_all_files(Where :: string(), NameRegexp :: string(),
    RelativePaths :: boolean()) -> [string()].
find_all_files(Where, NameRegexp, RelativePaths) ->
    case RelativePaths of
        false ->
            string:tokens(shell_cmd(
                ["find", Where, "-type f -name", "'" ++ NameRegexp ++ "'"]),
                "\n");
        true ->
            string:tokens(shell_cmd(
                ["cd", Where, "&&", "find . -type f -name",
                        "'" ++ NameRegexp ++ "'"]),
                "\n")
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds all directories in given directory. Returns dir names rather than
%% relative paths to them.
%% @end
%%--------------------------------------------------------------------
-spec find_all_dirs(Where :: string()) -> [string()].
find_all_dirs(Where) ->
    string:tokens(
        shell_cmd(["ls", "-d", Where ++ "/*/"]),
        "\n").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that given term is a list of list, converting it if needed.
%% @end
%%--------------------------------------------------------------------
-spec ensure_list_of_strings(List :: term()) -> [string()].
ensure_list_of_strings(List) ->
    case List of
        [] ->
            [];
        [H | _] when is_list(H) orelse is_atom(H) ->
            [str_utils:to_list(E) || E <- List];
        Other ->
            [str_utils:to_list(Other)]
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prints an info message on the console.
%% @end
%%--------------------------------------------------------------------
-spec info_msg(Message :: string()) -> ok.
info_msg(Message) ->
    msg(Message, [], "[SYNC] ").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prints an info message on the console.
%% @end
%%--------------------------------------------------------------------
-spec info_msg(Format :: string(), Args :: [term()]) -> ok.
info_msg(Format, Args) ->
    msg(Format, Args, "[SYNC] ").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prints an error message on the console.
%% @end
%%--------------------------------------------------------------------
-spec error_msg(Message :: string()) -> ok.
error_msg(Message) ->
    msg(Message, [], "[SYNC ERROR] ").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prints an error message on the console.
%% @end
%%--------------------------------------------------------------------
-spec error_msg(Format :: string(), Args :: [term()]) -> ok.
error_msg(Format, Args) ->
    msg(Format, Args, "[SYNC ERROR] ").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prints a message on the console.
%% @end
%%--------------------------------------------------------------------
-spec msg(Format :: string(), Args :: [term()], Prefix :: string()) -> ok.
msg(Format, Args, Prefix) ->
    io:format("~s~s~n", [Prefix, str_utils:format(Format, Args)]).
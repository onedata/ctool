%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of file utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(file_utils_tests).
-author("Wojciech Geisler").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(DATA_DIR, "./test/" ++ ?MODULE_STRING ++ "_data/").


%%%===================================================================
%%% Test generators
%%%===================================================================


onepanel_env_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun remove_file_test/1,
            fun remove_nonexistent_test/1,
            fun remove_empty_dir_test/1,
            fun remove_nonempty_dir_test/1,
            fun rename_directory_test/1,
            fun rename_file_test/1,
            fun rename_fails_when_target_exists_test/1,
            fun seconds_since_modification_test/1
        ]
    }.

%%%===================================================================
%%% Test functions and instantiators
%%%===================================================================

remove_file_test(Config) ->
    % given
    Name = "filename",
    Path = path(Config, Name),
    touch(Path),
    ?assertEqual(true, filelib:is_file(Path)),

    % when
    ?assertEqual(ok, file_utils:recursive_del(Path)),
    % then
    ?_assertEqual(false, filelib:is_file(Path)).


remove_nonexistent_test(Config) ->
    % given
    Name = "filename",
    Path = path(Config, Name),
    ?assertEqual(false, filelib:is_file(Path)),

    ?_assertEqual(ok, file_utils:recursive_del(Path)).


remove_empty_dir_test(Config) ->
    % given
    DirName = "testdir",
    Path = path(Config, DirName),
    file:make_dir(Path),
    ?assertEqual(true, filelib:is_dir(Path)),
    % when
    ?assertEqual(ok, file_utils:recursive_del(Path)),
    % then
    ?_assertEqual(false, filelib:is_dir(Path)).


remove_nonempty_dir_test(Config) ->
    % given
    Dir = path(Config, "testdir"),
    Subdir = filename:join(Dir, "subdir"),
    File = filename:join(Dir, "file"),
    NestedFile = filename:join(Dir, "nested"),

    ok = file:make_dir(Dir),
    ok = file:make_dir(Subdir),
    ok = touch(File),
    ok = touch(NestedFile),

    % when
    ?assertEqual(ok, file_utils:recursive_del(Dir)),
    % then
    ?_assertEqual(false, filelib:is_dir(Dir)).


rename_directory_test(Config) ->
    Old = path(Config, "olddir"),
    New = path(Config, "newdir"),

    Prepare = fun() ->
        reset_dir(Config),
        file:make_dir(Old),
        ok = touch(filename:join(Old, "file"))
    end,

    % ensure trailing slash does not change results
    Cases = [
        {Old, New},
        {Old ++ "/", New},
        {Old, New ++ "/"},
        {Old ++ "/", New ++ "/"}
    ],

    {foreach, Prepare,
        [fun() ->
            ?assertEqual(ok, file_utils:move(From, To)),

            ?assertNot(filelib:is_dir(Old)),
            ?assert(filelib:is_dir(New)),
            ?assert(filelib:is_file(filename:join(New, "file")))
        end || {From, To} <- Cases]
    }.


rename_file_test(Config) ->
    Old = path(Config, "oldfile"),
    New = path(Config, "newfile"),

    {setup,
        fun() -> ok = touch(Old) end,
        fun() ->
            ?assertEqual(ok, file_utils:move(Old, New)),

            ?assertNot(filelib:is_file(Old)),
            ?assert(filelib:is_file(New))
        end
    }.


rename_fails_when_target_exists_test(Config) ->
    From = path(Config, "olddir"),
    To = path(Config, "newdir"),
    Setup = fun() ->
        file:make_dir(From),
        file:make_dir(To)
    end,
    {setup, Setup, fun() ->
        ?assertEqual({error, eexist}, file_utils:move(From, To)),

        ?assert(filelib:is_dir(From)),
        ?assert(filelib:is_dir(To))
    end}.


seconds_since_modification_test(Config) ->
    Dir = path(Config, "testdir"),
    File = filename:join(Dir, "new-file"),
    ok = file:make_dir(Dir),

    ok = touch(File),
    CreationTime = time_utils:datetime_to_seconds(filelib:last_modified(File)),
    timer:sleep(rand:uniform(1250)),
    ?assertEqual({ok, time_utils:timestamp_seconds() - CreationTime}, file_utils:seconds_since_modification(File)),

    timer:sleep(rand:uniform(1250)),
    ok = touch(File),
    ModificationTime = time_utils:datetime_to_seconds(filelib:last_modified(File)),
    timer:sleep(rand:uniform(1250)),
    ?_assertEqual({ok, time_utils:timestamp_seconds() - ModificationTime}, file_utils:seconds_since_modification(File)).



%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    Workdir = mochitemp:mkdtemp(),
    #{cwd => Workdir}.

stop(#{cwd := Workdir}) ->
    mochitemp:rmtempdir(Workdir).

reset_dir(#{cwd := Workdir}) ->
    mochitemp:rmtempdir(Workdir),
    file:make_dir(Workdir).


%%%===================================================================
%%% Internal functions
%%%===================================================================

touch(Path) ->
    file:write_file(Path, <<>>).

path(#{cwd := Workdir}, Name) ->
    filename:join(Workdir, Name).


-endif.
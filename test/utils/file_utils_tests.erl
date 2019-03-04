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
            fun remove_nonexistent_test/1
        ]
    }.

%%%===================================================================
%%% Test functions
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


%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    Workdir = mochitemp:mkdtemp(),
    #{cwd => Workdir}.

stop(#{cwd := Workdir}) ->
    mochitemp:rmtempdir(Workdir).


%%%===================================================================
%%% Internal functions
%%%===================================================================

touch(Path) ->
    file:write_file(Path, <<>>).

path(#{cwd := Workdir}, Name) ->
    filename:join(Workdir, Name).


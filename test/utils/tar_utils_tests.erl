%%%--------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of tar_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(tar_utils_tests).
-author("Michal Stanisz").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(RANDOM_NAME(), binary:replace(base64:encode(crypto:strong_rand_bytes(12)), <<"/">>, <<"">>, [global])).

%%%===================================================================
%%% Tests functions
%%%===================================================================

create_archive_test() ->
    create_archive_test_base(#{gzip => false}, 512).

create_archive_gzip_test() ->
    create_archive_test_base(#{gzip => true}, 512).

create_archive_large_files_test() ->
    create_archive_test_base(#{gzip => false}, 16 * 1024 * 1024).

create_archive_large_files_gzip_test() ->
    create_archive_test_base(#{gzip => true}, 16 * 1024 * 1024).

file_with_long_name_test() ->
    TmpPath = mochitemp:mkdtemp(),
    LongFilename = binary:copy(<<"a">>, 120),
    FileContent = crypto:strong_rand_bytes(10),
    
    TarStream1 = tar_utils:open_archive_stream(),
    TarStream2 = tar_utils:new_file_entry(TarStream1, LongFilename, byte_size(FileContent), 8#664, 1616070881, 'REG'),
    TarStream3 = tar_utils:append_to_file_content(TarStream2, FileContent),
    Bytes = tar_utils:close_archive_stream(TarStream3),
    
    ?assertEqual(ok, erl_tar:extract({binary, Bytes}, [compressed, {cwd, TmpPath}])),
    ?assertEqual({ok, FileContent}, file:read_file(filename:join(TmpPath, LongFilename))),
    
    mochitemp:rmtempdir(TmpPath).

dir_with_long_name_test() ->
    TmpPath = mochitemp:mkdtemp(),
    LongDirname = binary:copy(<<"a">>, 120),
    
    TarStream1 = tar_utils:open_archive_stream(),
    TarStream2 = tar_utils:new_file_entry(TarStream1, LongDirname, 0, 8#775, 1616070881, 'DIR'),
    Bytes = tar_utils:close_archive_stream(TarStream2),
    
    ?assertEqual(ok, erl_tar:extract({binary, Bytes}, [compressed, {cwd, TmpPath}])),
    {ok, After} = file:list_dir(TmpPath),
    ?assertEqual(true, lists:member(binary_to_list(LongDirname), After)),
    
    mochitemp:rmtempdir(TmpPath).

symlink_test() ->
    FileName = ?RANDOM_NAME(),
    LinkName = ?RANDOM_NAME(),
    symlink_test_base(FileName, LinkName, FileName).

symlink_long_path_test() ->
    FileName = ?RANDOM_NAME(),
    LinkName = ?RANDOM_NAME(),
    symlink_test_base(FileName, LinkName, <<(binary:copy(<<"./">>, 100))/binary, FileName/binary>>).

symlink_long_linkname_test() ->
    FileName = ?RANDOM_NAME(),
    LinkName = binary:copy(<<"a">>, 120),
    symlink_test_base(FileName, LinkName, FileName).

symlink_long_linkname_and_path_test() ->
    FileName = ?RANDOM_NAME(),
    LinkName = binary:copy(<<"a">>, 120),
    symlink_test_base(FileName, LinkName, <<(binary:copy(<<"./">>, 100))/binary, FileName/binary>>).

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_archive_test_base(OpenOptions, FileSize) ->
    TmpPath = mochitemp:mkdtemp(),
    FileContent = crypto:strong_rand_bytes(FileSize),
    Dirname = ?RANDOM_NAME(),
    NestedFilePath = filename:join(Dirname, ?RANDOM_NAME()),
    FileName = ?RANDOM_NAME(),
    
    TarStream = tar_utils:open_archive_stream(OpenOptions),
    TarStream1 = tar_utils:new_file_entry(TarStream, Dirname, 0, 8#775, 1616070881, 'DIR'),
    TarStream2 = tar_utils:new_file_entry(TarStream1, NestedFilePath, 2 * byte_size(FileContent), 8#664, 1616070881, 'REG'),
    TarStream3 = tar_utils:append_to_file_content(TarStream2, FileContent),
    TarStream4 = tar_utils:append_to_file_content(TarStream3, FileContent),
    TarStream5 = tar_utils:new_file_entry(TarStream4, FileName, byte_size(FileContent), 8#664, 1616070881, 'REG'),
    TarStream6 = tar_utils:append_to_file_content(TarStream5, FileContent),
    Bytes = tar_utils:close_archive_stream(TarStream6),
    
    ?assertEqual(ok, erl_tar:extract({binary, Bytes}, [compressed, {cwd, TmpPath}])),
    {ok, After} = file:list_dir(TmpPath),
    ?assertEqual(true, lists:member(binary_to_list(Dirname), After)),
    ?assertEqual({ok, FileContent}, file:read_file(filename:join(TmpPath, FileName))),
    ?assertEqual({ok, binary:copy(FileContent, 2)}, file:read_file(filename:join(TmpPath, NestedFilePath))),
    
    mochitemp:rmtempdir(TmpPath).


symlink_test_base(FileName, LinkName, SymlinkPath) ->
    TmpPath = mochitemp:mkdtemp(),
    FileContent = crypto:strong_rand_bytes(rand:uniform(1000) + 1000),
    
    TarStream1 = tar_utils:open_archive_stream(),
    TarStream2 = tar_utils:new_file_entry(TarStream1, FileName, byte_size(FileContent), 8#664, 1616070881, 'REG'),
    TarStream3 = tar_utils:append_to_file_content(TarStream2, FileContent),
    TarStream4 = tar_utils:new_file_entry(TarStream3, LinkName, 0, 8#664, 1616070881, {'SYMLNK', SymlinkPath}),
    Bytes = tar_utils:close_archive_stream(TarStream4),
    
    ?assertEqual(ok, erl_tar:extract({binary, Bytes}, [compressed, {cwd, TmpPath}])),
    ?assertEqual({ok, FileContent}, file:read_file(filename:join(TmpPath, FileName))),
    ?assertEqual({ok, FileContent}, file:read_file(filename:join(TmpPath, LinkName))),
    {ok, #file_info{mode = Mode}} = file:read_link_info(filename:join(TmpPath, LinkName)),
    ?assertEqual(8#777, Mode rem 8#1000),
    
    mochitemp:rmtempdir(TmpPath).
    

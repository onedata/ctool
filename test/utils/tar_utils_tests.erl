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

-define(RANDOM_NAME(), binary:replace(base64:encode(crypto:strong_rand_bytes(12)), <<"/">>, <<"">>, [global])).

%%%===================================================================
%%% Tests functions
%%%===================================================================

create_archive_test() ->
    create_archive_test_base(#{gzip => false}).

create_archive_gzip_test() ->
    create_archive_test_base(#{gzip => true}).

file_with_long_name_test() ->
    TmpPath = mochitemp:mkdtemp(),
    LongFilename = binary:copy(<<"a">>, 120),
    FileContent = crypto:strong_rand_bytes(10),
    
    TarStream1 = tar_utils:open_archive_stream(),
    TarStream2 = tar_utils:new_file_entry(TarStream1, LongFilename, byte_size(FileContent), 8#664, 1616070881, regular),
    TarStream3 = tar_utils:append_to_file_content(TarStream2, FileContent),
    Bytes = tar_utils:close_archive_stream(TarStream3),
    
    ?assertEqual(ok, erl_tar:extract({binary, Bytes}, [compressed, {cwd, TmpPath}])),
    ?assertEqual({ok, FileContent}, file:read_file(filename:join(TmpPath, LongFilename))).

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_archive_test_base(OpenOptions) ->
    TmpPath = mochitemp:mkdtemp(),
    FileContent = crypto:strong_rand_bytes(512),
    Dirname = ?RANDOM_NAME(),
    NestedFilePath = filename:join(Dirname, ?RANDOM_NAME()),
    FileName = ?RANDOM_NAME(),
    
    TarStream = tar_utils:open_archive_stream(OpenOptions),
    TarStream1 = tar_utils:new_file_entry(TarStream, Dirname, 0, 8#775, 1616070881, directory),
    TarStream2 = tar_utils:new_file_entry(TarStream1, NestedFilePath, 2 * byte_size(FileContent), 8#664, 1616070881, regular),
    TarStream3 = tar_utils:append_to_file_content(TarStream2, FileContent),
    TarStream4 = tar_utils:append_to_file_content(TarStream3, FileContent),
    TarStream5 = tar_utils:new_file_entry(TarStream4, FileName, byte_size(FileContent), 8#664, 1616070881, regular),
    TarStream6 = tar_utils:append_to_file_content(TarStream5, FileContent),
    Bytes = tar_utils:close_archive_stream(TarStream6),
    
    ?assertEqual(ok, erl_tar:extract({binary, Bytes}, [compressed, {cwd, TmpPath}])),
    {ok, After} = file:list_dir(TmpPath),
    ?assertEqual(true, lists:member(binary_to_list(Dirname), After)),
    ?assertEqual({ok, FileContent}, file:read_file(filename:join(TmpPath, FileName))),
    ?assertEqual({ok, binary:copy(FileContent, 2)}, file:read_file(filename:join(TmpPath, NestedFilePath))).
    

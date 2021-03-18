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

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_archive_test_base(OpenOptions) ->
    FileContent = crypto:strong_rand_bytes(10),
    Dirname = ?RANDOM_NAME(),
    Filename = filename:join(Dirname, ?RANDOM_NAME()),
    TarStream = tar_utils:open_archive_stream(OpenOptions),
    TarStream1 = tar_utils:new_file_entry(TarStream, Dirname, 0, 8#775, 1616070881, directory),
    TarStream2 = tar_utils:new_file_entry(TarStream1, Filename, 2 * byte_size(FileContent), 8#664, 1616070881, regular),
    TarStream3 = tar_utils:append_to_file_content(TarStream2, FileContent),
    TarStream4 = tar_utils:append_to_file_content(TarStream3, FileContent),
    Bytes = tar_utils:close_archive_stream(TarStream4),
    ?assertEqual(ok, erl_tar:extract({binary, Bytes}, [compressed, {cwd, "/tmp/"}])),
    {ok, After} = file:list_dir("/tmp"),
    ?assertEqual(true, lists:member(binary_to_list(Dirname), After)),
    ?assertEqual({ok, binary:copy(FileContent, 2)}, file:read_file(filename:join("/tmp", Filename))).
    

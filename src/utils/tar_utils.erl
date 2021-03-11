%%%--------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions allowing for creating TAR archives.
%%% @end
%%%--------------------------------------------------------------------
-module(tar_utils).
-author("Michal Stanisz").

%% API
-export([prepare_header/5, data_padding/1, eof/0]).

-type tar_header() :: binary().
-export_type([tar_header/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec prepare_header(binary(), FileSize :: non_neg_integer(), Mode :: non_neg_integer(), 
    Timestamp :: non_neg_integer(), boolean()) -> tar_header().
prepare_header(Filename, FileSize, Mode, Timestamp, IsDir) ->
    {Type, FinalFilename} = case IsDir of
        true ->
            {<<"5">>, <<Filename/binary, "/">>};
        false ->
            {<<"0">>, Filename}
    end,
    prepare_header(FinalFilename, byte_size(Filename), FileSize, Mode, Timestamp, Type).


-spec data_padding(non_neg_integer()) -> binary().
data_padding(FileSize) when FileSize rem 512 == 0 -> <<>>;
data_padding(FileSize) ->
    BytesToFill = (512 - FileSize rem 512),
    str_utils:pad_right(<<>>, BytesToFill, <<0>>).


-spec eof() -> binary().
eof() ->
    str_utils:pad_right(<<>>, 1024, <<0>>).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec prepare_header(binary(), FilenameSize :: non_neg_integer(), FileSize :: non_neg_integer(), 
    Mode :: non_neg_integer(), Timestamp :: non_neg_integer(), boolean()) -> tar_header().
prepare_header(Filename, FilenameSize, FileSize, Mode, Timestamp, Type) when FilenameSize > 100 ->
    LongLinkHeader = prepare_header(
        <<46, 47, 46, 47, "@LongLink">>, 13, FilenameSize, 0, 0, <<"L">>),
    FileHeader = prepare_header(
        binary:part(Filename, 0, 100), min(byte_size(Filename), 100), FileSize, Mode, Timestamp, Type),
    <<
        LongLinkHeader/binary,
        Filename/binary,
        (data_padding(FilenameSize))/binary,
        FileHeader/binary
    >>;

prepare_header(Filename, FilenameSize, FileSize, Mode, Timestamp, Type) when FilenameSize =< 100 ->
    % use root(0) as owner/group - by default it will be overwritten when untarring
    PaddedOwner = str_utils:pad_left(format_octal(0), 7, <<"0">>),
    PaddedGroup = str_utils:pad_left(format_octal(0), 7, <<"0">>),
    PaddedSize = str_utils:pad_left(format_octal(FileSize), 11, <<"0">>),
    PaddedMode = str_utils:pad_left(format_octal(Mode), 7, <<"0">>),
    PaddedTimestamp = str_utils:pad_left(format_octal(Timestamp), 11, <<"0">>),
    HeaderFirstPart = <<
        (str_utils:pad_right(Filename, 100, <<0>>))/binary,
        PaddedMode/binary, 0,
        PaddedOwner/binary, 0,
        PaddedGroup/binary, 0,
        PaddedSize/binary, 0,
        PaddedTimestamp/binary, 0
    >>,
    HeaderSecondPart = <<(str_utils:pad_right(<<Type/binary >>, 101, <<0>>))/binary>>,
    ChecksumPlaceholder = binary:copy(<<32>>, 8),
    Checksum = calculate_checksum(
        <<HeaderFirstPart/binary, ChecksumPlaceholder/binary, HeaderSecondPart/binary>>),
    PaddedChecksum = <<(str_utils:pad_left(format_octal(Checksum), 6, <<"0">>))/binary, 0, 32>>,
    str_utils:pad_right(
        <<HeaderFirstPart/binary, PaddedChecksum/binary, HeaderSecondPart/binary>>, 512, <<0>>).


%% @private
-spec format_octal(integer()) -> 
    binary().
format_octal(Octal) ->
    iolist_to_binary(io_lib:fwrite("~.8B", [Octal])).


%% @private
-spec calculate_checksum(binary()) -> 
    Checksum :: non_neg_integer().
calculate_checksum(Bin) ->
    calculate_checksum(Bin, 0).

%% @private
-spec calculate_checksum(binary(), Acc :: non_neg_integer()) -> 
    Checksum :: non_neg_integer().
calculate_checksum(<<W, Rest/bytes>>, Acc0) ->
    Acc1 = Acc0 + W,
    calculate_checksum(Rest, Acc1);
calculate_checksum(<<>>, Acc) -> Acc.


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
-export([prepare_header/5, data_padding/1, ending_marker/0]).

-type tar_header() :: binary().
-export_type([tar_header/0]).

-define(BLOCK_SIZE, 512).
-define(DIRECTORY_TYPE_FLAG, <<"5">>).
-define(FILE_TYPE_FLAG, <<"0">>).
-define(LONG_LINK_TYPE_FLAG, <<"L">>).
-define(NUL, <<0>>).
-define(VALUE_PAD, <<"0">>).
-define(LONG_LINK_HEADER, <<46, 47, 46, 47, "@LongLink">>).
-define(MAX_FILENAME_LENGTH, 100).
-define(CHECKSUM_PLACEHOLDER, binary:copy(<<32>>, 8)).
-define(CHECKSUM_ENDING, <<0, 32>>).

%%%===================================================================
%%% API
%%%===================================================================

-spec prepare_header(binary(), FileSize :: non_neg_integer(), Mode :: non_neg_integer(), 
    Timestamp :: non_neg_integer(), boolean()) -> tar_header().
prepare_header(Filename, FileSize, Mode, Timestamp, IsDir) ->
    {Type, FinalFilename} = case IsDir of
        true ->
            {?DIRECTORY_TYPE_FLAG, <<Filename/binary, "/">>};
        false ->
            {?FILE_TYPE_FLAG, Filename}
    end,
    prepare_header(FinalFilename, byte_size(Filename), FileSize, Mode, Timestamp, Type).


-spec data_padding(non_neg_integer()) -> binary().
data_padding(FileSize) when FileSize rem ?BLOCK_SIZE == 0 -> <<>>;
data_padding(FileSize) ->
    BytesToFill = (?BLOCK_SIZE - FileSize rem ?BLOCK_SIZE),
    binary:copy(?NUL, BytesToFill).


-spec ending_marker() -> binary().
ending_marker() ->
    binary:copy(?NUL, 2 * ?BLOCK_SIZE).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec prepare_header(binary(), FilenameSize :: non_neg_integer(), FileSize :: non_neg_integer(), 
    Mode :: non_neg_integer(), Timestamp :: non_neg_integer(), binary()) -> tar_header().
prepare_header(Filename, FilenameSize, FileSize, Mode, Timestamp, Type) when FilenameSize > ?MAX_FILENAME_LENGTH ->
    LongLinkHeader = prepare_header(
        ?LONG_LINK_HEADER, byte_size(?LONG_LINK_HEADER), FilenameSize, 0, 0, ?LONG_LINK_TYPE_FLAG),
    TrimmedFilename = binary:part(Filename, 0, ?MAX_FILENAME_LENGTH),
    FileHeader = prepare_header(
        TrimmedFilename, byte_size(TrimmedFilename), FileSize, Mode, Timestamp, Type),
    <<
        LongLinkHeader/binary,
        Filename/binary,
        (data_padding(FilenameSize))/binary,
        FileHeader/binary
    >>;

prepare_header(Filename, FilenameSize, FileSize, Mode, Timestamp, Type) when FilenameSize =< ?MAX_FILENAME_LENGTH ->
    % use root(0) as owner/group - by default it will be overwritten when untarring
    PaddedOwner = str_utils:pad_left(format_octal(0), 7, ?VALUE_PAD),
    PaddedGroup = str_utils:pad_left(format_octal(0), 7, ?VALUE_PAD),
    PaddedSize = str_utils:pad_left(format_octal(FileSize), 11, ?VALUE_PAD),
    PaddedMode = str_utils:pad_left(format_octal(Mode), 7, ?VALUE_PAD),
    PaddedTimestamp = str_utils:pad_left(format_octal(Timestamp), 11, ?VALUE_PAD),
    HeaderFirstPart = <<
        (str_utils:pad_right(Filename, ?MAX_FILENAME_LENGTH, ?NUL))/binary,
        PaddedMode/binary, 0,
        PaddedOwner/binary, 0,
        PaddedGroup/binary, 0,
        PaddedSize/binary, 0,
        PaddedTimestamp/binary, 0
    >>,
    HeaderSecondPart = <<(str_utils:pad_right(<<Type/binary>>, 101, ?NUL))/binary>>,
    Checksum = calculate_checksum(
        <<HeaderFirstPart/binary, (?CHECKSUM_PLACEHOLDER)/binary, HeaderSecondPart/binary>>),
    PaddedChecksum = 
        <<(str_utils:pad_left(format_octal(Checksum), 6, ?VALUE_PAD))/binary, (?CHECKSUM_ENDING)/binary>>,
    str_utils:pad_right(
        <<HeaderFirstPart/binary, PaddedChecksum/binary, HeaderSecondPart/binary>>, ?BLOCK_SIZE, ?NUL).


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


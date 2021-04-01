%%%--------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions allowing for creating TAR archives on-the-fly.
%%% 
%%% Module operates on state object and every function (except `close_archive_stream`) 
%%% returns updated version of it. This updated state must be used in next call to this 
%%% module. After `close_archive_stream` has been called, new state should be acquired 
%%% using `open_archive_stream`.
%%% For each file it is responsibility of a calling module to provide exactly FileSize 
%%% bytes (as given in `new_file_entry`) in total in subsequent `append_to_file_content` 
%%% calls.
%%% 
%%% Typical usage of this module is as follows: 
%%%     1) `open_archive_stream`
%%%     2) `new_file_entry`
%%%     3) 0 or more calls to `append_to_file_content`
%%%     4) repeat from 2) for each new file to add
%%%     5) `close_archive_stream`
%%% 
%%% Any time between open and close `flush_buffer` function can be called to acquire bytes 
%%% generated so far.
%%%
%%% When using this module with option gzip set to true, every call must be made from 
%%% the same process.
%%% @end
%%%--------------------------------------------------------------------
-module(tar_utils).
-author("Michal Stanisz").

%% API
-export([open_archive_stream/0, open_archive_stream/1]).
-export([new_file_entry/6]).
-export([append_to_file_content/2, append_to_file_content/3]).
-export([close_archive_stream/1]).
-export([flush_buffer/1]).

-define(BLOCK_SIZE, 512).
-define(DIRECTORY_TYPE_FLAG, <<"5">>).
-define(FILE_TYPE_FLAG, <<"0">>).
-define(SYMLINK_TYPE_FLAG, <<"2">>).
-define(NUL, <<0>>).
-define(VALUE_PAD, <<"0">>).
-define(CHECKSUM_PLACEHOLDER, binary:copy(<<32>>, 8)).
-define(CHECKSUM_ENDING, <<0, 32>>).
-define(MAX_FILENAME_LENGTH, 100).
-define(MAX_SYMLINK_LENGTH, 100).
-define(LONG_LINK_PREFIX, <<46, 47, 46, 47, "@LongLink">>).
-define(FILENAME_LONG_LINK_TYPE_FLAG, <<"L">>).
-define(SYMLINK_LONG_LINK_TYPE_FLAG, <<"K">>).

-define(ZLIB_COMPRESSION_LEVEL, default).
-define(ZLIB_MEMORY_LEVEL, 8).
% The base two logarithm of the window size (the size of the history buffer). 
% It is to be in the range 8 through 15.
% The most significant bit (16) switches on gzip header and checksum.
-define(ZLIB_WINDOW_BITS, 15 + 16).
-define(ZLIB_METHOD, deflated).
-define(ZLIB_STRATEGY, default).

-record(stream, {
    buffer = <<>> :: bytes(),
    current_file_size = 0 :: non_neg_integer(),
    zstream = undefined :: zlib:zstream()
}).


-opaque stream() :: #stream{}.
-type bytes() :: binary().
-type options() :: #{
    gzip => boolean()
}.

-export_type([stream/0, bytes/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec open_archive_stream() -> stream().
open_archive_stream() ->
    open_archive_stream(#{gzip => true}).

-spec open_archive_stream(options()) -> stream().
open_archive_stream(Options) ->
    init_buffer(Options).


-spec new_file_entry(stream(), binary(), FileSize :: non_neg_integer(), Mode :: non_neg_integer(), 
    Timestamp :: non_neg_integer(), 'DIR' | 'REG' | {'SYMLNK', binary()}) -> stream().
new_file_entry(Stream, Filename, FileSize, Mode, Timestamp, FileType) ->
    Padding = data_padding(Stream),
    Header = file_header(Filename, FileSize, Mode, Timestamp, FileType),
    write_to_buffer(Stream#stream{current_file_size = 0}, <<Padding/binary, Header/binary>>).


-spec append_to_file_content(stream(), bytes()) -> stream().
append_to_file_content(Stream, Data) ->
    append_to_file_content(Stream, Data, byte_size(Data)).

-spec append_to_file_content(stream(), bytes(), non_neg_integer()) -> stream().
append_to_file_content(#stream{current_file_size = CurrentFileSize} = Stream, Data, DataSize) ->
    write_to_buffer(Stream#stream{current_file_size = CurrentFileSize + DataSize}, Data).


-spec close_archive_stream(stream()) -> bytes().
close_archive_stream(Stream) ->
    Padding = data_padding(Stream),
    EofMarker = ending_marker(),
    close_buffer(Stream, <<Padding/binary, EofMarker/binary>>).


-spec flush_buffer(stream()) -> {bytes(), stream()}.
flush_buffer(#stream{buffer = Bytes} = Stream) ->
    {Bytes, Stream#stream{buffer = <<>>}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec file_header(binary(), FileSize :: non_neg_integer(), Mode :: non_neg_integer(), 
    Timestamp :: non_neg_integer(), 'DIR' | 'REG' | {'SYMLNK', binary()}) -> bytes().
file_header(Filename, FileSize, Mode, Timestamp, FileType) ->
    {Type, FinalFilename, SymlinkPath, FinalMode} = case FileType of
        'DIR' ->
            {?DIRECTORY_TYPE_FLAG, str_utils:ensure_suffix(Filename, <<"/">>), <<>>, Mode};
        'REG' ->
            {?FILE_TYPE_FLAG, Filename, <<>>, Mode};
        {'SYMLNK', Path} ->
            {?SYMLINK_TYPE_FLAG, Filename, Path, 8#777}
    end,
    file_header(FinalFilename, byte_size(FinalFilename), FileSize, FinalMode, Timestamp, Type, SymlinkPath).

%% @private
-spec file_header(binary(), FilenameSize :: non_neg_integer(), FileSize :: non_neg_integer(),
    Mode :: non_neg_integer(), Timestamp :: non_neg_integer(), binary(), binary()) -> bytes().
file_header(
    Filename, FilenameSize, FileSize, Mode, Timestamp, Type, SymlinkPath
) when byte_size(SymlinkPath) > ?MAX_SYMLINK_LENGTH ->
    SymlinkSize = byte_size(SymlinkPath),
    SymlinkLongLinkHeader = file_header(
        ?LONG_LINK_PREFIX, byte_size(?LONG_LINK_PREFIX), SymlinkSize, 8#777, 0, ?SYMLINK_LONG_LINK_TYPE_FLAG, <<>>),
    TrimmedSymlinkPath = binary:part(SymlinkPath, 0, ?MAX_SYMLINK_LENGTH),
    FileHeader = file_header(
        Filename, FilenameSize, FileSize, Mode, Timestamp, Type, TrimmedSymlinkPath),
    <<
        SymlinkLongLinkHeader/binary,
        SymlinkPath/binary,
        (data_padding(SymlinkSize))/binary,
        FileHeader/binary
    >>;

file_header(
    Filename, FilenameSize, FileSize, Mode, Timestamp, Type, SymlinkPath
) when FilenameSize > ?MAX_FILENAME_LENGTH ->
    FilenameLongLinkHeader = file_header(
        ?LONG_LINK_PREFIX, byte_size(?LONG_LINK_PREFIX), FilenameSize, 0, 0, ?FILENAME_LONG_LINK_TYPE_FLAG, <<>>),
    TrimmedFilename = binary:part(Filename, 0, ?MAX_FILENAME_LENGTH),
    FileHeader = file_header(
        TrimmedFilename, byte_size(TrimmedFilename), FileSize, Mode, Timestamp, Type, SymlinkPath),
    <<
        FilenameLongLinkHeader/binary,
        Filename/binary,
        (data_padding(FilenameSize))/binary,
        FileHeader/binary
    >>;

file_header(
    Filename, FilenameSize, FileSize, Mode, Timestamp, Type, SymlinkPath
) when FilenameSize =< ?MAX_FILENAME_LENGTH ->
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
    HeaderSecondPart = <<(str_utils:pad_right(<<Type/binary, SymlinkPath/binary>>, 101, ?NUL))/binary>>,
    Checksum = calculate_checksum(
        <<HeaderFirstPart/binary, (?CHECKSUM_PLACEHOLDER)/binary, HeaderSecondPart/binary>>),
    PaddedChecksum = 
        <<(str_utils:pad_left(format_octal(Checksum), 6, ?VALUE_PAD))/binary, (?CHECKSUM_ENDING)/binary>>,
    str_utils:pad_right(
        <<HeaderFirstPart/binary, PaddedChecksum/binary, HeaderSecondPart/binary>>, ?BLOCK_SIZE, ?NUL).


%% @private
-spec ending_marker() -> bytes().
ending_marker() ->
    binary:copy(?NUL, 2 * ?BLOCK_SIZE).


%% @private
-spec data_padding(stream() | non_neg_integer()) -> bytes().
data_padding(#stream{current_file_size = FileSize}) ->
    data_padding(FileSize);
data_padding(FileSize) when FileSize rem ?BLOCK_SIZE == 0 ->
    <<>>;
data_padding(FileSize) ->
    BytesToFill = (?BLOCK_SIZE - FileSize rem ?BLOCK_SIZE),
    binary:copy(?NUL, BytesToFill).


%% @private
-spec format_octal(integer()) -> bytes().
format_octal(Octal) ->
    iolist_to_binary(io_lib:fwrite("~.8B", [Octal])).


%% @private
-spec calculate_checksum(bytes()) -> 
    Checksum :: non_neg_integer().
calculate_checksum(Bin) ->
    calculate_checksum(Bin, 0).

%% @private
-spec calculate_checksum(bytes(), Acc :: non_neg_integer()) -> 
    Checksum :: non_neg_integer().
calculate_checksum(<<W, Rest/bytes>>, Acc0) ->
    Acc1 = Acc0 + W,
    calculate_checksum(Rest, Acc1);
calculate_checksum(<<>>, Acc) -> Acc.


%% @private
-spec write_to_buffer(stream(), bytes()) -> stream().
write_to_buffer(Stream, Data) ->
    write_to_buffer(Stream, Data, none).

%% @private
-spec write_to_buffer(stream(), bytes(), zlib:zflush()) -> stream().
write_to_buffer(#stream{zstream = undefined, buffer = Buffer} = Stream, Data, _ForceFlush) ->
    Stream#stream{buffer = <<Buffer/binary, Data/binary>>};
write_to_buffer(#stream{zstream = ZStream, buffer = Buffer} = Stream, Data, ForceFlush) ->
    Deflated = iolist_to_binary(zlib:deflate(ZStream, Data, ForceFlush)),
    Stream#stream{buffer = <<Buffer/binary, Deflated/binary>>}.


%% @private
-spec init_buffer(options()) -> stream().
init_buffer(Options) ->
    ZStream = case maps:get(gzip, Options, false) of
        false -> undefined;
        true -> init_zstream()
    end,
    #stream{zstream = ZStream}.


-spec close_buffer(stream(), bytes()) -> bytes().
close_buffer(#stream{zstream = undefined} = Stream, Bytes) ->
    Stream2 = write_to_buffer(Stream, Bytes),
    {FinalBytes, _} = flush_buffer(Stream2),
    FinalBytes;
close_buffer(#stream{zstream = ZStream} = Stream, Data) when ZStream /= undefined ->
    Deflated = iolist_to_binary(zlib:deflate(ZStream, Data, finish)),
    zlib:close(ZStream),
    close_buffer(Stream#stream{zstream = undefined}, Deflated).


%% @private
-spec init_zstream() -> zlib:zstream().
init_zstream() ->
    ZStream = zlib:open(),
    ok = zlib:deflateInit(ZStream, ?ZLIB_COMPRESSION_LEVEL, ?ZLIB_METHOD, ?ZLIB_WINDOW_BITS,
        ?ZLIB_MEMORY_LEVEL, ?ZLIB_STRATEGY),
    ZStream.

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
-export([open_archive/0, open_archive/1]).
-export([new_file_entry/6]).
-export([append_to_file_content/2, append_to_file_content/3]).
-export([close_archive/1]).
-export([read_bytes/1]).

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

-define(ZLIB_COMPRESSION_LEVEL, default).
-define(ZLIB_MEMORY_LEVEL, 8).
-define(ZLIB_WINDOW_BITS, 31).
-define(ZLIB_METHOD, deflated).
-define(ZLIB_STRATEGY, default).

-record(state, {
    data = <<>> :: binary(),
    current_file_bytes = 0 :: non_neg_integer(),
    zstream = undefined :: zlib:zstream()
}).


-type state() :: #state{}.
-type bytes() :: binary().
-type options() :: #{
    gzip => boolean()
}.

-export_type([state/0, bytes/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec open_archive() -> state().
open_archive() ->
    open_archive(#{gzip => true}).


-spec open_archive(options()) -> state().
open_archive(Options) ->
    ZStream = case maps:get(gzip, Options, false) of
        false -> undefined;
        true ->
            Z = zlib:open(),
            ok = zlib:deflateInit(Z, ?ZLIB_COMPRESSION_LEVEL, ?ZLIB_METHOD, ?ZLIB_WINDOW_BITS,
                ?ZLIB_MEMORY_LEVEL, ?ZLIB_STRATEGY),
            Z
    end,
    #state{zstream = ZStream}.


-spec new_file_entry(state(), binary(), FileSize :: non_neg_integer(), Mode :: non_neg_integer(), 
    Timestamp :: non_neg_integer(), directory | regular) -> state().
new_file_entry(State, Filename, FileSize, Mode, Timestamp, FileType) ->
    Padding = data_padding(State),
    Header = file_header(Filename, FileSize, Mode, Timestamp, FileType),
    append_data(State#state{current_file_bytes = 0}, deflate(State, <<Padding/binary, Header/binary>>)).


-spec append_to_file_content(state(), bytes()) -> state().
append_to_file_content(State, Data) ->
    append_to_file_content(State, Data, byte_size(Data)).

-spec append_to_file_content(state(), bytes(), non_neg_integer()) -> state().
append_to_file_content(#state{current_file_bytes = CurrentFileSize} = State, Data, DataSize) ->
    append_data(State#state{current_file_bytes = CurrentFileSize + DataSize}, deflate(State, Data)).


-spec close_archive(state()) -> bytes().
close_archive(#state{zstream = ZStream} = State) ->
    Padding = data_padding(State),
    EofMarker = ending_marker(),
    Data = deflate(State, <<Padding/binary, EofMarker/binary>>, finish),
    ZStream =/= undefined andalso zlib:close(ZStream),
    {FinalBytes, _} = read_bytes(append_data(State, Data)),
    FinalBytes.


-spec read_bytes(state()) -> {bytes(), state()}.
read_bytes(#state{data = Bytes} = State) ->
    {Bytes, State#state{data = <<>>}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec append_data(state(), bytes()) -> state().
append_data(#state{data = Data} = State, NewData) ->
    State#state{data = <<Data/binary, NewData/binary>>}.


%% @private
-spec file_header(binary(), FileSize :: non_neg_integer(), Mode :: non_neg_integer(), 
    Timestamp :: non_neg_integer(), directory | regular) -> bytes().
file_header(Filename, FileSize, Mode, Timestamp, FileType) ->
    {Type, FinalFilename} = case FileType of
        directory ->
            {?DIRECTORY_TYPE_FLAG, str_utils:ensure_suffix(Filename, <<"/">>)};
        regular ->
            {?FILE_TYPE_FLAG, Filename}
    end,
    file_header(FinalFilename, byte_size(Filename), FileSize, Mode, Timestamp, Type).


%% @private
-spec data_padding(state() | non_neg_integer()) -> bytes().
data_padding(#state{current_file_bytes = FileSize}) ->
    data_padding(FileSize);
data_padding(FileSize) when FileSize rem ?BLOCK_SIZE == 0 -> 
    <<>>;
data_padding(FileSize) ->
    BytesToFill = (?BLOCK_SIZE - FileSize rem ?BLOCK_SIZE),
    binary:copy(?NUL, BytesToFill).


%% @private
-spec ending_marker() -> bytes().
ending_marker() ->
    binary:copy(?NUL, 2 * ?BLOCK_SIZE).

%% @private
-spec file_header(binary(), FilenameSize :: non_neg_integer(), FileSize :: non_neg_integer(), 
    Mode :: non_neg_integer(), Timestamp :: non_neg_integer(), binary()) -> bytes().
file_header(Filename, FilenameSize, FileSize, Mode, Timestamp, Type) when FilenameSize > ?MAX_FILENAME_LENGTH ->
    LongLinkHeader = file_header(
        ?LONG_LINK_HEADER, byte_size(?LONG_LINK_HEADER), FilenameSize, 0, 0, ?LONG_LINK_TYPE_FLAG),
    TrimmedFilename = binary:part(Filename, 0, ?MAX_FILENAME_LENGTH),
    FileHeader = file_header(
        TrimmedFilename, byte_size(TrimmedFilename), FileSize, Mode, Timestamp, Type),
    <<
        LongLinkHeader/binary,
        Filename/binary,
        (data_padding(FilenameSize))/binary,
        FileHeader/binary
    >>;

file_header(Filename, FilenameSize, FileSize, Mode, Timestamp, Type) when FilenameSize =< ?MAX_FILENAME_LENGTH ->
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


%% @private
-spec deflate(state(), binary()) -> binary().
deflate(State, Data) ->
    deflate(State, Data, none).

%% @private
-spec deflate(state(), binary(), zlib:zflush()) -> binary().
deflate(#state{zstream = undefined}, Data, _Flush) ->
    Data;
deflate(#state{zstream = ZStream}, Data, Flush) ->
    iolist_to_binary(zlib:deflate(ZStream, Data, Flush)).

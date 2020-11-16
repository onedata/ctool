%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module implements functions for parsing and processing parameters
%%% of http request (e.g. headers).
%%% @end
%%%--------------------------------------------------------------------
-module(http_parser).
-author("Bartosz Walkowicz").

-include("http/headers.hrl").

%% API
-export([
    parse_query_string/1,
    parse_range_header/2
]).

% Range of bytes (inclusive at both ends), e.g.:
% - {0, 99} means 100 bytes beginning at offset 0,
% - {10, 10} means 1 byte beginning at offset 10.
-type bytes_range() :: {
    InclusiveRangeStart :: non_neg_integer(),
    InclusiveRangeEnd :: non_neg_integer()
}.

-export_type([bytes_range/0]).

-type content_size() :: non_neg_integer().


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Parses and returns query string parameters. For every parameter
%% specified multiple times it's values are merged into list in reverse order.
%% @end
%%--------------------------------------------------------------------
-spec parse_query_string(cowboy_req:req()) -> map().
parse_query_string(Req) ->
    lists:foldl(fun({Param, Val}, AccMap) ->
        maps:update_with(Param, fun(OldVal) ->
            [Val | utils:ensure_list(OldVal)]
        end, Val, AccMap)
    end, #{}, cowboy_req:parse_qs(Req)).


-spec parse_range_header(cowboy_req:req(), content_size()) ->
    undefined | invalid | [bytes_range()].
parse_range_header(Req, ContentSize) ->
    case cowboy_req:header(?HDR_RANGE, Req) of
        undefined -> undefined;
        RawRange -> parse_bytes_ranges(RawRange, ContentSize)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns byte ranges obtained from parsing http 'Range' header:
%%
%% >> parse_byte_range(<<"bytes=1-5,-3,2-">>, 10)
%% [{1, 5}, {7, 9}, {2, 9}]
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_bytes_ranges(binary() | list(), content_size()) -> invalid | [bytes_range()].
parse_bytes_ranges(RangesBin, ContentSize) when is_binary(RangesBin) ->
    case binary:split(RangesBin, <<"=">>, [global]) of
        [<<"bytes">>, RawRanges] ->
            parse_bytes_ranges(
                binary:split(RawRanges, [<<",">>, <<" ">>, <<"\t">>], [global, trim_all]),
                ContentSize
            );
        _ ->
            invalid
    end;
parse_bytes_ranges(RawRanges, ContentSize) ->
    try
        lists:map(fun(RangeBin) -> parse_bytes_range(RangeBin, ContentSize) end, RawRanges)
    catch _:_  ->
        invalid
    end.


%% @private
-spec parse_bytes_range(binary() | [binary()], content_size()) -> bytes_range() | no_return().
parse_bytes_range(RangeBin, ContentSize) when is_binary(RangeBin) ->
    parse_bytes_range(binary:split(RangeBin, <<"-">>, [global]), ContentSize);
parse_bytes_range([<<>>, FromEndBin], ContentSize) ->
    validate_bytes_range(
        {max(0, ContentSize - binary_to_integer(FromEndBin)), ContentSize - 1},
        ContentSize
    );
parse_bytes_range([RangeStartBin, <<>>], ContentSize) ->
    validate_bytes_range(
        {binary_to_integer(RangeStartBin), ContentSize - 1},
        ContentSize
    );
parse_bytes_range([RangeStartBin, RangeEndBin], ContentSize) ->
    validate_bytes_range(
        {binary_to_integer(RangeStartBin), min(binary_to_integer(RangeEndBin), ContentSize - 1)},
        ContentSize
    );
parse_bytes_range(_InvalidRange, _ContentSize) ->
    throw(invalid).


%% @private
-spec validate_bytes_range(bytes_range(), content_size()) -> bytes_range() | no_return().
validate_bytes_range({RangeStart, RangeEnd}, ContentSize) when
    RangeStart < 0;
    RangeStart > RangeEnd;
    RangeStart >= ContentSize
->
    throw(invalid);
validate_bytes_range(Range, _ContentSize) ->
    Range.

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utilities for encoding strings using the RFC5987 standard
%%% (used in content-disposition HTTP headers).
%%% @end
%%%-------------------------------------------------------------------
-module(rfc5987).


-export([encode/1, decode/1]).


%%%===================================================================
%%% API
%%%===================================================================


-spec encode(binary()) -> binary().
encode(Bin) ->
    << <<(encode_byte(B))/binary>> || <<B>> <= Bin >>.


-spec decode(binary()) -> binary().
decode(Bin) ->
    decode(Bin, <<>>).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec decode(binary(), binary()) -> binary().
decode(<<>>, Acc) ->
    Acc;
decode(<<$%, H1, H2, Rest/binary>>, Acc) ->
    Byte = binary_to_integer(<<H1, H2>>, 16),
    decode(Rest, <<Acc/binary, Byte>>);
decode(<<B, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, B>>).


%% @private
-spec encode_byte(byte()) -> binary().
encode_byte(B) when
    (B >= $A andalso B =< $Z) orelse
        (B >= $a andalso B =< $z) orelse
        (B >= $0 andalso B =< $9) orelse
        B =:= $! orelse B =:= $# orelse B =:= $$ orelse B =:= $& orelse
        B =:= $+ orelse B =:= $- orelse B =:= $. orelse B =:= $^ orelse
        B =:= $_ orelse B =:= $` orelse B =:= $| orelse B =:= $~
->
    <<B>>;
encode_byte(B) ->
    H = integer_to_binary(B, 16),
    Padded = case byte_size(H) of
        1 -> <<"0", H/binary>>;
        _ -> H
    end,
    <<"%", Padded/binary>>.

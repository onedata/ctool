%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%%--------------------------------------------------------------------
%%% @doc
%%% XDR basic type encode/decode.
%%% Based on
%%% http://erlang.org/pipermail/erlang-questions/2000-August/001544.html
%%%
%%% Decode routines:  dec({Binary,Pos}) -> {Value, {Binary,NewPos}}
%%% Encode routines:  enc(Elem)       -> List
%%% @end
%%%-------------------------------------------------------------------
-module(xdrlib).
-author("Jakub Kudzia").

-compile([verbose, report_errors, report_warnings, trace]).

-export([enc_int/1, dec_int/1,
    enc_unsigned_int/1, dec_unsigned_int/1,
    enc_hyper/1, dec_hyper/1,
    enc_unsigned_hyper/1, dec_unsigned_hyper/1,
    enc_bool/1, dec_bool/1,
    enc_float/1, dec_float/1,
    enc_double/1, dec_double/1,
    enc_byte_array/2, dec_byte_array/2,
    enc_byte_varray/2, dec_byte_varray/2,
    enc_string/1, dec_string/1]).

-type xdr() :: binary().
-type xdr_iter() :: {binary(), pos_integer()}.

%% Float [S=1 | E=8 | F=23]
%% X = (-1)^S * 2^(E-127) * 1.F
%%
-define(FLOAT_BASE, 16#800000).
-define(FLOAT_BIAS, 127).

%% Double [S=1 | E=11 | F=52]
%% X = (-1)^S * 2^(E-1023) * 1.F
-define(DOUBLE_BASE, 16#10000000000000).
-define(DOUBLE_BIAS, 1023).

%%-------------------------------------------------------------------
%% @doc
%% Encodes integer to XDR format.
%% @end
%%-------------------------------------------------------------------
-spec enc_int(integer()) -> xdr().
enc_int(X) ->
    list_to_binary([
        ((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
        ((X) bsr 8) band 16#ff, (X) band 16#ff
    ]).

%%-------------------------------------------------------------------
%% @doc
%% Decodes integer in B on position P in XDR format.
%% @end
%%-------------------------------------------------------------------
-spec dec_int(xdr_iter()) -> {integer(), xdr_iter()}.
dec_int({B, P}) ->
    [X3, X2, X1, X0] = binary_to_list(B, P, P + 3),
    X = (X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0,
    if X3 >= 16#80 ->
        {X - 16#100000000, {B, P + 4}};
        true ->
            {X, {B, P + 4}}
    end.

%%-------------------------------------------------------------------
%% @doc
%% @equiv enc_unsigned_int(X)
%% @end
%%-------------------------------------------------------------------
-spec enc_unsigned_int(integer()) -> xdr().
enc_unsigned_int(X) ->
    enc_int(X).

%%-------------------------------------------------------------------
%% @doc
%% Decodes unsigned integer in B on position P in XDR format.
%% @end
%%-------------------------------------------------------------------
-spec dec_unsigned_int(xdr_iter()) -> {integer(), xdr_iter()}.
dec_unsigned_int({B, P}) ->
    [X3, X2, X1, X0] = binary_to_list(B, P, P + 3),
    {(X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0, {B, P + 4}}.


%%-------------------------------------------------------------------
%% @doc
%% Encodes hyper (8 bytes) integer to XDR format.
%% @end
%%-------------------------------------------------------------------
-spec enc_hyper(integer()) -> xdr().
enc_hyper(X) ->
    list_to_binary([((X) bsr 56) band 16#ff, ((X) bsr 48) band 16#ff,
        ((X) bsr 40) band 16#ff, ((X) bsr 32) band 16#ff,
        ((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
        ((X) bsr 8) band 16#ff, (X) band 16#ff]).

%%-------------------------------------------------------------------
%% @doc
%% Decodes hyper integer (8 bytes) in B on position P in XDR format.
%% @end
%%-------------------------------------------------------------------
-spec dec_hyper(xdr_iter()) -> {integer(), xdr_iter()}.
dec_hyper({B, P}) ->
    [X7, X6, X5, X4, X3, X2, X1, X0] = binary_to_list(B, P, P + 7),
    X = (X7 bsl 56) + (X6 bsl 48) + (X5 bsl 40) + (X4 bsl 32) +
        (X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0,
    if X7 >= 16#80 ->
        {X - 16#10000000000000000, {B, P + 8}};
        true ->
            {X, {B, P + 8}}
    end.

%%-------------------------------------------------------------------
%% @doc
%% @equiv enc_hyper(X)
%% @end
%%-------------------------------------------------------------------
-spec enc_unsigned_hyper(integer()) -> xdr().
enc_unsigned_hyper(X) ->
    enc_hyper(X).

%%-------------------------------------------------------------------
%% @doc
%% Decodes hyper unsigned integer (8 bytes) in B on position P in
%% XDR format.
%% @end
%%-------------------------------------------------------------------
-spec dec_unsigned_hyper(xdr_iter()) -> {integer(), xdr_iter()}.
dec_unsigned_hyper({B, P}) ->
    [X7, X6, X5, X4, X3, X2, X1, X0] = binary_to_list(B, P, P + 7),
    {(X7 bsl 56) + (X6 bsl 48) + (X5 bsl 40) + (X4 bsl 32) +
        (X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0, {B, P + 8}}.

%%-------------------------------------------------------------------
%% @doc
%% Encodes boolean to XDR format.
%% @end
%%-------------------------------------------------------------------
-spec enc_bool(boolean()) -> xdr().
enc_bool(true) -> list_to_binary([0, 0, 0, 1]);
enc_bool(false) -> list_to_binary([0, 0, 0, 0]).

%%-------------------------------------------------------------------
%% @doc
%% Decodes boolean in B on position P in XDR format.
%% @end
%%-------------------------------------------------------------------
-spec dec_bool(xdr_iter()) -> {boolean(), xdr_iter()}.
dec_bool({B, P}) ->
    case binary_to_list(B, P, P + 3) of
        [0, 0, 0, 0] -> {false, {B, P + 4}};
        [0, 0, 0, 1] -> {true, {B, P + 4}}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Encodes float to XDR format.
%% @end
%%-------------------------------------------------------------------
-spec enc_float(float()) -> xdr().
enc_float(X) when is_float(X) ->
    {S, E, F} = enc_ieee(X, ?FLOAT_BASE, ?FLOAT_BIAS),
    list_to_binary([(S bsl 7) bor ((E bsr 1) band 16#7f),
        ((F bsr 16) band 16#7f) bor ((E band 1) bsl 7),
        (F bsr 8) band 16#ff,
        (F band 16#ff)]).

%%-------------------------------------------------------------------
%% @doc
%% Decodes float in B on position P in XDR format.
%% @end
%%-------------------------------------------------------------------
-spec dec_float(xdr_iter()) -> {float(), xdr_iter()}.
dec_float({B, P}) ->
    [X3, X2, X1, X0] = binary_to_list(B, P, P + 3),
    E = (X3 band 16#7f) bsl 1 + (X2 bsr 7),
    F = (X2 band 16#7f) bsl 16 + (X1 bsl 8) + X0,
    if
        E == 0, F == 0 ->
            {0.0, {B, P + 4}};
        X3 >= 16#80 ->
            {-scale_float(E - ?FLOAT_BIAS, (1 + F / ?FLOAT_BASE)), {B, P + 4}};
        true ->
            {scale_float(E - ?FLOAT_BIAS, (1 + F / ?FLOAT_BASE)), {B, P + 4}}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Encodes double to XDR format.
%% @end
%%-------------------------------------------------------------------
-spec enc_double(float()) -> xdr().
enc_double(X) when is_float(X) ->
    {S, E, F} = enc_ieee(X, ?DOUBLE_BASE, ?DOUBLE_BIAS),
    list_to_binary([(S bsl 7) bor ((E bsr 4) band 16#7f),
        ((F bsr 48) band 16#0f) bor ((E band 16#f) bsl 4),
        (F bsr 40) band 16#ff,
        (F bsr 32) band 16#ff,
        (F bsr 24) band 16#ff,
        (F bsr 16) band 16#ff,
        (F bsr 8) band 16#ff,
        (F band 16#ff)]).

%%-------------------------------------------------------------------
%% @doc
%% Decodes double in B on position P in XDR format.
%% @end
%%-------------------------------------------------------------------
-spec dec_double(xdr_iter()) -> {float(), xdr_iter()}.
dec_double({B, P}) ->
    [X7, X6, X5, X4, X3, X2, X1, X0] = binary_to_list(B, P, P + 7),
    E = (X7 band 16#7f) bsl 4 + (X6 bsr 4),
    F = (X6 band 16#0f) bsl 48 +
        (X5 bsl 40) + (X4 bsl 32) + (X3 bsl 24) +
        (X2 bsl 16) + (X1 bsl 8) + X0,
    if
        E == 0, F == 0 ->
            {0.0, {B, P + 8}};
        X7 >= 16#80 ->
            {-scale_float(E - ?DOUBLE_BIAS, (1 + F / ?DOUBLE_BASE)), {B, P + 8}};
        true ->
            {scale_float(E - ?DOUBLE_BIAS, (1 + F / ?DOUBLE_BASE)), {B, P + 8}}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Encodes array of N elements
%% @end
%%-------------------------------------------------------------------
-spec enc_byte_array(iolist(), non_neg_integer()) -> xdr().
enc_byte_array(Data, N) ->
    Len = io_list_len(Data),
    io:format("Len: ~p~n", [Len]),
    if
        Len == N -> true;
        true -> exit({xdr, limit})
    end,
    list_to_binary([Data, enc_align(Len)]).

%%-------------------------------------------------------------------
%% @doc
%% Decodes array of N elements
%% @end
%%-------------------------------------------------------------------
-spec dec_byte_array(xdr_iter(), non_neg_integer()) -> {binary(), xdr_iter()}.
dec_byte_array({B, P}, 0) ->
    %% Special clause to handle the case where P is past the
    %% end of the binary.
    {list_to_binary([]), {B, P}};
dec_byte_array({B, P}, N) ->
    {_B1, B2} = split_binary(B, P - 1),
    {B3, _B4} = split_binary(B2, 4),
    %% Count carefully! Recall that P is a 1-based index.
    {B3, {B, P + align(N)}}.

%%-------------------------------------------------------------------
%% @doc
%% Encodes string to XDR format.
%% @end
%%-------------------------------------------------------------------
-spec enc_string(string() | binary()) -> xdr().
enc_string(Data) when is_list(Data) ->
    enc_string(list_to_binary(Data));
enc_string(Data) when is_binary(Data) ->
    Len = byte_size(Data),
    list_to_binary([enc_unsigned_int(Len), Data, enc_align(Len)]).

%%-------------------------------------------------------------------
%% @doc
%% Decodes string from XDR.
%% @end
%%-------------------------------------------------------------------
-spec dec_string(xdr_iter()) -> {binary(), xdr_iter()}.
dec_string({B, P}) ->
    {Len, {B1, P1}} = dec_unsigned_int({B, P}),
    AlignedLen = align(Len),
    String = binary_part(B1, P1 -1, Len),
    {String, {B1, P1 + AlignedLen}}.

%%-------------------------------------------------------------------
%% @doc
%% Encodes array of variable length
%% @end
%%-------------------------------------------------------------------
-spec enc_byte_varray(iolist(), max | non_neg_integer()) -> [xdr()].
enc_byte_varray(Data, Max) when is_integer(Max) ->
    Len = io_list_len(Data),
    if
        Max >= Len -> true;
        true -> exit({xdr, limit})
    end,
    %% Rely on enc_unsigned_int() and enc_align to give us binaries.
    %% Assume that Data is a binary, also.  (Won't hurt if it isn't, though).
    case Len rem 4 of
        0 -> [enc_unsigned_int(Len), Data];
        _ -> [enc_unsigned_int(Len), Data, enc_align(Len)]
    end;
enc_byte_varray(Data, max) ->
    Len = io_list_len(Data),
    %% Ditto.
    case Len rem 4 of
        0 -> [enc_unsigned_int(Len), Data];
        _ -> [enc_unsigned_int(Len), Data, enc_align(Len)]
    end.

%%-------------------------------------------------------------------
%% @doc
%% Decodes array of variable length
%% @end
%%-------------------------------------------------------------------
-spec dec_byte_varray(xdr_iter(), non_neg_integer()) -> {binary(), xdr_iter()}.
dec_byte_varray(R0, Max) when is_integer(Max) ->
    {N, R1} = dec_unsigned_int(R0),
    if
        Max >= N -> true;
        true -> exit({xdr, limit})
    end,
    dec_byte_array(R1, N);
dec_byte_varray(R0, max) ->
    {N, R1} = dec_unsigned_int(R0),
    dec_byte_array(R1, N).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc
%% unscale a float return {Sign, Exponent, Fraction}
%% @end
%%-------------------------------------------------------------------
-spec enc_ieee(float(), integer(), any()) -> tuple().
enc_ieee(X, Base, Bias) when X < 0 ->
    {E, F} = unscale_float(-X),
    {1, E + Bias, trunc((F - 1) * Base)};
enc_ieee(X, Base, Bias) when X > 0 ->
    {E, F} = unscale_float(X),
    {0, E + Bias, trunc((F - 1) * Base)};
enc_ieee(_X, _, _) ->
    {0, 0, 0}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Scales float number.
%% @end
%%-------------------------------------------------------------------
scale_float(E, M) when E >= 0 -> (1 bsl E) * M;
scale_float(E, M) -> 1 / (1 bsl -E) * M.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Unscales float number.
%% @end
%%-------------------------------------------------------------------
-spec unscale_float(float()) -> {number(), number()}.
unscale_float(X) when X == 1.0 -> {0, 0};
unscale_float(X) when X > 1.0 -> exp_down(X, 0);
unscale_float(X) when X < 1.0 -> exp_up(X, 0);
unscale_float(X) -> {0, X}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Scales exponent down.
%% @end
%%-------------------------------------------------------------------
-spec exp_down(float(), integer()) -> {number(), number()}.
exp_down(X, E) when X >= 2 -> exp_down(X / 2, E + 1);
exp_down(X, E) -> {E, X}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Scales exponent up.
%% @end
%%-------------------------------------------------------------------
-spec exp_up(float(), integer()) -> {number(), number()}.
exp_up(X, E) when X < 1 -> exp_up(X * 2, E - 1);
exp_up(X, E) -> {E, X}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Encode alignment.
%% @end
%%-------------------------------------------------------------------
-spec enc_align(integer()) -> xdr().
enc_align(Len) ->
    list_to_binary(lists:duplicate(align(Len) - Len, 0)).

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Increment the argument a minimal amount to bring it to a multiple of 4.
%% @end
%%-------------------------------------------------------------------
-spec align(integer()) -> integer().
align(Len) ->
    Len + ((4 - (Len rem 4)) rem 4).

%%-------------------------------------------------------------------
%% @private
%% @doc
%% @equiv io_list_len(L, 0)
%% @end
%%-------------------------------------------------------------------
-spec io_list_len(iolist()) -> non_neg_integer().
io_list_len(L) -> io_list_len(L, 0).

%%-------------------------------------------------------------------
%% @private
%% @doc
%% io list length
%% @end
%%-------------------------------------------------------------------
-spec io_list_len(iolist(), non_neg_integer()) -> non_neg_integer().
io_list_len([H | T], N) ->
    if
        H >= 0, H =< 255 -> io_list_len(T, N + 1);
        is_list(H) -> io_list_len(T, io_list_len(H, N));
        is_binary(H) -> io_list_len(T, size(H) + N);
        true -> exit({xdr, opaque})
    end;
io_list_len(H, N) when is_binary(H) ->
    size(H) + N;
io_list_len([], N) ->
    N.
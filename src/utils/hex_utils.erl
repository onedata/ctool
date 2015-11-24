%% Copyright (c) 2007 Mochi Media, Inc.
%% Author: Bob Ippolito <bob@mochimedia.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
%% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
%% ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% https://github.com/mochi/mochiweb/

%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module is taken from 'mochiweb' project
%% and slightly adopted for onedata needs.
%%
%% @end
%% ===================================================================
-module(hex_utils).

-export([to_hex/1, to_bin/1, to_int/1, dehex/1, hexdigit/1]).

%% @doc Convert an iolist to a hexadecimal string.
-spec to_hex(integer() | binary() | iolist()) -> string().
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) when is_binary(B) ->
    to_hex(B, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).

%% @doc Convert a hexadecimal string to a binary.
-spec to_bin(string()) -> binary().
to_bin(L) ->
    to_bin(L, []).

%% @doc Convert a hexadecimal string to an integer.
-spec to_int(string()) -> integer().
to_int(L) ->
    erlang:list_to_integer(L, 16).

%% @doc Convert a hex digit to its integer value.
-spec dehex(char()) -> integer().
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

%% @doc Convert an integer less than 16 to a hex digit.
-spec hexdigit(integer()) -> char().
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

%% Internal API

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



-endif.

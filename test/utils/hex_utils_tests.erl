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

%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of hex utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(hex_utils_tests).
-author("Lukasz Opiola").

-include_lib("eunit/include/eunit.hrl").

-export([to_hex_test/0, to_bin_test/0, to_int_test/0]).


to_hex_test() ->
    "ff000ff1" = hex_utils:to_hex([255, 0, 15, 241]),
    "ff000ff1" = hex_utils:to_hex(16#ff000ff1),
    "0" = hex_utils:to_hex(16#0).

to_bin_test() ->
    <<255, 0, 15, 241>> = hex_utils:to_bin("ff000ff1"),
    <<255, 0, 10, 161>> = hex_utils:to_bin("Ff000aA1").

to_int_test() ->
    16#ff000ff1 = hex_utils:to_int("ff000ff1"),
    16#ff000aa1 = hex_utils:to_int("FF000Aa1"),
    16#0 = hex_utils:to_int("0").
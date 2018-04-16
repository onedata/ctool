%% Copyright (C) 2012-2013 Jeremy Ong
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
%% https://github.com/jeremyong/websocket_client/
%%
%% ===================================================================
%% @doc: This SUITE tests the abilities of websocket_client to:
%%      - connect to a webscoket server
%%      - send data
%%      - receive data
%% @end
%% ===================================================================
-module(wc_SUITE).

-include_lib("ctool/include/test/test_utils.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).
-export([
    test_text_frames/1,
    test_binary_frames/1,
    test_control_frames/1,
    test_quick_response/1
]).


all() -> [
    test_text_frames,
    test_binary_frames,
    test_control_frames,
    test_quick_response
].


%%%===================================================================
%%% Setup and teardown functions
%%%===================================================================

% Starts required deps and an echo server written in cowboy.
init_per_suite(Config) ->
    crypto:start(),
    {ok, AppsStarted} = application:ensure_all_started(cowboy),
    ok = echo_server:start(),
    [{apps_started, lists:reverse(AppsStarted)}, {?CTH_ENV_UP, ?DISABLE} | Config].


% Cleanup
end_per_suite(Config) ->
    AppsStarted = ?config(apps_started, Config),
    lists:foreach(fun(App) -> application:stop(App) end, AppsStarted),
    crypto:stop(),
    Config.


%%%===================================================================
%%% Test functions
%%%===================================================================

% Tests if sending text messages to echo server results in receiving the same
% messages.
test_text_frames(_) ->
    {ok, Pid} = ws_client:start_link(),
    %% Short message
    Short = short_msg(),
    ws_client:send_text(Pid, Short),
    {text, Short} = ws_client:recv(Pid),
    %% Payload length greater than 125 (actual 150).
    Medium = medium_msg(),
    ws_client:send_text(Pid, Medium),
    {text, Medium} = ws_client:recv(Pid),

    %% Now check that websocket_client:send is working
    Pid ! {send_text, Medium},
    {text, Medium} = ws_client:recv(Pid),

    %% Payload length greater than 65535
    Long = long_msg(),
    ws_client:send_text(Pid, Long),
    {text, Long} = ws_client:recv(Pid),
    ws_client:stop(Pid),
    ok.


% Tests if sending binary frames to echo server results in receiving the same
% messages.
test_binary_frames(_) ->
    {ok, Pid} = ws_client:start_link(),
    %% Short message
    Short = short_msg(),
    ws_client:send_binary(Pid, Short),
    {binary, Short} = ws_client:recv(Pid),
    %% Payload length greater than 125 (actual 150).
    Medium = medium_msg(),
    ws_client:send_binary(Pid, Medium),
    {binary, Medium} = ws_client:recv(Pid),
    %% Payload length greater than 65535
    Long = long_msg(),
    ws_client:send_binary(Pid, Long),
    {binary, Long} = ws_client:recv(Pid),
    ws_client:stop(Pid),
    ok.


% Tests if sending control messages (ping-pong) to echo server results
% in receiving control responses.
test_control_frames(_) ->
    {ok, Pid} = ws_client:start_link(),
    %% Send ping with short message
    Short = short_msg(),
    ws_client:send_ping(Pid, Short),
    {pong, Short} = ws_client:recv(Pid),
    %% Server will echo the ping as well
    {ping, Short} = ws_client:recv(Pid),
    {pong, Short} = ws_client:recv(Pid),
    %% Send ping without message
    ws_client:send_ping(Pid, <<>>),
    {pong, <<>>} = ws_client:recv(Pid),
    ws_client:stop(Pid),
    ok.


% Tests if the client properly receives the first frame sent by the server
% right after connecting.
test_quick_response(_) ->
    %% Connect to the server and...
    {ok, Pid} = ws_client:start_link("ws://localhost:8080/hello/?q=world!"),
    %% ...make sure we receive the first frame.
    {text, <<"world!">>} = ws_client:recv(Pid, 100),
    ws_client:stop(Pid),
    %% Also, make sure the HTTP response is parsed correctly.
    {ok, Pid2} = ws_client:start_link("ws://localhost:8080/hello/?q=Hello%0D%0A%0D%0AWorld%0D%0A%0D%0A!"),
    {text, <<"Hello\r\n\r\nWorld\r\n\r\n!">>} = ws_client:recv(Pid2, 100),
    ws_client:stop(Pid2),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Construction of messages of different lengths for tests.
short_msg() ->
    <<"hello">>.
medium_msg() ->
    <<"ttttttttttttttttttttttttt"
    "ttttttttttttttttttttttttt"
    "ttttttttttttttttttttttttt"
    "ttttttttttttttttttttttttt"
    "ttttttttttttttttttttttttt"
    "ttttttttttttttttttttttttt">>.
long_msg() ->
    Medium = medium_msg(),
    %% 600 bytes
    L = <<Medium/binary, Medium/binary, Medium/binary, Medium/binary>>,
    %% 2400 bytes
    L1 = <<L/binary, L/binary, L/binary, L/binary>>,
    %% 9600 bytes
    L2 = <<L1/binary, L1/binary, L1/binary, L1/binary>>,
    %% 38400 bytes
    L3 = <<L2/binary, L2/binary, L2/binary, L2/binary>>,
    %% 76800 bytes
    <<L3/binary, L3/binary>>.


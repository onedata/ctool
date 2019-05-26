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
%% @doc: This is a wrapper on websocket_client module used in CT tests
%% for more clear expression of the tests.
%% @end
%% ===================================================================
-module(ws_client).

-behaviour(websocket_client_handler_behaviour).

-export([
    start_link/0,
    start_link/1,
    send_text/2,
    send_binary/2,
    send_ping/2,
    recv/2,
    recv/1,
    stop/1
]).

% websocket_client_handler_behaviour implementation
-export([
    init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-record(state, {
    buffer = [] :: list(),
    waiting = undefined :: undefined | pid()
}).

% Starts a client connection to given server
start_link() ->
    start_link("ws://localhost:8080").

% Starts a client connection to given server
% Returns Pid of the client process.
start_link(Url) ->
    websocket_client:start_link(Url, ?MODULE, [], []).

% Stops the client
stop(Pid) ->
    Pid ! stop.

% Orders the client to send text message
send_text(Pid, Msg) ->
    websocket_client:cast(Pid, {text, Msg}).

% Orders the client to send binary message
send_binary(Pid, Msg) ->
    websocket_client:cast(Pid, {binary, Msg}).

% Orders the client to send control (ping) message
send_ping(Pid, Msg) ->
    websocket_client:cast(Pid, {ping, Msg}).

% Orders the client to receive data from websocket and
% send it back to this process
recv(Pid) ->
    recv(Pid, 5000).

% Orders the client to receive data from websocket and
% send it back to this process
recv(Pid, Timeout) ->
    Pid ! {recv, self()},
    receive
        M -> M
    after
        Timeout -> error
    end.

% Callback called after a connection is created
init(_, _WSReq) ->
    {ok, #state{}}.

% Callback called after a frame is received.
websocket_handle(Frame, _, State = #state{waiting = undefined, buffer = Buffer}) ->
    {ok, State#state{buffer = [Frame | Buffer]}};
websocket_handle(Frame, _, State = #state{waiting = From}) ->
    From ! Frame,
    {ok, State#state{waiting = undefined}}.

% Callback called when the controlling process receives a message
% Send given text to the server.
websocket_info({send_text, Text}, WSReq, State) ->
    websocket_client:send({text, Text}, WSReq),
    {ok, State};
% Receive data from socket (but there is nothing to receive)
websocket_info({recv, From}, _, State = #state{buffer = []}) ->
    {ok, State#state{waiting = From}};
% Receive data from socket and if there is something, send it back to
% calling process
websocket_info({recv, From}, _, State = #state{buffer = [Top | Rest]}) ->
    From ! Top,
    {ok, State#state{buffer = Rest}};
% Stop
websocket_info(stop, _, State) ->
    {close, <<>>, State}.

% Called when the WS connection is closed.
websocket_terminate(_Close, _, _State) ->
    ok.

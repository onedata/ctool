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
%% @doc: This is a simple echo server written in cowboy
%% used for WS client tests.
%% @end
%% ===================================================================
-module(echo_server).
-behaviour(cowboy_websocket).

-export([
    start/0
]).

% Cowboy callbacks
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-record(state, {to_send}).

% Start the cowboy server
start() ->
    io:format("Starting echo server.~n"),
    Dispatch = cowboy_router:compile([{'_', [
        {"/hello/", ?MODULE, []},
        {'_', ?MODULE, []}
    ]}]),
    {ok, _} = cowboy:start_clear(echo_listener,
        [
            {port, 8080},
            {num_acceptors, 2},
            {max_connections, 100}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.


% Upgrade to websocket in init
init(Req, _Opts) ->
    ParsedQs = cowboy_req:parse_qs(Req),
    State = case proplists:lookup(<<"q">>, ParsedQs) of
        none ->
            #state{};
        {<<"q">>, Text} ->
            #state{to_send = Text}
    end,
    {cowboy_websocket, Req, State}.


% Called after websocket connection is established
websocket_init(#state{to_send = undefined} = State) ->
    {ok, State};
websocket_init(#state{to_send = Text}) ->
    self() ! {send, Text},
    websocket_init(#state{}).


% Called when a frame arrives, just send the same message back
websocket_handle(Frame, State) ->
    {reply, Frame, State}.


% Called when the controlling process receives a message
% This one is used to schedule a send action in init
websocket_info({send, Text}, State) ->
    {reply, {text, Text}, State};

% Called when the controlling process receives a message
websocket_info(_Msg, State) ->
    {ok, State}.

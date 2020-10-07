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

%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module is an example of handler module for websocket_client.
%%
%% @end
%% ===================================================================
-module(ws_ping_example).

-behaviour(websocket_client_handler_behaviour).

-export([
    start_link/0,
    init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).


%%--------------------------------------------------------------------
%% @doc
%% Start the example
%% @end
%%--------------------------------------------------------------------
start_link() ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("wss://echo.websocket.org", [], ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Callback called when connection is received.
%% @see websocket_client_handler_behaviour:init/2
%% @end
%%--------------------------------------------------------------------
-spec init([term()], websocket_req:req()) ->
    {ok, State :: term()} | {ok, State :: term(), Keepalive :: integer()}.
init([], _ConnState) ->
    websocket_client:cast(self(), {text, <<"message 1">>}),
    %% Execute a ping every 1000 milliseconds
    {ok, 2, 1000}.


%%--------------------------------------------------------------------
%% @doc
%% Callback called when data is received via WebSocket protocol.
%% @see websocket_client_handler_behaviour:websocket_handle/3
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle({text | binary | ping | pong, binary()},
    websocket_req:req(), State :: term()) ->
    {ok, State :: term()} |
    {reply, websocket_req:frame(), State :: term()} |
    {close, Reply :: binary(), State :: term()}.
websocket_handle({pong, _Msg}, _ConnState, State) ->
    io:format("Received pong ~n"),

    %% This is how to access info about the connection/request
    Proto = websocket_req:protocol(_ConnState),
    io:format("On protocol: ~p~n", [Proto]),

    {ok, State};
websocket_handle({text, Msg}, _ConnState, 5) ->
    io:format("Received msg ~p~n", [Msg]),
    {close, <<>>, 10};
websocket_handle({text, Msg}, _ConnState, State) ->
    io:format("Received msg ~p~n", [Msg]),
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(State)),
    Reply = {text, <<"hello, this is message #", BinInt/binary>>},
    io:format("Replying: ~p~n", [Reply]),
    {reply, Reply, State + 1}.


%%--------------------------------------------------------------------
%% @doc
%% Callback called when a message is sent to the process handling
%% the connection.
%% @see websocket_client_handler_behaviour:websocket_info/3
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(term(), websocket_req:req(), State :: term()) ->
    {ok, State :: term()} |
    {reply, websocket_req:frame(), State :: term()} |
    {close, Reply :: binary(), State :: term()}.
websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang message received">>}, State}.


%%--------------------------------------------------------------------
%% @doc
%% Callback called when the connection is closed.
%% @see websocket_client_handler_behaviour:websocket_terminate/3
%% @end
%%--------------------------------------------------------------------
-spec websocket_terminate({Reason, term()} | {Reason, integer(), binary()},
    websocket_req:req(), State :: term()) -> ok when
    Reason :: normal | error | remote.
websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n",
        [State, Reason]),
    ok.

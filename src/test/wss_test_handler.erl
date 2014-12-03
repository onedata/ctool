%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides simple WebSocket client handler used for
%%      test connections.
%% @end
%% ===================================================================
-module(wss_test_handler).
-author("Krzysztof Trzepla").

%% WebSocket client handler callbacks
-export([init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3]).

%% session state
-record(state, {pid}).

%% ====================================================================
%% WebSocket client callbacks
%% ====================================================================

%% init/2
%% ====================================================================
%% @doc Initializes the state for a session.
%% @end
-spec init(Args, Req) -> {ok, State} | {ok, State, KeepAlive} when
    Args :: term(),
    Req :: websocket_req:req(),
    State :: any(),
    KeepAlive :: integer().
%% ====================================================================
init([Pid | _], _Req) when is_pid(Pid) ->
    Pid ! {self(), connected},
    {ok, #state{pid = Pid}}.


%% websocket_handle/3
%% ====================================================================
%% @doc Handles the data received from the Websocket connection.
%% @end
-spec websocket_handle(InFrame, Req, State) ->
    {ok, State} | {reply, OutFrame, State} | {close, Payload, State} when
    InFrame :: {text | binary | ping | pong, binary()},
    Req :: websocket_req:req(),
    State :: any(),
    Payload :: binary(),
    OutFrame :: cowboy_websocket:frame().
%% ====================================================================
websocket_handle({binary, Data}, _Req, #state{pid = Pid} = State) ->
    Pid ! {self(), {binary, Data}},
    {ok, State};

websocket_handle(_InFrame, _Req, State) ->
    {ok, State}.


%% websocket_info/3
%% ====================================================================
%% @doc Handles the Erlang message received.
%% @end
-spec websocket_info(Info, Req, State) ->
    {ok, State} | {reply, OutFrame, State} | {close, Payload, State} when
    Info :: any(),
    Req :: websocket_req:req(),
    State :: any(),
    Payload :: binary(),
    OutFrame :: cowboy_websocket:frame().
%% ====================================================================
websocket_info({send, Data}, _Req, State) ->
    {reply, {binary, Data}, State};

websocket_info({close, Payload}, _Req, State) ->
    {close, Payload, State};

websocket_info(_Info, _Req, State) ->
    {ok, State}.


%% websocket_terminate/3
%% ====================================================================
%% @doc Performs any necessary cleanup of the state.
%% @end
-spec websocket_terminate(Reason, Req, State) -> ok when
    Reason :: {CloseType, Payload} | {CloseType, Code, Payload},
    CloseType :: normal | error | remote,
    Payload :: binary(),
    Code :: integer(),
    Req :: websocket_req:req(),
    State :: any().
%% ====================================================================
websocket_terminate({close, Code, _Payload}, _Req, #state{pid = Pid}) ->
    Pid ! {self(), {closed, Code}},
    ok;

websocket_terminate(_Reason, _Req, _State) ->
    ok.
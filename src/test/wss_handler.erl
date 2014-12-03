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
-module(wss_handler).
-author("Krzysztof Trzepla").

%% WebSocket client handler callbacks
-export([init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3]).

%% API
-export([send/2, recv/1, recv/2, disconnect/1, flush_errors/0]).

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


%% ====================================================================
%% API functions
%% ====================================================================

%% recv/1
%% ====================================================================
%% @equiv recv(Socket, 500).
-spec recv(SocketRef :: pid()) ->
    {ok, Data :: binary()} | {error, timout} | {error, Reason :: any()}.
%% ====================================================================
recv(Socket) ->
    recv(Socket, 500).


%% recv/2
%% ====================================================================
%% @doc Receives WebSocket frame from given socket. Timeouts after Timeout.
-spec recv(SocketRef :: pid(), Timeout :: non_neg_integer()) ->
    {ok, Data :: binary()} | {error, timout} | {error, Reason :: any()}.
%% ====================================================================
recv(Socket, Timeout) ->
    receive
        {Socket, {binary, Data}} -> {ok, Data};
        {Socket, Other} -> {error, Other}
    after Timeout ->
        {error, timeout}
    end.


%% send/2
%% ====================================================================
%% @doc Sends asynchronously Data over WebSocket.
-spec send(SocketRef :: pid(), Data :: binary()) -> ok.
%% ====================================================================
send(Socket, Data) ->
    Socket ! {send, Data},
    ok.


%% disconnect/1
%% ====================================================================
%% @doc Closes WebSocket connection.
-spec disconnect(Socket :: pid()) -> ok.
%% ====================================================================
disconnect(Socket) ->
    Socket ! {close, <<>>},
    ok.


%% flush_errors/0
%% ====================================================================
%% @doc Flushes errors.
-spec flush_errors() -> ok.
%% ====================================================================
flush_errors() ->
    receive
        {error, _} -> flush_errors()
    after 0 ->
        ok
    end.
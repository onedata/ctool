%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% An implementation of ranch_transport behavior on top of ssl2.
%%% For function documentation check Ranch's ranch_transport docs.
%%% @end
%%%-------------------------------------------------------------------
-module(ranch_ssl2).
-author("Konrad Zemek").

-behaviour(ranch_transport).

%% API
-export([name/0, secure/0, messages/0, listen/1, accept/2, accept_ack/2,
    connect/3, connect/4, recv/3, send/2, setopts/2, controlling_process/2,
    peername/1, sockname/1, shutdown/2, close/1, sendfile/2, sendfile/4,
    sendfile/5]).

-spec name() -> atom().
name() ->
    ssl2.

-spec secure() -> boolean().
secure() ->
    true.

-spec messages() -> {OK :: atom(), Closed :: atom(), Error :: atom()}.
messages() ->
    {ssl2, ssl2_closed, ssl2_error}.

-spec listen(ssl2:listen_opts()) -> {ok, ssl2:socket()} | {error, atom()}.
listen(Opts) ->
    Port = proplists:get_value(port, Opts),
    ssl2:listen(Port, Opts).

-spec accept(ssl2:acceptor(), timeout()) ->
    {ok, ssl2:socket()} |
    {error, closed | timeout | atom()}.
accept(Socket, Timeout) ->
    ssl2:accept(Socket, Timeout).

-spec accept_ack(ssl2:socket(), timeout()) -> ok.
accept_ack(Socket, Timeout) ->
    case ssl2:handshake(Socket, Timeout) of
        ok ->
            ok;
    %% Socket most likely stopped responding, don't error out.
        {error, Reason} when Reason =:= timeout; Reason =:= closed ->
            ok = close(Socket),
            exit(normal);
        {error, Reason} ->
            ok = close(Socket),
            error(Reason)
    end.

-spec connect(string(), inet:port_number(), ssl2:listen_opts()) ->
    {ok, ssl2:socket()} |
    {error, atom()}.
connect(Host, Port, Opts) ->
    connect(Host, Port, Opts, infinity).

-spec connect(string(), inet:port_number(), ssl2:listen_opts(), timeout()) ->
    {ok, ssl2:socket()} |
    {error, atom()}.
connect(Host, Port, Opts, Timeout) ->
    ssl2:connect(Host, Port, Opts, Timeout).

-spec recv(ssl2:socket(), non_neg_integer(), timeout()) ->
    {ok, any()} |
    {error, closed | timeout | atom()}.
recv(Socket, Size, Timeout) ->
    ssl2:recv(Socket, Size, Timeout).

-spec send(ssl2:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Data) ->
    ssl2:send(Socket, Data).

-spec setopts(ssl2:socket(), ssl2:opts()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    ssl2:setopts(Socket, Opts).

-spec controlling_process(ssl2:socket(), pid()) ->
    ok | {error, closed | atom()}.
controlling_process(Socket, Pid) ->
    ssl2:controlling_process(Socket, Pid).

-spec peername(ssl2:socket()) ->
    {ok, {inet:ip_address(), inet:port_number()}} |
    {error, atom()}.
peername(Socket) ->
    ssl2:peername(Socket).

-spec sockname(ssl2:socket() | ssl2:acceptor()) ->
    {ok, {inet:ip_address(), inet:port_number()}} |
    {error, atom()}.
sockname(Socket) ->
    ssl2:sockname(Socket).

-spec shutdown(ssl2:socket(), read | write | read_write) ->
    ok | {error, atom()}.
shutdown(Socket, Type) ->
    ssl2:shutdown(Socket, Type).

-spec close(ssl2:socket()) -> ok.
close(Socket) ->
    ssl2:close(Socket).

-spec sendfile(ssl2:socket(), file:name() | file:fd()) ->
    {ok, non_neg_integer()} |
    {error, atom()}.
sendfile(Socket, Filename) ->
    sendfile(Socket, Filename, 0, 0, []).

-spec sendfile(ssl2:socket(), file:name() | file:fd(),
    non_neg_integer(), non_neg_integer()) ->
    {ok, non_neg_integer()} |
    {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
    sendfile(Socket, File, Offset, Bytes, []).

-spec sendfile(ssl2:socket(), file:name() | file:fd(), non_neg_integer(),
    non_neg_integer(), ranch_transport:sendfile_opts()) ->
    {ok, non_neg_integer()} |
    {error, atom()}.
sendfile(Socket, File, Offset, Bytes, Opts) ->
    ranch_transport:sendfile(?MODULE, Socket, File, Offset, Bytes, Opts).

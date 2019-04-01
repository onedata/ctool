%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module allows sending emails using an SMTP relay server.
%%% @end
%%%-------------------------------------------------------------------
-module(smtp_client).

-include("logging.hrl").

%% API
-export([send_email/7]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Sends an email using a relay email service.
%% Exemplary Options:
%% #{
%%    relay => "smtp.gmail.com",
%%    username => "username@gmail.com",
%%    password => "password",
%%      Protocol is optional, by default TLS (port 587) is used, SMTP (25) and
%%      SSL (465) are also allowed, though TLS is most commonly supported.
%%    protocol => tls | ssl | smtp
%% }
%% @end
%%--------------------------------------------------------------------
-spec send_email(FromEmail :: binary(), FromName :: binary(), To :: [binary()],
    BCC :: [binary()], Subject :: binary(), Body :: binary(),
    Options :: map()) -> ok | {error, term()}.
send_email(FromEmail, FromName, To, BCC, Subject, Body, Options) ->
    % Resolve options for gen_smtp_client
    Relay = [{relay, maps:get(relay, Options)}],
    UserName = [{username, maps:get(username, Options)}],
    Password = [{password, maps:get(password, Options)}],
    ProtocolOpts = case maps:get(protocol, Options, tls) of
        smtp ->
            [{port, 25}];
        ssl ->
            [{port, 465}, {ssl, true}, {auth, always}];
        tls ->
            [{port, 587}, {tls, always}, {auth, always}]
    end,
    ResolvedOpts = lists:append([Relay, UserName, Password, ProtocolOpts]),
    ToWithBrackets = [<<"<", Addr/binary, ">">> || Addr <- To],
    ToString = str_utils:join_binary(ToWithBrackets, <<",">>),
    Data = <<
        "Subject: ", Subject/binary, "\r\n",
        "From: ", FromName/binary, " <", FromEmail/binary, ">\r\n",
        "To: ", ToString/binary, "\r\n\r\n",
        Body/binary
    >>,
    AllRecipients = lists:usort(To ++ BCC),
    Res = gen_smtp_client:send_blocking(
        {FromEmail, AllRecipients, Data}, ResolvedOpts
    ),
    case Res of
        Bin when is_binary(Bin) ->
            ok;
        Error ->
            Error
    end.

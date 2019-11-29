%%%-------------------------------------------------------------------
%%% @author Michał Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module allows to forward http requests to another port on the
%%% same host.
%%% @end
%%%-------------------------------------------------------------------
-module(http_port_forwarder).
-author("Michał Stanisz").

-behaviour(cowboy_handler).

-include("http/codes.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req, [Port, ConnectOptsFun] = State) ->
    Qs = case cowboy_req:qs(Req) of
        <<"">> -> <<"">>;
        Bin -> <<"?", Bin/binary>>
    end,
    URL = str_utils:format_bin("~s://127.0.0.1:~B~s~s", [
        cowboy_req:scheme(Req), Port, cowboy_req:path(Req), Qs
    ]),
    Headers = cowboy_req:headers(Req),
    MethodBin = cowboy_req:method(Req),
    {ok, Body, _} = cowboy_req:read_body(Req),
    Method = binary_to_atom(string:lowercase(MethodBin), utf8),
    case http_client:request(Method, URL, Headers, Body, ConnectOptsFun()) of
        {ok, Code, RespHeaders} ->
            {ok, cowboy_req:reply(Code, RespHeaders, Req), State};
        {ok, Code, RespHeaders, RespBody} ->
            {ok, cowboy_req:reply(Code, RespHeaders, RespBody, Req), State};
        {error, _} = Error ->
            ?debug("Error in HTTP port forwarder: ~w", [Error]),
            {ok, cowboy_req:reply(?HTTP_503_SERVICE_UNAVAILABLE, Req), State}
    end.


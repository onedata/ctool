%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains useful functions loosely connected with HTTP:
%%%     - URL decoding, encoding
%%%     - base64url decoding, encoding
%%%     - JS and HTML escaping
%%% @end
%%%-------------------------------------------------------------------
-module(http_utils).

% URL encoding/decoding
-export([url_encode/1, url_decode/1]).

% Safe escaping
-export([js_escape/1, html_encode/1]).

% base64url encoding/decoding
-export([base64url_encode/1, base64url_decode/1]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Performs safe URL encoding
%% @end
%%--------------------------------------------------------------------
-spec url_encode(Data :: binary() | string()) -> binary().
url_encode(Data) ->
    hackney_url:urlencode(Data).


%%--------------------------------------------------------------------
%% @doc Performs URL-uncoded string decoding
%% @end
%%--------------------------------------------------------------------
-spec url_decode(Data :: binary() | string()) -> binary().
url_decode(Data) ->
    hackney_url:urldecode(Data).

%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec js_escape(String :: binary() | string()) -> binary().
js_escape(undefined) ->
    <<"">>;
js_escape(Value) when is_list(Value) ->
    js_escape(iolist_to_binary(Value));
js_escape(Value) ->
    js_escape(Value, <<"">>).
js_escape(<<"\\", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\\\">>);
js_escape(<<"\r", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\r">>);
js_escape(<<"\n", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\n">>);
js_escape(<<"\"", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\\"">>);
js_escape(<<"'", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\'">>);
js_escape(<<"<script", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "&lt;script">>);
js_escape(<<"script>", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "script&gt;">>);
js_escape(<<C, Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, C>>);
js_escape(<<"">>, Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @doc Performs safe URL encoding.
%% @end
%%--------------------------------------------------------------------
-spec html_encode(String :: binary() | string()) -> binary().
html_encode(List) when is_list(List) ->
    html_encode(to_binary(List));

html_encode(<<"">>) -> <<"">>;
html_encode(<<$<, Rest/binary>>) -> <<"&lt;", (html_encode(Rest))/binary>>;
html_encode(<<$>, Rest/binary>>) -> <<"&gt;", (html_encode(Rest))/binary>>;
html_encode(<<$", Rest/binary>>) -> <<"&quot;", (html_encode(Rest))/binary>>;
html_encode(<<$', Rest/binary>>) -> <<"&#39;", (html_encode(Rest))/binary>>;
html_encode(<<$&, Rest/binary>>) -> <<"&amp;", (html_encode(Rest))/binary>>;
html_encode(<<H, Rest/binary>>) -> <<H, (html_encode(Rest))/binary>>.


%%--------------------------------------------------------------------
%% @doc Performs URL-safe encoding to base64 (RFC 4648).
%% This differs from base64 in the following ways:
%% '-' is used in place of '+' (62),
%% '_' is used in place of '/' (63),
%% padding is implicit rather than explicit ('=').
%% @end
%%--------------------------------------------------------------------
-spec base64url_encode(binary()) -> binary().
base64url_encode(Data) ->
    mochiweb_base64url:encode(Data).


%%--------------------------------------------------------------------
%% @doc Decodes back from URL-safe base64 (RFC 4648).
%% This differs from base64 in the following ways:
%% '-' is used in place of '+' (62),
%% '_' is used in place of '/' (63),
%% padding is implicit rather than explicit ('=').
%% @end
%%--------------------------------------------------------------------
-spec base64url_decode(binary()) -> binary().
base64url_decode(Data) ->
    mochiweb_base64url:decode(Data).

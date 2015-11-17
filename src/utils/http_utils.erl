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
%%% @end
%%%-------------------------------------------------------------------
-module(http_utils).

-export([base64url_encode/1, base64url_decode/1]).


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

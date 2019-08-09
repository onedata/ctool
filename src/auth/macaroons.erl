%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @todo VFS-5554 This module is deprecated, kept for backward compatibility
%%% The module implements serialization/deserialization for deprecated macaroons.
%%% @end
%%%-------------------------------------------------------------------
-module(macaroons).
-author("Lukasz Opiola").

-include_lib("ctool/include/api_errors.hrl").

%% API
-export([serialize/1, deserialize/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Serializes a macaroon into a binary token.
%% @end
%%--------------------------------------------------------------------
-spec serialize(macaroon:macaroon()) -> {ok, Token :: binary()} | {error, term()}.
serialize(Macaroon) ->
    try macaroon:serialize(Macaroon) of
        {ok, Token64} -> {ok, base62:from_base64(Token64)};
        _ -> ?ERROR_BAD_TOKEN
    catch
        _:_ -> ?ERROR_BAD_TOKEN
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deserializes a macaroon from a binary token.
%% @end
%%--------------------------------------------------------------------
-spec deserialize(Token :: binary()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
deserialize(<<>>) -> ?ERROR_BAD_TOKEN;
deserialize(Macaroon) ->
    try macaroon:deserialize(base62:to_base64(Macaroon)) of
        {ok, M} -> {ok, M};
        _ -> ?ERROR_BAD_TOKEN
    catch
        _:_ -> ?ERROR_BAD_TOKEN
    end.

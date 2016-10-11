%%%-------------------------------------------------------------------
%%% @author Mateusz Paciorek
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% Module providing wrapper functions for different macaroon
%%% serialization methods.
%%% @end
%%%-------------------------------------------------------------------
-module(token_utils).
-author("Mateusz Paciorek").

-include("logging.hrl").

%% API
-export ([serialize64/1, serialize62/1, deserialize/1]).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Wrapper for macaroon:serialize to indicate base64 token format.
%% @end
%%--------------------------------------------------------------------
-spec serialize64(Macaroon :: macaroon:macaroon()) ->
    {ok, binary()} | {error, {too_long, term()}}.
serialize64(M) ->
    macaroon:serialize(M).

%%--------------------------------------------------------------------
%% @doc
%% Substitutes all non-alphanumeric characters that may be produced by
%% macaroon:serialize.
%% @end
%%--------------------------------------------------------------------
-spec serialize62(Macaroon :: macaroon:macaroon()) ->
    {ok, binary()} | {error, {too_long, term()}}.
serialize62(M) ->
    case macaroon:serialize(M) of
        {ok, Token64} ->
            {ok, << <<(encode(C))/binary>> || <<C>> <= Token64 >>};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Reverts substitution made in from_base64 function and deserializes
%% token. If token cannot be decoded as base62, tries to decode it
%% again as base64 or base64url.
%% @end
%%--------------------------------------------------------------------
-spec deserialize(Token :: binary()) ->
    {ok, macaroon:macaroon()} | {error, macaroon_invalid}.
deserialize(Token) ->
    try
        macaroon:deserialize(to_base64(Token, <<>>))
    catch
        Error ->
            ?debug("Failed to decode token as base62: ~p", [Error]),
            macaroon:deserialize(Token)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Internal function for deserialize/1.
%% @end
%%--------------------------------------------------------------------
-spec to_base64(binary(), binary()) -> binary().
to_base64(<<"0", C:1/binary, Rest/binary>>, Result) ->
    to_base64(Rest, <<Result/binary, (decode(C))>>);
to_base64(<<C:1/binary, Rest/binary>>, Result) ->
    to_base64(Rest, <<Result/binary, C/binary>>);
to_base64(<<>>, Result) ->
    Result.

%%--------------------------------------------------------------------
%% @doc
%% Function returning proper substitution for disallowed character.
%% @end
%%--------------------------------------------------------------------
-spec encode(integer()) -> binary().
encode($0) -> <<$0, $0>>;
encode($_) -> <<$0, $1>>;
encode($-) -> <<$0, $2>>;
encode($/) -> <<$0, $3>>;
encode($+) -> <<$0, $4>>;
encode($=) -> <<$0, $5>>;
encode(C) -> <<C>>.

%%--------------------------------------------------------------------
%% @doc
%% Function returning encoded characters for substitution.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> integer().
decode(<<$0>>) -> $0;
decode(<<$1>>) -> $_;
decode(<<$2>>) -> $-;
decode(<<$3>>) -> $/;
decode(<<$4>>) -> $+;
decode(<<$5>>) -> $=;
decode(_) -> throw(invalid_escape_sequence).
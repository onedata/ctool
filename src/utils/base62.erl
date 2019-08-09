%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Functions for converting base64 to base62 and back.
%%% @end
%%%--------------------------------------------------------------------
-module(base62).
-author("Lukasz Opiola").

%% API
-export([from_base64/1, to_base64/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec from_base64(binary()) -> binary().
from_base64(Token) ->
    <<<<(escape(C))/binary>> || <<C>> <= Token>>.


-spec to_base64(binary()) -> binary().
to_base64(Binary) ->
    to_base64(Binary, <<>>).

-spec to_base64(binary(), binary()) -> binary().
to_base64(<<"0", C:1/binary, Rest/binary>>, Result) ->
    to_base64(Rest, <<Result/binary, (unescape(C))>>);
to_base64(<<C:1/binary, Rest/binary>>, Result) ->
    to_base64(Rest, <<Result/binary, C/binary>>);
to_base64(<<>>, Result) ->
    Result.


-spec escape(char()) -> binary().
escape($0) -> <<$0, $0>>;
escape($_) -> <<$0, $1>>;
escape($-) -> <<$0, $2>>;
escape($/) -> <<$0, $3>>;
escape($+) -> <<$0, $4>>;
escape($=) -> <<$0, $5>>;
escape(C) -> <<C>>.


-spec unescape(binary()) -> char().
unescape(<<$0>>) -> $0;
unescape(<<$1>>) -> $_;
unescape(<<$2>>) -> $-;
unescape(<<$3>>) -> $/;
unescape(<<$4>>) -> $+;
unescape(<<$5>>) -> $=;
unescape(_) -> error(badarg).
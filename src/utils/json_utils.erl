%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Json parser utility functions
%%% @end
%%%--------------------------------------------------------------------
-module(json_utils).
-author("Tomasz Lichon").

%% API
-export([encode/1, decode/1]).
-export([encode_map/1, decode_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert an erlang term to JSON, producing
%% binary result. The output is in UTF8 encoding.
%% Possible terms, can be nested:
%% {struct, Props} - Props is a structure as a proplist,
%%    e.g.: [{id, 13}, {message, "mess"}]
%% {Props} - alias for above
%% {array, Array} - Array is a list, e.g.: [13, "mess"]
%% @end
%%--------------------------------------------------------------------
-spec encode(term()) -> binary().
encode(Term) ->
    Encoder = mochijson2:encoder([{utf8, true}]),
    iolist_to_binary(Encoder(Term)).

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert JSON binary to an erlang term.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> proplists:proplist().
decode(<<"">>) -> [];
decode(JSON) ->
    try mochijson2:decode(JSON, [{format, proplist}]) catch _:_ -> throw(invalid_json) end.

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert an erlang map to JSON, producing
%% binary result. The output is in UTF8 encoding.
%% @end
%%--------------------------------------------------------------------
-spec encode_map(maps:map()) -> binary().
encode_map(Map) ->
    jiffy:encode(Map).

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert JSON binary to an erlang map.
%% @end
%%--------------------------------------------------------------------
-spec decode_map(binary()) -> maps:map().
decode_map(JSON) ->
    try jiffy:decode(JSON, [return_maps]) catch _:_ -> throw(invalid_json) end.


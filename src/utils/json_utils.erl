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
-export([encode_deprecated/1, decode_deprecated/1]).
-export([map_to_list/1, list_to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert an erlang map to JSON, producing
%% binary result. The output is in UTF8 encoding.
%% @end
%%--------------------------------------------------------------------
-spec encode(maps:map()) -> binary().
encode(Map) ->
    iolist_to_binary(jiffy:encode(Map)).

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert JSON binary to an erlang map.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> maps:map().
decode(<<"">>) -> maps:new();
decode(JSON) ->
    try jiffy:decode(JSON, [return_maps]) catch _:_ -> throw(invalid_json) end.

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert an erlang map to JSON, producing
%% binary result. The output is in UTF8 encoding.
%% @end
%%--------------------------------------------------------------------
-spec encode_deprecated(term()) -> binary().
encode_deprecated(Term) ->
    iolist_to_binary(jiffy:encode(list_to_map(Term))).

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert JSON binary to an erlang map.
%% @end
%%--------------------------------------------------------------------
-spec decode_deprecated(binary()) -> proplists:proplist().
decode_deprecated(<<"">>) -> [];
decode_deprecated(JSON) ->
    try map_to_list(jiffy:decode(JSON, [return_maps])) 
    catch _:_ -> throw(invalid_json) end.

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that converts erlang map to erlang list.
%% @end
%%--------------------------------------------------------------------
-spec map_to_list(maps:map()) -> proplists:proplist().
map_to_list({Key, Value}) ->
        {Key, map_to_list(Value)};

map_to_list(Value) ->
    case erlang:is_map(Value) of
        true -> lists:map(fun map_to_list/1, maps:to_list(Value));
        false -> case erlang:is_list(Value) of
            true -> lists:map(fun map_to_list/1, Value);
            false -> Value
        end                   
    end.

%%--------------------------------------------------------------------
%% @doc
%% Convenience function that converts erlang proplist to erlang map.
%% @end
%%--------------------------------------------------------------------
list_to_map({Key, []}) ->
        {Key, []};

list_to_map({Key, Value}) ->
        {Key, list_to_map(Value)};

list_to_map(Value) ->
    case is_proplist(Value) of
        true -> maps:from_list(lists:map(fun list_to_map/1, Value));
        false -> case erlang:is_list(Value) of
                    true -> lists:map(fun list_to_map/1, Value);
                    false -> Value
                 end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private 
%% @doc Test if list is a proplist.
%% @end
%%--------------------------------------------------------------------
is_proplist([]) -> true;
is_proplist([{_,_}|L]) -> is_proplist(L);
is_proplist(_) -> false.

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

% Representation of any valid JSON term in erlang
-type json_term() :: jiffy:json_value().
-export_type([json_term/0]).

%% API
-export([encode/1, encode/2, decode/1, decode/2]).
-export([encode_deprecated/1, decode_deprecated/1]).
-export([map_to_list/1, list_to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @equiv encode(JsonTerm, [])
%% @end
%%--------------------------------------------------------------------
-spec encode(json_term()) -> binary().
encode(JsonTerm) ->
    encode(JsonTerm, []).


%%--------------------------------------------------------------------
%% @doc
%% Converts an erlang representation of JSON term to a binary JSON.
%% Objects are represented as maps. The output is in UTF8 encoding.
%% @end
%%--------------------------------------------------------------------
-spec encode(json_term(), JiffyOpts :: list()) -> binary().
encode(Map, JiffyOpts) ->
    iolist_to_binary(jiffy:encode(Map, JiffyOpts)).


%%--------------------------------------------------------------------
%% @doc
%% @equiv decode(JSON, [])
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | iolist()) -> json_term().
decode(JSON) ->
    decode(JSON, []).


%%--------------------------------------------------------------------
%% @doc
%% Converts a binary JSON to an erlang representation of JSON term.
%% Objects are represented as maps. The output is in UTF8 encoding.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | iolist(), JiffyOpts :: list()) -> json_term().
decode(<<"">>, _) -> maps:new();
decode(JSON, JiffyOpts) ->
    try
        jiffy:decode(JSON, [return_maps | JiffyOpts])
    catch _:_ ->
        throw(invalid_json)
    end.


%%--------------------------------------------------------------------
%% @doc
%% DEPRECATED
%% Converts an erlang representation of JSON term to a binary JSON.
%% Objects are represented as proplists. The output is in UTF8 encoding.
%% @end
%%--------------------------------------------------------------------
-spec encode_deprecated(term()) -> binary().
encode_deprecated(Term) ->
    iolist_to_binary(jiffy:encode(list_to_map(Term))).


%%--------------------------------------------------------------------
%% @doc
%% DEPRECATED
%% Converts a binary JSON to an erlang representation of JSON term.
%% Objects are represented as proplists. The output is in UTF8 encoding.
%% @end
%%--------------------------------------------------------------------
-spec decode_deprecated(binary()) -> proplists:proplist().
decode_deprecated(<<"">>) -> [];
decode_deprecated(JSON) ->
    try map_to_list(jiffy:decode(JSON, [return_maps])) 
    catch _:_ -> throw(invalid_json) end.


%%--------------------------------------------------------------------
%% @doc
%% Converts a nested map to a proplist.
%% @end
%%--------------------------------------------------------------------
-spec map_to_list(map()) -> proplists:proplist().
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
%% Converts a nested proplist to a map.
%% @end
%%--------------------------------------------------------------------
-spec list_to_map(proplists:proplist()) -> map().
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


%% @private
-spec is_proplist(term()) -> boolean().
is_proplist([]) -> true;
is_proplist([{_,_}|L]) -> is_proplist(L);
is_proplist(_) -> false.

%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015-2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Json utility functions
%%% @end
%%%--------------------------------------------------------------------
-module(json_utils).
-author("Tomasz Lichon").

-include("errors.hrl").

% Representation of any valid JSON term in erlang
-type json_term() :: jiffy:json_value().

% Jiffy accepts atoms as map keys, but Onedata convention is to use
% binaries in maps intended for JSON serialization.
-type json_map() :: #{binary() => json_term()}.

-type filter() :: [binary()].

-export_type([json_term/0, json_map/0, filter/0]).

%% API
-export([encode/1, encode/2, decode/1, decode/2]).
-export([encode_deprecated/1, decode_deprecated/1]).
-export([map_to_list/1, list_to_map/1]).
-export([find/2, insert/3, merge/1]).

%% Get byte size of json array index stored in binary: e. g. "[12]" -> 2
-define(INDEX_SIZE(__BINARY), (byte_size(__BINARY) - 2)).


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


%%--------------------------------------------------------------------
%% @doc
%% Find sub-json in json tree
%% @end
%%--------------------------------------------------------------------
-spec find(json_term(), filter()) -> json_term() | no_return().
find(Json, []) ->
    Json;
find(Json, [Name | Rest]) ->
    IndexSize = ?INDEX_SIZE(Name),
    case Name of
        <<"[", Element:IndexSize/binary, "]">> when is_list(Json) ->
            case catch (binary_to_integer(Element) + 1) of
                {'EXIT', _} ->
                    throw({error, ?ENOATTR});
                Index ->
                    case length(Json) < Index of
                        true ->
                            throw({error, ?ENOATTR});
                        false ->
                            find(lists:nth(Index, Json), Rest)
                    end
            end;
        _ when is_map(Json) ->
            case maps:find(Name, Json) of
                error ->
                    throw({error, ?ENOATTR});
                {ok, SubJson} ->
                    find(SubJson, Rest)
            end;
        _ ->
            throw({error, ?ENOATTR})
    end.


%%--------------------------------------------------------------------
%% @doc
%% Insert sub-json to json tree
%% @end
%%--------------------------------------------------------------------
-spec insert(undefined | json_term(), JsonToInsert :: json_term(), filter()) ->
    map() | no_return().
insert(_Json, JsonToInsert, []) ->
    JsonToInsert;
insert(undefined, JsonToInsert, [Name | Rest]) ->
    IndexSize = ?INDEX_SIZE(Name),
    case Name of
        <<"[", Element:IndexSize/binary, "]">> ->
            case catch (binary_to_integer(Element) + 1) of
                {'EXIT', _} ->
                    #{Name => insert(undefined, JsonToInsert, Rest)};
                Index ->
                    lists:foldl(
                        fun(_, Acc) -> [null | Acc] end,
                        [insert(undefined, JsonToInsert, Rest)],
                        lists:seq(1, Index - 1)
                    )
            end;
        _ ->
            #{Name => insert(undefined, JsonToInsert, Rest)}
    end;
insert(Json, JsonToInsert, [Name | Rest]) ->
    IndexSize = ?INDEX_SIZE(Name),
    case Name of
        <<"[", Element:IndexSize/binary, "]">> when is_list(Json) ->
            case catch (binary_to_integer(Element) + 1) of
                {'EXIT', _} ->
                    throw({error, ?ENOATTR});
                Index ->
                    Length = length(Json),
                    case Length < Index of
                        true ->
                            Json
                            ++ [null || _ <- lists:seq(Length + 1, Index - 1)]
                            ++ [insert(undefined, JsonToInsert, Rest)];
                        false ->
                            setnth(
                                Index, Json,
                                insert(lists:nth(Index, Json), JsonToInsert, Rest)
                            )
                    end
            end;
        _ when is_map(Json) ->
            SubJson = maps:get(Name, Json, undefined),
            Json#{Name => insert(SubJson, JsonToInsert, Rest)};
        _ ->
            throw({error, ?ENOATTR})
    end.


%% @private
-spec merge([json_term()]) -> json_term().
merge(JsonTerms) ->
    lists:foldl(fun
        (Json, ParentJson) when is_map(Json) andalso is_map(ParentJson) ->
            ChildKeys = maps:keys(Json),
            ParentKeys = maps:keys(ParentJson),
            ChildOnlyKey = ChildKeys -- ParentKeys,
            CommonKeys = ChildKeys -- ChildOnlyKey,

            ResultingJson = maps:merge(
                ParentJson,
                maps:with(ChildOnlyKey, Json)
            ),

            lists:foldl(fun(Key, Acc) ->
                ChildValue = maps:get(Key, Json),
                ParentValue = maps:get(Key, ParentJson),
                Acc#{Key => merge([ParentValue, ChildValue])}
            end, ResultingJson, CommonKeys);

        (Json, _ParentJson) ->
            Json
    end, #{}, JsonTerms).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set nth element of list
%% @end
%%--------------------------------------------------------------------
-spec setnth(non_neg_integer(), list(), term()) -> list().
setnth(1, [_ | Rest], New) -> [New | Rest];
setnth(I, [E | Rest], New) -> [E | setnth(I - 1, Rest, New)].


%% @private
-spec is_proplist(term()) -> boolean().
is_proplist([]) -> true;
is_proplist([{_,_}|L]) -> is_proplist(L);
is_proplist(_) -> false.

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

-include("logging.hrl").

% Representation of any valid JSON term in erlang
-type json_term() :: jiffy:json_value().

% Jiffy accepts atoms as map keys, but Onedata convention is to use
% binaries in maps intended for JSON serialization.
-type json_map() :: #{binary() => json_term()}.

% List of binaries pointing to sub-json of a json, e.g
% [<<"attr1">>, <<"[2]">>] points to element <<"a">> in
% #{<<"attr1">> => [1, null, <<"a">>], <<"attr2">> => <<"val2">>}
-type query() :: [binary()].

-export_type([json_term/0, json_map/0, query/0]).

%% API
-export([encode/1, encode/2, decode/1, decode/2]).
-export([encode_deprecated/1, decode_deprecated/1]).
-export([map_to_list/1, list_to_map/1]).
-export([query/2, insert/3, merge/1]).

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
    catch Class:Reason ->
        ?debug("Failed to decode invalid json (~w:~p), payload:~n~p", [Class, Reason, JSON]),
        error(invalid_json)
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
    catch _:_ -> error(invalid_json) end.


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
%% Find sub-json in json tree specified by filter, eg.g:
%%
%% query(#{<<"a">> => #{<<"b">> => 1}}, [<<"a">>, <<"b">>]) -> {ok, 1}
%% query(#{<<"a">> => [1, 2, <<"a">>}, [<<"a">>, <<"[2]">>]) -> {ok, <<"a">>}
%%
%% @end
%%--------------------------------------------------------------------
-spec query(json_term(), query()) -> {ok, json_term()} | error.
query(Json, []) ->
    {ok, Json};
query(Json, [Name | Rest]) when is_map(Json) ->
    case maps:find(Name, Json) of
        {ok, SubJson} ->
            query(SubJson, Rest);
        error ->
            error
    end;
query(Json, [Name | Rest]) when is_list(Json) ->
    case decode_array_index(Name) of
        {ok, Index} ->
            case length(Json) < Index of
                true ->
                    error;
                false ->
                    query(lists:nth(Index, Json), Rest)
            end;
        error ->
            error
    end;
query(_, _) ->
    error.


%%--------------------------------------------------------------------
%% @doc
%% Insert sub-json to json tree under specified by filter path, e.g:
%%
%% insert(#{<<"a">> => #{<<"b">> => 1}}, <<"val">>, [<<"a">>, <<"c">>]) ->
%%      {ok, #{<<"a">> => #{<<"b">> => 1, <<"c">> => <<"val">>}}}
%% insert(#{<<"a">> => [1, 2, <<"a">>}, 5, [<<"a">>, <<"[3]">>]) ->
%%      {ok, #{<<"a">> => [1, 2, <<"a">>, 5}}
%%
%% @end
%%--------------------------------------------------------------------
-spec insert(undefined | json_term(), JsonToInsert :: json_term(), query()) ->
    {ok, json_term()} | error.
insert(Json, JsonToInsert, Filter) ->
    try
        {ok, insert_internal(Json, JsonToInsert, Filter)}
    catch _:_ ->
        error
    end.


-spec merge([json_term()]) -> json_term().
merge(JsonTerms) ->
    lists:foldl(fun
        (ChildJson, ParentJson) when is_map(ChildJson) andalso is_map(ParentJson) ->

            maps:fold(fun(ChildKey, ChildValue, Acc) ->
                case maps:find(ChildKey, ParentJson) of
                    {ok, ParentValue} ->
                        Acc#{ChildKey => merge([ParentValue, ChildValue])};
                    error ->
                        Acc#{ChildKey => ChildValue}
                end
            end, ParentJson, ChildJson);

        (Json, _ParentJson) ->
            Json
    end, #{}, JsonTerms).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec insert_internal(undefined | json_term(), JsonToInsert :: json_term(), query()) ->
    json_term() | no_return().
insert_internal(_Json, JsonToInsert, []) ->
    JsonToInsert;
insert_internal(undefined, JsonToInsert, [Name | Rest]) ->
    case decode_array_index(Name) of
        {ok, Index} ->
            lists:foldl(
                fun(_, Acc) -> [null | Acc] end,
                [insert_internal(undefined, JsonToInsert, Rest)],
                lists:seq(1, Index - 1)
            );
        error ->
            % Not an array index - should be treated as normal object field
            #{Name => insert_internal(undefined, JsonToInsert, Rest)}
    end;
insert_internal(Json, JsonToInsert, [Name | Rest]) when is_map(Json) ->
    SubJson = maps:get(Name, Json, undefined),
    Json#{Name => insert_internal(SubJson, JsonToInsert, Rest)};
insert_internal(Json, JsonToInsert, [Name | Rest]) when is_list(Json) ->
    Length = length(Json),
    case decode_array_index(Name) of
        {ok, Index} ->
            case Length < Index of
                true ->
                    Json
                    ++ [null || _ <- lists:seq(Length + 1, Index - 1)]
                    ++ [insert_internal(undefined, JsonToInsert, Rest)];
                false ->
                    replace_element(
                        Index,
                        insert_internal(lists:nth(Index, Json), JsonToInsert, Rest),
                        Json
                    )
            end;
        error ->
            throw(badarg)
    end;
insert_internal(_, _, _) ->
    throw(badarg).


%% @private
-spec decode_array_index(binary()) -> {ok, integer()} | error.
decode_array_index(EncodedArrayIndex) ->
    try
        IndexSize = ?INDEX_SIZE(EncodedArrayIndex),
        <<"[", IndexBin:IndexSize/binary, "]">> = EncodedArrayIndex,
        {ok, binary_to_integer(IndexBin) + 1}
    catch _:_ ->
        error
    end.


%% @private
-spec replace_element(non_neg_integer(), term(), list()) -> list().
replace_element(1, New, [_ | Rest]) ->
    [New | Rest];
replace_element(Index, New, [Elem | Rest]) ->
    [Elem | replace_element(Index - 1, New, Rest)].


%% @private
-spec is_proplist(term()) -> boolean().
is_proplist([]) -> true;
is_proplist([{_,_}|L]) -> is_proplist(L);
is_proplist(_) -> false.

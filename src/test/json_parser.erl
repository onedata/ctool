%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Json utils functions
%%% @end
%%%-------------------------------------------------------------------
-module(json_parser).
-author("Tomasz Lichon").

%% API
-export([parse_json_binary_to_atom_proplist/1, get_value/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse json binary as proplist of atoms
%% i. e.
%% json binary: {"a": ["a1"], "b": ["b1", "b2"]}
%% is converted to erlang proplist: [{a, [a1]}, {b, [b1, b2]}]
%% @end
%%--------------------------------------------------------------------
-spec parse_json_binary_to_atom_proplist(JsonBinary :: binary()) -> list() | no_return().
parse_json_binary_to_atom_proplist(JsonBinary) ->
    Json = json_utils:decode(JsonBinary),
    convert_to_atoms(Json).

%%--------------------------------------------------------------------
%% @doc
%% Get value from json file by given key.
%% i. e.
%% to get "a2" from json: {"a": {"a1" : ["a2"]}, "b": ["b1", "b2"]}
%% in file file.json,
%% call: get_value("a/a1", "file.json", "/")
%% @end
%%--------------------------------------------------------------------
-spec get_value(Key :: string(), JsonPath :: string(), JsonKeySeparator :: string()) -> atom().
get_value(Key, JsonPath, JsonKeySeparator) ->
    {ok, JsonBinary} = file:read_file(JsonPath),
    JsonProplist = parse_json_binary_to_atom_proplist(JsonBinary),
    Keys = string:tokens(Key, JsonKeySeparator),
    get_value_from_key(Keys, JsonProplist).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse proplist containing binaries to proplist containing atoms
%% @end
%%--------------------------------------------------------------------
-spec convert_to_atoms(List :: list()) -> list() | no_return().
convert_to_atoms([]) ->
    [];
convert_to_atoms({K, V}) ->
    {binary_to_atom(K, unicode), convert_to_atoms(V)};
convert_to_atoms([Head | Tail]) ->
    [convert_to_atoms(Head) | convert_to_atoms(Tail)];
convert_to_atoms(Binary) when is_binary(Binary) ->
    binary_to_atom(Binary, unicode);
convert_to_atoms(Other) ->
    Other.

%%--------------------------------------------------------------------
%% @doc
%% Get value from a json represented as a proplist
%% @end
%%--------------------------------------------------------------------
-spec get_value_from_key(list(), list() | atom()) -> atom().
get_value_from_key(_, undefined) -> undefined;
get_value_from_key([Key], JsonProplist) ->
    proplists:get_value(list_to_atom(Key), JsonProplist);
get_value_from_key([Key | Keys], JsonProplist) ->
    Sublist  = proplists:get_value(list_to_atom(Key), JsonProplist),
    get_value_from_key(Keys, Sublist).


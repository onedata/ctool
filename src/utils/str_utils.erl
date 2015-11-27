%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains various functions operating on strings (both lists and binaries).
%%% @end
%%%-------------------------------------------------------------------
-module(str_utils).

% Conversion
-export([to_list/1, to_binary/1, join_binary/2]).

% Conversion between unicode and binaries
-export([unicode_list_to_binary/1, binary_to_unicode_list/1]).

% String formatting
-export([format/2, format_bin/2]).

% File path manipulation
-export([ensure_ends_with_slash/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Converts any term to list.
%% @end
%%--------------------------------------------------------------------
-spec to_list(Term :: term()) -> list().
to_list(List) when is_list(List) -> List;
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Term) -> format("~p", [Term]).


%%--------------------------------------------------------------------
%% @doc Converts any term to binary.
%% @end
%%--------------------------------------------------------------------
-spec to_binary(Term :: term()) -> binary().
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(List) when is_list(List) -> iolist_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_binary(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
to_binary(Term) -> list_to_binary(to_list(Term)).


%%--------------------------------------------------------------------
%% @doc Joins a list of binaries into one binary, using a separator.
%% @end
%%--------------------------------------------------------------------
-spec join_binary(Terms :: [binary()], Separator :: binary()) -> binary().
join_binary([], _Sep) ->
    <<>>;
join_binary([Part], _Sep) ->
    Part;
join_binary([Head | Tail], Sep) ->
    lists:foldl(
        fun(A, B) ->
            <<B/binary, Sep/binary, A/binary>>
        end, Head, Tail).


%%--------------------------------------------------------------------
%% @doc Converts a unicode list to utf8 binary.
%% @end
%%--------------------------------------------------------------------
-spec unicode_list_to_binary(String :: string() | binary()) -> binary().
unicode_list_to_binary(Binary) when is_binary(Binary) ->
    Binary;
unicode_list_to_binary(String) ->
    unicode:characters_to_binary(String).


%%--------------------------------------------------------------------
%% @doc Converts a utf8 binary to unicode list.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_unicode_list(Binary :: binary() | string()) -> string().
binary_to_unicode_list(String) when is_list(String) ->
    String;
binary_to_unicode_list(Binary) ->
    unicode:characters_to_list(Binary).


%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec format(Format :: string(), Args :: [term()]) -> list().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).


%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec format_bin(Format :: string(), Args :: [term()]) -> binary().
format_bin(Format, Args) ->
    to_binary(format(Format, Args)).


%%--------------------------------------------------------------------
%% @doc
%% Appends '/' to the end of filepath if last character is different
%% @end
%%--------------------------------------------------------------------
-spec ensure_ends_with_slash(binary()) -> binary().
ensure_ends_with_slash(<<"">>) ->
    <<"/">>;
ensure_ends_with_slash(Path) ->
    case binary:last(Path) of
        $/ -> Path;
        _ -> <<Path/binary, "/">>
    end.
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

-module(gui_str).
-include("logging.hrl").

% Conversion
-export([to_list/1, to_binary/1, join_to_binary/2]).

% Conversion between unicode and binaries
-export([unicode_list_to_binary/1, binary_to_unicode_list/1]).

% String formatting
-export([format/2, format_bin/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Converts any term to list.
%% @end
%%--------------------------------------------------------------------
-spec to_list(Term :: term()) -> list().
to_list(undefined) -> [];
to_list(Term) when is_list(Term) -> Term;
to_list(Term) when is_binary(Term) -> binary_to_list(Term);
to_list(Term) ->
    try
        wf:to_list(Term)
    catch _:_ ->
        lists:flatten(io_lib:format("~p", [Term]))
    end.


%%--------------------------------------------------------------------
%% @doc Converts any term to binary.
%% @end
%%--------------------------------------------------------------------
-spec to_binary(Term :: term()) -> binary().
to_binary(Term) when is_binary(Term) -> Term;
to_binary(Term) -> list_to_binary(to_list(Term)).


%%--------------------------------------------------------------------
%% @doc Joins a list of binaries into one binary, using delimiter.
%% @end
%%--------------------------------------------------------------------
-spec join_to_binary(List :: [binary()], Delimiter :: binary()) -> binary().
join_to_binary(Terms, Delimiter) ->
    join_to_binary(Terms, Delimiter, <<"">>).

join_to_binary([], _, Acc) ->
    Acc;

join_to_binary([H | T], Delimiter, Acc) ->
    join_to_binary(T, <<Acc/binary, Delimiter/binary, (to_binary(H))/binary>>).


%%--------------------------------------------------------------------
%% @doc Converts a unicode list to utf8 binary.
%% @end
%%--------------------------------------------------------------------
-spec unicode_list_to_binary(String :: string()) -> binary().
unicode_list_to_binary(String) ->
    unicode:characters_to_binary(String).


%%--------------------------------------------------------------------
%% @doc Converts a utf8 binary to unicode list.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_unicode_list(Binary :: binary()) -> string().
binary_to_unicode_list(Binary) ->
    unicode:characters_to_list(Binary).


%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec format(Format :: string(), Args :: [term()]) -> list().
format(Format, Args) ->
    wf:f(Format, Args).


%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec format_bin(Format :: string(), Args :: [term()]) -> binary().
format_bin(Format, Args) ->
    to_binary(wf:f(Format, Args)).
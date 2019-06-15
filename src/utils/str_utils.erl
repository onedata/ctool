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
-export([to_list/1, to_binary/1]).
-export([join_binary/1, join_binary/2, reverse_binary/1, binary_starts_with/2]).

% Conversion between unicode and binaries
-export([unicode_list_to_binary/1, binary_to_unicode_list/1]).

% String formatting
-export([format/2, format_bin/2, format_byte_size/1]).

-export([rand_hex/1]).


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
%% @doc Joins a list of binaries into one binary.
%% @end
%%--------------------------------------------------------------------
-spec join_binary(Binaries :: [binary()]) -> binary().
join_binary(Binaries) ->
    join_binary(Binaries, <<"">>).


%%--------------------------------------------------------------------
%% @doc Joins a list of binaries into one binary, using given separator.
%% @end
%%--------------------------------------------------------------------
-spec join_binary(Binaries :: [binary()], Separator :: binary()) -> binary().
join_binary([], _Sep) ->
    <<>>;
join_binary([Part], _Sep) ->
    Part;
join_binary([Head | Tail], Sep) ->
    lists:foldl(
        fun(Part, Acc) ->
            <<Acc/binary, Sep/binary, Part/binary>>
        end, Head, Tail).


%%--------------------------------------------------------------------
%% @doc Reverse given binary.
%%--------------------------------------------------------------------
-spec reverse_binary(binary()) -> binary().
reverse_binary(Binary) ->
    S = size(Binary) * 8,
    <<X:S/integer-little>> = Binary,
    <<X:S/integer-big>>.


%--------------------------------------------------------------------
%% @doc Predicate that tells whether binary starts with given prefix.
%%--------------------------------------------------------------------
-spec binary_starts_with(Binary :: binary(), Prefix :: binary()) -> boolean().
binary_starts_with(Binary, Prefix) ->
    Size = byte_size(Prefix),
    case Binary of
        <<Prefix:Size/binary, _/binary>> -> true;
        _ -> false
    end.


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
%% @doc Returns a formatted string.
%% @end
%%--------------------------------------------------------------------
-spec format(Format :: string(), Args :: [term()]) -> list().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).


%%--------------------------------------------------------------------
%% @doc Returns a formatted binary.
%% @end
%%--------------------------------------------------------------------
-spec format_bin(Format :: string(), Args :: [term()]) -> binary().
format_bin(Format, Args) ->
    to_binary(format(Format, Args)).


%%--------------------------------------------------------------------
%% @doc Returns a formatted byte size with corresponding unit.
%% @end
%%--------------------------------------------------------------------
-spec format_byte_size(non_neg_integer()) -> binary().
format_byte_size(Size) ->
    format_byte_size(Size, ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]).

format_byte_size(Size, [_ | Units]) when Size >= 1024 ->
    format_byte_size(Size / 1024, Units);
format_byte_size(Size, [Unit | _]) ->
    case trunc(Size) == Size of
        true -> format_bin("~B ~s", [trunc(Size), Unit]);
        false -> format_bin("~.2f ~s", [Size, Unit])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Generates random binary hex string of given size.
%% @end
%%--------------------------------------------------------------------
-spec rand_hex(non_neg_integer()) -> binary().
rand_hex(Size) ->
    hex_utils:hex(crypto:strong_rand_bytes(Size)).

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

-include("validation.hrl").

% Conversion
-export([to_list/1, to_binary/1]).
-export([join_as_binaries/2, join_binary/1, join_binary/2, reverse_binary/1]).
-export([ensure_suffix/2, truncate_overflow/2]).
-export([binary_starts_with/2, binary_ends_with/2]).

% Conversion between unicode and binaries
-export([unicode_list_to_binary/1, binary_to_unicode_list/1]).

% String formatting
-export([format/1, format/2, format_bin/2, format_byte_size/1]).

-export([md5_digest/1]).
-export([rand_hex/1]).
-export([pad_left/3, pad_right/3]).
-export([validate_name/1, validate_name/5]).


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
to_list(Term) -> format("~tp", [Term]).


%%--------------------------------------------------------------------
%% @doc Converts any term to binary.
%% @end
%%--------------------------------------------------------------------
-spec to_binary(Term :: term()) -> binary().
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(List) when is_list(List) -> iolist_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_binary(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
to_binary(Term) -> unicode_list_to_binary(to_list(Term)).


%%--------------------------------------------------------------------
%% @doc Joins a list of terms represented as binary into one binary.
%% @end
%%--------------------------------------------------------------------
-spec join_as_binaries(Terms :: [term()], Separator :: term()) -> binary().
join_as_binaries(Terms, Separator) ->
    join_binary([to_binary(T) || T <- Terms], to_binary(Separator)).


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


%%--------------------------------------------------------------------
%% @doc Predicate saying whether binary starts with given prefix.
%%--------------------------------------------------------------------
-spec binary_starts_with(Binary :: binary(), Prefix :: binary()) -> boolean().
binary_starts_with(Binary, Prefix) ->
    byte_size(Prefix) == binary:longest_common_prefix([Binary, Prefix]).


%%--------------------------------------------------------------------
%% @doc Predicate saying whether binary ends with given prefix.
%%--------------------------------------------------------------------
-spec binary_ends_with(Binary :: binary(), Prefix :: binary()) -> boolean().
binary_ends_with(Binary, Suffix) ->
    byte_size(Suffix) == binary:longest_common_suffix([Binary, Suffix]).


%%--------------------------------------------------------------------
%% @doc Ensures a string ends with a given string by adding the suffix
%% if not already present.
%% @end
%%--------------------------------------------------------------------
-spec ensure_suffix(String :: unicode:chardata(), Suffix :: unicode:chardata()) ->
    binary().
ensure_suffix(String, Suffix)->
    StringBin = unicode_list_to_binary(String),
    SuffixBin = unicode_list_to_binary(Suffix),
    case binary_ends_with(StringBin, SuffixBin) of
        true -> StringBin;
        false -> <<StringBin/binary, SuffixBin/binary>>
    end.


-spec truncate_overflow(binary(), non_neg_integer()) -> binary().
truncate_overflow(Binary, MaxSize) ->
    case byte_size(Binary) > MaxSize of
        true ->
            Part = binary:part(Binary, 0, MaxSize),
            <<Part/binary, "... [truncated]">>;
        false ->
            Binary
    end.

%%--------------------------------------------------------------------
%% @doc Converts a unicode list to utf8 binary.
%% @end
%%--------------------------------------------------------------------
-spec unicode_list_to_binary(unicode:chardata()) -> binary().
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
%% @equiv format(Format, []).
%% @end
%%--------------------------------------------------------------------
-spec format(Format :: string()) -> string().
format(Format) ->
    format(Format, []).


%%--------------------------------------------------------------------
%% @doc Returns a formatted string.
%% @end
%%--------------------------------------------------------------------
-spec format(Format :: string(), Args :: [term()]) -> string().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).


%%--------------------------------------------------------------------
%% @doc Returns a formatted binary.
%% @end
%%--------------------------------------------------------------------
-spec format_bin(Format :: string(), Args :: [term()]) -> binary().
format_bin(Format, Args) ->
    unicode_list_to_binary(format(Format, Args)).


%%--------------------------------------------------------------------
%% @doc Returns a formatted byte size with corresponding unit.
%% @end
%%--------------------------------------------------------------------
-spec format_byte_size(non_neg_integer()) -> string().
format_byte_size(Size) ->
    format_byte_size(Size, ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]).

format_byte_size(Size, [_ | Units]) when Size >= 1024 ->
    format_byte_size(Size / 1024, Units);
format_byte_size(Size, [Unit | _]) ->
    case trunc(Size) == Size of
        true -> format("~B ~s", [trunc(Size), Unit]);
        false -> format("~.2f ~s", [Size, Unit])
    end.


-spec md5_digest(term() | [binary() | term()]) -> binary().
md5_digest(Term) when not is_list(Term) ->
    md5_digest([Term]);
md5_digest(DigestComponents) when length(DigestComponents) > 0 ->
    FinalCtx = lists:foldl(fun
        (Bin, Ctx) when is_binary(Bin) -> crypto:hash_update(Ctx, Bin);
        (Term, Ctx) -> crypto:hash_update(Ctx, term_to_binary(Term))
    end, crypto:hash_init(md5), DigestComponents),
    hex_utils:hex(crypto:hash_final(FinalCtx)).


%%--------------------------------------------------------------------
%% @doc
%% Generates a hex string from a random binary of given size. The resulting
%% string is two times longer than the number of bytes (Size), as each byte is
%% encoded by two hex characters.
%% @end
%%--------------------------------------------------------------------
-spec rand_hex(non_neg_integer()) -> binary().
rand_hex(Size) ->
    hex_utils:hex(crypto:strong_rand_bytes(Size)).


-spec pad_left(binary(), non_neg_integer(), binary()) -> binary().
pad_left(Binary, ExpectedSize, Char) ->
    BytesToFill = max(ExpectedSize - byte_size(Binary), 0),
    Padding = binary:copy(Char, BytesToFill),
    <<Padding/binary, Binary/binary>>.


-spec pad_right(binary(), non_neg_integer(), binary()) -> binary().
pad_right(Binary, ExpectedSize, Char) ->
    BytesToFill = max(ExpectedSize - byte_size(Binary), 0),
    Padding = binary:copy(Char, BytesToFill),
    <<Binary/binary, Padding/binary>>.


%%--------------------------------------------------------------------
%% @doc
%% Validates a name against universal name format.
%% @end
%%--------------------------------------------------------------------
-spec validate_name(binary()) -> boolean().
validate_name(Name) ->
    validate_name(
        Name, ?NAME_FIRST_CHARS_ALLOWED, ?NAME_MIDDLE_CHARS_ALLOWED,
        ?NAME_LAST_CHARS_ALLOWED, ?NAME_MAXIMUM_LENGTH
    ).


%%--------------------------------------------------------------------
%% @doc
%% Validates a name against given format.
%% @end
%%--------------------------------------------------------------------
-spec validate_name(Name :: binary(), FirstRgx :: binary(), MiddleRgx :: binary(),
    LastRgx :: binary(), MaxLength :: non_neg_integer()) -> boolean().
validate_name(Name, _, _, _, _) when not is_binary(Name) ->
    false;
validate_name(Name, FirstRgx, MiddleRgx, LastRgx, MaxLength) ->
    Regexp = <<
        "^[", FirstRgx/binary, "][", MiddleRgx/binary,
        "]{0,", (integer_to_binary(MaxLength - 2))/binary,
        "}[", LastRgx/binary, "]$"
    >>,
    try re:run(Name, Regexp, [{capture, none}, unicode, ucp]) of
        match -> true;
        _ -> false
    catch _:_ ->
        false
    end.

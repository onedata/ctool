%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utilities regarding unicode binaries.
%%% @end
%%%-------------------------------------------------------------------
-module(unicode_utils).

-export([remove_diacritics/1]).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Removes accents, diacritics etc. by replacing them with as-close-as-possible
%% ASCII counterparts (e.g. "ć" -> "c", "Ø" -> "O").
%% NOTE: the procedure is heuristic and may not work for some character sets.
%% NOTE: the resulting string may still include other unicode chars.
%% @end
%%--------------------------------------------------------------------
-spec remove_diacritics(binary()) -> binary().
remove_diacritics(UnicodeList) when is_list(UnicodeList) ->
    remove_diacritics_internal(UnicodeList);
remove_diacritics(Binary) when is_binary(Binary) ->
    unicode:characters_to_binary(remove_diacritics_internal(unicode:characters_to_list(Binary))).


%%%===================================================================
%%% Helpers
%%%===================================================================

-spec remove_diacritics_internal(string()) -> string().
remove_diacritics_internal("") ->
    "";
remove_diacritics_internal(UnicodeList) ->
    [Grapheme | Tail] = string:next_grapheme(UnicodeList),
    PossiblyConvertedGraphemeCluster = case non_latin_to_ascii_counterpart([Grapheme]) of
        [Grapheme] ->
            case unicode:characters_to_nfkd_list([Grapheme]) of
                [Grapheme] ->
                    [Grapheme];
                NfkdList ->
                    % remove combining chars
                    lists:filter(fun
                        (Char) when Char >= 16#300, Char =< 16#36F -> false;
                        (_Char) -> true
                    end, NfkdList)
            end;
        AsciiCounterpart ->
            AsciiCounterpart
    end,
    PossiblyConvertedGraphemeCluster ++ remove_diacritics_internal(Tail).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTE: this is (probably) a non-exhaustive list, expected to be
%% improved over time as more and more nations start using Onedata :)
%% @end
%%--------------------------------------------------------------------
-spec non_latin_to_ascii_counterpart(string:grapheme_cluster()) -> string().
non_latin_to_ascii_counterpart("æ") -> "ae";
non_latin_to_ascii_counterpart("Æ") -> "AE";
non_latin_to_ascii_counterpart("ǽ") -> "ae";
non_latin_to_ascii_counterpart("Ǽ") -> "AE";
non_latin_to_ascii_counterpart("œ") -> "oe";
non_latin_to_ascii_counterpart("Œ") -> "OE";
non_latin_to_ascii_counterpart("ß") -> "ss";
non_latin_to_ascii_counterpart("ẞ") -> "SS";

non_latin_to_ascii_counterpart("þ") -> "b";
non_latin_to_ascii_counterpart("Þ") -> "B";
non_latin_to_ascii_counterpart("đ") -> "d";
non_latin_to_ascii_counterpart("Đ") -> "D";
non_latin_to_ascii_counterpart("ð") -> "d";
non_latin_to_ascii_counterpart("Ð") -> "D";
non_latin_to_ascii_counterpart("ƒ") -> "f";
non_latin_to_ascii_counterpart("Ƒ") -> "F";
non_latin_to_ascii_counterpart("ħ") -> "h";
non_latin_to_ascii_counterpart("Ħ") -> "H";
non_latin_to_ascii_counterpart("ı") -> "i";
non_latin_to_ascii_counterpart("ɪ") -> "I";
non_latin_to_ascii_counterpart("ł") -> "l";
non_latin_to_ascii_counterpart("Ł") -> "L";
non_latin_to_ascii_counterpart("ø") -> "o";
non_latin_to_ascii_counterpart("Ø") -> "O";
non_latin_to_ascii_counterpart("ǿ") -> "o";
non_latin_to_ascii_counterpart("Ǿ") -> "O";
non_latin_to_ascii_counterpart("ŧ") -> "t";
non_latin_to_ascii_counterpart("Ŧ") -> "T";
non_latin_to_ascii_counterpart(Grapheme) -> Grapheme.

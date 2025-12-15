%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utilities for comparing Onedata versions that are based on Calendar Versioning.
%%% The following rules apply:
%%%   * A version is built like the following: YY.Minor[.Patch][-Label]
%%%     (see examples below).
%%%   * YY denotes the current year, although the release may be published
%%%     before the corresponding calendar year starts, or after a new year has started.
%%%   * The change of year does not imply breaking of compatibility, but it's possible
%%%     to break the compatibility ONLY when the year is incremented.
%%%   * Minor versions are released when there has been at least one new feature that
%%%     is backward compatible.
%%%   * Patch versions are released when there has been an important hotfix / bugfix
%%%     worth releasing, but not new features.
%%%   * If the Patch version is "0", it's omitted from the notation, e.g.: 25.0.
%%%   * Label(s) can be used for pre-release version and build metadata. The label
%%%     is always preceded by a dash "-" and contains only letters, and is always followed
%%%     by a dot and an integer. The allowed labels are: alpha, beta, rc. Examples:
%%%       -alpha.1
%%%       -alpha.2
%%%       -beta.21
%%%       -rc.4
%%%   * Labeled versions are LOWER than "full versions". The following ordering of labels
%%%     is enforced:
%%%     25.0-alpha.1 < 25.0-alpha.4 < 25.0-beta.1 < 25.0-beta.2 < 25.0-rc.1 < 25.0
%%%
%%% Exemplary release numbers, ordered in ascending order:
%%%   25.0-alpha.1
%%%   25.0-alpha.2
%%%   25.0-beta.1
%%%   25.0-rc.1
%%%   25.0
%%%   25.0.1
%%%   25.0.2
%%%   25.1
%%%   25.1.1
%%%   26.0-beta.15
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_calver).
-author("Lukasz Opiola").


-type label_specifier() :: alpha | beta | rc | undefined.

-record(od_calver, {
    year :: non_neg_integer(),
    minor :: non_neg_integer(),
    patch = 0 :: non_neg_integer(),
    label_specifier = undefined :: label_specifier(),
    label_ordinal = 0 :: non_neg_integer()
}).
-type od_calver() :: #od_calver{}.

-type version() :: binary().
-export_type([version/0]).


%% API
-export([compare/2]).
-export([compare_year/2]).


%%%===================================================================
%%% API
%%%===================================================================


-spec compare(version(), version()) -> lower | equal | greater.
compare(V1, V2) ->
    compare_terms(parse(V1), parse(V2)).


-spec compare_year(version(), version()) -> lower | equal | greater.
compare_year(V1, V2) ->
    compare_terms((parse(V1))#od_calver.year, (parse(V2))#od_calver.year).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec compare_terms(term(), term()) -> lower | equal | greater.
compare_terms(T1, T2) when T1 < T2 -> lower;
compare_terms(T1, T2) when T1 == T2 -> equal;
compare_terms(T1, T2) when T1 > T2 -> greater.


%% @private
-spec parse(version()) -> od_calver().
parse(VersionBin) when is_binary(VersionBin) ->
    Version = binary_to_list(VersionBin),

    {MainPart, LabelPart} = case string:split(Version, "-", all) of
        [Main] -> {Main, undefined};
        [Main, Label] -> {Main, Label}
    end,

    Numbers = [list_to_integer(N) || N <- string:split(MainPart, ".", all)],
    {Year, Minor, Patch} = case Numbers of
        [Y, M] -> {Y, M, 0};
        [Y, M, P] -> {Y, M, P}
    end,

    {LabelSpecifier, LabelOrdinal} = case LabelPart of
        undefined ->
            {undefined, 0};

        _ when Year > 21 ->
            % modern format: $label.$ordinal (e.g. alpha.2)
            [LabelStr, OrdStr] = string:split(LabelPart, ".", all),
            {parse_label(LabelStr), list_to_integer(OrdStr)};

        _ ->
            % legacy format (Year <= 21): $label$ordinal (e.g. alpha2)
            {LabelStr, OrdStr} = split_legacy_label(LabelPart),
            {parse_label(LabelStr), list_to_integer(OrdStr)}
    end,

    #od_calver{
        year = Year,
        minor = Minor,
        patch = Patch,
        label_specifier = LabelSpecifier,
        label_ordinal = LabelOrdinal
    }.


%% @private
-spec parse_label(string()) -> alpha | beta | rc.
parse_label("alpha") -> alpha;
parse_label("beta") -> beta;
parse_label("rc") -> rc.


%% @private
-spec split_legacy_label(string()) -> {string(), string()}.
split_legacy_label(Label) ->
    {DigitsRev, LettersRev} = lists:splitwith(fun(C) ->
        C >= $0 andalso C =< $9
    end, lists:reverse(Label)),
    {lists:reverse(LettersRev), lists:reverse(DigitsRev)}.
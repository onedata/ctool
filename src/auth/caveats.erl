%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Functions for manipulating token caveats.
%%% @end
%%%--------------------------------------------------------------------
-module(caveats).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("logging.hrl").

-type type() :: cv_time | cv_authorization_none | cv_audience |
cv_ip | cv_asn | cv_country | cv_region | cv_api |
cv_data_space | cv_data_access | cv_data_path | cv_data_objectid.
-type caveat() :: #cv_time{} | #cv_authorization_none{} | #cv_audience{} |
#cv_ip{} | #cv_asn{} | #cv_country{} | #cv_region{} | #cv_api{} |
#cv_data_space{} | #cv_data_access{} | #cv_data_path{} | #cv_data_objectid{}.
-type serialized() :: binary().

% Caveats related to data access
-type space_id() :: binary().
-type data_access_type() :: read | write.
-type data_path() :: binary().

-export_type([type/0, caveat/0]).
-export_type([space_id/0, data_access_type/0, data_path/0]).

% Separator used to create (white|black)lists in caveats
-define(SEP, <<"|">>).
% Separator used to create a list of file paths - null character, which is
% forbidden inside a POSIX path
-define(PATH_SEP, <<0>>).
-define(SPLIT(List), binary:split(List, ?SEP, [global])).
-define(SPLIT(List, Sep), binary:split(List, Sep, [global])).
-define(JOIN(List), str_utils:join_binary(List, ?SEP)).
-define(JOIN(List, Sep), str_utils:join_binary(List, Sep)).

%% API
-export([add/2, get_caveats/1, find/2, filter/2]).
-export([type/1, all_types/0]).
-export([build_verifier/2]).
-export([serialize/1, deserialize/1]).
-export([sanitize/1]).
-export([unverified_description/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec add(Caveat | [Caveat], macaroon:macaroon()) -> macaroon:macaroon() when
    Caveat :: caveat() | serialized().
add(Caveats, Macaroon) when is_list(Caveats) ->
    lists:foldl(fun add/2, Macaroon, Caveats);
add(Caveat, Macaroon) when is_binary(Caveat) ->
    macaroon:add_first_party_caveat(Macaroon, Caveat);
add(Caveat, Macaroon) ->
    macaroon:add_first_party_caveat(Macaroon, serialize(Caveat)).


-spec get_caveats(macaroon:macaroon()) -> [caveat()].
get_caveats(Macaroon) ->
    [deserialize(C) || C <- macaroon:first_party_caveats(Macaroon)].


-spec find(type(), [caveat()]) -> {true, [caveat()]} | false.
find(Type, Caveats) ->
    case filter([Type], Caveats) of
        [] -> false;
        List -> {true, List}
    end.


-spec filter([type()], [caveat()]) -> [caveat()].
filter(Types, Caveats) ->
    lists:filter(fun(Caveat) -> lists:member(type(Caveat), Types) end, Caveats).


-spec type(caveat()) -> type().
type(Caveat) when is_tuple(Caveat) ->
    element(1, Caveat).


-spec all_types() -> [type()].
all_types() -> [
    cv_time, cv_authorization_none, cv_audience,
    cv_ip, cv_asn, cv_country, cv_region, cv_api,
    cv_data_space, cv_data_access, cv_data_path, cv_data_objectid
].


-spec build_verifier(aai:auth_ctx(), [type()]) -> macaroon_verifier:verifier().
build_verifier(AuthCtx, SupportedCaveats) ->
    Verifier = macaroon_verifier:create(),
    macaroon_verifier:satisfy_general(Verifier, fun(SerializedCaveat) ->
        try
            Caveat = deserialize(SerializedCaveat),
            lists:member(type(Caveat), SupportedCaveats) andalso verify(Caveat, AuthCtx)
        catch Type:Reason ->
            ?debug_stacktrace("Cannot verify caveat ~s due to ~p:~p", [
                SerializedCaveat, Type, Reason
            ]),
            false
        end
    end).


-spec serialize(caveat()) -> serialized().
serialize(#cv_time{valid_until = ?INFINITY}) ->
    <<"time < infinity">>;
serialize(#cv_time{valid_until = ValidUntil}) ->
    <<"time < ", (integer_to_binary(ValidUntil))/binary>>;

serialize(#cv_authorization_none{}) ->
    <<"authorization = none">>;

serialize(#cv_audience{whitelist = []}) ->
    error(badarg);
serialize(#cv_audience{whitelist = Whitelist}) ->
    <<"audience = ", (?JOIN([aai:serialize_audience(A) || A <- Whitelist]))/binary>>;

serialize(#cv_ip{whitelist = []}) ->
    error(badarg);
serialize(#cv_ip{whitelist = Whitelist}) ->
    SerializedMasks = lists:map(fun(Mask) ->
        {ok, MaskBin} = ip_utils:serialize_mask(Mask),
        MaskBin
    end, Whitelist),
    <<"ip = ", (?JOIN(SerializedMasks))/binary>>;

serialize(#cv_asn{whitelist = []}) ->
    error(badarg);
serialize(#cv_asn{whitelist = Whitelist}) ->
    SerializedASNs = lists:map(fun integer_to_binary/1, Whitelist),
    <<"asn = ", (?JOIN(SerializedASNs))/binary>>;

serialize(#cv_country{list = []}) ->
    error(badarg);
serialize(#cv_country{type = whitelist, list = List}) ->
    <<"geo.country = ", (?JOIN(List))/binary>>;
serialize(#cv_country{type = blacklist, list = List}) ->
    <<"geo.country != ", (?JOIN(List))/binary>>;

serialize(#cv_region{list = []}) ->
    error(badarg);
serialize(#cv_region{type = whitelist, list = List}) ->
    <<"geo.region = ", (?JOIN(List))/binary>>;
serialize(#cv_region{type = blacklist, list = List}) ->
    <<"geo.region != ", (?JOIN(List))/binary>>;

serialize(#cv_api{whitelist = []}) ->
    error(badarg);
serialize(#cv_api{whitelist = Whitelist}) ->
    SerializedEntries = lists:map(fun cv_api:serialize_matchspec/1, Whitelist),
    <<"api = ", (?JOIN(SerializedEntries))/binary>>;

serialize(#cv_data_space{whitelist = []}) ->
    error(badarg);
serialize(#cv_data_space{whitelist = Whitelist}) ->
    <<"data.space = ", (?JOIN(Whitelist))/binary>>;

serialize(#cv_data_access{type = read}) ->
    <<"data.access = read">>;
serialize(#cv_data_access{type = write}) ->
    <<"data.access = write">>;

serialize(#cv_data_path{whitelist = []}) ->
    error(badarg);
serialize(#cv_data_path{whitelist = Whitelist}) ->
    <<"data.path = ", (base64:encode(?JOIN(Whitelist, ?PATH_SEP)))/binary>>;

serialize(#cv_data_objectid{whitelist = []}) ->
    error(badarg);
serialize(#cv_data_objectid{whitelist = Whitelist}) ->
    <<"data.objectid = ", (?JOIN(Whitelist))/binary>>.


-spec deserialize(serialized()) -> caveat().
deserialize(<<"time < infinity">>) ->
    #cv_time{valid_until = ?INFINITY};
deserialize(<<"time < ", ValidUntil/binary>>) ->
    #cv_time{valid_until = binary_to_integer(ValidUntil)};

deserialize(<<"authorization = none">>) ->
    #cv_authorization_none{};

deserialize(<<"audience = ", SerializedAudiences/binary>>) ->
    Whitelist = lists:map(fun aai:deserialize_audience/1, ?SPLIT(SerializedAudiences)),
    #cv_audience{whitelist = Whitelist};

deserialize(<<"ip = ", SerializedMasks/binary>>) ->
    Whitelist = lists:map(fun(MaskBin) ->
        {ok, Mask} = ip_utils:parse_mask(MaskBin),
        Mask
    end, ?SPLIT(SerializedMasks)),
    #cv_ip{whitelist = Whitelist};

deserialize(<<"asn = ", SerializedASNs/binary>>) ->
    Whitelist = lists:map(fun binary_to_integer/1, ?SPLIT(SerializedASNs)),
    #cv_asn{whitelist = Whitelist};

deserialize(<<"geo.country = ", List/binary>>) ->
    #cv_country{type = whitelist, list = ?SPLIT(List)};
deserialize(<<"geo.country != ", List/binary>>) ->
    #cv_country{type = blacklist, list = ?SPLIT(List)};

deserialize(<<"geo.region = ", List/binary>>) ->
    #cv_region{type = whitelist, list = ?SPLIT(List)};
deserialize(<<"geo.region != ", List/binary>>) ->
    #cv_region{type = blacklist, list = ?SPLIT(List)};

deserialize(<<"api = ", SerializedEntries/binary>>) ->
    Whitelist = lists:map(fun cv_api:deserialize_matchspec/1, ?SPLIT(SerializedEntries)),
    #cv_api{whitelist = Whitelist};

deserialize(<<"data.space = ", List/binary>>) ->
    #cv_data_space{whitelist = ?SPLIT(List)};

deserialize(<<"data.access = read">>) ->
    #cv_data_access{type = read};
deserialize(<<"data.access = write">>) ->
    #cv_data_access{type = write};

deserialize(<<"data.path = ", ListB64/binary>>) ->
    #cv_data_path{whitelist = ?SPLIT(base64:decode(ListB64), ?PATH_SEP)};

deserialize(<<"data.objectid = ", List/binary>>) ->
    #cv_data_objectid{whitelist = ?SPLIT(List)}.


%%--------------------------------------------------------------------
%% @doc
%% Parses given caveat if needed and checks if it is semantically correct.
%% Returns the sanitized caveat upon success.
%% @end
%%--------------------------------------------------------------------
-spec sanitize(caveat() | serialized()) -> {true, caveat()} | false.
sanitize(C = #cv_time{valid_until = ?INFINITY}) -> {true, C};
sanitize(C = #cv_time{valid_until = Int}) when is_integer(Int) -> {true, C};
sanitize(_ = #cv_time{}) -> false;

sanitize(C = #cv_authorization_none{}) -> {true, C};

sanitize(C = #cv_audience{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        [A = aai:deserialize_audience(aai:serialize_audience(A)) || A <- Whitelist],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_ip{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        [{ok, M} = ip_utils:parse_mask(element(2, {ok, _} = ip_utils:serialize_mask(M))) || M <- Whitelist],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_asn{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        [true = is_integer(Asn) || Asn <- Whitelist],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_country{type = Type, list = List}) when List /= [] ->
    try
        true = lists:member(Type, [whitelist, blacklist]),
        [<<_:16>> = Country || Country <- List],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_region{type = Type, list = List}) when List /= [] ->
    try
        true = lists:member(Type, [whitelist, blacklist]),
        [true = ip_utils:is_region(Region) || Region <- List],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_api{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        C = deserialize(serialize(C)),
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_data_space{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        [<<_:8, _/binary>> = SpaceId || SpaceId <- Whitelist],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_data_access{type = read}) -> {true, C};
sanitize(C = #cv_data_access{type = write}) -> {true, C};
sanitize(_ = #cv_data_access{}) -> false;

sanitize(C = #cv_data_path{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        [<<_:8, _/binary>> = Path || Path <- Whitelist],
        {true, deserialize(serialize(C))}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_data_objectid{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        [<<_:8, _/binary>> = ObjectId || ObjectId <- Whitelist],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(Serialized) when is_binary(Serialized) ->
    try
        sanitize(deserialize(Serialized))
    catch _:_ ->
        false
    end;

sanitize(_) ->
    false.


-spec unverified_description(caveat()) -> binary().
unverified_description(#cv_time{valid_until = ValidUntil}) ->
    str_utils:format_bin(
        "unverified time caveat: this token has expired at ~s",
        [time_utils:epoch_to_iso8601(ValidUntil)]
    );

unverified_description(#cv_authorization_none{}) ->
    <<"this token carries no authorization (can be used solely for identity verification)">>;

unverified_description(#cv_audience{whitelist = AllowedAudiences}) ->
    str_utils:format_bin(
        "unverified audience caveat: the consumer of this token must authorize as ~s "
        "(use the x-onedata-audience-token header)",
        [?JOIN([aai:audience_to_printable(A) || A <- AllowedAudiences], <<" or ">>)]
    );

unverified_description(#cv_ip{whitelist = Whitelist}) ->
    str_utils:format_bin(
        "unverified IP caveat: this token can only be used from an IP address that matches: ~s",
        [?JOIN([element(2, {ok, _} = ip_utils:serialize_mask(M)) || M <- Whitelist], <<" or ">>)]
    );

unverified_description(#cv_asn{whitelist = Whitelist}) ->
    str_utils:format_bin(
        "unverified ASN caveat: this token can only be used from an IP address that "
        "maps to the following Autonomous System Number: ~s",
        [?JOIN(lists:map(fun integer_to_binary/1, Whitelist), <<" or ">>)]
    );

unverified_description(#cv_country{type = Type, list = List}) ->
    ComesStr = case Type of
        whitelist -> "comes";
        blacklist -> "does NOT come"
    end,
    str_utils:format_bin(
        "unverified country caveat: this token can only be used from an IP address that "
        "~s from the following country: ~s",
        [ComesStr, ?JOIN(List, <<" or ">>)]
    );

unverified_description(#cv_region{type = Type, list = List}) ->
    ComesStr = case Type of
        whitelist -> "comes";
        blacklist -> "does NOT come"
    end,
    str_utils:format_bin(
        "unverified region caveat: this token can only be used from an IP address that "
        "~s from the following region: ~s",
        [ComesStr, ?JOIN(List, <<" or ">>)]
    );

unverified_description(#cv_api{whitelist = Whitelist}) ->
    str_utils:format_bin(
        "unverified API caveat: this token can only be used for API calls that match "
        "the following spec: ~s",
        [?JOIN(lists:map(fun cv_api:serialize_matchspec/1, Whitelist), <<" or ">>)]
    );

unverified_description(#cv_data_space{whitelist = Whitelist}) ->
    str_utils:format_bin(
        "unverified data space caveat: this token can only be used to access data "
        "in the following space: ~s",
        [?JOIN(Whitelist, <<" or ">>)]
    );

unverified_description(#cv_data_access{type = Type}) ->
    str_utils:format_bin(
        "unverified data access caveat: this token allows for ~s-only data access",
        [Type]
    );

unverified_description(#cv_data_path{whitelist = Whitelist}) ->
    str_utils:format_bin(
        "unverified data path caveat: this token can only be used to access data "
        "under the following paths (and nested directories): ~ts",
        [?JOIN(Whitelist, <<" or ">>)]
    );

unverified_description(#cv_data_objectid{whitelist = Whitelist}) ->
    str_utils:format_bin(
        "unverified data path caveat: this token can only be used to access data "
        "with the following FILE_ID (and nested directories): ~ts",
        [?JOIN(Whitelist, <<" or ">>)]
    ).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec verify(caveat(), aai:auth_ctx()) -> boolean().
verify(#cv_time{valid_until = ?INFINITY}, _AuthCtx) ->
    true;
verify(#cv_time{valid_until = ValidUntil}, #auth_ctx{current_timestamp = Now}) ->
    Now < ValidUntil;

verify(#cv_authorization_none{}, _AuthCtx) ->
    % This caveat always verifies, but should not be listed in SupportedCaveats
    % (see build_verifier/2) when verifying authorization. It is useful to
    % verify the subject's identity alone.
    true;

verify(#cv_audience{whitelist = Whitelist}, #auth_ctx{audience = Audience} = AuthCtx) ->
    is_allowed(whitelist, Whitelist, fun
        (?AUD(group, GroupId)) ->
            Checker = AuthCtx#auth_ctx.group_membership_checker,
            Checker(Audience, GroupId);
        (Entry) ->
            Audience =:= Entry
    end);

verify(#cv_ip{}, #auth_ctx{ip = undefined}) ->
    false;
verify(#cv_ip{whitelist = Whitelist}, #auth_ctx{ip = IP}) ->
    is_allowed(whitelist, Whitelist, fun(Mask) -> ip_utils:matches_mask(IP, Mask) end);

verify(#cv_asn{whitelist = Whitelist}, #auth_ctx{ip = IP}) ->
    {ok, ASN} = ip_utils:lookup_asn(IP),
    is_allowed(whitelist, Whitelist, fun(Entry) -> ASN =:= Entry end);

verify(#cv_country{type = Type, list = List}, #auth_ctx{ip = IP}) ->
    {ok, Country} = ip_utils:lookup_country(IP),
    is_allowed(Type, List, fun(Entry) -> Country =:= Entry end);

verify(#cv_region{type = Type, list = List}, #auth_ctx{ip = IP}) ->
    {ok, Regions} = ip_utils:lookup_region(IP),
    is_allowed(Type, List, fun(Entry) -> lists:member(Entry, Regions) end);

% Below caveats are lazy - always true when verifying a token (still, they must
% be explicitly supported). They are checked when the resulting aai:auth() object
% is consumed to perform an operation.
verify(#cv_api{}, _AuthCtx) ->
    true;
verify(#cv_data_space{}, _AuthCtx) ->
    true;
verify(#cv_data_access{}, _AuthCtx) ->
    true;
verify(#cv_data_path{}, _AuthCtx) ->
    true;
verify(#cv_data_objectid{}, _AuthCtx) ->
    true.


%% @private
-spec is_allowed(whitelist | blacklist, [term()], fun((term()) -> boolean())) -> boolean().
is_allowed(whitelist, Whitelist, Predicate) ->
    lists:any(Predicate, Whitelist);
is_allowed(blacklist, Blacklist, Predicate) ->
    lists:all(fun(Blacklisted) -> not Predicate(Blacklisted) end, Blacklist).

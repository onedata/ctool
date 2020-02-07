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
-include("errors.hrl").

-type type() :: cv_time
| cv_ip | cv_asn | cv_country | cv_region
| cv_scope
| cv_service | cv_consumer
| cv_interface | cv_api
| cv_data_readonly | cv_data_path | cv_data_objectid.
-type caveat() :: #cv_time{}
| #cv_ip{} | #cv_asn{} | #cv_country{} | #cv_region{}
| #cv_scope{}
| #cv_service{} | #cv_consumer{}
| #cv_interface{} | #cv_api{}
| #cv_data_readonly{} | #cv_data_path{} | #cv_data_objectid{}.
-type serialized() :: binary().
-type as_json() :: json_utils:json_term().

-export_type([type/0, caveat/0]).

% Separator used to create (white|black)lists in caveats
-define(SEP, <<"|">>).
-define(SPLIT(List), binary:split(List, ?SEP, [global])).
-define(JOIN(List), str_utils:join_binary(List, ?SEP)).
-define(JOIN(List, Sep), str_utils:join_binary(List, Sep)).

%% API
-export([add/2, get_caveats/1, find/2, filter/2]).
-export([type/1, all_types/0]).
-export([build_verifier/2]).
-export([serialize/1, deserialize/1]).
-export([to_json/1, from_json/1]).
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
    cv_time,
    cv_ip, cv_asn, cv_country, cv_region,
    cv_scope,
    cv_service, cv_consumer,
    cv_interface, cv_api,
    cv_data_readonly, cv_data_path, cv_data_objectid
].


-spec build_verifier(aai:auth_ctx(), SupportedCaveats :: [type()]) ->
    macaroon_verifier:verifier().
build_verifier(AuthCtx, SupportedCaveats) ->
    Verifier = macaroon_verifier:create(),
    macaroon_verifier:satisfy_general(Verifier, fun(SerializedCaveat) ->
        Caveat = try
            deserialize(SerializedCaveat)
        catch _:_ ->
            throw(?ERROR_TOKEN_CAVEAT_UNKNOWN(SerializedCaveat))
        end,
        try
            lists:member(type(Caveat), SupportedCaveats) andalso verify(Caveat, AuthCtx)
        catch Type:Reason ->
            ?debug_stacktrace("Cannot verify caveat ~s due to ~p:~p", [
                SerializedCaveat, Type, Reason
            ]),
            false
        end
    end).


-spec serialize(caveat()) -> serialized().
serialize(#cv_time{valid_until = ValidUntil}) ->
    <<"time < ", (integer_to_binary(ValidUntil))/binary>>;

serialize(#cv_ip{whitelist = []}) ->
    error(badarg);
serialize(#cv_ip{whitelist = Whitelist}) ->
    SerializedMasks = [element(2, {ok, _} = ip_utils:serialize_mask(M)) || M <- Whitelist],
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

serialize(#cv_scope{scope = identity_token}) ->
    <<"scope = identity_token">>;

serialize(#cv_service{whitelist = []}) ->
    error(badarg);
serialize(#cv_service{whitelist = Whitelist}) ->
    <<"service = ", (?JOIN([aai:serialize_service(S) || S <- Whitelist]))/binary>>;

serialize(#cv_consumer{whitelist = []}) ->
    error(badarg);
serialize(#cv_consumer{whitelist = Whitelist}) ->
    <<"consumer = ", (?JOIN([aai:serialize_subject(C) || C <- Whitelist]))/binary>>;

serialize(#cv_interface{interface = Interface}) ->
    <<"interface = ", (cv_interface:serialize_interface(Interface))/binary>>;

serialize(#cv_api{whitelist = []}) ->
    error(badarg);
serialize(#cv_api{whitelist = Whitelist}) ->
    SerializedEntries = lists:map(fun cv_api:serialize_matchspec/1, Whitelist),
    <<"api = ", (?JOIN(SerializedEntries))/binary>>;

serialize(#cv_data_readonly{}) ->
    <<"data.readonly">>;

serialize(#cv_data_path{whitelist = []}) ->
    error(badarg);
serialize(#cv_data_path{whitelist = Whitelist}) ->
    B64EncodedPaths = lists:map(fun base64:encode/1, Whitelist),
    <<"data.path = ", (?JOIN(B64EncodedPaths))/binary>>;

serialize(#cv_data_objectid{whitelist = []}) ->
    error(badarg);
serialize(#cv_data_objectid{whitelist = Whitelist}) ->
    <<"data.objectid = ", (?JOIN(Whitelist))/binary>>.


-spec deserialize(serialized()) -> caveat().
deserialize(<<"time < ", ValidUntil/binary>>) ->
    #cv_time{valid_until = binary_to_integer(ValidUntil)};

deserialize(<<"ip = ", SerializedMasks/binary>>) when size(SerializedMasks) > 0 ->
    Whitelist = [element(2, {ok, _} = ip_utils:parse_mask(M)) || M <- ?SPLIT(SerializedMasks)],
    #cv_ip{whitelist = Whitelist};

deserialize(<<"asn = ", SerializedASNs/binary>>) when size(SerializedASNs) > 0 ->
    Whitelist = lists:map(fun binary_to_integer/1, ?SPLIT(SerializedASNs)),
    #cv_asn{whitelist = Whitelist};

deserialize(<<"geo.country = ", List/binary>>) when size(List) > 0 ->
    #cv_country{type = whitelist, list = ?SPLIT(List)};
deserialize(<<"geo.country != ", List/binary>>) when size(List) > 0 ->
    #cv_country{type = blacklist, list = ?SPLIT(List)};

deserialize(<<"geo.region = ", List/binary>>) when size(List) > 0 ->
    #cv_region{type = whitelist, list = ?SPLIT(List)};
deserialize(<<"geo.region != ", List/binary>>) when size(List) > 0 ->
    #cv_region{type = blacklist, list = ?SPLIT(List)};

%% @todo VFS-6098 deprecated, kept for backward compatibility
deserialize(<<"authorization = none">>) ->
    #cv_scope{scope = identity_token};
deserialize(<<"scope = identity_token">>) ->
    #cv_scope{scope = identity_token};

deserialize(<<"service = ", SerializedServices/binary>>) when size(SerializedServices) > 0 ->
    Whitelist = lists:map(fun aai:deserialize_service/1, ?SPLIT(SerializedServices)),
    #cv_service{whitelist = Whitelist};

deserialize(<<"consumer = ", SerializedConsumers/binary>>) when size(SerializedConsumers) > 0 ->
    Whitelist = lists:map(fun aai:deserialize_subject/1, ?SPLIT(SerializedConsumers)),
    #cv_consumer{whitelist = Whitelist};

deserialize(<<"interface = ", Interface/binary>>) ->
    #cv_interface{interface = cv_interface:deserialize_interface(Interface)};

deserialize(<<"api = ", SerializedEntries/binary>>) when size(SerializedEntries) > 0 ->
    Whitelist = lists:map(fun cv_api:deserialize_matchspec/1, ?SPLIT(SerializedEntries)),
    #cv_api{whitelist = Whitelist};

deserialize(<<"data.readonly">>) ->
    #cv_data_readonly{};

deserialize(<<"data.path = ", B64EncodedEntries/binary>>) when size(B64EncodedEntries) > 0 ->
    Whitelist = lists:map(fun base64:decode/1, ?SPLIT(B64EncodedEntries)),
    #cv_data_path{whitelist = Whitelist};

deserialize(<<"data.objectid = ", Whitelist/binary>>) when size(Whitelist) > 0 ->
    #cv_data_objectid{whitelist = ?SPLIT(Whitelist)}.


-spec to_json(caveat()) -> as_json().
to_json(#cv_time{valid_until = ValidUntil}) ->
    #{
        <<"type">> => <<"time">>,
        <<"validUntil">> => ValidUntil
    };

to_json(#cv_ip{whitelist = []}) ->
    error(badarg);
to_json(#cv_ip{whitelist = Whitelist}) ->
    SerializedMasks = [element(2, {ok, _} = ip_utils:serialize_mask(M)) || M <- Whitelist],
    #{
        <<"type">> => <<"ip">>,
        <<"whitelist">> => SerializedMasks
    };

to_json(#cv_asn{whitelist = []}) ->
    error(badarg);
to_json(#cv_asn{whitelist = Whitelist}) ->
    #{
        <<"type">> => <<"asn">>,
        <<"whitelist">> => Whitelist
    };

to_json(#cv_country{list = []}) ->
    error(badarg);
to_json(#cv_country{type = FilterType, list = List}) ->
    #{
        <<"type">> => <<"geo.country">>,
        <<"filter">> => serialize_filter_type(FilterType),
        <<"list">> => List
    };

to_json(#cv_region{list = []}) ->
    error(badarg);
to_json(#cv_region{type = FilterType, list = List}) ->
    #{
        <<"type">> => <<"geo.region">>,
        <<"filter">> => serialize_filter_type(FilterType),
        <<"list">> => List
    };

to_json(#cv_scope{scope = identity_token}) ->
    #{
        <<"scope">> => <<"identityToken">>
    };

to_json(#cv_service{whitelist = []}) ->
    error(badarg);
to_json(#cv_service{whitelist = Whitelist}) ->
    #{
        <<"type">> => <<"service">>,
        <<"whitelist">> => [aai:serialize_service(A) || A <- Whitelist]
    };

to_json(#cv_consumer{whitelist = []}) ->
    error(badarg);
to_json(#cv_consumer{whitelist = Whitelist}) ->
    #{
        <<"type">> => <<"consumer">>,
        <<"whitelist">> => [aai:serialize_subject(C) || C <- Whitelist]
    };

to_json(#cv_interface{interface = Interface}) ->
    #{
        <<"type">> => <<"interface">>,
        <<"interface">> => cv_interface:serialize_interface(Interface)
    };

to_json(#cv_api{whitelist = []}) ->
    error(badarg);
to_json(#cv_api{whitelist = Whitelist}) ->
    #{
        <<"type">> => <<"api">>,
        <<"whitelist">> => lists:map(fun cv_api:serialize_matchspec/1, Whitelist)
    };

to_json(#cv_data_readonly{}) ->
    #{
        <<"type">> => <<"data.readonly">>
    };

to_json(#cv_data_path{whitelist = []}) ->
    error(badarg);
to_json(#cv_data_path{whitelist = Whitelist}) ->
    #{
        <<"type">> => <<"data.path">>,
        <<"whitelist">> => lists:map(fun base64:encode/1, Whitelist)
    };

to_json(#cv_data_objectid{whitelist = []}) ->
    error(badarg);
to_json(#cv_data_objectid{whitelist = Whitelist}) ->
    #{
        <<"type">> => <<"data.objectid">>,
        <<"whitelist">> => Whitelist
    }.


-spec from_json(as_json()) -> caveat().
from_json(#{<<"type">> := <<"time">>, <<"validUntil">> := ValidUntil}) ->
    #cv_time{valid_until = ValidUntil};

from_json(#{<<"type">> := <<"ip">>, <<"whitelist">> := SerializedMasks}) when length(SerializedMasks) > 0 ->
    Whitelist = [element(2, {ok, _} = ip_utils:parse_mask(M)) || M <- SerializedMasks],
    #cv_ip{whitelist = Whitelist};

from_json(#{<<"type">> := <<"asn">>, <<"whitelist">> := Whitelist}) when length(Whitelist) > 0 ->
    #cv_asn{whitelist = Whitelist};

from_json(#{<<"type">> := <<"geo.country">>, <<"filter">> := FilterType, <<"list">> := List}) when length(List) > 0 ->
    #cv_country{type = deserialize_filter_type(FilterType), list = List};

from_json(#{<<"type">> := <<"geo.region">>, <<"filter">> := FilterType, <<"list">> := List}) when length(List) > 0 ->
    #cv_region{type = deserialize_filter_type(FilterType), list = List};

from_json(#{<<"scope">> := <<"identityToken">>}) ->
    #cv_scope{scope = identity_token};

from_json(#{<<"type">> := <<"service">>, <<"whitelist">> := SerializedServices}) when length(SerializedServices) > 0 ->
    Whitelist = lists:map(fun aai:deserialize_service/1, SerializedServices),
    #cv_service{whitelist = Whitelist};

from_json(#{<<"type">> := <<"consumer">>, <<"whitelist">> := SerializedConsumers}) when length(SerializedConsumers) > 0 ->
    Whitelist = lists:map(fun aai:deserialize_subject/1, SerializedConsumers),
    #cv_consumer{whitelist = Whitelist};

from_json(#{<<"type">> := <<"interface">>, <<"interface">> := Interface}) ->
    #cv_interface{interface = cv_interface:deserialize_interface(Interface)};

from_json(#{<<"type">> := <<"api">>, <<"whitelist">> := SerializedEntries}) when length(SerializedEntries) > 0 ->
    Whitelist = lists:map(fun cv_api:deserialize_matchspec/1, SerializedEntries),
    #cv_api{whitelist = Whitelist};

from_json(#{<<"type">> := <<"data.readonly">>}) ->
    #cv_data_readonly{};

from_json(#{<<"type">> := <<"data.path">>, <<"whitelist">> := B64EncodedEntries}) when length(B64EncodedEntries) > 0 ->
    Whitelist = lists:map(fun base64:decode/1, B64EncodedEntries),
    #cv_data_path{whitelist = Whitelist};

from_json(#{<<"type">> := <<"data.objectid">>, <<"whitelist">> := Whitelist}) when length(Whitelist) > 0 ->
    #cv_data_objectid{whitelist = Whitelist}.


%%--------------------------------------------------------------------
%% @doc
%% Parses given caveat if needed and checks if it is semantically correct.
%% Returns the sanitized caveat upon success.
%% @end
%%--------------------------------------------------------------------
-spec sanitize(caveat() | as_json()) -> {true, caveat()} | false.
sanitize(C = #cv_time{valid_until = Int}) when is_integer(Int) -> {true, C};
sanitize(_ = #cv_time{}) -> false;

sanitize(C = #cv_ip{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        lists:foreach(fun(Mask) ->
            Serialized = case Mask of
                Bin when is_binary(Bin) -> Bin;
                _ -> element(2, {ok, _} = ip_utils:serialize_mask(Mask))
            end,
            {ok, _} = ip_utils:parse_mask(Serialized)
        end, Whitelist),
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

sanitize(C = #cv_scope{scope = identity_token}) -> {true, C};

sanitize(C = #cv_service{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        [S = aai:deserialize_service(aai:serialize_service(S)) || S <- Whitelist],
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_consumer{whitelist = Whitelist}) when Whitelist /= [] ->
    try
        lists:foreach(fun(Subject) ->
            Subject = aai:deserialize_subject(aai:serialize_subject(Subject)),
            case Subject of
                ?SUB(user, Id) when is_binary(Id) -> ok;
                ?SUB(group, Id) when is_binary(Id) -> ok;
                ?SUB(?ONEPROVIDER, Id) when is_binary(Id) -> ok
            end
        end, Whitelist),
        {true, C}
    catch _:_ ->
        false
    end;

sanitize(C = #cv_interface{interface = Interface}) ->
    try
        Interface = cv_interface:deserialize_interface(cv_interface:serialize_interface(Interface)),
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

sanitize(C = #cv_data_readonly{}) -> {true, C};

sanitize(C = #cv_data_path{whitelist = Whitelist}) when Whitelist /= [] ->
    case data_access_caveats:sanitize_path_caveat(C) of
        true -> {true, C};
        false -> false
    end;

sanitize(C = #cv_data_objectid{whitelist = Whitelist}) when Whitelist /= [] ->
    case data_access_caveats:sanitize_objectid_caveat(C) of
        true -> {true, C};
        false -> false
    end;

sanitize(AsJson) when is_map(AsJson) ->
    try
        sanitize(from_json(AsJson))
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

unverified_description(#cv_service{whitelist = AllowedServices}) ->
    str_utils:format_bin(
        "unverified service caveat: the service using this token must authenticate as ~s "
        "(use the x-onedata-service-token header)",
        [?JOIN([list_to_binary(aai:service_to_printable(S)) || S <- AllowedServices], <<" or ">>)]
    );

unverified_description(#cv_consumer{whitelist = AllowedConsumers}) ->
    str_utils:format_bin(
        "unverified consumer caveat: the consumer of this token must authenticate as ~s - "
        "you should probably provide your access token in one of the headers:~n"
        "   * 'x-auth-token' if consuming an invite token or using token verification API~n"
        "   * 'x-onedata-consumer-token' if performing an operation on behalf of somebody else with their access token",
        [?JOIN([list_to_binary(aai:subject_to_printable(S)) || S <- AllowedConsumers], <<" or ">>)]
    );

unverified_description(#cv_interface{interface = Interface}) ->
    str_utils:format_bin(
        "unverified interface caveat: this token can only be used in the '~s' interface",
        [Interface]
    );

unverified_description(#cv_api{whitelist = Whitelist}) ->
    str_utils:format_bin(
        "unverified API caveat: this token can only be used for API calls that match "
        "the following spec: ~s",
        [?JOIN(lists:map(fun cv_api:serialize_matchspec/1, Whitelist), <<" or ">>)]
    );

unverified_description(#cv_scope{scope = identity_token}) ->
    <<"this token carries no authorization (can be used only for identity verification)">>;

unverified_description(#cv_data_readonly{}) ->
    <<"unverified data access caveat: this token allows for readonly data access">>;

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
verify(#cv_time{valid_until = ValidUntil}, #auth_ctx{current_timestamp = Now}) ->
    Now < ValidUntil;

verify(#cv_ip{}, #auth_ctx{ip = undefined}) ->
    false;
verify(#cv_ip{whitelist = Whitelist}, #auth_ctx{ip = IP}) ->
    is_allowed(whitelist, Whitelist, fun(Mask) -> ip_utils:matches_mask(IP, Mask) end);

verify(#cv_asn{}, #auth_ctx{ip = undefined}) ->
    false;
verify(#cv_asn{whitelist = Whitelist}, #auth_ctx{ip = IP}) ->
    {ok, ASN} = ip_utils:lookup_asn(IP),
    is_allowed(whitelist, Whitelist, fun(Entry) -> ASN =:= Entry end);

verify(#cv_country{}, #auth_ctx{ip = undefined}) ->
    false;
verify(#cv_country{type = Type, list = List}, #auth_ctx{ip = IP}) ->
    {ok, Country} = ip_utils:lookup_country(IP),
    is_allowed(Type, List, fun(Entry) -> Country =:= Entry end);

verify(#cv_region{}, #auth_ctx{ip = undefined}) ->
    false;
verify(#cv_region{type = Type, list = List}, #auth_ctx{ip = IP}) ->
    {ok, Regions} = ip_utils:lookup_region(IP),
    is_allowed(Type, List, fun(Entry) -> lists:member(Entry, Regions) end);

verify(#cv_service{whitelist = Whitelist}, #auth_ctx{service = Service}) ->
    is_allowed(whitelist, Whitelist, fun
        (?SERVICE(Type, ?ID_WILDCARD)) ->
            Service#service_spec.type =:= Type;
        (Entry) ->
            Service =:= Entry
    end);

verify(#cv_consumer{whitelist = Whitelist}, #auth_ctx{consumer = Consumer} = AuthCtx) ->
    is_allowed(whitelist, Whitelist, fun
        (?SUB(group, _)) when AuthCtx#auth_ctx.group_membership_checker == undefined ->
            false;
        (?SUB(group, GroupId)) ->
            Checker = AuthCtx#auth_ctx.group_membership_checker,
            Checker(Consumer, GroupId);
        (?SUB(Type, ?ID_WILDCARD)) ->
            Consumer#subject.type =:= Type;
        (?SUB(Type, Id)) ->
            Consumer#subject.id =:= Id andalso Consumer#subject.type =:= Type
    end);

% interface = oneclient caveat is treated as a data access caveat and limits the
% available API, but does not require the allow_data_access_caveats policy.
verify(#cv_interface{}, #auth_ctx{interface = undefined}) ->
    false;
verify(#cv_interface{interface = Interface}, #auth_ctx{interface = Interface}) ->
    true;
verify(#cv_interface{}, _AuthCtx) ->
    false;

% API caveats are lazy - always true when verifying a token, but checked when
% the resulting aai:auth() object is used to perform an operation.
% Each system component that accepts tokens must explicitly check the API
% caveats in its internal logic.
verify(#cv_api{}, _AuthCtx) ->
    true;

verify(#cv_scope{scope = identity_token}, #auth_ctx{scope = identity_token}) ->
    true;
verify(#cv_scope{scope = identity_token}, #auth_ctx{scope = unlimited}) ->
    false;

% Data access caveats are allowed only if the authorizing party requested so.
% These caveats are supported only in Oneprovider, on interfaces used for data
% access. The proper verification of these caveats is performed in Oneprovider,
% here only a general check is done.
verify(#cv_data_readonly{}, AuthCtx) ->
    AuthCtx#auth_ctx.data_access_caveats_policy == allow_data_access_caveats;
verify(#cv_data_path{}, AuthCtx) ->
    AuthCtx#auth_ctx.data_access_caveats_policy == allow_data_access_caveats;
verify(#cv_data_objectid{}, AuthCtx) ->
    AuthCtx#auth_ctx.data_access_caveats_policy == allow_data_access_caveats.


%% @private
-spec is_allowed(whitelist | blacklist, [term()], fun((term()) -> boolean())) -> boolean().
is_allowed(whitelist, Whitelist, Predicate) ->
    lists:any(Predicate, Whitelist);
is_allowed(blacklist, Blacklist, Predicate) ->
    lists:all(fun(Blacklisted) -> not Predicate(Blacklisted) end, Blacklist).


%% @private
-spec serialize_filter_type(whitelist | blacklist) -> binary().
serialize_filter_type(whitelist) -> <<"whitelist">>;
serialize_filter_type(blacklist) -> <<"blacklist">>.


%% @private
-spec deserialize_filter_type(binary()) -> whitelist | blacklist.
deserialize_filter_type(<<"whitelist">>) -> whitelist;
deserialize_filter_type(<<"blacklist">>) -> blacklist.
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

-type api_operation() :: create | get | update | delete | all.
% Matchspec is in GRI format, with possible wildcards, eg.:
%   od_space.*.instance:private
%   od_user.7496a16de55bf6bc42f606c5b268daa0.*:private
%   *.*.instance:private
%   *.*.*:*
-type api_matchspec() :: binary().

% Caveats related to data access
-type space_id() :: binary().
-type data_access_type() :: read | write.
-type data_path() :: binary().

-export_type([type/0, caveat/0]).
-export_type([api_operation/0, api_matchspec/0]).
-export_type([space_id/0, data_access_type/0, data_path/0]).

% Separator used to create (white|black)lists in caveats
-define(SEP, <<"|">>).
% Separator used to create a list of file paths
-define(PATH_SEP, <<"//">>).
-define(SPLIT(List), binary:split(List, ?SEP, [global])).
-define(SPLIT(List, Sep), binary:split(List, Sep, [global])).
-define(JOIN(List), str_utils:join_binary(List, ?SEP)).
-define(JOIN(List, Sep), str_utils:join_binary(List, Sep)).

%% API
-export([add/2]).
-export([build_verifier/2]).
-export([type/1, all_types/0]).
-export([deserialize/1, serialize/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec add(caveat() | [caveat()], macaroon:macaroon()) -> macaroon:macaroon().
add(Caveats, Macaroon) when is_list(Caveats) ->
    lists:foldl(fun add/2, Macaroon, Caveats);
add(Caveat, Macaroon) ->
    macaroon:add_first_party_caveat(Macaroon, serialize(Caveat)).


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


-spec type(caveat()) -> type().
type(Caveat) when is_tuple(Caveat) ->
    element(1, Caveat).


-spec all_types() -> [type()].
all_types() -> [
    cv_time, cv_authorization_none, cv_audience,
    cv_ip, cv_asn, cv_country, cv_region, cv_api,
    cv_data_space, cv_data_access, cv_data_path, cv_data_objectid
].


-spec serialize(caveat()) -> serialized().
serialize(#cv_time{valid_until = ?INFINITY}) ->
    <<"time < infinity">>;
serialize(#cv_time{valid_until = ValidUntil}) ->
    <<"time < ", (integer_to_binary(ValidUntil))/binary>>;

serialize(#cv_authorization_none{}) ->
    <<"authorization = none">>;

serialize(#cv_audience{audience = Audience}) ->
    <<"audience = ", (aai:serialize_audience(Audience))/binary>>;

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
    SerializedEntries = lists:map(fun({Operation, MatchSpec}) ->
        <<(serialize_api_operation(Operation))/binary, "/", MatchSpec/binary>>
    end, Whitelist),
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
    <<"data.path = ", (?JOIN(Whitelist, ?PATH_SEP))/binary>>;

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

deserialize(<<"audience = ", Audience/binary>>) ->
    #cv_audience{audience = aai:deserialize_audience(Audience)};

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
    Whitelist = lists:map(fun(Entry) ->
        [Operation, MatchSpec] = ?SPLIT(Entry, <<"/">>),
        {deserialize_api_operation(Operation), MatchSpec}
    end, ?SPLIT(SerializedEntries)),
    #cv_api{whitelist = Whitelist};

deserialize(<<"data.space = ", List/binary>>) ->
    #cv_data_space{whitelist = ?SPLIT(List)};

deserialize(<<"data.access = read">>) ->
    #cv_data_access{type = read};
deserialize(<<"data.access = write">>) ->
    #cv_data_access{type = write};

deserialize(<<"data.path = ", List/binary>>) ->
    #cv_data_path{whitelist = ?SPLIT(List, ?PATH_SEP)};

deserialize(<<"data.objectid = ", List/binary>>) ->
    #cv_data_objectid{whitelist = ?SPLIT(List)}.

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

verify(#cv_audience{audience = CaveatAud}, #auth_ctx{audience = CtxAud}) ->
    CaveatAud =:= CtxAud;

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


%% @private
-spec serialize_api_operation(api_operation()) -> binary().
serialize_api_operation(create) -> <<"create">>;
serialize_api_operation(get) -> <<"get">>;
serialize_api_operation(update) -> <<"update">>;
serialize_api_operation(delete) -> <<"delete">>;
serialize_api_operation(all) -> <<"all">>;
serialize_api_operation(_) -> error(badarg).


%% @private
-spec deserialize_api_operation(binary()) -> api_operation().
deserialize_api_operation(<<"create">>) -> create;
deserialize_api_operation(<<"get">>) -> get;
deserialize_api_operation(<<"update">>) -> update;
deserialize_api_operation(<<"delete">>) -> delete;
deserialize_api_operation(<<"all">>) -> all;
deserialize_api_operation(_) -> error(badarg).

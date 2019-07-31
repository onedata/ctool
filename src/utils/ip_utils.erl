%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for processing (masked) IP addresses and performing
%%% geolocation lookups.
%%% @end
%%%-------------------------------------------------------------------
-module(ip_utils).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").

-type ip() :: inet:ip4_address() | nonempty_string() | binary().
-export_type([ip/0]).

% Autonomous System Number registered by IANA for the Internet Service Provider
% who manages the IP address.
-type asn_id() :: integer().

% PL, FR, PT, ...
-type country_code() :: <<_:16>>.

% Africa, Antarctica, Asia, Europe, EU, NorthAmerica, SouthAmerica, Oceania
-type region() :: binary().

-type mask_length() :: 0..32.
-type mask() :: {inet:ip4_address(), mask_length()}.
-type mask_str() :: nonempty_string() | binary().

%% macro for use in guard for checking ip address {A,B,C,D}
-define(IS_IP(A, B, C, D),
    (((A) bor (B) bor (C) bor (D)) band (bnot 16#ff)) =:= 0
).

%% API
-export([lookup_asn/1, lookup_country/1, lookup_region/1]).
-export([parse_mask/1, matches_mask/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the ASN ID for given IP address (if mapping exists).
%% @end
%%--------------------------------------------------------------------
-spec lookup_asn(ip()) -> {ok, asn_id()} | {error, geo_db:lookup_error()}.
lookup_asn(IP) ->
    case geo_db:lookup(asn, IP) of
        {ok, #{<<"autonomous_system_number">> := Id}} -> {ok, Id};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the country code (2 letters) for given IP address (if mapping exists).
%% @end
%%--------------------------------------------------------------------
-spec lookup_country(ip()) -> {ok, country_code()} | {error, geo_db:lookup_error()}.
lookup_country(IP) ->
    case geo_db:lookup(country, IP) of
        {ok, #{<<"country">> := #{<<"iso_code">> := Code}}} -> {ok, Code};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the region for given IP address (if mapping exists).
%% Returns a list because EU countries yield two regions: [<<"Europe">>, <<"EU">>].
%% @end
%%--------------------------------------------------------------------
-spec lookup_region(ip()) -> {ok, [region()]} | {error, geo_db:lookup_error()}.
lookup_region(IP) ->
    case geo_db:lookup(country, IP) of
        {ok, #{<<"country">> := Country, <<"continent">>:= #{<<"code">> := Code}}} ->
            EURegion = case maps:find(<<"is_in_european_union">>, Country) of
                {ok, true} -> [<<"EU">>];
                _ -> []
            end,
            {ok, [code_to_region(Code) | EURegion]};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Parses an IP mask from format: "189.45.21.0/24" (string or binary).
%% @end
%%--------------------------------------------------------------------
-spec parse_mask(mask_str()) -> {ok, mask()} | {error, einval}.
parse_mask(IpSlashMask) when is_list(IpSlashMask) ->
    parse_mask(list_to_binary(IpSlashMask));
parse_mask(IpSlashMask) ->
    case binary:split(IpSlashMask, <<"/">>, [global, trim_all]) of
        [IP] -> parse_mask(IP, 32);
        [IP, Len] -> parse_mask(IP, Len);
        _ -> {error, einval}
    end.


%% @private
-spec parse_mask(ip(), MaskLength :: binary() | integer()) -> {ok, mask()} | {error, einval}.
parse_mask(IP, MaskLength) when is_binary(MaskLength) ->
    try binary_to_integer(MaskLength) of
        Int -> parse_mask(IP, Int)
    catch
        _:_ -> {error, einval}
    end;
parse_mask(_IP, MaskLength) when MaskLength < 0 ->
    {error, einval};
parse_mask(_IP, MaskLength) when MaskLength > 32 ->
    {error, einval};
parse_mask(IP, MaskLength) ->
    case to_ip4_address(IP) of
        {ok, IP4Address} -> {ok, {IP4Address, MaskLength}};
        {error, einval} -> {error, einval}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if given IP address matches given mask.
%% @end
%%--------------------------------------------------------------------
-spec matches_mask(ip(), mask() | mask_str()) -> boolean().
matches_mask(IP, {MaskIP, MaskLength}) ->
    case subnet_as_int(IP, MaskLength) of
        {ok, Int} -> {ok, Int} == subnet_as_int(MaskIP, MaskLength);
        _ -> false
    end;
matches_mask(IP, Mask) ->
    case parse_mask(Mask) of
        {ok, {MaskIP, MaskLength}} -> matches_mask(IP, {MaskIP, MaskLength});
        {error, einval} -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec subnet_as_int(ip(), mask_length()) -> {ok, integer()} | {error, einval}.
subnet_as_int(IP, MaskLength) ->
    case to_ip4_address(IP) of
        {ok, IP4Address} ->
            IpBytes = list_to_binary(tuple_to_list(IP4Address)),
            Rest = (size(IpBytes) * 8) - MaskLength,
            <<Subnet:MaskLength, _Host:Rest>> = IpBytes,
            {ok, Subnet};
        {error, einval} ->
            {error, einval}
    end.


-spec to_ip4_address(ip()) -> {ok, inet:ip4_address()} | {error, einval}.
to_ip4_address(IP) when is_binary(IP) ->
    to_ip4_address(binary_to_list(IP));
to_ip4_address(IP) when is_list(IP) ->
    inet:parse_ipv4strict_address(IP);
to_ip4_address({A, B, C, D}) when ?IS_IP(A, B, C, D) ->
    {ok, {A, B, C, D}}.


-spec code_to_region(<<_:16>>) -> region().
code_to_region(<<"AF">>) -> <<"Africa">>;
code_to_region(<<"AN">>) -> <<"Antarctica">>;
code_to_region(<<"AS">>) -> <<"Asia">>;
code_to_region(<<"EU">>) -> <<"Europe">>;
code_to_region(<<"NA">>) -> <<"NorthAmerica">>;
code_to_region(<<"OC">>) -> <<"Oceania">>;
code_to_region(<<"SA">>) -> <<"SouthAmerica">>.

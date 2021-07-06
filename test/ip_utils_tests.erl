%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of IP utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(ip_utils_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("errors.hrl").

geo_lookup_test_() ->
    {setup,
        fun geo_lookup_setup/0,
        fun geo_lookup_cleanup/1,
        fun geo_lookup/1
    }.

-define(ASN_DATA(Id), #{
    <<"autonomous_system_number">> => Id
}).
-define(COUNTRY_DATA(Code, Continent, EU), #{
    <<"country">> => #{
        <<"iso_code">> => Code,
        <<"is_in_european_union">> => EU
    },
    <<"continent">> => #{<<"code">> => Continent}
}).
geo_lookup_setup() ->
    meck:new(geo_db),
    meck:expect(geo_db, lookup, fun
        (asn, "1.2.3.4") -> {ok, ?ASN_DATA(1234)};
        (asn, "11.22.33.44") -> {ok, ?ASN_DATA(99)};
        (asn, "99.99.99.99") -> {error, not_found};

        (country, "19.28.37.46") -> {ok, ?COUNTRY_DATA(<<"EG">>, <<"AF">>, false)};
        (country, "10.20.30.40") -> {ok, ?COUNTRY_DATA(<<"AQ">>, <<"AN">>, false)};
        (country, "100.50.25.10") -> {ok, ?COUNTRY_DATA(<<"CH">>, <<"AS">>, false)};
        (country, "1.2.3.4") -> {ok, ?COUNTRY_DATA(<<"PL">>, <<"EU">>, true)};
        (country, "11.22.33.44") -> {ok, ?COUNTRY_DATA(<<"UA">>, <<"EU">>, false)};
        (country, "9.8.7.6") -> {ok, ?COUNTRY_DATA(<<"US">>, <<"NA">>, false)};
        (country, "7.5.3.1") -> {ok, ?COUNTRY_DATA(<<"AU">>, <<"OC">>, false)};
        (country, "56.67.78.89") -> {ok, ?COUNTRY_DATA(<<"BR">>, <<"SA">>, false)};

        (country, "99.99.99.99") -> {error, not_found}
    end).


geo_lookup_cleanup(_) ->
    ?assert(meck:validate(geo_db)),
    ok = meck:unload(geo_db).


geo_lookup(_) ->
    ?assertEqual({ok, 1234}, ip_utils:lookup_asn("1.2.3.4")),
    ?assertEqual({ok, 99}, ip_utils:lookup_asn("11.22.33.44")),
    ?assertEqual({error, not_found}, ip_utils:lookup_asn("99.99.99.99")),

    ?assertEqual({ok, <<"EG">>}, ip_utils:lookup_country("19.28.37.46")),
    ?assertEqual({ok, <<"AQ">>}, ip_utils:lookup_country("10.20.30.40")),
    ?assertEqual({ok, <<"CH">>}, ip_utils:lookup_country("100.50.25.10")),
    ?assertEqual({ok, <<"PL">>}, ip_utils:lookup_country("1.2.3.4")),
    ?assertEqual({ok, <<"UA">>}, ip_utils:lookup_country("11.22.33.44")),
    ?assertEqual({ok, <<"US">>}, ip_utils:lookup_country("9.8.7.6")),
    ?assertEqual({ok, <<"AU">>}, ip_utils:lookup_country("7.5.3.1")),
    ?assertEqual({ok, <<"BR">>}, ip_utils:lookup_country("56.67.78.89")),
    ?assertEqual({error, not_found}, ip_utils:lookup_country("99.99.99.99")),

    ?assertEqual({ok, [<<"Africa">>]}, ip_utils:lookup_region("19.28.37.46")),
    ?assertEqual({ok, [<<"Antarctica">>]}, ip_utils:lookup_region("10.20.30.40")),
    ?assertEqual({ok, [<<"Asia">>]}, ip_utils:lookup_region("100.50.25.10")),
    ?assertEqual({ok, [<<"Europe">>, <<"EU">>]}, ip_utils:lookup_region("1.2.3.4")),
    ?assertEqual({ok, [<<"Europe">>]}, ip_utils:lookup_region("11.22.33.44")),
    ?assertEqual({ok, [<<"NorthAmerica">>]}, ip_utils:lookup_region("9.8.7.6")),
    ?assertEqual({ok, [<<"Oceania">>]}, ip_utils:lookup_region("7.5.3.1")),
    ?assertEqual({ok, [<<"SouthAmerica">>]}, ip_utils:lookup_region("56.67.78.89")),
    ?_assertEqual({error, not_found}, ip_utils:lookup_region("99.99.99.99")).


to_ip4_address_test() ->
    T = fun ip_utils:to_ip4_address/1,
    ?assertEqual({ok, {1, 2, 3, 4}}, T("1.2.3.4")),
    ?assertEqual({ok, {105, 21, 112, 14}}, T(<<"105.21.112.14">>)),
    ?assertEqual({ok, {99, 98, 97, 96}}, T({99, 98, 97, 96})),

    ?assertEqual({error, ?EINVAL}, T("1.2.3.400")),
    ?assertEqual({error, ?EINVAL}, T("zxcklvj")),
    ?assertEqual({error, ?EINVAL}, T(<<"723417">>)),
    ?assertEqual({error, ?EINVAL}, T(<<"256.256.256.267">>)),
    ?assertEqual({error, ?EINVAL}, T({-1, 678, 0, 906})).


to_binary_test() ->
    T = fun ip_utils:to_binary/1,
    ?assertEqual({ok, <<"1.2.3.4">>}, T("1.2.3.4")),
    ?assertEqual({ok, <<"105.21.112.14">>}, T(<<"105.21.112.14">>)),
    ?assertEqual({ok, <<"99.98.97.96">>}, T({99, 98, 97, 96})),

    ?assertEqual({error, ?EINVAL}, T("1.2.3.400")),
    ?assertEqual({error, ?EINVAL}, T("zxcklvj")),
    ?assertEqual({error, ?EINVAL}, T(<<"723417">>)),
    ?assertEqual({error, ?EINVAL}, T(<<"256.256.256.267">>)),
    ?assertEqual({error, ?EINVAL}, T({-1, 678, 0, 906})).


parse_mask_test() ->
    P = fun ip_utils:parse_mask/1,
    ?assertEqual({ok, {{1, 2, 3, 4}, 32}}, P("1.2.3.4")),
    ?assertEqual({ok, {{1, 2, 3, 4}, 32}}, P(<<"1.2.3.4">>)),

    ?assertEqual({ok, {{9, 8, 7, 6}, 0}}, P("9.8.7.6/0")),
    ?assertEqual({ok, {{9, 8, 7, 6}, 15}}, P("9.8.7.6/15")),
    ?assertEqual({ok, {{9, 8, 7, 6}, 23}}, P(<<"9.8.7.6/23">>)),
    ?assertEqual({ok, {{9, 8, 7, 6}, 32}}, P(<<"9.8.7.6/32">>)),

    ?assertEqual({error, ?EINVAL}, P("1.2.3.4/33")),
    ?assertEqual({error, ?EINVAL}, P("1.2.3.4/65")),
    ?assertEqual({error, ?EINVAL}, P(<<"1.2.3.4/-5">>)),
    ?assertEqual({error, ?EINVAL}, P("345.21.222.1111")),
    ?assertEqual({error, ?EINVAL}, P("345.21.222.1111/3")),
    ?assertEqual({error, ?EINVAL}, P(<<"345.21.222.1111/15">>)),
    ?assertEqual({error, ?EINVAL}, P("vxczzxcv/sdf/zzz")).


matches_mask_test() ->
    M = fun ip_utils:matches_mask/2,

    ?assert(M({1, 2, 3, 4}, "1.2.3.4")),
    ?assertNot(M({1, 2, 3, 4}, "1.2.3.5")),
    ?assert(M(<<"1.2.3.4">>, "1.2.3.4/32")),
    ?assertNot(M("1.2.3.4", "1.2.4.5/32")),
    ?assertNot(M(<<"1.2.3.4">>, "1.2.4.5/24")),
    ?assert(M({1, 2, 3, 4}, "1.2.4.5/16")),

    ?assert(M("126.21.57.22", <<"126.0.0.0/8">>)),
    ?assertNot(M("126.21.57.22", <<"126.0.0.0/24">>)),
    ?assert(M(<<"126.21.57.22">>, {{126, 0, 0, 0}, 8})),
    ?assertNot(M({126, 21, 57, 22}, "126.0.0.0/24")),

    lists:foreach(fun(Len) ->
        ?assert(M("128.64.32.16", {{128, 64, 32, 16}, Len}))
    end, lists:seq(0, 32)),

    ?assert(M({99, 88, 77, 66}, {{0, 0, 0, 0}, 0})),
    ?assert(M("254.12.199.20", {{0, 0, 0, 0}, 0})),
    ?assert(M(<<"51.198.221.67">>, {{0, 0, 0, 0}, 0})),
    ?assertNot(M(<<"51.198.221.67">>, {{51, 198, 221, 68}, 32})),

    ?assertNot(M({99, 88, 77, 66}, "zxcvzxcv")),
    ?assertNot(M("99.88.77.66", "5555/6")),
    ?assertNot(M(<<"99.88.77.66">>, <<"eh?">>)),

    ?assert(M("255.255.255.255", "255.255.255.255/32")),
    ?assert(M("255.255.255.255", "255.255.255.254/31")),
    ?assert(M("255.255.255.255", "255.255.255.252/30")),
    ?assert(M("255.255.255.255", "255.255.255.248/29")),
    ?assert(M("255.255.255.255", "255.255.255.240/28")),
    ?assert(M("255.255.255.255", "255.255.255.224/27")),
    ?assert(M("255.255.255.255", "255.255.255.192/26")),
    ?assert(M("255.255.255.255", "255.255.255.128/25")),

    ?assert(M("255.255.255.255", "255.255.255.0/24")),
    ?assert(M("255.255.255.255", "255.255.254.0/23")),
    ?assert(M("255.255.255.255", "255.255.252.0/22")),
    ?assert(M("255.255.255.255", "255.255.248.0/21")),
    ?assert(M("255.255.255.255", "255.255.240.0/20")),
    ?assert(M("255.255.255.255", "255.255.224.0/19")),
    ?assert(M("255.255.255.255", "255.255.192.0/18")),
    ?assert(M("255.255.255.255", "255.255.128.0/17")),

    ?assert(M("255.255.255.255", "255.255.0.0/16")),
    ?assert(M("255.255.255.255", "255.254.0.0/15")),
    ?assert(M("255.255.255.255", "255.252.0.0/14")),
    ?assert(M("255.255.255.255", "255.248.0.0/13")),
    ?assert(M("255.255.255.255", "255.240.0.0/12")),
    ?assert(M("255.255.255.255", "255.224.0.0/11")),
    ?assert(M("255.255.255.255", "255.192.0.0/10")),
    ?assert(M("255.255.255.255", "255.128.0.0/9")),

    ?assert(M("255.255.255.255", "255.0.0.0/8")),
    ?assert(M("255.255.255.255", "254.0.0.0/7")),
    ?assert(M("255.255.255.255", "252.0.0.0/6")),
    ?assert(M("255.255.255.255", "248.0.0.0/5")),
    ?assert(M("255.255.255.255", "240.0.0.0/4")),
    ?assert(M("255.255.255.255", "224.0.0.0/3")),
    ?assert(M("255.255.255.255", "192.0.0.0/2")),
    ?assert(M("255.255.255.255", "128.0.0.0/1")),

    ?assert(M("255.255.255.255", "0.0.0.0/0")),


    ?assert(M("255.255.255.255", "255.255.255.255/32")),
    ?assert(M("255.255.255.254", "255.255.255.255/31")),
    ?assert(M("255.255.255.252", "255.255.255.255/30")),
    ?assert(M("255.255.255.248", "255.255.255.255/29")),
    ?assert(M("255.255.255.240", "255.255.255.255/28")),
    ?assert(M("255.255.255.224", "255.255.255.255/27")),
    ?assert(M("255.255.255.192", "255.255.255.255/26")),
    ?assert(M("255.255.255.128", "255.255.255.255/25")),

    ?assert(M("255.255.255.0", "255.255.255.255/24")),
    ?assert(M("255.255.254.0", "255.255.255.255/23")),
    ?assert(M("255.255.252.0", "255.255.255.255/22")),
    ?assert(M("255.255.248.0", "255.255.255.255/21")),
    ?assert(M("255.255.240.0", "255.255.255.255/20")),
    ?assert(M("255.255.224.0", "255.255.255.255/19")),
    ?assert(M("255.255.192.0", "255.255.255.255/18")),
    ?assert(M("255.255.128.0", "255.255.255.255/17")),

    ?assert(M("255.255.0.0", "255.255.255.255/16")),
    ?assert(M("255.254.0.0", "255.255.255.255/15")),
    ?assert(M("255.252.0.0", "255.255.255.255/14")),
    ?assert(M("255.248.0.0", "255.255.255.255/13")),
    ?assert(M("255.240.0.0", "255.255.255.255/12")),
    ?assert(M("255.224.0.0", "255.255.255.255/11")),
    ?assert(M("255.192.0.0", "255.255.255.255/10")),
    ?assert(M("255.128.0.0", "255.255.255.255/9")),

    ?assert(M("255.0.0.0", "255.255.255.255/8")),
    ?assert(M("254.0.0.0", "255.255.255.255/7")),
    ?assert(M("252.0.0.0", "255.255.255.255/6")),
    ?assert(M("248.0.0.0", "255.255.255.255/5")),
    ?assert(M("240.0.0.0", "255.255.255.255/4")),
    ?assert(M("224.0.0.0", "255.255.255.255/3")),
    ?assert(M("192.0.0.0", "255.255.255.255/2")),
    ?assert(M("128.0.0.0", "255.255.255.255/1")),

    ?assert(M("0.0.0.0", "255.255.255.255/0")).

-endif.
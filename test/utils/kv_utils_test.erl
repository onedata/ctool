%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for kv_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(kv_utils_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


-define(MAP, #{
    other1 => something1,
    key1 => value1,
    key2 => #{key3 => value3, other3 => something3},
    bad1 => atom,
    bad2 => [{'subkey', 'not', '2', 'tuple'}]
}).
-define(LIST, [
    {other1, something1},
    {key1, value1},
    {key2, [{key3, value3}, {other3, something3}]},
    {bad1, atom},
    {bad2, [{'subkey', 'not', '2', 'tuple'}]}
]).
-define(MIXED1, [
    {other1, something1},
    {key1, value1},
    {key2, #{key3 => value3, other3 => something3}},
    {bad1, atom},
    {bad2, [{'subkey', 'not', '2', 'tuple'}]}
]).
-define(MIXED2, #{
    other1 => something1,
    key1 => value1,
    key2 => [{key3, value3}, {other3, something3}],
    bad1 => atom,
    bad2 => [{'subkey', 'not', '2', 'tuple'}]
}).
-define(ALL, [?MAP, ?LIST, ?MIXED1, ?MIXED2]).

%%%===================================================================
%%% Test functions
%%%===================================================================

get_test_() -> [[
    {"empty path returns whole term",
        ?_assertEqual('not-a-container', kv_utils:get([], 'not-a-container'))},
    {"get single key",
        ?_assertEqual(value1, kv_utils:get(key1, Container))},
    {"get single key path",
        ?_assertEqual(value1, kv_utils:get([key1], Container))},
    {"get nested path",
        ?_assertEqual(value3, kv_utils:get([key2, key3], Container))},
    {"fail on missing key", [
        ?_assertError({badkeys, [missing]}, kv_utils:get(missing, Container)),
        ?_assertError({badkeys, [missing, key3]}, kv_utils:get([missing, key3], Container)),
        ?_assertError({badkeys, [key2, missing]}, kv_utils:get([key2, missing], Container))
    ]},
    {"return default on missing key", [
        ?_assertEqual(default, kv_utils:get(missing, Container, default)),
        ?_assertEqual(default, kv_utils:get([missing, key3], Container, default)),
        ?_assertEqual(default, kv_utils:get([key2, missing], Container, default))

    ]}
] || Container <- ?ALL].

find_test_() -> [[
    {"empty path returns whole term",
        ?_assertEqual({ok, 'not-a-container'}, kv_utils:find([], 'not-a-container'))},
    {"find single key",
        ?_assertEqual({ok, value1}, kv_utils:find(key1, Container))},
    {"find single key path",
        ?_assertEqual({ok, value1}, kv_utils:find([key1], Container))},
    {"find nested path",
        ?_assertEqual({ok, value3}, kv_utils:find([key2, key3], Container))},
    {"not find key",
        ?_assertEqual(error, kv_utils:find(missing, Container))},
    {"not find nested key 1",
        ?_assertEqual(error, kv_utils:find([missing, key3], Container))},
    {"not find nested key 2",
        ?_assertEqual(error, kv_utils:find([key2, missing], Container))}
] || Container <- ?ALL].

get_and_find_raise_badnested_test_() -> [[
    {"fail on bad container",
        ?_assertError({badnested, 'not-a-container'}, kv_utils:Fun(key, 'not-a-container'))},
    {"fail on bad subcontainer",
        ?_assertError({badnested, _}, kv_utils:Fun([bad1, subkey], Container))},
    {"fail on bad subcontainer",
        ?_assertError({badnested, _}, kv_utils:Fun([bad2, subkey], Container))}
] || Container <- ?ALL, Fun <- [get, find]].


put_test_() -> [
    {"fail on bad container",
        ?_assertError({badnested, 'not-a-container'}, kv_utils:put(key, value, 'not-a-container'))},
    {"put new single key",
        ?_assertEqual(#{key1 => value1, key2 => value2},
            kv_utils:put(key2, value2, #{key1 => value1}))},
    {"put new single key path",
        ?_assertEqual(#{key1 => value1, key2 => value2},
            kv_utils:put([key2], value2, #{key1 => value1}))},
    {"put new nested path, inherit container type", [
        ?_assertEqual(#{key1 => value1, key2 => #{subkey1 => value2}},
            kv_utils:put([key2, subkey1], value2, #{key1 => value1})),
        ?_assertEqual([{key2, [{subkey1, value2}]}, {key1, value1}],
            kv_utils:put([key2, subkey1], value2, [{key1, value1}])),
        ?_assertEqual([{key1, value1}, {key2, #{subkey1 => #{subkey2 => value2}}}],
            kv_utils:put([key2, subkey1, subkey2], value2, [{key1, value1}, {key2, #{}}]))
    ]},
    {"override old value", [
        ?_assertEqual(#{key1 => #{key2 => new}},
            kv_utils:put([key1, key2], new, #{key1 => #{key2 => old}})),
        ?_assertEqual([{key1, [{key2, new}]}],
            kv_utils:put([key1, key2], new, [{key1, [{key2, old}]}])),
        ?_assertEqual([{key1, #{key2 => new}}],
            kv_utils:put([key1, key2], new, [{key1, #{key2 => old}}])),
        ?_assertEqual(#{key1 => [{key2, new}]},
            kv_utils:put([key1, key2], new, #{key1 => [{key2, old}]}))
    ]}
] ++ [
    [
        {"fail on bad subcontainer",
            ?_assertError({badnested, _}, kv_utils:put([bad1, subkey], value, Container))},
        {"fail on bad subcontainer",
            ?_assertError({badnested, _}, kv_utils:put([bad2, subkey], value, Container))}
    ] || Container <- ?ALL].


remove_test_() -> [
    ?_assertEqual(#{k1 => v1}, kv_utils:remove(k2, #{k1 => v1, k2 => v2})),
    ?_assertEqual(#{k1 => v1}, kv_utils:remove(k2, #{k1 => v1})),
    ?_assertEqual(#{k1 => v1}, kv_utils:remove([k2], #{k1 => v1, k2 => v2})),
    ?_assertEqual(#{k1 => v1}, kv_utils:remove([k2], #{k1 => v1})),
    ?_assertEqual([{k1, v1}], kv_utils:remove(k2, [{k1, v1}, {k2, v2}])),
    ?_assertEqual([{k1, v1}], kv_utils:remove(k2, [{k1, v1}])),
    ?_assertEqual([{k1, v1}], kv_utils:remove([k2], [{k1, v1}, {k2, v2}])),
    ?_assertEqual([{k1, v1}], kv_utils:remove([k2], [{k1, v1}])),
    ?_assertEqual(#{k1 => v1, k2 => #{k3 => #{k4 => v4}}},
        kv_utils:remove([k2, k3, k5], #{k1 => v1, k2 => #{k3 => #{k4 => v4, k5 => v5}}})),
    ?_assertEqual([{k1, v1}, {k2, [{k4, v4}]}],
        kv_utils:remove([k2, k3], [{k1, v1}, {k2, [{k4, v4}, {k3, v3}]}])),
    {"delete tuple of any arity",
        ?_assertEqual([{k1, v1}], kv_utils:remove([k2], [{k1, v1}, {k2, a, b}]))}
] ++ [
    [
        {"skip bad subcontainer", [
            ?_assertEqual(Container, kv_utils:remove([bad2, subkey, subkey3, subkey4], Container)),
            ?_assertEqual(Container, kv_utils:remove([bad1, subkey1], Container))
        ]}
    ] || Container <- ?ALL
].


rename_test_() -> [
    ?_assertEqual({ok, #{new_key => value}},
        kv_utils:rename(old_key, new_key, #{old_key => value})),
    ?_assertEqual(error,
        kv_utils:rename(old_key, new_key, #{other_key => value})),
    ?_assertEqual({ok, #{key1 => #{key2 => #{key3 => value}}}},
        kv_utils:rename(old_key, [key1, key2, key3], #{old_key => value})),
    ?_assertEqual({ok, #{stays => stayed, new_key => value}},
        kv_utils:rename(old_key, new_key, #{stays => stayed, old_key => value})),
    ?_assertEqual({ok, [{new_key, value}, {stays, stayed}]},
        kv_utils:rename(old_key, new_key, [{stays, stayed}, {old_key, value}]))
].


copy_and_copy_found_test_() -> [[
    {"replace value", [
        ?_assertEqual(#{<<"key1">> => value1},
            kv_utils:Fun([{key1, <<"key1">>}], Container, #{<<"key1">> => old})),
        ?_assertEqual(#{<<"key1">> => value1, key2 => v},
            kv_utils:Fun([{key1, <<"key1">>}], Container, #{<<"key1">> => old, key2 => v}))
    ]},
    {"nested from",
        ?_assertEqual(#{<<"key1">> => value3},
            kv_utils:Fun([{[key2, key3], <<"key1">>}], Container, #{<<"key1">> => old}))},
    {"nested to",
        ?_assertEqual(#{<<"key1">> => #{<<"subkey1">> => #{<<"subkey2">> => value1}}},
            kv_utils:Fun([
                {key1, [<<"key1">>, <<"subkey1">>, <<"subkey2">>]}
            ], Container, #{}))},
    {"nested, from and to",
        ?_assertEqual(#{key => #{subkey => value3}},
            kv_utils:Fun([{[key2, key3], [key, subkey]}], Container,
                #{key => #{subkey => old}}))},
    {"nested from and to with mixed target",
        ?_assertEqual(#{key => [{subkey, value3}, other]},
            kv_utils:Fun([{[key2, key3], [key, subkey]}], Container,
                #{key => [other]}))},
    {"do nothing with empty mappings", [
        ?_assertEqual(Container, kv_utils:Fun([], Container, Container)),
        ?_assertEqual(#{}, kv_utils:Fun([], Container, #{})),
        ?_assertEqual([], kv_utils:Fun([], Container, []))
    ]},
    {"use default mappings",
        ?_assertEqual(#{{target} => value1, key2 => default2, key3 => value3},
            kv_utils:Fun([
                {key1, {target}, default1},
                {missing, key2, default2},
                {[key2, key3], key3}
            ], Container, #{}))},
    {"fail on bad subcontainer", [
        ?_assertError({badnested, _}, kv_utils:Fun([{[bad1, subkey], key}], Container, #{})),
        ?_assertError({badnested, _}, kv_utils:Fun([{[bad2, subkey, subkey2], key}], Container, #{})),
        ?_assertError({badnested, _}, kv_utils:Fun([{key1, [bad1, subkey]}], #{key1 => value}, Container))
    ]}
] || Container <- ?ALL, Fun <- [copy, copy_found]].


copy_test_() -> [[
    {"fail on missing",[
        ?_assertError({badkeys, [missing]},
            kv_utils:copy([{missing, key1}], Container, #{key1 => old})),
        ?_assertError({badkeys, [missing, subkey]},
            kv_utils:copy([{[missing, subkey], key1}], Container, #{key1 => old})),
        ?_assertError({badkeys, [missing]},
            kv_utils:copy([
                {key1, key1},
                {missing, key2}
            ], Container, Container))
    ]},
    {"multiple mappings",
        ?_assertEqual(#{{target} => value1, key3 => value3},
            kv_utils:copy([
                {key1, {target}},
                {[key2, key3], key3}
            ], Container, #{}))}
] || Container <- ?ALL].


copy_found_test_() -> [[
    {"skip missing",
        ?_assertEqual(#{key1 => old},
            kv_utils:copy_found([{missing, key1}], Container, #{key1 => old}))},
    {"multiple mappings",
        ?_assertEqual(#{{target} => value1, key3 => value3},
            kv_utils:copy_found([
                {key1, {target}},
                {missing, key2},
                {[key2, key3], key3}
            ], Container, #{}))}
] || Container <- ?ALL].


find_many_test() -> [[
    {"skip missing",
        ?_assertEqual(#{},
            kv_utils:find_many([{missing, key1}], Container))},
    {"multiple mappings",
        ?_assertEqual(#{{target} => value1, key3 => value3},
            kv_utils:find_many([
                {key1, {target}},
                {missing, key2},
                {[key2, key3], key3}
            ], Container))}
    ] || Container <- ?ALL].



-endif.

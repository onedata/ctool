%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for manipulating privileges
%%% and proplists in form {Record, Privileges}.
%%% @end
%%%--------------------------------------------------------------------
-module(privileges_utils).
-author("Lukasz Opiola").


-export([privileges_union/2, proplists_union/1]).

privileges_union(PrivilegesA, PrivilegesB) ->
    ordsets:union(
        ordsets:from_list(PrivilegesA),
        ordsets:from_list(PrivilegesB)
    ).


proplists_union(PrivilegesProplist) ->
    PrivilegesMap = lists:foldl(
        fun({Id, Privs}, AccMap) ->
            NewPrivs = case maps:get(Id, AccMap, undefined) of
                undefined ->
                    ordsets:from_list(Privs);
                OtherPrivs ->
                    privileges_union(Privs, OtherPrivs)
            end,
            maps:put(Id, NewPrivs, AccMap)
        end, #{}, PrivilegesProplist),
    ordsets:from_list(maps:to_list(PrivilegesMap)).

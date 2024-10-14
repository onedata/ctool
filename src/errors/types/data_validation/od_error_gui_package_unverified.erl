%%%-------------------------------------------------------------------
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for ?MODULE.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_gui_package_unverified).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_GUI_PACKAGE_UNVERIFIED(ShaSum)) ->
    #{
        <<"id">> => ?ERROR_GUI_PACKAGE_UNVERIFIED_ID,
        <<"details">> => #{
            <<"shaSum">> => ShaSum
        },
        <<"description">> => ?fmt("Provided GUI package could not be verified - unknown SHA sum '~ts'.", [ShaSum])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_GUI_PACKAGE_UNVERIFIED_ID, <<"details">> := #{<<"shaSum">> := ShaSum}}) ->
    ?ERROR_GUI_PACKAGE_UNVERIFIED(ShaSum).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

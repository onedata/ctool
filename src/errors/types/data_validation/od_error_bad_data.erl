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
-module(od_error_bad_data).

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
to_json(?ERROR_BAD_DATA(Key, {error, _} = SpecificError)) -> #{
    <<"id">> => <<"badData">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"specificError">> => to_json(SpecificError)
    },
    <<"description">> => ?fmt("Bad value provided for \"~ts\" (see details).", [Key])
};
to_json(?ERROR_BAD_DATA(Key, HumanReadableHint)) -> #{
    <<"id">> => <<"badData">>,
    <<"details">> => #{
        <<"key">> => Key,
        <<"hint">> => HumanReadableHint
    },
    <<"description">> => ?fmt("Bad value provided for \"~ts\": ~ts.", [Key, HumanReadableHint])
%%};
%%to_json(?ERROR_BAD_DATA(Key)) -> #{
%%    <<"id">> => <<"badData">>,
%%    <<"details">> => #{
%%        <<"key">> => Key
%%    },
%%    <<"description">> => ?fmt(
%%        "Bad value: provided \"~ts\" has an invalid format or is incomprehensible "
%%        "in the context of this operation.",
%%        [Key]
%%    )
}.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := <<"badData">>, <<"details">> := #{<<"key">> := Key, <<"specificError">> := SpecificError}}) ->
    ?ERROR_BAD_DATA(Key, from_json(SpecificError));

from_json(#{<<"id">> := <<"badData">>, <<"details">> := #{<<"key">> := Key, <<"hint">> := HumanReadableHint}}) ->
    ?ERROR_BAD_DATA(Key, HumanReadableHint).

%%from_json(#{<<"id">> := <<"badData">>, <<"details">> := #{<<"key">> := Key}}) ->
%%    ?ERROR_BAD_DATA(Key).
%%

-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.

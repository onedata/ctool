%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains definition of custom label element.
%%% This file is taken from n2o and slightly modified. "Title" field
%%% has been added.
%%% @end
%%%-------------------------------------------------------------------
-module(flatui_label).

-include("gui/common.hrl").

%% API
-export([reflect/0, render_element/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Used to list all record fields.
%% @end
%%--------------------------------------------------------------------
-spec reflect() -> [atom()].
reflect() ->
    record_info(fields, flatui_label).

%%--------------------------------------------------------------------
%% @doc Produces HTML in binary.
%% @end
%%--------------------------------------------------------------------
-spec render_element(Record :: #flatui_label{}) -> list().
render_element(Record) ->
    wf_tags:emit_tag(<<"label">>, wf:render(Record#flatui_label.body), [
        {<<"id">>, Record#flatui_label.id},
        {<<"class">>, Record#flatui_label.class},
        {<<"style">>, Record#flatui_label.style},
        {<<"for">>, Record#flatui_label.for},
        {<<"title">>, Record#flatui_label.title}
    ]).
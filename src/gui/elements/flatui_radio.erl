%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains definition of custom checkbox element.
%% IMPORTANT: for the checkbox to work properly, JS function ".checkbox()"
%% must be called on it. This is done automatically on every checkbox after page loads,
%% but checkboxes added dynamically must be initialized with the function.
%% This can be done easily using the init_checkbox function.
%%
%% This file is taken from n2o and slightly modified.
%% @end
%% ===================================================================
-module(flatui_radio).

-include("gui/common.hrl").

%% API
-export([reflect/0, render_element/1, init_checkbox/1]).


%% ====================================================================
%% API functions
%% ====================================================================

%% reflect/0
%% ====================================================================
%% @doc Used to list all record fields.
%% @end
-spec reflect() -> [atom()].
%% ====================================================================
reflect() ->
    record_info(fields, flatui_radio).


%% render_element/1
%% ====================================================================
%% @doc Produces HTML in binary.
%% @end
-spec render_element(Record :: #flatui_radio{}) -> binary().
%% ====================================================================
render_element(Record) ->
    Id = case Record#flatui_radio.id of
             undefined -> wf:temp_id();
             I when is_binary(I)-> binary_to_list(I);
             I -> I
         end,
    case Record#flatui_radio.postback of
        undefined -> ignore;
        Postback ->
            Data = "[" ++ string:join([begin
                                           {Key, Id} = if is_atom(Src) -> S = atom_to_list(Src),
                                               {"atom('" ++ S ++ "')", S};
                                                           true -> {"utf8.toByteArray('" ++ Src ++ "')", Src} end,
                                           "tuple(" ++ Key ++ ", querySource('" ++ Id ++ "'))" end || Src <- Record#flatui_radio.source]
            ++ ["tuple(tuple(utf8.toByteArray('" ++ Id ++ "'), bin('detail')), event.detail)"], ",") ++ "]",
            PostbackBin = wf_event:new(Postback, Id, Record#flatui_radio.delegate, event, Data),
            wf:wire([wf:f("$('#~s').change(function (event){", [Id]), PostbackBin, "});"])
    end,
    Label = [wf_tags:emit_tag(<<"input">>, [], [
        % global
        {<<"accesskey">>, Record#flatui_radio.accesskey},
        {<<"class">>, Record#flatui_radio.class},
        {<<"contenteditable">>, case Record#flatui_radio.contenteditable of true -> "true"; false -> "false"; _ ->
            undefined end},
        {<<"contextmenu">>, Record#flatui_radio.contextmenu},
        {<<"dir">>, case Record#flatui_radio.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ ->
            undefined end},
        {<<"draggable">>, case Record#flatui_radio.draggable of true -> "true"; false -> "false"; _ ->
            undefined end},
        {<<"dropzone">>, Record#flatui_radio.dropzone},
        {<<"hidden">>, case Record#flatui_radio.hidden of "hidden" -> "hidden"; _ -> undefined end},
        {<<"id">>, Id},
        {<<"lang">>, Record#flatui_radio.lang},
        {<<"spellcheck">>, case Record#flatui_radio.spellcheck of true -> "true"; false -> "false"; _ ->
            undefined end},
        {<<"style">>, Record#flatui_radio.style},
        {<<"tabindex">>, Record#flatui_radio.tabindex},
        {<<"title">>, Record#flatui_radio.title},
        {<<"translate">>, case Record#flatui_radio.contenteditable of "yes" -> "yes"; "no" -> "no"; _ ->
            undefined end},
        % spec
        {<<"autofocus">>, Record#flatui_radio.autofocus},
        {<<"checked">>, if Record#flatui_radio.checked == true -> <<"checked">>; true -> undefined end},
        {<<"data-toggle">>, <<"checkbox">>},
        {<<"disabled">>, if Record#flatui_radio.disabled == true -> "disabled"; true -> undefined end},
        {<<"form">>, Record#flatui_radio.form},
        {<<"name">>, Record#flatui_radio.name},
        {<<"required">>, if Record#flatui_radio.required == true -> "required"; true -> undefined end},
        {<<"type">>, <<"radio">>},
        {<<"value">>, Record#flatui_radio.value} | Record#flatui_radio.data_fields
    ]),
        case Record#flatui_radio.body of undefined -> []; B -> B end],
    wf_tags:emit_tag(<<"label">>, wf:render(Label), [
        {<<"id">>, Record#flatui_radio.label_id},
        {<<"class">>, Record#flatui_radio.label_class},
        {<<"style">>, Record#flatui_radio.label_style},
        {<<"title">>, Record#flatui_radio.label_title},
        {<<"for">>, Id}]).


%% init_checkbox/1
%% ====================================================================
%% @doc Initializes a checkbox with given id.
%% For the checkbox to work properly, it must be initialized first.
%% This is done automatically on every checkbox after page loads,
%% but checkboxes added dynamically must be initialized with the function.
%% @end
-spec init_checkbox(ID :: binary()) -> ok.
%% ====================================================================
init_checkbox(ID) ->
    gui_jq:wire(<<"$('#", ID/binary, "').checkbox();">>).

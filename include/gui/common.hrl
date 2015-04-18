%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains common GUI macros, types and records
%%% for all projects using n2o.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(GUI_COMMON_HRL).
-define(GUI_COMMON_HRL, 1).

-include_lib("n2o/include/wf.hrl").

%% Any custom element records should go here.

% FlatUI compliant radio button
-record(flatui_radio, {?ELEMENT_BASE(flatui_radio),
    label_id,
    label_class,
    label_style,
    label_title,
    autofocus,
    checked = false,
    disabled,
    form,
    name,
    required,
    value,
    postback,
    html_name
}).

% FlatUI compliant checkbox
-record(flatui_checkbox, {?ELEMENT_BASE(flatui_checkbox),
    label_id,
    label_class,
    label_style,
    label_title,
    autofocus,
    checked = false,
    disabled,
    form,
    name,
    required,
    value,
    postback
}).

-record(flatui_label, {?ELEMENT_BASE(flatui_label),
    for,
    form,
    postback
}).

-endif.


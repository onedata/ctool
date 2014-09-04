%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains common GUI macros, types and records
%% for all projects using n2o.
%% @end
%% ===================================================================

-ifndef(GUI_COMMON_HRL).
-define(GUI_COMMON_HRL, 1).

-include_lib("n2o/include/wf.hrl").

%% Any custom element records should go here.

%% Includes from cowboy
-type req() :: cowboy_req:req().
-export_type([req/0]).

-endif.


%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This behaviour specifies an API for logger plugin - a module
%% that can customize logging procedures. Every project using logging
%% must implement this behaviour and the implmeneting module
%% must be called logger_plugin.
%% @end
%% ===================================================================

-module(logger_plugin_behaviour).

-export([behaviour_info/1]).

%% behaviour_info/1
%% ====================================================================
%% @doc Defines the behaviour (lists the callbacks and their arity)
-spec behaviour_info(Arg) -> Result when
    Arg :: callbacks | Other,
    Result :: [Fun_def]
    | undefined,
    Fun_def :: tuple(),
    Other :: any().
%% ====================================================================
behaviour_info(callbacks) ->
    [
        {gather_metadata, 0}
    ];

behaviour_info(_Other) ->
    undefined.


%% ====================================================================
%% Callbacks descriptions
%% ====================================================================

%% gather_metadata/0
%% ====================================================================
%% Function: gather_metadata() -> list().
%% Desription: Should return a list of key, value tuples to be concatenated
%% to standard log metadata.
%% ====================================================================
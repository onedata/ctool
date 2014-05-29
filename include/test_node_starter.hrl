%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Header file for functions used by ct test to start nodes for testing
%%% @end
%%% Created : 04. May 2014 12:15 PM
%%%-------------------------------------------------------------------
-author("Tomasz Lichon").

-ifndef(TEST_NODE_STARTER_HRL).
-define(TEST_NODE_STARTER_HRL, 1).

%% This macro adds all ebin directories needed by ct tests to code path
-define(INIT_CODE_PATH, begin
							% prepare dirs (you must be in working directory for ct run, i. e.
							% app_name/test_distributed/log/ct_run.tester@172.16.67.81.2014-05-26_15.32.54
	                        {ok, CWD} = file:get_cwd(),
	                        CtTestRoot = filename:join(CWD, "../.."),
	                        ProjectRoot = filename:join(CtTestRoot,".."),
	                        Ebin = filename:join(ProjectRoot,"ebin"),
	                        Deps = filename:join(ProjectRoot,"deps"),
	                        {ok, DepDirs} = file:list_dir(Deps),
	                        DepEbinDirs = lists:map(fun(Dir) -> filename:join([Deps,Dir,"ebin"]) end,DepDirs),

	                        % add dirs to code path
	                        code:add_path(CWD),
	                        code:add_path(ProjectRoot),
	                        code:add_path(CtTestRoot),
	                        code:add_path(Ebin),
	                        code:add_paths(DepEbinDirs),

                            % add dirs to suite state ets
                            ets:new(suite_state, [set, named_table, public]),
                            ets:delete_all_objects(suite_state),
                            ets:insert(suite_state, {test_root, filename:join(CWD, "..")}),
                            ets:insert(suite_state, {ct_root, CtTestRoot}),

                            % change working directory to ct root
                            shell_default:cd(CtTestRoot),

                            % clear db
                            os:cmd("./clear_test_db.sh")
end).

-define(DB_NODE,?NODE(?CURRENT_HOST,db)).

-define(CURRENT_HOST, list_to_atom(lists:last(string:tokens(atom_to_list(node()), "@")))).

-define(NODE(NodeHost,NodeName), list_to_atom(atom_to_list(NodeName)++"@"++atom_to_list(NodeHost))).

-define(GET_NODE_NAME(FullName),list_to_atom(hd(string:tokens(atom_to_list(FullName), "@")))).

-define(GET_HOST(FullName), list_to_atom(lists:last(string:tokens(atom_to_list(FullName), "@")))).

-endif.
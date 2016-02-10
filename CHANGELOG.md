# Release notes for project ctool


CHANGELOG
---------

### 2.0.1

* Dependencies management update


### 2.0.0


* VFS-1454 use set_cookie function once per node
* VFS-1476, docks update
* VFS-1421 Add ALL_PERMS and RW_ALL acl flags.
* VFS-1428 Add utility function to get token issuer.
* VFS-1148 move sync to ctool
* VFS-1454 bugfix, handle many keys for clsuter_manager cookie in env.json
* VFS-1493, Stop application before  docker stop
* VFS-1454 handling different cookies defined in test environment
* VFS-1400 Adjust ends_with_slash function to empty strings.
* VFS-1400 Add a few str_utils functions.
* VFS-1403 Define acl xattr name.
* VFS-1457 initial cluster-manager renaming
* VFS-1382 add default apps comment
* VFS-1403 Add acl header.
* VFS-1403 Add ENOATTR posix error code.
* VFS-1382 moving ct coverage to cluster-worker
* VFS-1382 making ct work in cluster-worker
* VFS-1371 Include all POSIX errors in errors.hrl .
* VFS-1401 add ensure_defined function to utils
* VFS-1403 Rename xattr.key to xattr.name.
* VFS-1403 Add xattr.
* VFS-1378 add ensure_ends_with_slash utils fun
* VFS-1148 improve unicode manipulation fins
* VFS-1378 adjust tests to include pools
* VFS-1378 add connection pools handling in REST API
* VFS-1378 add tmp dir manipulation funs, update web-client ref
* VFS-1378 add hex_utils lib and some tests
* VFS-1334 Add utility function that both validates and unloads mocks.
* VFS-1388 Add ensure_path_ends_with_slash function.
* VFS-1388 Parse empty json.
* VFS-1378 move more related funcitons to http_utils
* VFS-1378 rename gui_str to str_utils
* VFS-1378 move some http relate functions to http_utils
* VFS-1378 switch from ibrowse to hackney, adjust API
* VFS-1334 Add assertions that check given expression until it is valid or a timeout occurs.
* VFS-1334 Increase rpc timeouts for test utils functions.
* VFS-1334 Change modules loading strategy.
* VFS-1334 Change mocking approach. Enable modules loading on test init.
* VFS-1312 Add extend set of receive assertions.
* VFS-1338 Move posix errors and file_attr record to ctool.
* VFS-1312 Add assertReceived and update assertMatch assertions.
* VFS-1291 Add json utility functions.
* VFS-1291 Add tracer utility functions.
* VFS-1244 use macaroons for user auth
* VFS-1244 add gr_users function for /user/authorize endpoint
* Update deps to R18-compatible.
* VFS-1193 update gproc
* VFS-1191, rename redirect handler
* VFS-1193 update annotations framework
* VFS-1172, disable hsts
* VFS-1145 Add ranch_ssl2 transport.
* VFS-1174, cover testing update
* VFS-1172, add gr edpoint for test regsitration of providers
* VFS-1164, stress test framework base
* VFS-1147 Fix function spec.
* VFS-1145 Add ranch_ssl2 transport.
* Cover analysis blocked for performance.
* Cover analysis documentation added.
* VFS-1128 Add utility function for function call execution time measurement. Add repeat logging in performance tests.
* VFS-1129 remove REQUEST_DISPATCHER definition
* VFS-1129 Add definition of request dispatcher name.
* VFS-1025, add code purge to test_utils unload mock
* VFS-1118, export memory monitoring
* VFS-1025, save start env log to file
* VFS-1025, update space
* VFS-1025, add lower bound for node load
* VFS-1025, change ifdef clause
* VFS-1025, add global deifinitions hrl
* VFS-1025, add tests for load balancing module
* VFS-1096 Change method of getting git repository name.
* VFS-1099 Extend list of exported types from dns_server module.
* VFS-1053 rename bin-oneprovider -> bin-worker
* VFS-1025, update load balancing
* VFS-1025, set DNS list shuffling to random
* VFS-1050 Change performance annotation and add rebar git plugin.
* VFS-1050, Perf tests fails shown in sumup
* VFS-1025, move monitoring logic to ctool
* VFS-1023, Integration with Bamboos
* VFS-1050 Add time difference utility functions.
* VFS-1050 Add unit to performance tests results.
* VFS-1050 Change JSON structure.
* VFS-1050 Add repeat number to detailed performance test results.
* VFS-1051 rename oneprovider_ccm
* VFS-1051 separate ccm app
* VFS-1000, perf test output extension
* Ignore stderr while starting env
* VFS-1051 meck adjustments
* VFS-1010 Fix ct tests start.
* VFS-1010 Fix test utils spec.
* VFs-1037 Add terminate log.
* VFS-1019 change random:uniform to crypto:rand_uniform
* VFS-1019 adjust cleaning to case when no docker is up
* VFS-1019 assertion dialyzer fix
* VFS-1019 do not cluster appmock nodes
* VFS-1019 change gr_plugin behavior doc + sync global when starting cluster
* VFS-1000, start script update
* VFS-1019 add appmock to ct_tests
* VFS-1000, start script update
* VFS-1037 move test_node_starter:cmd/1 to utils module
* VFS-1019 get_host get_host_as_atom util functions
* VFS-1037 utils:random_element(List)
* VFS-1000, perf test framework update
* VFS-1047 Run provider_up with a logdir argument.
* VFS-997 Add set/get env functions to test utils.
* VFS-997 Fix mock unload.
* VFS-1000, annotations for performance tests
* VFS-997 Change receive_msg function.
* VFS-997 Add some utility functions.
* VFS-997 Change spec.
* VFS-997 Add behaviours to erl_first_file opts.
* VFS-997 Remove unnecessary dependencies.
* VFS-997 Add test utils.
* VFS-1000, start environment function update
* VFS-997 Loading test code on worker nodes.
* VFS-1000 Remove unnecessary os:cmd call.
* VFS-1010 Remove old cruft.
* VFS-1000, start and clean environment functions' docs added
* add log_bad_request macro
* VFS-1010 Modify test code to work with new bamboos structure.
* VFS-1000 Modify test_node_starter to work with changed repo layout.
* add waiting for main gen_server init
* VFS-1000, clean environment function added
* VFS-1000, prepare env function added
* average and aggregate_over_first_element util functions
* fix utils:mtime/0
* VFS-937 Websocket handler for test connections.
* VFS-953 Checking for existing atoms.
* VFS-953 Extends space details record.
* VFS-953 Extend descriptions.
* VFS-954, fix bug in bootstrap css
* VFS-954, update icons set
* VFS-935, remove unneeded code
* VFS-935, add unit tests for dns_server
* VFS-935, move dns server modules back to ctool
* VFS-935, temporarily move dns server code to GR for easier development
* VFS-937 Add encoding/decoding protobuffs' messages helper functions.
* VFS-914, remove unused code
* VFS-914, add TTL arg to answer records
* VFS-914, update specs
* VFS-914, allow answers with wrong requests
* VFS-914, fix error with dns class mathing
* ceil function
* VFS-914, move common DNS modules to ctool
* VFS-900 Setting default environmental variables read from sys.config.
* VFS-613, add debounce fun
* VFS-613, fix collapsing top menu
* VFS-613, remove css responsible for responsive top menu
* VFS-613, add top menu scrolling
* VFS-897 Add effective user privileges method.



### 1.0.0


* VFS-910 Add syntax highlighting script and style.
* check_my_ports method changed from get to post
* VFS-870, revert changes
* VFS-870, fix select dropdown
* VFS-870, rework register escape event
* trim_spaces function
* VFS-870, add radio buttons
* VFS-886, add several icons
* VFS-864 Remove connection timeout for ip address check.
* VFS-870, add icomoon icons
* VFS-837 Change from 'veil' to 'onedata'.
* VFS-866 make binary_join pretty
* VFS-866 add support for user's logins
* VFS-859 Add client name to provider details record.
* VFS-789: export binary_join
* VFS-789: add binary_join
* VFS-859 Add token management functions for OpenID endpoint.
* VFS-679 Step up the documentation.
* VFS-847 Add flatui label.
* VFS-847, fix error reporting in gui session handler
* VFS-679 Modify get_token_response documentation.
* VFS-847, add label title
* VFS-847, add custom checkbox element
* VFS-807, add missing n2o files
* remove custom transition port
* VFS-807, move common scripts to ctool
* VFS-679 Remove base64decode internal function.
* VFS-679 Change 'email' OpenID claim name and content.
* VFS-847, remove redirect after login mechanism
* VFS-838 Add error messages parsing.
* VFS-831 Fix jquery functions.
* VFS-831 Fix function spec.
* VFS-831 Add noauth request on /openid/client/tokens endpoint.
* VFS-831 Change n2o git tag.
* VFS-732, add cookie function wrapper
* add gproc to app src
* VFS-732, enable toggling server cert verification
* VFS-790 Add <<"undefined">> when user's default Space is not provided.
* VFS-790 Add generic runner and error handling layer.
* VFS-790 Default user Space in API.
* VFS-732 update n2o version
* VFS-790 Extend gui_jq API.
* VFS-790 Add Bootbox API.
* VFS-790 Extend gr_providers endpoint.
* VFS-790 Add try_user request type.
* VFS-790 Fix email parsing in grant_token.
* VFS-790 Extend gr_openid endpoint.
* VFS-790 Extend gr_openid endpoint.
* VFS-790 Fix gr_openid tests.
* VFS-790 Add gr_openid endpoint.
* VFS-790 Remove Access Token.
* VFS-790 Fix registration error.
* VFS-790 Add insecure requests.
* VFS-790 Rename field in provider_info record.
* VFS-790 Extend gr_providers module with check_ip_address and check_port functions.
* VFS-790 Update form_params function after cowboy update.
* VFS-790 Add gr_users tests.
* VFS-790 Add gr_spaces tests.
* VFS-790 Add gr_providers tests.
* VFS-790 Add gr_groups tests.
* VFS-790 Add gr_endpoint tests.
* VFS-790 Add space endpoint.
* VFS-790 Add group endpoint.
* VFS-790 Add user endpoint.
* VFS-732, fix record name clash in wf/ibrowse
* VFS-732, update n2o version
* VFS-790 Add provider endpoint.
* VFS-732, update cowboy t onewest version
* VFS-754: make ensure_* smarter
* VFS-754: add ensure_list
* VFS-754: add token ctx
* VFS-658 Fix privacy policy style.
* add ProjectRoot to suite state ets
* VFS-658, add email validatoin func
* VFS-658, move more functionalities to gui_utils
* VFS-658, disable error logger HWM dureing tests
* VFS-658, move redirect_handler to common code
* VFS-658, logging.hrl update
* VFS-658, minor fix
* VFS-658, add session context to comet processes
* Fix formatting.
* Refactoring.
* Remove unnecessary module usage.
* Add some key binding functions.
* Add some jquery helper functions.
* Add clear expired session functions.
* Remove user_record functions.
* Add fade out function.
* Add click function.
* Add checkbox function.
* Add put and get to session function.
* Add fade out function.
* Add form_param function.
* VFS-658, better test output
* VFS-658, readme update
* VFS-658, change include paths
* VFS-658, spec update
* VFS-658, move logger tests out to ctool
* VFS-658, move some deps to ctool
* created dirs for test modules
* makefile and readme fixes
* db cleanup removed
* ifdefs added to header
* cleaning cluster directories moved to cluster repo
* macro refactoring
* vcn_utils takan from cluster, start_test_node_with_dist_app fix
* get_db_node refactoring
* new macros added
* start_test_nodes_with_dist_app implementation, some refactoring
* asserions added
* start/stop_app_on_nodes
* start_test_node return value fix
* get_db_node added
* stop_test_nodes implemented
* start/stop test node
* start/stop deps for tester node
* add codepath os current working dir
* INIT_DIST_TEST implemented (an extension to INIT_CODE_PATH)
* always define TEST as true when using assertions
* new assertions added
* naming update
* app name generalization
* naming update
* start_deps/stop_deps generalized + some naming changes
* lib_dir change reverted
* lib_dir test change
* lib_dir test change
* starting ctool app on slave nodes
* test_node_starter logic taken from globalregistry
* added files: app.src, makefile, rebar, license, gitignore


________

Generated by sr-release. 

About
=====
*ctool* is library application that contains helper modules for *onedata* project.


Goals
-----
*ctool* gathers code and dependencies that are common for all *onedata* subprojects.


Getting Started
---------------

#### 1. Dependencies
Including *ctool* in a project will cause it to automatically include following dependencies:

* lager
* ranch
* cowboy
* erlydtl
* n2o

#### 2. Functionalities
*ctool* incorporates following functionalities:

* CT (common test) utilities
* logging system, based on lager
* common GUI utilities

#### 3. Logging system
Useful information on how to use the logging system:

* The application including *ctool* must contain a module called exactly logger_plugin, which must implement 
logger_plugin_behaviour. This allows for customisation of logging.
* logger module exports several configuration functions to set loglevels, enable bigger verbosity with stacktraces etc.
* logging.hrl file contains logging macros that must be used instead of direct lager calls as they introduce additional
logic layer required to customize logging.
* There are two convenience macros in logging.hrl - 'dump' and 'dump_all', which might be found useful when debugging
and developing.

#### 4. GUI utils
Useful information on GUI utilities:

* Custom n2o elements that might be reusable in multiple projects should be added to *ctool*. An example is the 
element_form module. A record connected with such element must be included in common.hrl header file.
* Modules with names starting with 'gui_' are essentially a wrapper to n2o's 'wf' module. It is important that they are
used instead of native n2o calls so such issues as XSS can be controlled. If there is any functionality missing, it
should be added to *ctool* rather than the including project.
* gui_session_handler implements custom, secure cookie handling. It is preferable to default n2o's session handler. To
use it, one must call `gui_utils:init_n2o_ets_and_envs/2` before starting a cowboy listener, and 
`gui_utils:cleanup_n2o/1` on cleanup. session_logic_behaviour must be implemented and the module name must be provided
in init. 
* For better security, a cowboy callback 'onrequest' should point to `gui_utils:onrequest_adjust_headers/1`.
* *ctool* environment variable 'root_cacert_file' holds a default path to ca-cert bundle. It should be overwritten if
the bundle is somewhere else on the system. 

Support
-------
For more information visit project *Confluence* or write to <wrzeszcz@agh.edu.pl>.

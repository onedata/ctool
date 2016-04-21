# ctool
ctool is library application that contain utlity functions for logging, monitoring and testing.

# User Guide
ctool provies various functionality in a form of erlang modules. It is usually added to other projects as rebar dependency. It may be also build and used separetely - to build it use `make.py` script. 

Most important elements of ctool are:
* `logging.hrl` - logging macros that must be used instead of direct [lager](https://github.com/basho/lager) calls as they introduce additional logic layer required to customize logging.
* `performance.hrl` - macros for performance and stress tests creation
* `assertions.hrl` - assertions for [ct](http://erlang.org/doc/man/common_test.html) tests (based on assertions provided by [eunit](http://erlang.org/doc/apps/eunit/)).
* `test_utils.erl` - functions for mocking in [ct](http://erlang.org/doc/man/common_test.html)
* `test_node_starter.erl` - functions for environment setting/teardown.
 tests.
* `tracer.erl` - utility functions for debugging.


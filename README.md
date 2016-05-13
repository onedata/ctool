# ctool
*ctool* is library application that contain utlity functions for logging, monitoring and testing.

# User Guide
*ctool* provies various functionality in a form of erlang modules. It is usually added to other projects as *rebar* dependency. It may be also build and used separetely - to build it use `make.py` script. 

Most important elements of *ctool* are:

* `logging.hrl` - logging macros that must be used instead of direct [lager](https://github.com/basho/lager) calls as they introduce additional logic layer required to customize logging.
* `performance.hrl` - macros for performance and stress tests creation
* `assertions.hrl` - assertions for [ct](http://erlang.org/doc/man/common_test.html) tests (based on assertions provided by [eunit](http://erlang.org/doc/apps/eunit/)).
* `test_utils.erl` - functions for mocking in *ct*
* `test_node_starter.erl` - functions for environment setting/teardown.
 tests.
* `tracer.erl` - utility functions for debugging.

# APIs
Following logging macros are defined:

 * `debug`
 * `info`
 * `notice`
 * `warning`
 * `error`
 * `critical`
 * `alert`
 * `emergency`

Each may be used in following ways (example with debug macro):

 * `?debug(_Message)`
 * `?debug(_Format, _Args)`
 * `?debug_stacktrace(_Message)`
 * `?debug_stacktrace(_Format, _Args)`

There are 2 performance macros:

 * `ALL` - to show which test should be started during performance tests,
 * `PERFORMANCE` - to provide additional parameters to test.

Sample usage in *ct* test file:

```erlang
all() ->
    ?ALL([t1, t2], [t2, t3]).

sample_test(Config) ->
    ?PERFORMANCE(Config, [
            {repeats, ?REPEATS},
            {success_rate, ?SUCCESS_RATE},
            {parameters, [
                [{name, p1}, {value, 10}, {description, "Param 1"}],
                [{name, p2}, {value, 3}, {description, "Param 2"}]
            ]},
            {description, "Test description"},
            {config, [{name, performance config},
                {parameters, [
                    [{name, p1}, {value, 100}],
                    [{name, p2}, {value, 50}]
                ]},
                {description, "Basic performance config for test"}
            ]}
        ]).
sample_test_base(Config) ->
	1 = 1.
```

For example above use [ct_run](https://github.com/onedata/bamboos/blob/develop/docker/ct_run.py) script:

 * Use `./ct_run` to execute tests *t1* and *t2*.
 * Use `./ct_run --performance` to execute tests *t2* and *t3*.


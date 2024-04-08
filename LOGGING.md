# logging examples
Exemplary usages of logging macros and samples of corresponding output. 

```erlang
% catches and returns known errors (no logging in this case),
% otherwise generates an INTERNAL_SERVER_ERROR with reference and logs using ?error_exception
?catch_exceptions(error(exception))
```

```
[E 15:06:03.213 <0.373.0>] An unexpected exception (ref: 7560c10c30) ocurred in logging_examples:test/0 line 108
> Caught: error:exception
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 108
```

*****

```erlang
% an error log NOT related to an exception
?error("Custom message")
?warning("Custom formatted message: ~tp", [TermA])
?notice(?autoformat_with_msg(
    "Custom formatted message~nwith some multine content~nand autoformatted terms:", [TermA, TermB]
)),
```

```
[E 22:19:32.730 <0.373.0>] Custom message
[W 22:19:32.730 <0.373.0>] Custom formatted message: <<"2a526a3a3ae8e4c9">>
[N 22:19:32.730 <0.373.0>] Custom formatted message
with some multine content
and autoformatted terms: 
    TermA = <<"2a526a3a3ae8e4c9">>
    TermB = {tuple,#{<<"with nested">> =>
                         [terms,that,will,be,printed,as,multiline,text]}}
```

*****

```erlang
% DEPRECATED error log with stacktrace - use ?error_exception for that
?error_stacktrace("Custom message", Stacktrace)
```

```
[E 15:06:03.214 <0.373.0>] An unexpected exception ocurred in logging_examples:test/0 line 121
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details: Custom message
```

*****

```erlang
% DEPRECATED error log with stacktrace - use ?error_exception for that
?error_stacktrace("Custom formatted message: ~tp", [TermA], Stacktrace)
```

```
[E 15:06:03.214 <0.373.0>] An unexpected exception ocurred in logging_examples:test/0 line 123
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details: Custom formatted message: <<"047e23a2184d180e">>
```

*****

```erlang
% generic exception log
?error_exception(Class, Reason, Stacktrace)
```

```
[E 15:06:03.214 <0.373.0>] An unexpected exception ocurred in logging_examples:test/0 line 131
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
```

*****

```erlang
% generic exception log with details as simple message
?emergency_exception("Custom message", Class, Reason, Stacktrace)
```

```
[M 15:06:03.214 <0.373.0>] An unexpected exception ocurred in logging_examples:test/0 line 134
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details: Custom message
```

*****

```erlang
% generic exception log with details as formatted message
?critical_exception("Custom formatted message: ~tp", [TermA], Class, Reason, Stacktrace)
```

```
[C 15:06:03.214 <0.373.0>] An unexpected exception ocurred in logging_examples:test/0 line 137
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details: Custom formatted message: <<"047e23a2184d180e">>
```

*****

```erlang
% generic exception log with additional terms printed out (by variable names)
?alert_exception(?autoformat([TermA, TermB]), Class, Reason, Stacktrace)
```

```
[A 15:06:03.214 <0.373.0>] An unexpected exception ocurred in logging_examples:test/0 line 140
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details:
    TermA = <<"047e23a2184d180e">>
    TermB = {tuple,#{<<"with nested">> =>
                         [terms,that,will,be,printed,as,multiline,text]}}
```

*****

```erlang
% generic exception log with details as formatted message and
% additional terms printed out (by variable names)
?warning_exception(?notice(
    ?autoformat_with_msg(
        "Custom formatted message~nwith some multine content~nand autoformatted terms:", [TermA, TermB]
    )),
    Class, Reason, Stacktrace
)
```

```
[W 22:16:40.926 <0.373.0>] An unexpected exception occurred in logging_examples:test/0 line 139
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details: Custom formatted message
with some multine content
and autoformatted terms: 
    TermA = <<"77b6b86da69e5d84">>
    TermB = {tuple,#{<<"with nested">> =>
                         [terms,that,will,be,printed,as,multiline,text]}}
```

*****

```erlang
% returns thrown errors that are known (no logging in this case),
% otherwise generates an INTERNAL_SERVER_ERROR with reference and logs using ?error_exception
?examine_exception(Class, Reason, Stacktrace)
```

```
[E 15:06:03.214 <0.373.0>] An unexpected exception (ref: d334179a76) ocurred in logging_examples:test/0 line 144
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
```

*****

```erlang
% as above, but with custom details message
?examine_exception("Custom message", Class, Reason, Stacktrace)
```

```
[E 15:06:03.215 <0.373.0>] An unexpected exception (ref: 1a82a9f8b2) ocurred in logging_examples:test/0 line 146
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details: Custom message
```

*****

```erlang
% as above, but with custom details formatted message
?examine_exception("Custom formatted message: ~tp", [TermA], Class, Reason, Stacktrace)
```

```
[E 15:06:03.215 <0.373.0>] An unexpected exception (ref: c8eb13824b) ocurred in logging_examples:test/0 line 148
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details: Custom formatted message: <<"047e23a2184d180e">>
```

*****

```erlang
% as above, but with additional terms printed out (by variable names)
?examine_exception(?autoformat([TermA, TermB]), Class, Reason, Stacktrace)
```

```
[E 15:06:03.215 <0.373.0>] An unexpected exception (ref: a7e62b0184) ocurred in logging_examples:test/0 line 150
> Caught: error:{some_exception,that_was_not_expected}
> Stacktrace:
    shell:eval_loop/3 line 627
    shell:eval_exprs/7 line 642
    shell:exprs/7 line 686
    erl_eval:do_apply/6 line 689
    logging_examples:test/0 line 111
> Details:
    TermA = <<"047e23a2184d180e">>
    TermB = {tuple,#{<<"with nested">> =>
                         [terms,that,will,be,printed,as,multiline,text]}}
```

*****

```erlang
% logs and returns an INTERNAL_SERVER_ERROR with error reference
% used when there was no unexpected exception
?report_internal_server_error("Custom message")
?report_internal_server_error("Custom formatted message: ~tp", [TermA])
?report_internal_server_error(?autoformat([TermA, TermB]))
```

```
[E 15:06:03.215 <0.373.0>] An error (ref: e911138d1d) ocurred in logging_examples:test/0 line 154
> Details: Custom message
[E 15:06:03.215 <0.373.0>] An error (ref: 3b01a46a92) ocurred in logging_examples:test/0 line 156
> Details: Custom formatted message: <<"047e23a2184d180e">>
[E 15:06:03.215 <0.373.0>] An error (ref: ade1a2474c) ocurred in logging_examples:test/0 line 158
> Details:
    TermA = <<"047e23a2184d180e">>
    TermB = {tuple,#{<<"with nested">> =>
                         [terms,that,will,be,printed,as,multiline,text]}}
```
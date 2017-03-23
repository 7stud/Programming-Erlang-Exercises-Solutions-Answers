If you haven't tried **eunit** for testing yet, the simplest way to use **eunit** is to run all the functions named \*_test in a file.  For example, suppose you have a file like this:

```erlang
-moduel(a).
-export([do/0, go/0]).

do_test() ->
    ...
    
do() ->
    ...


go_test() ->
    ...
   
go() ->
    ...
    
```

You can run a specific test function just like any other function:
```erlang
1> c(a).
{ok,a}

2> a:do_test().
dummy
```
With **eunit**, you can run both `do_test()` and `go_test()` with one command:

```erlang
4> c(a).
{ok,a}

5> a:test().
  All 2 tests passed.
ok
```
All you have to do to enable eunit is include this line in your file:

    -include_lib("eunit/include/eunit.hrl").
    
and make sure your test function names end in `_test` (eunit automatically exports the test functions for you). That feature of eunit comes in handy in this exercise.






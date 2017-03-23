If you haven't tried **eunit** for testing yet, a very simple way to use **eunit** is to run all the functions named \*_test in a file.  For example, you might have a file like this:

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


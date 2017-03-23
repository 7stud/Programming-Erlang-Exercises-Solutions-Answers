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
All you have to do is enable **eunit** by adding the following line to the top of your file:

    -include_lib("eunit/include/eunit.hrl").
    
and make sure the names of your test functions end in `_test` (eunit automatically exports the test functions for you). That feature of eunit comes in handy in this exercise.

The book uses the old "suffix rules" in the makefile, which have been replaced by "pattern rules".  I put a modern interpretation of a basic makefile with explanations in the Chapter 10 directory.  Here is that makefile modified to run tests:

```makefile
modules = a b

all: $(modules:%=%.beam) test
.PHONY: all

%.beam: %.erl
	erlc -W $< 
	
test: $(modules:%=%_test)
.PHONY: test

%_test: %.erl
	erl -noshell -s $* test -s init stop  # $* is the part of the file name matched by the % wildcard

clean:
	rm $(modules:%=%.beam) erl_crash.dump
.PHONY: clean
```

I learned about makefiles while reading this chapter:

If you are familiar with C/C++:

http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/

Some really excellent make resources:

http://www.oreilly.com/openbook/make3/book/ch02.pdf

https://www.gnu.org/software/make/manual/make.html#Overview

(Oh yeah, use gmake instead of make.)






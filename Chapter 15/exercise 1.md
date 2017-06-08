If you wade through the C code (see explanations below) and finally get to the makefile, you might wonder WTF?  `example_lid.c`?  `unit_test`?  Where are those files?  At the start of the chapter, on (the unmarked) p. 233, the book describes running foreign language code _inside_ the Erlang virtual machine, and calls that method _unsafe_.  Then the book goes on to say that the chapter won't be covering that method.  Yet, `example_lid.c` is for doing that _unsafe_ thing that supposedly wasn't going to be covered, and therefore it doesn't belong in the example.  

Below is a modern makefile (see my answers for chapter 10 for an explanation of how the makefile works) that I created for compiling just the files discussed in the chapter, which are:
```
example1.c
example1_driver.c
erl_comm.c

example1.erl
```
I decided to incorporate a modified version of the very simple unit_test file, which can be found in the source code, and my version is also included below.

makefile:
```
modules := example1 unit_test

.PHONY: all
all: ${modules:%=%.beam} example1
	@erl -noshell -s unit_test start

%.beam: %.erl
	erlc -W $<

example1:  example1.c erl_comm.c example1_driver.c
	gcc -o example1 example1.c erl_comm.c example1_driver.c

.PHONY: clean
clean:
	rm example1 *.beam
```

(modified) unit_test:
```erlang
-module(unit_test).
-export([start/0]).

start() ->
    io:format("Testing drivers~n"),
    example1:start(),
    6 = example1:twice(3),
    10 = example1:sum(6,4),
    
    %example1_lid:start(),
    %8 = example1_lid:twice(4),
    %20 = example1_lid:sum(15,5),
    
    io:format("All tests worked~n"),
    init:stop().
```

Compiling and running:
```
$ gmake
erlc -W example1.erl
erlc -W unit_test.erl
gcc -o example1 example1.c erl_comm.c example1_driver.c

Testing drivers
All tests worked

~/erlang_programs/chap15$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

1> example1:start().
true
2> example1:sum(45, 32).
77
3> example1:twice(10).
20
```

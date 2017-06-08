If you wade through the C code (see explanations below) and finally get to the makefile, you might wonder WTF?  `example_lid.c`?  `unit_test`?  Where are those files?  At the start of the chapter, on (the unmarked) p. 233, the book describes running foreign language code _inside_ the Erlang virtual machine, and calls that method _unsafe_.  Then the book goes on to say that the chapter won't be covering that method.  Yet, `example_lid.c` is for doing that very _unsafe_ thing, and as a resut it doesn't belong in the example.  

Below is a modern makefile I created for compiling just the files discussed in the chapter:
```
example1.c
example1_driver.c
erl_comm.c
example1.erl
```
I decided to incoporate a modifed version of the very simple unit_test file, which can be found in the source code, and my version is also included below.

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

#### Explanation of C code.

I'm going to use some psuedo code mixed in with C code.

```read_exact([], 2)```

That tries to read in two bytes, which represents the Length of the message.

```i = read(0, [], 2)```

That reads from `stdin`, which is identified with a 0, and puts the bytes read into the empty unsigned char array. `2` is the number of bytes to read.  `buf+got` is pointer arithmetic and moves the buff pointer to a new spot in the array marking the end of the data read so far.  `read()` returns:

1. The number of bytes read, or
2. 0 if read hits end-of-file, or
3. a negative number if read() encounters an error.


The trickiest bit is in ```read_cmd([byte1, byte2])```:

    len = (buff[0] << 8 ) | buff[1]);
    
When you use the left bit shift operator on buff[0], which is an unsigned char, or one byte long, the bits are _not_ shifted off to the left leaving 0.  Rather, buff[0] is first converted to an int type (probably 4 bytes long), then the bits are shifted.  An example to illustrate how that line of code works i probably best.  Suppose the length of the message is 258.  That means the two bytes in buff will look like this:

     buff = [0000 0001, 0000 0010]  
     
Note that `0000 0001 0000 0010` is 256, but erlang split the length into two single bytes.  When you specify `{packet, 2}` for a port option (or a TCP socket option in chapter 17), erlang automatically calculates the Length of the message you are sending, then prepends two bytes containing the Length of the message to the beginning of the message.  The other side can then read the first two bytes of a message stream to get the Length of the message, then stop reading when it has received Length bytes.

The question is how do you convert buff[0], which is 1, and buff[1], which is 2 back into 256.

     0000 00001 << 8   --> ... 0001 0000 0000  (That is 256)

Then if you OR that result with buff[1]:

    len = ... 0001 0000 0000 | buff[1]);

buff[1] will be automatically converted to an int then the OR'ing will be done:

```
... 0001 0000 0000
|   
....0000 0000 0010
--------------------
....0001 0000 0010
```

So, you end up with an int containg the original length of 256.


			   
                     

    
  


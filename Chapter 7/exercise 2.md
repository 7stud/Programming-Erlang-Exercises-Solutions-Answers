It took me many rereadings of this exercise to figure out what it was asking us to do. Suppose the Term argument is a list, look at the output here:

```erlang
35> term_to_binary([1, 2, 3]).
<<131,107,0,3,1,2,3>>

36> term_to_binary([1, 2, 3, 4, 5, 6]).
<<131,107,0,6,1,2,3,4,5,6>>
```

The return value of `term_to binary()` is longer when there are more items in the list.  I originally interpreted the exercise to mean that we should store 4 in the first part of the binary, then add 4 bytes to the binary from the return value of `term_to_binary()`, which would effectively chop off some of the return value for a long enough list.  

But the exercise description really means:

> ...return a binary consisting of a 4-byte header *containing a number N*, followed by N bytes of data...

In other words, get the number of bytes of the binary returned by `term_to_binary()`, put that size into 4 bits of a result binary, then add the return value of `term_to_binary()` to the result binary.  Effectively, the first 4 bits will tell you how many bytes contain the data that follows.

Here's my solution:
```
term_to_packet(Term) ->
    Bin = term_to_binary(Term),
    N = byte_size(Bin),
    <<N:4, Bin/binary>>.
```

In the shell:
```erlang
50> term_to_binary([1, 2, 3]).   
<<131,107,0,3,1,2,3>>

51> bin:term_to_packet([1, 2, 3]).
<<120,54,176,0,48,16,32,3:4>>
```
Well, the output is totally incomprehensible because erlang took the 4 bits we used for the size of the Term, then erlang took 4 bits from the first byte of the binary representing the Term to form a new byte 120; then erlang took the remaining 4 bits of the first byte and added them to 4 bits from the second byte of the binary representing the Term to form another full byte, 54; etc.; etc.; then there were 4 bits left over at the end of the binary, with the integer 3 in them.  All in all, `term_to_packet()`'s output is incomprehensible.

Let's remove the the first 4 bits of the result, then see what we have:

```erlang
53> Result = bin:term_to_packet([1, 2, 3]).
<<120,54,176,0,48,16,32,3:4>>

54> <<N:4, Rest/binary>> = Result.
<<120,54,176,0,48,16,32,3:4>>

55> N.  
7

56> Rest.
<<131,107,0,3,1,2,3>>
```

...and Rest is the return value of `term_to_binary()` that we stored in our binary.  In this case, it doesn't even matter what the number N is: we can just remove it, and the rest of the binary is the binary representing the Term.  But, imagine if several packets were combined into one binary.  In that case, you would need to know how many bytes to read in order to get the data for one Term.  Note that the size N, which is 7, matches the number of the bytes in the binary Rest--as the exercise calls for.

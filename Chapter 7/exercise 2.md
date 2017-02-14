It took me many rereadings of this exercise to figure out what it was asking us to do. Suppose the Term argument is a list, look at the output here:

```erlang
35> term_to_binary([1, 2, 3]).
<<131,107,0,3,1,2,3>>

36> term_to_binary([1, 2, 3, 4, 5, 6]).
<<131,107,0,6,1,2,3,4,5,6>>
```

When there are more items in the list, the return value of `term_to binary()` is longer .  I originally interpreted the exercise to mean that we should store 4 in the first part of the binary, then add 4 bytes to the binary from the return value of `term_to_binary()`, which would effectively chop off some of the return value for a long enough list.  

But the exercise description really means:

> ...return a binary consisting of a 4-byte header *containing a number N*, followed by N bytes of data...

In other words, the exercise wants us to get the number of bytes of the binary returned by `term_to_binary()`, put that number into 4 bytes of a result binary, then add the return value of `term_to_binary()` to the result binary.  That way, the first 4 bytes will tell you how many bytes contain the Term that follows.

Here's my solution:
```erlang
term_to_packet(Term) ->
    Bin = term_to_binary(Term),
    N = byte_size(Bin),
    <<N:4/unit:8, Bin/binary>>.  
```

In the shell:
```erlang
50> term_to_binary([1, 2, 3]).   
<<131,107,0,3,1,2,3>>

162> bin:term_to_packet([1, 2, 3]).           
<<0,0,0,7, 131,107,0,3,1,2,3>>   %I added a space.
```

If you remove the first 4 bytes of the result, you get:

```erlang
177> c(bin).                                
{ok,bin}

178> f().   
ok

179> Result = bin:term_to_packet([1, 2, 3]).
<<0,0,0,7,131,107,0,3,1,2,3>>

180> <<N:4/unit:8, Rest/binary>> = Result.  
<<0,0,0,7,131,107,0,3,1,2,3>>

181> N.
7

182> Rest.
<<131,107,0,3,1,2,3>>
```

Rest is the return value of `term_to_binary()` that we stored in our *packet*.  In this case, it doesn't even matter what the number N is: we can just remove the first 4 bytes, and the rest of the binary is the binary representing the Term.  But, imagine if several packets were combined into one binary.  In that case, you would need to know how many bytes to read in order to get the binary for one Term.  Note that the size N, which is 7, matches the number of the bytes in the binary Rest--as the exercise calls for.

Wow, it took me a long time to figure out what this exercise was asking us to do. Suppose the Term is a list, look at the output here

```erlang
35> term_to_binary([1, 2, 3]).
<<131,107,0,3,1,2,3>>

36> term_to_binary([1, 2, 3, 4, 5, 6]).
<<131,107,0,6,1,2,3,4,5,6>>
```

The return value of `term_to binary()` is longer, the more items there are in the list.  I originally interpreted the question to store 4 in a header, then add 4 bytes of the return value of term_to_binary()--effectively chopping off some of the return value for a long enough list. 

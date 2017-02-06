Here is a naive implementation of `time_func()`:

```erlang
time_func(F) -> 
    Start = now(),
    F(),
    End = now(),
    [
     element(I, End) - element(I, Start) || I <- lists:seq(1, size(Start) )
    ].
```

If you think about it a little bit, when you have Start and End times like this:

```
Start:  {X, Y, 999999}  
End:    {X, Y+1, 200}
```

i.e the function F took 201 micro seconds to execute, and then you subtract the elements of the two tuples, you get:

```
Start:  {X, Y, 999999}  
End:    {X, Y+1, 200}
------------------------
        {0, 1, -999799}
```

What you need to do, is borrow 1 from the Secs and add it to the the Micros:
```
        {0, 0, 1*1000000 + (-999799)  = {0, 0, 201}
```

Therefore, in order to normalize the timestamp so that all the terms are positive, you have to examine each term to see if it's negative, and if it is, then you have to go to the bigger term on the left and borrow 1 from it.  Erlang does provide a function called `timer:now_diff` that will neatly subtract two timestamps for you, but because the exercise didn't mention that that was one of the functions we should to look at, I thought I would try to implement my own function to accomplish the same thing:


```erlang
fix_timestamp(L) ->
    [T|Ts] = lists:reverse(L),  %[Megas, Secs, Micros] => [Micros, Secs, Megas]
    Acc = [],
    fix_timestamp(T, Ts, Acc).

fix_timestamp(T, [], Acc) ->
    [T|Acc];
fix_timestamp(T1, [T2|Tail], Acc) ->
    if
        T1 < 0 -> 
            fix_timestamp(T2-1, Tail, [1000000+T1|Acc]);
        true -> 
            fix_timestamp(T2, Tail, [T1|Acc])
    end.
```

Here's how fix_timestamp() works in the shell:

16> c(lib_misc).
{ok,lib_misc}
17> lib_misc:fix_timestamp([0, 1, -999799]).
[0,0,201]

18> lib_misc:fix_timestamp([1, -500, -1000]).
[0,999499,999000]  %borrow 1 from Secs to fix Micros, which leaves Secs equal to -501, then borrow 1 from Megas, to fix Secs.

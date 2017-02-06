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

But if you think about it a little bit, if you have Start and End times like this:

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

In order to fix the negative term, you need to borrow 1 from the Secs and add it to the the Micros:
```
        {0, 0, -999799 + 1*1000000} => {0, 0, 201}
```

Therefore, in order to normalize the timestamp so that all the terms are positive, you have to examine each term produced by the naive solution to see if the term is negative, and if it is, then you have to go to the bigger term on the left and borrow 1 from it.  

Erlang does provide a function called `timer:now_diff` that will neatly subtract two timestamps for you, but because the exercise didn't mention that function in the list of functions that we should to look at, I thought I would try to implement my own function to accomplish the same thing:


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

Here's how `fix_timestamp()` works in the shell:

16> c(lib_misc).
{ok,lib_misc}

17> lib_misc:fix_timestamp([0, 1, -999799]).
[0,0,201]

18> lib_misc:fix_timestamp([1, -500, -1000]).
[0,999499,999000]  %borrow 1 from Secs to fix Micros, which leaves Secs equal to -501, then borrow 1 from Megas, to fix Secs.

Adding fix_timestamp() to the naive solution:

```erlang
time_func(F) -> 
    Start = now(),
    %io:format("~w~n", [Start]),
    F(),
    End = now(),
    %io:format("~w~n", [End]),
    Times = [
     element(I, End) - element(I, Start) || I <- lists:seq(1, size(Start) )
    ],
    fix_timestamp(Times).
```

For testing, I created a for-loop function to run time_func() on a given function F, a given number of times N:

```erlang
for2(F, Arg, N) ->
    io:format("~s~n", ["looping..."]),
    loop(F, Arg, N).
loop(_, _, 0) ->
    done;
loop(F, Arg, N) when N > 0 ->
    Result = F(Arg),
    io:format("~p~n", [Result]),
    loop(F, Arg, N-1).
```

In the following output, you can see negative terms produced by the naive solution:

```erlang
21> c(lib_misc).

%Define some random function that takes a couple of seconds to execute:
22> F = fun() -> [X*X || X <- lists:seq(1, 1000000), X rem 2 =:= 0] end.

%Apparently, in order to pass a named function as an argument to another function
%you have to use the following syntax:
23> lib_misc:for2(fun lib_misc:time_func/1, F, 10).
looping...
[0,2,167616]
[0,3,-860245]
[0,2,156090]
[0,2,177153]
[0,2,152959]
[0,2,148198]
[0,2,142560]
[0,3,-840577]
[0,2,164850]
[0,2,238998]
done
```

Now look at the output using the modified solution that calls fix_timestamp:

```erlang
30> c(lib_misc).

%Define some random function that takes a couple of seconds to execute:
31> F = fun() -> [X*X || X <- lists:seq(1, 1000000), X rem 2 =:= 0] end.

32> lib_misc:for2(fun lib_misc:time_func/1, F, 20). 
looping...
[0,2,228709]
[0,2,187678]
[0,2,175273]
[0,2,187329]
[0,2,223996]
[0,2,162220]
[0,2,180352]
[0,2,199191]
[0,2,155705]
[0,2,162073]
[0,2,168187]
[0,2,162762]
[0,2,158665]
[0,2,166814]
[0,2,169264]
[0,2,163484]
[0,2,225341]
[0,2,185394]
[0,2,127794]
[0,2,134793]
done
24> 
```

Note that there are no negative terms anymore.  

And, if you want to return a timestamp tuple from time_func:

```erlang
time_func(F) -> 
    Start = now(),
    %io:format("~w~n", [Start]),
    F(),
    End = now(),
    %io:format("~w~n", [End]),
    Times = [
     element(I, End) - element(I, Start) || I <- lists:seq(1, size(Start) )
    ],
    list_to_tuple(
      fix_timestamp(Times)
    ).

```




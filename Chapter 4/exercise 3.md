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

If you have Start and End times like this:

```
Start:  {X, Y, 999999}  
End:    {X, Y+1, 200}
```

i.e. the function F took 201 micro seconds to execute, and then you subtract the elements of the two tuples, you get:

```
  End:    {X, Y+1, 200}
- Start:  {X, Y, 999999}  
------------------------
          {0, 1, -999799}
```

In order to fix the negative term, you need to borrow 1 from the Secs and add it to the the Micros:
```
        {0, 0, -999799 + 1*1000000} => {0, 0, 201}
```

Therefore, in order to normalize a timestamp so that all the terms are positive, you have to examine each term produced by the naive solution to see if the term is negative, and if it is, then you have to go to the bigger term on the left and borrow 1 from it.  

Erlang does provide a function called `timer:now_diff` that will neatly subtract two timestamps for you, but because the exercise didn't mention that function in the list of functions that we should to look at, I thought I would try to implement my own function to accomplish the same thing:

```erlang
ts_diff(End, Start) ->
    fix_times(
      [element(I, End) - element(I, Start) || I <- lists:seq(1, size(End))]
    ).

fix_times(L) ->
    [T|Ts] = lists:reverse(L),  %[Megas, Secs, Micros] => [Micros, Secs, Megas]
    Acc = [],
    fix_times_acc(T, Ts, Acc).

fix_times_acc(T, [], Acc) ->
    [T|Acc];
fix_times_acc(T1, [T2|Tail], Acc) ->
    if
        T1 < 0 -> 
            fix_times_acc(T2-1, Tail, [1000000+T1|Acc]);
        true -> 
            fix_times_acc(T2, Tail, [T1|Acc])
    end.

```

Here's how `fix_times()` works in the shell:

```erlang
42> lib_misc:fix_times([0, 1, -999799]). 
[0,0,201]

43> lib_misc:fix_times([1, -500, -1000]).
[0,999499,999000]   %borrow 1 from Secs to fix Micros, which leaves Secs equal to -501, then borrow 1 from Megas to fix Secs.
```

For testing, I created a for-loop function to run `time_func()` on a given function F, a given number of times N:

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

Here's the modified solution:

```erlang
time_func(F) -> 
    Start = now(),
    F(),
    End = now(),
    ts_diff(End, Start).

ts_diff(End, Start) ->
    fix_times(
      [element(I, End) - element(I, Start) || I <- lists:seq(1, size(End))]
    ).

fix_times(L) ->
    [T|Ts] = lists:reverse(L),  %[Megas, Secs, Micros] => [Micros, Secs, Megas]
    Acc = [],
    fix_times_acc(T, Ts, Acc).

fix_times_acc(T, [], Acc) ->
    [T|Acc];
fix_times_acc(T1, [T2|Tail], Acc) ->
    if
        T1 < 0 -> 
            fix_times_acc(T2-1, Tail, [1000000+T1|Acc]);
        true -> 
            fix_times_acc(T2, Tail, [T1|Acc])
    end.
```

In the shell:

```erlang
30> c(lib_misc).

%Define some random function that takes a couple of seconds to execute:
31> F = fun() -> [X*X || X <- lists:seq(1, 1000000), X rem 2 =:= 0] end.

%This time run the for-loop 20 times:
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

Finally, to return a tuple from `ts_diff()` like `timer:now_diff()` does:

```erlang
ts_diff(End, Start) ->
    Result = fix_times(
      [element(I, End) - element(I, Start) || I <- lists:seq(1, size(End))]
     ),
    list_to_tuple(Result).   % ****CHANGE HERE**** 

```

Here's an alternate solution for `time_func()` that converts the Start and End tuples to total microseconds, subtracts them, then converts the result back to a timestamp tuple:

```erlang
time_func(F) -> 
    Start = now(),
    F(),
    End = now(),
    DiffMicros = total_micros(End) - total_micros(Start),
    timestamp(DiffMicros, size(End)).  

total_micros(Tuple) ->
    TSize = size(Tuple),
    Result = [
        element(I, Tuple) * math:pow(1000000, TSize-I)  
        || I <- lists:seq(1, TSize)
    ],
    round(lists:sum(Result)).  %round() converts to integer.

timestamp(_, 0) ->
    [];
timestamp(Micros, TSize) ->
    Units = round(math:pow(1000000, TSize-1)),
    UnitsCount = Micros div Units,
    NewMicros = Micros - (Units * UnitsCount),
    %I tried the following syntax(on p. 56) instead of using an Acc:
    [UnitsCount|timestamp(NewMicros, TSize-1)].
```

In the shell, 

```erlang

164> c(lib_misc).                                   
{ok,lib_misc}

165> lib_misc:timestamp(100, 3).                    
[0,0,100]

166> lib_misc:timestamp(0, 3).  
[0,0,0]

167> lib_misc:timestamp(1000000*1000000+1500000, 3).
[1,1,500000]

%Define some random function that takes a couple of seconds to execute:
31> F = fun() -> [X*X || X <- lists:seq(1, 1000000), X rem 2 =:= 0] end.

168> lib_misc:for2(fun lib_misc:time_func/1, F, 10).
looping...
[0,2,166518]
[0,2,63600]
[0,2,138325]
[0,2,118236]
[0,2,134436]
[0,2,89460]
[0,2,114554]
[0,2,112772]
[0,2,121008]
[0,2,118641]
done
```




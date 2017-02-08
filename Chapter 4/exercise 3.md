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

fix_times_acc(T1, [T2|Tail], Acc) ->
    if
        T1 < 0 -> 
            fix_times_acc(T2-1, Tail, [1000000+T1|Acc]);
        true -> 
            fix_times_acc(T2, Tail, [T1|Acc])
    end;
fix_times_acc(T, [], Acc) ->
    [T|Acc].

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
    NewMicros = Micros rem Units,
    %I tried the following methodology(from p. 56) instead of using an Acc:
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

-----------------------------

Here's my solution for `my_date_string()`:

```C
my_date_string() ->
    {Y, Mon, D} = date(),
    {H, M, S} = time(),
    io_lib:format("~w-~w-~w ~w:~2..0w:~2..0w", [Y, Mon, D, H, M, S] ).
```

In the shell:

```erlang
43> c(lib_misc).
{ok,lib_misc}

44> lib_misc:my_date_string().
["2017",45,"2",45,"7",32,"11",58,"53",58,"54"]
```

What the heck is all that garbage?  Well, look at this:

```erlang
45> Now = lib_misc:my_date_string().
["2017",45,"2",45,"7",32,"11",58,"55",58,"32"]

46> io:format("~s~n", [Now]).
2017-2-7 11:55:32
ok
```

It's a bit confusing how that works.  Let's start with converting the integer 2017:

```erlang
50> io_lib:format("~w", [2017]).
["2017"]
```

The ***control sequence*** `~w` takes the single integer 2017 and turns it into four characters: '2', '0', '1', '7', which is represented by the list:

    "2017"

Remember, a string is really a list in erlang, which you can see here:

```erlang
51> [$2, $0, $1, $7].  %using the $ syntax on p. 40 to get the ascii code for each character
"2017"
```

Or, equivalently:

```erlang
53> [50,48,49,55].
"2017"
```

So `~w` takes a single integer 2017, and converts it into a list of four integers: `[50,48,49,55]`.  If ***all*** the integers in a list happen to be ascii codes for printable characters, then the erlang shell prints out the list as a string. If the integers in the list represent characters, then that's the output you want to see; but if the integers in the list happen to be the temperature readings on successive days, than you certainly don't want to see a string!  To prevent the shell from converting a list of integers to characters, you can use `~w`:

```erlang
54> X = [50,48,49,55].
"2017"

55> io:format("~w~n", [X]).
[50,48,49,55]
ok
```

After `lib_format()` converts the single integer 2017 to a list of four integers [50,48,49,55], `lib_format()` returns the result wrapped in a list:

    ["2017"]

That's equivalent to:
```erlang
> [[50,48,49,55]].
["2017"]
```

Okay, that's one term.  Now let's look at the whole result:

```erlang
45> Now = lib_misc:my_date_string().
["2017",45,"2",45,"7",32,"11",58,"55",58,"32"]
```

The whole result is equivalent to:
    
    %   "2017"           "2" 
    %      V              V
    [ [50,48,49,55], 45, [50], .... ]
    %                ^
    %                | an integer, which is also the ascii code for a '-'
    
That looks like a nightmare of nested lists!  How do we get a single string out of that?  Well, look at what the ***control sequence*** `~s` does to a list of nested lists:

```erlang
57> io:format("~s~n", [ ["hello", 97] ]).   %97 is an inteer, which is also the ascii code for the character 'a'.
helloa
ok
```

Remember, `["hello", 97]` is equivalent to `[ [104,101,108,108,111], 97]`:

```erlang
59> io:format("~w~n", ["hello"]).  %A quick way to get all the ascii codes for a string.
[104,101,108,108,111]
ok

60> [ [104,101,108,108,111], 97].
["hello",97]

62> Nested = [ [104,101,108,108,111], 97].
["hello",97]

63> io:format("~s~n", [Nested]).
helloa
ok
```

A hah!  The control sequence `~s` will take a list of nested lists and concatenate them all into a single string, i.e. a flat list with no nested lists!  That is what I used to display the return value of `my_date_string()`.  (Edit: Alright, I decided to flatten the list myself. My `flatten()` function is at the end).

The control sequences are described in the [io:format/2 docs](http://erlang.org/doc/man/io.html#format-2).  The general form of a control sequence is:

    ~Width.Precision.PaddingModControl.
    
 To represent hours like this:
 
     1:09:05
     
I used a field Width of 2; nothing for the Precision because I don't think it applies to integers; 0 for the padding; nothing for Mod; and `w` for the Control character, giving me:

    ~2..0w
    
If you don't use a formating sequence like that, and the time has single digits in it, you will get output like this:
 
    1:9:5
 
 You can see that by altering `my_date_string()`:
 ```C
 my_date_string() ->
    %{Y, Mon, D} = date(),
    %{H, M, S} = time(),
    io_lib:format("~w-~w-~w ~w:~2..0w:~2..0w", [Y, Mon, D, 1, 9, 5] ).  % ***Hard coded single digits for the time****

```

I decided I liked the way:

    1:09:05
    
looked better than:

    01:09:05
    
so I didn't use a field width, etc. for the hours.

---------------------Edit------------
```erlang
my_date_string() ->
    {Y, Month, D} = date(),
    {H, M, S} = time(),
    Result = io_lib:format("~w-~w-~w ~w:~2..0w:~2..0w", [Y, Month, D, H, M, S] ),
    flatten(Result).
    
flatten(L) -> 
    Result = flatten_acc(L, []),
    lists:reverse(Result).

flatten_acc([H|T], Acc) ->
    if
        is_list(H)  -> flatten_acc(T, flatten_acc(H, Acc));
        true        -> flatten_acc(T, [H|Acc])
    end;    
flatten_acc([], Acc) ->
    Acc.
```



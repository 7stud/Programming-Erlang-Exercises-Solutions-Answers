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

If you think about it a little bit, when the Start time is something like `{X, Y, 999999}`  and the end time is `{X, Y+1, 200}`, i.e the function F took 201 micro seconds to execute, and you subtranct 200 - 999999, like the code does, then you will get a negative number, -999799, for the number of micro seconds the function took to execute.

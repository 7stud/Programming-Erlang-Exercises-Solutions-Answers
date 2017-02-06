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

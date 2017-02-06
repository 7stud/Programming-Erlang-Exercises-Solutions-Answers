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

i.e the function F took 201 micro seconds to execute, and then you subtract the elements of the two tuples, you will get:

```
Start:  {X, Y, 999999}  
End:    {X, Y+1, 200}
------------------------
        {0, 1, -999799}
```

What you need to do, is borrow 1 from the Secs and add it to the the Micros:

`{0, 0, 1*1000000 + (-999799)  = {0, 0, 201}`

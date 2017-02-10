Here is my myfile:read() function:

```erlang
-module(myfile).
-compile(export_all).

read(Fname) ->
    case file:read_file(Fname) of
        {ok, Bin}       -> Bin;
        {error, Why}    ->
            error(lists:flatten(io_lib:format(
                "Couldn't read file ~s: ~w",
                [Fname, Why]
            )))
    end.
```

`io_lib:format()` is to sprintf() as `io:format()` is to printf(), i.e. `io_lib:format()` returns a formatted string rather than sending it to stdout.  

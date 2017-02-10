Here's `myfile:read()`:

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

`io_lib:format()` is to sprintf() as `io:format()` is to printf(), i.e. `io_lib:format()` returns a formatted string rather than sending it to stdout.   The problem with `io_lib:format()` is that it returns a list of nested lists, which won't readily display as a string unless you use `io:format("~s", [FormattedString])` on it.  Luckily, `lists:flatten(DeepList)` will flatten a nested list, which the shell then will display as a string if all the integers in the list happen to be the ascii codes for printable characters.

In the shell:
```erlang
4> c(myfile).        
{ok,myfile}

5> myfile:read("non_existant").
** exception error: "Couldn't read file non_existant: enoent"
     in function  myfile:read/1 (myfile.erl, line 8)
     
6> myfile:read("out.txt").     
<<"goodbye mars: \"hello world\"\n">>
```

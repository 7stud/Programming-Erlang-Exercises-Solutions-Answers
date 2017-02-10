On p. 97, the book says that detailed error messages should go to a log file.  Great, more file I/O without any instruction!  Here is the format for the log file that I settled on:

```C
$ cat mylog.log
2017-2-9 21:56:05
Error: a
Stack trace:
[{e,gen_e,1,[{file,"e.erl"},{line,10}]},
 {e,demo3,0,[{file,"e.erl"},{line,15}]},
 {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,661}]},
 {shell,exprs,7,[{file,"shell.erl"},{line,684}]},
 {shell,eval_exprs,7,[{file,"shell.erl"},{line,639}]},
 {shell,eval_loop,3,[{file,"shell.erl"},{line,624}]}]
---
2017-2-9 21:56:06
Error: a
Stack trace:
[{e,gen_e,1,[{file,"e.erl"},{line,10}]},
 {e,demo3,0,[{file,"e.erl"},{line,15}]},
 {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,661}]},
 {shell,exprs,7,[{file,"shell.erl"},{line,684}]},
 {shell,eval_exprs,7,[{file,"shell.erl"},{line,639}]},
 {shell,eval_loop,3,[{file,"shell.erl"},{line,624}]}]
---
2017-2-9 21:56:07
Error: a
Stack trace:
[{e,gen_e,1,[{file,"e.erl"},{line,10}]},
 {e,demo3,0,[{file,"e.erl"},{line,15}]},
 {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,661}]},
 {shell,exprs,7,[{file,"shell.erl"},{line,684}]},
 {shell,eval_exprs,7,[{file,"shell.erl"},{line,639}]},
 {shell,eval_loop,3,[{file,"shell.erl"},{line,624}]}]
---


```

Here's the format string I used:

```erlang
           error (might not be something convertible to a string, so I used ~w)
             |
             V
"~s~nError: ~w~nStack trace:~n~p~n---~n",
  ^                            ^
  |                            |
timestamp                stacktrace(~p breaks up long lines at sensible places)
```

We had to write a function called `my_date_string()` for Exercise 4 in Chapter 4, and I used that function for the timestamp in the log file.  Here is the code:

```erlang
-module(e).
-compile(export_all).
-import(lib_misc, [my_date_string/0]).


gen_e(1) -> a;
gen_e(2) -> throw(a);
gen_e(3) -> exit(a);
gen_e(4) -> {'EXIT', a};
gen_e(5) -> error(a).


demo3() ->
    try gen_e(5)
    catch
        error:X ->
            case file:open("mylog.log", [append]) of
                {ok, F} -> 
                    io:format(
                      F, 
                      "~s~nError: ~w~nStack trace:~n~p~n---~n",
                      [my_date_string(), X, erlang:get_stacktrace()]
                    ),
                    file:close(F),

                    lists:flatten(
                      io_lib:format("There was an error: ~p", [X])
                    )
            end
    end.
    
```

In the shell:
```erlang
85> c(e).     
{ok,e}

86> e:demo3().
"There was an error: a"

87> e:demo3().
"There was an error: a"

88> e:demo3().
"There was an error: a"
```

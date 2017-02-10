On p. 97, the book says that detailed error messages should go to a log file.  Great, more file I/O without any instruction!  Here is the format for the output to the log file that I settled on:

```C
$ cat mylog.log
2017-2-9 21:15:15
Error: a
Stack trace:
[{e,gen_e,1,[{file,[101,46,101,114,108]},{line,10}]},{e,demo3,0,[{file,[101,46,101,114,108]},{line,15}]},{erl_eval,do_apply,6,[{file,[101,114,108,95,101,118,97,108,46,101,114,108]},{line,661}]},{shell,exprs,7,[{file,[115,104,101,108,108,46,101,114,108]},{line,684}]},{shell,eval_exprs,7,[{file,[115,104,101,108,108,46,101,114,108]},{line,639}]},{shell,eval_loop,3,[{file,[115,104,101,108,108,46,101,114,108]},{line,624}]}]
---
2017-2-9 21:15:16
Error: a
Stack trace:
[{e,gen_e,1,[{file,[101,46,101,114,108]},{line,10}]},{e,demo3,0,[{file,[101,46,101,114,108]},{line,15}]},{erl_eval,do_apply,6,[{file,[101,114,108,95,101,118,97,108,46,101,114,108]},{line,661}]},{shell,exprs,7,[{file,[115,104,101,108,108,46,101,114,108]},{line,684}]},{shell,eval_exprs,7,[{file,[115,104,101,108,108,46,101,114,108]},{line,639}]},{shell,eval_loop,3,[{file,[115,104,101,108,108,46,101,114,108]},{line,624}]}]
---
2017-2-9 21:15:16
Error: a
Stack trace:
[{e,gen_e,1,[{file,[101,46,101,114,108]},{line,10}]},{e,demo3,0,[{file,[101,46,101,114,108]},{line,15}]},{erl_eval,do_apply,6,[{file,[101,114,108,95,101,118,97,108,46,101,114,108]},{line,661}]},{shell,exprs,7,[{file,[115,104,101,108,108,46,101,114,108]},{line,684}]},{shell,eval_exprs,7,[{file,[115,104,101,108,108,46,101,114,108]},{line,639}]},{shell,eval_loop,3,[{file,[115,104,101,108,108,46,101,114,108]},{line,624}]}]
---
```

For Exercise 4 in Chapter 4, we had to write a function called `my_date_string()`.  I used that for the timestamp in the log file.  Here is the code:

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
                    Msg = erlang:list_to_binary(
                        io_lib:format(
                            "~s~nError: ~w~nStack trace:~n~w~n---~n", 
                            [my_date_string(), X, erlang:get_stacktrace()] 
                         )
                    ),
                    file:write(F, Msg),
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

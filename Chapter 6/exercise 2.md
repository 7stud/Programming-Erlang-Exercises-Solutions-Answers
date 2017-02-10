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

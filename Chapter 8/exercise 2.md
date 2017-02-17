Which module exports the most functions?
```erlang
-module(my).
-compile(export_all).

most_exports(Modules) ->
    most_exports(Modules, #{count => 0, name => []} ).

most_exports([{Module, _} | Modules], MaxCount) ->
    #{count := Max} = MaxCount,  %I wonder which is faster: maps:get() or pattern matching?  You should be able to write: MaxCount#{count}
    ExportCount = length( Module:module_info(exports) ),

    if 
        ExportCount > Max ->    %then replace count and name list in the map...
            NewMaxCount = MaxCount#{count := ExportCount, name := [Module]},
            most_exports(Modules, NewMaxCount);

        ExportCount =:= Max ->  %then add the Module to the name list in the map...
            NameList = maps:get(name, MaxCount),
            NewMaxCount = MaxCount#{
                name := [Module|NameList]
            },
            most_exports(Modules, NewMaxCount);

        ExportCount < Max ->    %then do nothing to the map...
            most_exports(Modules, MaxCount)
    end;
most_exports([], MaxCount) ->
    #{count := Max, name := Names} = MaxCount,
    {Max, Names}.

  ```

In the shell:
```erlang
154> c(mod1).
{ok,mod1}

%-module(mod1).
%-compile(export_all).
%
%t1() -> hello.
%t2() -> goodbye.
%t3() -> world.

155> c(mod2).
{ok,mod2}

%-module(mod2).
%-compile(export_all).
%
%t1() -> a.
%t2() -> b.
%t3() -> c.

156> my:most_exports([{mod1, blah}, {mod2, bleh}]).
{5,[mod2,mod1]}   % Five?

157> mod1:module_info(exports).
[{t1,0},{t2,0},{t3,0},{module_info,0},{module_info,1}]   %Ah, I remember the chapter mentioning this-- 
                                                         %it's at the top of p. 120.
158> my:most_exports(code:all_loaded()).
{310,[erlang]}
```

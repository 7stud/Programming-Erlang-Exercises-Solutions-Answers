I'm still hooked on maps! Which module exports the most functions:
```erlang
-module(my).
-compile(export_all).

most_exports(Modules) ->
    count_exports(Modules, #{count => 0, name => []} ).

count_exports([{Module, _} | Modules], MaxCount) ->
    #{count := Max} = MaxCount,
    ModCount = length( Module:module_info(exports) ),

    if 
        ModCount > Max ->  %then replace count and name list...
            NewMaxCount = MaxCount#{count := ModCount, name := [Module]},
            count_exports(Modules, NewMaxCount);
        ModCount =:= Max ->  %then add the Module to the name list...
            NameList = maps:get(name, MaxCount),
            NewMaxCount = MaxCount#{
                name := [Module|NameList]
            },
            count_exports(Modules, NewMaxCount);
        ModCount < Max ->  %then do nothing...
            count_exports(Modules, MaxCount)
    end;
count_exports([], MaxCount) ->
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

156> my:most_exports([{mod1, blah}, {mod2, blah}]).
{5,[mod2,mod1]}   % Five?

157> mod1:module_info(exports).
[{t1,0},{t2,0},{t3,0},{module_info,0},{module_info,1}]   %Ah, I remember the chapter mentioning this-- 
                                                         %it's at the top of p. 120.

158> my:most_exports(code:all_loaded()).
{310,[erlang]}
```
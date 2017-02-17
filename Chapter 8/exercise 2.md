--**Which module exports the most functions?**
```erlang
-module(my).
-compile(export_all).

most_exports(Modules) ->
    most_exports(Modules, #{count => 0, name => []} ).

most_exports([{Module, _} | Modules], MaxMap) ->
    #{count := Max} = MaxMap,  %I wonder which is faster: maps:get() or pattern matching?  You should be able to write: MaxMap#{count}
    ExportCount = length( Module:module_info(exports) ),

    if 
        ExportCount > Max ->    %then replace count and name list in the map...
            NewMaxMap = MaxMap#{count := ExportCount, name := [Module]},
            most_exports(Modules, NewMaxMap);

        ExportCount =:= Max ->  %then add the Module to the name list in the map...
            NameList = maps:get(name, MaxMap),
            NewMaxMap = MaxMap#{name := [Module|NameList]},
            most_exports(Modules, NewMaxMap);

        ExportCount < Max ->    %then do nothing to the map...
            most_exports(Modules, MaxMap)
    end;
most_exports([], MaxMap) ->
    #{count := Max, name := Names} = MaxMap,
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

--**What is the most common function name that is exported?**
```erlang
-module(my).
-compile(export_all).

most_cmn_export(Modules) ->
    most_cmn_export(Modules, #{}).

most_cmn_export([ {Module, _} | Modules ], CountMap) ->
    Exports = Module:module_info(exports),
    NewCountMap = add_names(Exports, CountMap),
    most_cmn_export(Modules, NewCountMap);
most_cmn_export([], CountMap) ->
    %io:format("~p~n", [CountMap]),
    get_max(maps:to_list(CountMap), #{count => 0, name => []} ).
    
add_names([{Func, _}|Funcs], CountMap) ->
    Count = maps:get(Func, CountMap, 0),
    NewCountMap = CountMap#{Func => Count+1},
    add_names(Funcs, NewCountMap);
add_names([], CountMap) ->
    CountMap.


get_max([ {Name, Count} | Tuples ], CountMap) ->
    Max = maps:get(count, CountMap),

    if
        Count > Max -> 
            get_max(Tuples, CountMap#{count := Count, name := Name} );

        Count =:= Max ->
            Name = maps:get(name, CountMap),
            get_max(Tuples, CountMap#{name := [Name|name]} );

        Count < Max ->
            get_max(Tuples, CountMap)
    end;
get_max([], CountMap) ->
    #{count := Max, name := Name} = CountMap,
    {Max, Name}.
```

Here's an alternative implementation of the base case:
```erlang
most_cmn_export([], CountMap) ->
    lists:nth(1, lists:sort(
      fun(T1, T2) -> element(2, T1) >= element(2, T2) end,
      maps:to_list(CountMap)
    )).
```
Sorting seems like it would be less efficient, so I avoided it in my original answer.

--**Which functions are unique?**

I needed the same CountMap produced by the previous code.  Rather then repeating the code to construct the CountMap in this solution, I decided to refactor the previous solution, so that I could use the part that constructs the CountMap in this solution:

```erlang
most_cmn_export(Modules) ->
    CountMap = get_count_map(Modules, #{}),
    CountList = maps:to_list(CountMap),
    get_max(CountList, #{count => 0, name => []}).

get_count_map([ {Module, _} | Modules ], CountMap) ->
    Exports = Module:module_info(exports),
    NewCountMap = add_names(Exports, CountMap),
    get_count_map(Modules, NewCountMap);
get_count_map([], CountMap) ->
    %io:format("~p~n", [CountMap]),
    %get_max(maps:to_list(CountMap), #{count => 0, name => []} ).
    CountMap.

...
...
...
```

Now, here's the solution for getting the unique functions in the specified modules:
```erlang
unique_funcs(Modules) ->
    CountMap = get_count_map(Modules, #{}),
    Uniques = maps:filter(
      fun(_Name, Count) -> Count =:= 1 end,
      CountMap
    ),
    maps:keys(Uniques).

 ```
 
 Or, with a list comprehension:
 ```erlang
 unique_funcs(Modules) ->
    [
      Name || {Name, Count} <- maps:to_list( get_count_map(Modules, #{}) ),
      Count =:= 1
    ].
 ``







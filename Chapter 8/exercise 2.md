--**Which module exports the most functions?**
```erlang
-module(my).
-compile(export_all).

most_exports(Modules) ->
    most_exports(Modules, #{func_count => 0, module => []} ).

most_exports([{Module, _} | Modules], MaxMap) ->
    #{func_count := Max} = MaxMap,  %I wonder which is faster: maps:get() or pattern matching?  You should be able to write: MaxMap#{count}
    ExportCount = length( Module:module_info(exports) ),

    if 
        ExportCount > Max ->    %then replace func_count and module list in the map...
            NewMaxMap = MaxMap#{func_count := ExportCount, module := [Module]},
            most_exports(Modules, NewMaxMap);

        ExportCount =:= Max ->  %then add the Module to the module list in the map...
            ModuleList = maps:get(module, MaxMap),
            NewMaxMap = MaxMap#{module := [Module|ModuleList]},
            most_exports(Modules, NewMaxMap);

        ExportCount < Max ->    %then do nothing to the map...
            most_exports(Modules, MaxMap)
    end;
most_exports([], MaxMap) ->
    #{func_count := Max, module := Module} = MaxMap,
    {Max, Module}.

  ```
In case there's a tie, I used a list for the module name in the MaxMap.  The middle if-clause handles a tie.


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
{325,[erlang]}
```

--**What is the most common function name that is exported?**
```erlang
-module(test).
-compile(export_all).

most_cmn_export(Modules) ->
    most_cmn_export(Modules, #{}).

most_cmn_export([ {Module, _} | Modules ], FuncCountMap) ->
    FuncNames = Module:module_info(exports),
    NewFuncCountMap = add_names(FuncNames, FuncCountMap),
    most_cmn_export(Modules, NewFuncCountMap);
most_cmn_export([], FuncCountMap) ->
    %io:format("~p~n", [CountMap]),
    get_max(maps:to_list(FuncCountMap), #{count => 0, func_name => []} ).

add_names([FuncName|FuncNames], FuncCountMap) ->
    Count = maps:get(FuncName, FuncCountMap, 0),
    NewFuncCountMap = FuncCountMap#{FuncName => Count+1},
    add_names(FuncNames, NewFuncCountMap);
add_names([], FuncCountMap) ->
    FuncCountMap.


get_max([ {FuncName, FuncCount} | Tail ], MaxMap) ->
    Max = maps:get(count, MaxMap),

    if
        FuncCount > Max -> 
            get_max(Tail, MaxMap#{count := FuncCount, func_name := [FuncName]} );

        FuncCount =:= Max ->
            FuncNameList = maps:get(func_name, MaxMap),
            get_max(Tail, MaxMap#{func_name := [FuncName|FuncNameList]} );

        FuncCount < Max ->
            get_max(Tail, MaxMap)
    end;
get_max([], MaxMap) ->
    #{count := Max, func_name := FuncName} = MaxMap,
    {Max, FuncName}.
```

In the shell:
```erlang
1> c(test).
{ok,test}

2> c(mod1).
{ok,mod1}

%-module(mod1).
%-compile(export_all).
%
%t1() -> hello.
%t2() -> goodbye.
%t3() -> world.

3> c(mod2).
{ok,mod2}

%-module(mod2).
%-compile(export_all).
%
%t1() -> a.
%t2() -> b.
%t3() -> c.

4> test:most_cmn_export([{mod1, blah}, {mod2, bleh}]).
{2,[{t3,0},{t2,0},{t1,0},{module_info,1},{module_info,0}]}

5> test:most_cmn_export(code:all_loaded()). 
{122,[{module_info,1},{module_info,0}]}
```

The if-expression in this solution is very similar to the if-expression in the previous solution.  Therefore, I endeavored to refactor the previous solution to create a generic function that encapsulated the if-expression, then I could call that function again in this solution. Here's what I came up with:

```erlang
update(MaxMap, NewItem, NewItemCount) ->
    #{count := CurrentMax} = MaxMap,

    if 
        NewItemCount > CurrentMax ->    %then replace the count and the item list in the map...
            MaxMap#{count := NewItemCount, item := [NewItem]};

        NewItemCount =:= CurrentMax ->  %then add the item to the item list in the map...
            ItemList = maps:get(item, MaxMap),
            MaxMap#{item := [NewItem|ItemList]};

        NewItemCount < CurrentMax ->    %then do nothing to the map...
            MaxMap
    end.
```
The ```update()``` function requires that a MaxMap use the generic keys: count and item.  Here's the previous solution refactored to use the ```update()``` function:

```erlang
-module(my).
-compile(export_all).

most_exports(Modules) ->
    most_exports(Modules, #{count => 0, item => []} ).

most_exports([{Module, _} | Modules], MaxMap) ->
    ExportCount = length( Module:module_info(exports) ),
    NewMaxMap = update(MaxMap, Module, ExportCount),
    most_exports(Modules, NewMaxMap);
most_exports([], MaxMap) ->
    #{count := Max, item := Module} = MaxMap,
    {Max, Module}.


update(MaxMap, NewItem, NewItemCount) ->
    #{count := CurrentMax} = MaxMap,

    if 
        NewItemCount > CurrentMax ->    %then replace the count and the item list in the map...
            MaxMap#{count := NewItemCount, item := [NewItem]};

        NewItemCount =:= CurrentMax ->  %then add the item to the item list in the map...
            ItemList = maps:get(item, MaxMap),
            MaxMap#{item := [NewItem|ItemList]};

        NewItemCount < CurrentMax ->    %then do nothing to the map...
            MaxMap
    end.
```    

Now, I can employ the `update()` function in this solution:

```erlang
-module(my).
-compile(export_all).

most_cmn_export(Modules) ->
    FuncCountMap = create_func_count_map(Modules, #{}),
    FuncCountList = maps:to_list(FuncCountMap),
    get_max(FuncCountList, #{count => 0, item => []}).

create_func_count_map([ {Module, _} | Modules ], FuncCountMap) ->
    FuncNames = Module:module_info(exports),
    NewFuncCountMap = add_names(FuncNames, FuncCountMap),
    create_func_count_map(Modules, NewFuncCountMap);
create_func_count_map([], FuncCountMap) ->
    %io:format("~p~n", [FuncCountMap]),
    FuncCountMap.
 
add_names([{Func, _}|Funcs], FuncCountMap) ->
    FuncCount = maps:get(Func, FuncCountMap, 0),
    NewFuncCountMap = FuncCountMap#{Func => FuncCount+1},
    add_names(Funcs, NewFuncCountMap);
add_names([], FuncCountMap) ->
    FuncCountMap.

get_max([ {FuncName, FuncCount} | Tuples ], MaxMap) ->
    NewMaxMap = update(MaxMap, FuncName, FuncCount),
    get_max(Tuples, NewMaxMap);
get_max([], MaxMap) ->
    #{count := Max, item := FuncName} = MaxMap,
    {Max, FuncName}.

```

--**Which functions are unique?**

I needed the same CountMap produced by the previous solution.  Rather then repeating the code to construct a CountMap in this solution, I decided to refactor the previous solution so that I could call a function to get the CountMap:

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
 
 Or, using a list comprehension:
 ```erlang
 unique_funcs(Modules) ->
    [
      Name || {Name, Count} <- maps:to_list( get_count_map(Modules, #{}) ),
      Count =:= 1
    ].
 ```







The `map_search_pred()` function:

-module(my).
-compile(export_all).

map_search_pred(Map, Pred) ->
    find(maps:keys(Map), Pred, Map).

find([Key|Keys], Pred, Map) ->
    Val = maps:get(Key, Map),

    case Pred(Key, Val) of
        true -> {Key, Val};
        false -> find(Keys, Pred, Map)
    end;
find([], _, _) ->
    none.

In the shell:

```erlang
62> c(my).
{ok,my}

63> f(Map).
ok

64> Map = #{
64> 1 => 10,
64> 2 => 20,
64> 3 => 3 
64> }.
#{1 => 10,2 => 20,3 => 3}

65> Pred = fun(X, Y) -> X =:= Y end.
#Fun<erl_eval.12.52032458>

68> my:map_search_pred(Map, Pred).
{3,3}

69> Pred2 = fun(X, Y) -> X+Y =:= 22 end.
#Fun<erl_eval.12.52032458>

70> my:map_search_pred(Map, Pred2).     
{2,20}

71> Pred3 = fun(X, Y) -> X*Y =:= 0 end.
#Fun<erl_eval.12.52032458>

72> my:map_search_pred(Map, Pred3).    
none
```

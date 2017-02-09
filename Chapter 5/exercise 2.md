The `map_search_pred()` function:
```erlang
-module(my).
-compile(export_all).

map_search_pred(Map, Pred) ->
    map_search_pred(maps:keys(Map), Pred, Map).

map_search_pred([Key|Keys], Pred, Map) ->
    Val = maps:get(Key, Map),

    case Pred(Key, Val) of
        true -> {Key, Val};
        false -> map_search_pred(Keys, Pred, Map)
    end;
map_search_pred([], _, _) ->
    none.

```

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

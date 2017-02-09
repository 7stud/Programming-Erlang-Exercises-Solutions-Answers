I know ruby pretty well, and once again, just like the python datetime exercise in Chapter 4, this is a vast exercise. A Hash is ruby's equivalent of a map in erlang.  One thing you have to know about ruby is that a *block* in ruby is similar to an anonymous function, which is equivalent to a fun in erlang.  So ruby methods that take a block are methods that accept an anonymous function as an argument.

Here is my implementation of Ruby's `Hash#any?` method, which is very similar to the `map_search_pred()` function in Exercise 2.  The `Hash#any?` method traverses a Hash looking for an element for which block(Key, Val) is true, and if found the method immediately returns true. 

```erlang
-module(my).
-compile(export_all).

any(Map, Pred) -> any(maps:keys(Map), Map, Pred).

any([Key|Keys], Map, Pred) ->
    Val = maps:get(Key, Map),
    
    case Pred(Key, Val) of
        true -> true;
        false -> any(Keys, Map, Pred)
    end;
any([], _, _) ->
    false.
```

In the shell:

```erlang
85> f().

86> Map = #{                            
86> 1 => 10,
86> 2 => 20,
86> 3 => 3 
86> }.
#{1 => 10,2 => 20,3 => 3}

87> c(my).

88> Pred1 = fun(X, Y) -> X =:= Y end.   
#Fun<erl_eval.12.52032458>

89> Pred2 = fun(X, Y) -> X+Y =:= 22 end.
#Fun<erl_eval.12.52032458>

90> Pred3 = fun(X, Y) -> X*Y =:= 0 end. 
#Fun<erl_eval.12.52032458>

93> my:any(Map, Pred1).                 
true

94> my:any(Map, Pred2).
true

95> my:any(Map, Pred3).
false
```



